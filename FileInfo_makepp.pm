package FileInfo;

use FileInfo;			# Override some subroutines from the
				# generic FileInfo package.
# $Id: FileInfo_makepp.pm,v 1.2.2.1 2003/07/25 03:32:50 grholt Exp $

#
# This file defines some additional subroutines for the FileInfo package that
# are useful only within makepp.  This allows FileInfo.pm to be used outside
# of makepp itself.
#

use strict;


$FileInfo::build_info_subdir = ".makepp";
				# The name of the subdirectory that we store
				# build information in.

@FileInfo::build_infos_to_update = ();
				# References to all the build info files that
				# have to be flushed to disk.

END { &update_build_infos; }

=head2 build_info_string

  my $string = $finfo->build_info_string("key");

Returns information about this file which was saved on the last build.  This
information is stored in a separate file, and is automatically invalidated
if the file it refers to has changed.  It is intended for remembering things
like the command used to build the file when it was last built, or the
signatures of the dependencies.

See also: set_build_info_string

=cut
sub build_info_string {
  my $finfo = $_[0];
  return undef unless $finfo->file_exists;

  my $binfo = $finfo->{BUILD_INFO};

  unless ($binfo) {		# We haven't loaded the build information?
    $binfo = load_build_info_file($finfo);
				# See if there's a build info file.  If we get
				# here, it wasn't available.
    $binfo ||= {};		# If we can't find any build information,
				# at least cache the failure so we don't try
				# again.
    $finfo->{BUILD_INFO} = $binfo;
  }

  return $binfo->{$_[1]};
}

=head2 get_rule

  my $rule = $finfo->get_rule;

Returns the rule to build the file, if there is one, or undef if there is none.

If an argument is present, get_rule tries harder to find a rule.  If there is
no rule currently available but no makefile has been read from that directory,
then it attempts to load a makefile from that directory.

=cut

sub get_rule {
  Makefile::implicitly_load($_[0]->{".."}); # Make sure we've loaded a makefile
				# for this directory.
  return $_[0]->{RULE};
}

=head2 exists_or_can_be_built

  if ($finfo->exists_or_can_be_built) { ... }

Returns true (actually, returns the FileInfo structure) if the file
exists and is readable, or does not yet exist but can be built.  This
function determines whether a file exists by checking the build
signature, not by actually looking in the file system, so if you set
up a signature function that can return a valid build signature for a
pseudofile (like a dataset inside an HDF file or a member of an
archive) then this function will return true.

If this is not what you want, then consider the function file_exists(), which
looks in the file system to see whether the file exists or not.

=cut

sub exists_or_can_be_built {
  my $finfo = $_[0];
  return undef if $finfo->{IS_PHONY}; # Never return phony targets.
  if (!$finfo->is_readable) {	# File exists?
    return undef if $finfo->{EXISTS}; # Can't be read--ignore it.  This is used
				# to inhibit imports from repositories.
  }

  return $finfo if $finfo->{EXISTS} || # We know it exists?
      $finfo->{ALTERNATE_VERSIONS} || # Exists in repository?
	$finfo->{ADDITIONAL_DEPENDENCIES} ||
				# For legacy makefiles, sometimes an idiom like
				# this is used:
				#   y.tab.c: y.tab.h
				#   y.tab.h: parse.y
				#       yacc -d parse.y
				# in order to indicate that the yacc command
				# has two targets.  We need to support this
				# by indicating that files with extra 
				# dependencies are buildable, even if there
				# isn't an actual rule for them.
	  $finfo->get_rule ||	# Rule for building it?
	    $finfo->signature;	# Has a valid signature.
  return undef;
};

=head2 flush_build_info

  clean_fileinfos($dirinfo)

Discards all the build information for all files in the given directory
after making sure they've been written out to disk.  Also discards all
FileInfos for files which we haven't tried to build and don't have a
build rule.

=cut
sub clean_fileinfos {
#
# For some reason, the code below doesn't actually save very much memory at
# all, and it occasionally causes problems like extra rebuilds or 
# forgetting about rules for some targets.  I don't understand how this
# is possible, but it happened.
#

#   my $dirinfo = $_[0];		# Get the directory.

#   &update_build_infos;		# Make sure everything's written out.
#   my ($fname, $finfo);

#   my @deletable;

#   while (($fname, $finfo) = each %{$dirinfo->{DIRCONTENTS}}) {
# 				# Look at each file:
#     delete $finfo->{BUILD_INFO}; # Build info can get pretty large.
#     delete $finfo->{LSTAT};	# Toss this too, because we probably won't need
# 				# it again.
#     $finfo->{DIRCONTENTS} and clean_fileinfos($finfo);
# 				# Recursively clean the whole tree.
#     next if exists $finfo->{BUILD_HANDLE}; # Don't forget the files we tried to build.
#     next if $finfo->{RULE};	# Don't delete something with a rule.
#     next if $finfo->{DIRCONTENTS}; # Don't delete directories.
#     next if $finfo->{ALTERNATE_VERSIONS}; # Don't delete repository info.
#     next if $finfo->{IS_PHONY};
#     next if $finfo->{ADDITIONAL_DEPENDENCIES}; # Don't forget info about
# 				# extra dependencies, either.
#     next if $finfo->{TRIGGERED_WILD}; # We can't delete it if reading it back
# 				# in will trigger a wildcard routine again.
#     if ($fname eq 'all') {
#       warn("I'm deleting all now!!!\n");
#     }
#     push @deletable, $fname;	# No reason to keep this finfo structure
# 				# around.  (Can't delete it, though, while
# 				# we're in the middle of iterating.)
#   }
#   if (@deletable) {		# Something to delete?
#     delete @{$dirinfo->{DIRCONTENTS}}{@deletable}; # Get rid of all the unnecessary FileInfos.
#     delete $dirinfo->{READDIR};	# We might need to reread this dir.
#   }
}


=head2 move_or_link_target

  $status = $finfo->move_or_link_target($repository_file);

Links a file in from a repository or variant build cache into the current
directory.

Returns 0 if successful, nonzero if something went wrong.

=cut

$FileInfo::made_temporary_link = 0; # True if we made any temporary links.

sub move_or_link_target {
  my ($dest_finfo, $src_finfo) = @_;

  if ($dest_finfo->{DIRCONTENTS}) { # Is this a directory?
    $dest_finfo->mkdir;         # Just make it, don't soft link to it.
    return;
  }

  ++$FileInfo::made_temporary_link; # Remember to undo this temporary link.

  $dest_finfo->{".."}->mkdir;	# Make the directory (and its parents) if
				# it doesn't exist.

  $main::log_level and
    main::print_log("Linking repository file ",
		    $src_finfo->name,  " to ", $dest_finfo->name);

  if ($dest_finfo->is_symbolic_link) { # If it's already a symbolic link,
				# maybe it's correct.
    $dest_finfo->{LINK_DEREF} or $dest_finfo->dereference;
				# Get the link value.
    $dest_finfo->{LINK_DEREF} == $src_finfo and return 0;
				# If it's already right, don't do anything.
  }
      

  my $binfo = $src_finfo->{BUILD_INFO} ||
    load_build_info_file($src_finfo) || {};
				# Get the build information for the old file.
  my %build_info = %$binfo;	# Make a copy of everything.
  $build_info{FROM_REPOSITORY} = $src_finfo->relative_filename($dest_finfo->{".."});
				# Remember that we got it from a repository.
  $dest_finfo->{NEEDS_BUILD_UPDATE} = 1; # Haven't updated the build info.
  $dest_finfo->{BUILD_INFO} = \%build_info;
  push @FileInfo::build_infos_to_update, $dest_finfo;
				# Update it soon.
  update_build_infos();		# Update it now.  This way, the file is marked
				# as coming from a repository even if the
				# build command is aborted.  Next time around
				# we'll know that it came from a repository
				# and we can delete it appropriately.

  $dest_finfo->unlink;		# Get rid of anything that might already
				# be there.
  eval { $dest_finfo->symlink($src_finfo); }; # Make the link.
  if ($@) {			# Did something go wrong?
    main::print_error("Error linking ", $src_finfo->name, " to ",
		      $dest_finfo->name, ":\n$@");
    return 1;			# Indicate failure.
  }
  $dest_finfo->may_have_changed; # Flush the stat array cache.


  return 0;			# Indicate success.
}

#
# For aesthetic reasons, we need to get rid of all our temporary symbolic
# links when everything is done.
#
sub cleanup_temporary_links {
  return unless $FileInfo::made_temporary_link; # Don't bother if there weren't
				# any repository files used.

  traverse sub {		# Look at every file we know anything about.
    foreach my $finfo (@_) {
      my $binfo = $finfo->{BUILD_INFO};	# Does this file have build info?
      next unless $binfo;	# If not, it wasn't linked from a repository.
      if ($binfo->{FROM_REPOSITORY}) {
				# Did we get the file from the repository?
	$finfo->unlink;		# Discard the file.
      }
    }
  };
}

=head2 name

  $string = $finfo->name;
  $string = $finfo->name($cwd);

Returns the name of the file.  With no extra argument, returns the absolute
filename; with the argument, returns the filename relative to that
directory.

=cut
sub name {
  ref($_[1]) eq 'FileInfo' and return &relative_filename;
  &absolute_filename;
}

=head2 set_additional_dependencies

  $finfo->set_additional_dependencies($dependency_string, $makefile, $makefile_line);

Indicates that the list of objects in $dependency_string are extra dependencies
of the file.  These dependencies are appended to the list of dependencies
from the rule to build the file.  $makefile and $makefile_line are used only
when we have to expand the list of dependencies.  We can't do this until we
actually need to make the file, because we might not be able to expand
wildcards or other things properly.

=cut

sub set_additional_dependencies {
  my ($finfo, $dependency_string, $makefile, $makefile_line) = @_;

  push(@{$finfo->{ADDITIONAL_DEPENDENCIES}},
       [ $dependency_string, $makefile, $makefile_line ]);
				# Store a copy of this information.
  $finfo->publish;		# For legacy makefiles, sometimes an idiom like
				# this is used:
				#   y.tab.c: y.tab.h
				#   y.tab.h: parse.y
				#       yacc -d parse.y
				# in order to indicate that the yacc command
				# has two targets.  We need to support this
				# by indicating that files with extra 
				# dependencies are buildable, even if there
				# isn't an actual rule for them.
}

=head2 set_build_info_string

  $finfo->set_build_info_string($key, $value, $key, $value, ...);

Sets the build info string for the given key(s).  This can be read back in 
later or on a subsequent build by build_info_string().

You should call update_build_infos() to flush the build information to disk,
or else it will never be stored.  It's a good idea to call
update_build_infos() fairly frequently, so that nothing is lost in the case of
a machine crash or someone killing your program.

=cut
sub set_build_info_string {
  my $finfo = shift @_;

  my $binfo = $finfo->{BUILD_INFO}; # Access the build information.
  unless ($binfo) {		# No known build information?
    $binfo = $finfo->{BUILD_INFO} = load_build_info_file($finfo) || {};
				# Load the old information from disk, if we
				# haven't already, so if there are fields we
				# aren't overriding, these are preserved.
  }

  while (@_) {
    my $key = shift @_;
    my $val = shift @_;

    $binfo->{$key} = $val;
  }
  $finfo->{NEEDS_BUILD_UPDATE} = 1; # Remember that we haven't updated this
				# file yet.
  push @FileInfo::build_infos_to_update, $finfo;
}

=head2 set_rule

  $finfo->set_rule($rule);

Sets a rule for building the specified file.  If there is already a rule,
which rule overrides is determined by the following procedure:

=over 4

=item 1.

A rule that recursively invokes make never overrides any other rule.
This is a hack necessary to deal with some legacy makefiles which have
rules for targets that actually invoke the proper rule in some other
makefile, something which is no longer necessary with makepp.

=item 2.

If either rule is an explicit rule, and not a pattern rule or a backward
inference rule, then the explicit rule is used.  If both rules are
explicit rules, then this is an error.

Note that a pattern rule which is specified like this:

  %.o: %.c : foreach abc.c def.c ghi.c

where no wildcards are involved is treated as an explicit rule for
abc.o, def.o, and ghi.o.

=item 3.

A pattern rule overrides a backward inference rule.  (This should never
happen, since backward inference rules should only be generated if no pattern
rule exists.)

=item 4.

A pattern rule from a "nearer" makefile overrides one from a "farther"
makefile.  Nearness is determined by the length of the relative file
name of the target compared to the makefile's cwd.

=item 5.

A pattern rule seen later overrides one seen earlier.  Thus more specific
pattern rules should be placed after the more general pattern rules.

=item 6.

A builtin rule is always overridden by any other kind of rule, and never
overrides anything.

=back

=cut
sub set_rule {
  my ($finfo, $rule) = @_; # Name the arguments.

  if (!defined($rule)) {	# Are we simply discarding the rule now to
				# save memory?  (There's no point in keeping
				# the rule around after we've built the thing.)
    delete $finfo->{RULE};
    return;
  }	

  my $rule_is_default = ($rule->source =~ /\bmakepp_builtin_rules\.mk:/);

  $finfo->{IS_PHONY} &&		# If we know this is a phony target, don't
    $rule_is_default and	# ever let a default rule attempt to build it.
      return;

  my $oldrule = $finfo->{RULE};	# Is there a previous rule?

  if ($oldrule && $oldrule->{LOAD_IDX} < $oldrule->{MAKEFILE}{LOAD_IDX}) {
    $oldrule = undef;		# If the old rule is from a previous load
				# of a makefile, discard it without comment.
    delete $finfo->{BUILD_HANDLE}; # Avoid the warning message below.  Also,
				# if the rule has genuinely changed, we may
				# need to rebuild.
  }

  if ($oldrule) {
    my $oldrule_is_default = ($oldrule->source =~ /\bmakepp_builtin_rules\.mk:/);
    if ($rule_is_default) {	# Never let a builtin rule override a rule
      return;			# in the makefile.
    }
    elsif (!$oldrule_is_default) { # If the old rule isn't a default rule:
      $main::log_level and
	main::print_log("Alternate rules (", $rule->source, " and ",
			$oldrule->source, ") for target ",
			$finfo->name);

      my $new_rule_recursive = ($rule->{COMMAND_STRING} || '') =~ /\$[\(\{]MAKE[\)\}]/;
      my $old_rule_recursive = ($oldrule->{COMMAND_STRING} || '') =~ /\$[\(\{]MAKE[\)\}]/;
				# Get whether the rules are recursive.

      if ($new_rule_recursive && !$old_rule_recursive) {
				# This rule does not override anything if
				# it invokes a recursive make.
	$main::log_level and 
	  main::print_log(" Rule ", $rule->source,
			  " ignored because it invokes \$(MAKE)");
	return;
      }

      if ($old_rule_recursive && !$new_rule_recursive) {
	$main::log_level and 
	  main::print_log(" Rule ", $oldrule->source,
			  " discarded because it invokes \$(MAKE)");

	delete $finfo->{BUILD_HANDLE};
				# Don't give a warning message about a rule
				# which was replaced, because it's ok in this
				# case to use a different rule.
      }
      else {
	if (!$oldrule->{PATTERN_LEVEL}) { # Old rule not a pattern rule?
	  if ($rule->{PATTERN_LEVEL}) { # New rule is?
	    $main::log_level and
	      main::print_log(" Rule ", $rule->source, " ignored because it is a pattern rule");
	    return;
	  } else {
            main::print_error("warning: conflicting rules (", $rule->source,
                              " and ", $oldrule->source, " for target ",
                              $finfo->name);
	  }
          
	}			# End if old rule was an explicit rule.
        else {
          #
          # Apparently both are pattern rules.  Figure out which one should override.
          #
          if ($rule->{MAKEFILE} != $oldrule->{MAKEFILE}) { # Skip comparing
            # the cwds if they are from the same makefile.
            if (length($rule->build_cwd->relative_filename($finfo->{".."})) <
                length($oldrule->build_cwd->relative_filename($finfo->{".."}))) {
              $main::log_level and
                main::print_log(" Rule ", $rule->source,
                                " chosen because it is from a nearer makefile");
            } else {
              $main::log_level and
                main::print_log(" Rule ", $oldrule->source,
                                " kept because it is from a nearer makefile");
              return;
            }
          } else {              # If they're from the same makefile, use the
                                # one that has a shorter chain of inference.
            if (($rule->{PATTERN_LEVEL} || 0) < $oldrule->{PATTERN_LEVEL}) {
              $main::log_level and
                main::print_log(" Rule ", $rule->source,
                                " chosen because it has a shorter chain of inference");
            } elsif (($rule->{PATTERN_LEVEL} || 0) > $oldrule->{PATTERN_LEVEL}) {
              $main::log_level and
                main::print_log(" Rule ", $oldrule->source,
                                " chosen because it has a shorter chain of inference");
              return;
            } else {
              if ($rule->source eq $oldrule->source) {
                main::print_error("warning: rule ", $rule->source, " produces ", $finfo->name, " in two different ways");
              }
            }
          }
        }
      }
    }

  }

#
# If we get here, we have decided that the new rule (in $rule) should override
# the old one (if there is one).
#
#  $main::log_level and
#    main::print_log(0, $rule->source, " applies to target ", $finfo->name);

  $finfo->{RULE} = $rule;	# Store the new rule.
  $rule->{LOAD_IDX} = $rule->{MAKEFILE}{LOAD_IDX};
                                # Remember which makefile load it came from.

  if (exists $finfo->{BUILD_HANDLE} && !$finfo->{BUILT_WITH_NONDEFAULT}) {
    main::print_error("warning: I became aware of the rule ", $rule->source,
		      " for target ", $finfo->name,
		      " after I had already tried to build it using the default rules");
  }
  publish($finfo);		# Now we can build this file; we might not have
				# been able to before.
}

=head2 signature

   $str = $fileinfo->signature;

Returns the signature of this file.  The signature is usually the file time,
but users can change it to be anything they want.

Returns undef if the file doesn't exist.

=cut

sub signature {
  my $finfo = $_[0];
  return $finfo->{SIGNATURE} if $finfo->{SIGNATURE};
				# Return the cached value, if there is one.
  my $sigfunc = $finfo->{SIGNATURE_FUNC} || \&default_signature_func;

  my $sig = &$sigfunc($finfo);

  if ($sig) {
    publish($finfo);		# If we have a signature, activate any wildcard
				# routines eagerly waiting for the appearance
				# of this file.
    $finfo->{SIGNATURE} = $sig;	# Cache the signature.
  }	
  return $sig;
}

=head2 update_build_infos

  FileInfo::update_build_infos();

Flushes our cache of build information to disk.  You should call this fairly
frequently, or else if the machine crashes or some other bad thing happens,
some build information may be lost.

=cut
sub update_build_infos {

  local *BUILD_INFO;

  foreach my $finfo (@FileInfo::build_infos_to_update) {
    next unless $finfo->{NEEDS_BUILD_UPDATE};
				# Skip if we already updated it.  If two
				# build info strings for the same file are
				# changed, it can get on the list twice.
    delete $finfo->{NEEDS_BUILD_UPDATE}; # Do not update it again.
    my $build_info_subdir = $finfo->{".."}->absolute_filename_nolink .
      "/$FileInfo::build_info_subdir";

    my $build_info_subdir_info = file_info($FileInfo::build_info_subdir, $finfo->{".."});
    $build_info_subdir_info->mkdir; # Make sure the build info subdir exists.

    my $build_info_fname = "$build_info_subdir/$finfo->{NAME}";
				# Form the name of the build info file.

    my $build_info = $finfo->{BUILD_INFO}; # Access the hash.
    $build_info->{SIGNATURE} ||= $finfo->signature;
				# Make sure we have a valid signature.  Use
				# ||= instead of just = because when we're
				# called to write the build info for a file
				# from a repository, the build info is created
				# before the link to avoid the race condition
				# where a soft link is created and we are
				# interrupted before marking it as from a
				# repository.
    $build_info->{SIGNATURE} or next;
				# If the file has been deleted, don't bother
				# writing the build info stuff.
    open(BUILD_INFO, "> $build_info_fname") || next;
				# If we can't write it, don't bother saving it.
    my ($key, $val);

    while (($key, $val) = each %$build_info) {
      $val =~ s/([=\\\0-\037\177-\255])/"\\" . sprintf("%03o", ord($1))/eg;
      $key =~ s/([=\\\0-\037\177-\255])/"\\" . sprintf("%03o", ord($1))/eg;
				# Protect any special characters.
				# (This does not modify the value inside the
				# BUILD_INFO hash.)
      print BUILD_INFO "$key=$val\n";
    }
    close BUILD_INFO;
  }
  @FileInfo::build_infos_to_update = ();
				# Clean out the list of files to update.
}

###############################################################################
#
# Internal subroutines (don't call these):
#

#
# The default signature function.  This returns the file modification time.
#
# Arguments:
# a) The finfo struct.
#
sub default_signature_func {
  my ($finfo) = $_[0];
  $finfo->is_dir and return 1;	# If this is a directory, the modification time
				# is meaningless (it's inconsistent across
				# file systems, and it may change depending
				# on whether the contents of the directory
				# has changed), so just return a non-zero
				# constant.
  return $finfo->file_mtime;
}


#
# Load a build info file, if it matches the signature on the actual file.
# Returns undef if this build info file didn't exist or wasn't valid.
# Arguments:
# a) The FileInfo struct for the file.
#
sub load_build_info_file {
  my $finfo = $_[0];

  my %build_info;
  
  my $build_info_fname = $finfo->{".."}->absolute_filename . "/$FileInfo::build_info_subdir/" . $finfo->{NAME};
				# Form the name of the info file.

  local *BUILD_INFO;	# Make a local file handle.
  return undef unless open(BUILD_INFO, $build_info_fname);
				# Access the file.
  my $sig = $finfo->signature || ''; # Calculate the signature for the file, so
				# we know whether the build info has changed.
  my $line;
  while (defined($line = <BUILD_INFO>)) {	# Read another line.
    $line =~ s/\r//;		# Strip out the silly windows-format lines.
    if ($line =~ /(.*?)=(.*)/) { # Check the format.
      my $key = $1;
      my $val = $2;
      $val =~ s/\\([0-7]{1,3})/chr(oct($1))/eg;
      $key =~ s/\\([0-7]{1,3})/chr(oct($1))/eg;
				# Convert special characters.
      $build_info{$key} = $val;
    }
    else {
      warn $build_info_fname . ": build info file corrupted\n";
      last;
    }
  }
  close BUILD_INFO;

  if (($build_info{SIGNATURE} || '') ne $sig) {
				# Exists but has the wrong signature?
    CORE::unlink($build_info_fname); # Get rid of bogus file.
    return undef;
  }

  return \%build_info;
}

1;
