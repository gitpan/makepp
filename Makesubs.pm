###############################################################################
#
# This package contains subroutines which are can be called from a makefile.
# Subroutines in this package are called in two ways:
# 1) Any line which isn't a rule or an assignment and has at the left margin a
#    word is interpreted as a subroutine call to a subroutine in the makefile
#    package, or if not in the makefile package, in this package.
# 2) Any function that is in a make expression (e.g., $(xyz abc)) attempts to
#    call a perl function in the make package, and failing that, in this
#    package.
#
# All names in this package are automatically exported to each makefile
# package by Makefile::load.
#

package Makesubs;

use strict qw(vars subs);

use SelfLoader;

SelfLoader->load_stubs();	# Force it to load the stubs immediately.

use TextSubs;
use FileInfo;
use FileInfo_makepp;
use MakeEvent qw(wait_for when_done);

#
# Import a few subroutines from elsewhere:
#
foreach (qw(find_makepp_info find_makepp_info_register 
	    parse_command_line print_log print_error)) {
  *$_ = *{"main::$_"};
}

*read_makefile_line = *Makefile::read_makefile_line;
*read_makefile_line_stripped = *Makefile::read_makefile_line_stripped;
*unread_makefile_line = *Makefile::unread_makefile_line;

###############################################################################
#
# Routines which are invoked as statements.  These are all prefixed with
# "s_"; the s_ is added before we search for the name.
#

#
# This subroutine includes the text of another makefile at this point
# in the parsing process.
#
my @system_include_path = (file_info("$main::datadir", $main::original_cwd));
				# A list of directories to search for 
				# include files supplied with makepp.
sub s_include {
  my ($text_line, $makefile, $makefile_line) = @_;
				# Name the arguments.

  my @files = split(' ', $makefile->expand_text($text_line, $makefile_line));
				# Get a list of files.
  foreach my $file (@files) {
    my $finfo;
    for (my $dirinfo = $makefile->{CWD}; $dirinfo;
	 $dirinfo = $dirinfo->{".."}) { # Look in all directories above us.
      $finfo = file_info($file, $dirinfo);
      if ($finfo->exists_or_can_be_built) { # Found file in the path?
	wait_for main::build($finfo, 0) and # Build it if necessary, or link
				# it from a repository.
	  die "can't build " . $finfo->absolute_filename . ", needed at $makefile_line\n";
				# Quit if the build failed.
	last;			# We're done searching.
      }
    }

#
# If it wasn't found anywhere in the directory tree, search the standard
# include files supplied with makepp.  We don't try to build these files or
# link them from a repository.
#
    unless ($finfo->file_exists) { # Not found anywhere in directory tree?
      foreach (@system_include_path) {
	$finfo = file_info($file, $_); # See if it's here.
	last if $finfo->file_exists;
      }
      $finfo->file_exists or 
	die "can't find include file $file\n";
    }

    $main::log_level and
      main::print_log("Including ", $finfo->name);
    $makefile->read_makefile($finfo); # Read the file.
  }
  '';
}

###############################################################################
#
# Command scanners included with makepp:
#

my %dir_warnings;
my %already_warned_missing;
#
# Scan C command, looking for sources and includes and libraries.
#
@Makesubs::system_include_dirs = ( file_info("/usr/local/include"),
				   file_info("/usr/include") );

#@system_lib_dirs = ( file_info("/usr/local/lib"),
#		      file_info("/usr/lib") );
sub scanner_c_compilation {
  my ($action, $rule) = @_;	# Name the arguments.

  my $build_cwd = $rule->build_cwd; # Access the default directory.

  if ($main::log_level) {
    my $printable_cmd = $action;
    $printable_cmd =~ s/\n/\\n/g; # Make it fit on one line.
    main::print_log("Scanning command $printable_cmd from directory ", $build_cwd->name);
  }

  my @include_dirs;
  my @lib_dirs;
  my @libs;			# Libs that we link with.
  my @obj_files;		# Objects and libraries.
  my @source_files;
  my $stop_at_obj;		# True if we don't use the libraries.
  my %all_sources;		# Hash of FileInfo structs for each source
				# file.
  local $_;			# Don't mess up caller's $_.

  $action =~ s/\n.*//s;		# Strip out everything after the newline, since
				# that won't be a compilation command.
  my $pos = index_ignoring_quotes($action, ";");
  $pos > 0 and			# Was there a semicolon in this rule?
    $action = substr($action, 0, $pos); # Discard everything after
				# that, since it's not a gcc command.

  my @cmd_words = split_on_whitespace($action);
				# Get the pieces of the command.
  shift @cmd_words;		# Get rid of the compiler name.

  while (defined($_ = shift @cmd_words)) { # Get the next word.
    if (/^-I(.*)/) {		# Specifying a new include directory?
      my $dirinfo = file_info($1, $build_cwd)->dereference;
      if (!$dirinfo->is_or_will_be_dir) {
	$main::warn_level and
	  main::print_error("warning: invalid directory " . $dirinfo->absolute_filename . " mentioned in compile command " . $rule->source)
	    unless $dir_warnings{$dirinfo->absolute_filename}++;
				# Don't give the same warning more than
				# once.
      } else {
	push @include_dirs, $dirinfo;
      }
    }
    elsif (/^-L(.*)/) {		# Specifying new link library directory?
      my $dirinfo = file_info($1, $build_cwd)->dereference;
      if (!$dirinfo->is_or_will_be_dir) {
	$main::warn_level && 
	  main::print_error("warning: invalid directory $1 mentioned in build command")
	    unless $dir_warnings{$rule->source, $1}++;
				# Don't give the same warning more than
				# once.
      } else {
	push @lib_dirs, $dirinfo;
      }
    }
    elsif ($_ eq "-c") {
      $stop_at_obj = 1;		# Ignore the link specs.
    }
    elsif ($_ eq "-o") {	# Specify output file?
      my $tname = shift @cmd_words; # Get the name.
      $tname and $rule->add_target(file_info($tname, $build_cwd));
    }
    elsif (/^-l(.*)$/) {	# Specify a link library?
      push @libs, $1;
    }
    elsif (/^-/) {		# Some other option?
				# Just ignore it.
    } elsif (/^[\<\>](.*)/) {	# Redirect?
      $1 or shift @cmd_words;	# Get from the next word if not in this word.
    }
    elsif (/\.(?:o|lo|la|a|sa|so|so\.*)$/) { # Extension of an object file?
      push @obj_files, file_info($_, $build_cwd);
    }
    elsif (main::is_cpp_source_name($_)) { # Looks like a source file?
      my $src_info = file_info($_, $build_cwd);
      unless ($all_sources{$src_info}++) { # Already seen this source?
	push @source_files, file_info($_, $build_cwd);
      }	
    }
  }

#
# Scan the source files for includes:
#
  for (my $src_idx = 0; $src_idx < @source_files; ++$src_idx) {
				# Use this instead of a foreach because we will
				# add additional files to this list.
    my $src_info = $source_files[$src_idx];
    my $status = main::wait_for(main::build($src_info));
				# Make sure the file is available.
    $rule->add_dependency($src_info); # Mark this as a dependency.
    $status and return $status;	# Quit if the build failed.
#
# Get the include information for this file.  Check the information from the
# last build, if there is any.  Otherwise, we have to rescan the file.
#
    my @includes;
    my @system_includes;
    my $includes = $src_info->build_info_string("INCLUDES");
				# Do we already know what this file includes?
    my $system_includes = $src_info->build_info_string("SYSTEM_INCLUDES");
    if (defined($includes) && defined($system_includes)) {
      @includes = split(' ', $includes);
      @system_includes = split(' ', $system_includes);
    }
    else {			# We don't know what this file includes.
				# We'll have to scan it.
#
# If this directory is writable, then scan the file.  Otherwise, don't bother;
# we assume it's some system include file which doesn't change.
#
      my $absname = $src_info->absolute_filename;
      if ($absname !~ m@/usr/(?:X11(?:R.)?/|local/)include\b@ &&
				# Never scan stuff in the system directories.
				# This can lead to problems if we build as
				# a user and then install as root.  This won't
				# completely solve the problem, but it will
				# make it much less common.
	  $src_info->{".."}->is_writable) {
	print "Scanning $absname\n" unless $main::quiet_flag;
	local *SRCFILE;		# Make a local file handle.
	if (!open(SRCFILE, $absname)) {
	  main::print_error("warning: could not scan $absname for includes--$!");
	  next;			# Don't try to save the information.
	} else {
	  while (defined($_ = <SRCFILE>)) {
	    if (/^\s*\#\s*include\s*([\<\"])([^\"\<\>]+)[\"\>]/) { # Either form of include?
	      my $include_fname = $2;
	      if ($1 eq '"') {	# Non-system include?
		push @includes, $2;
	      } else {
		push @system_includes, $2;
	      }
	    }
	  }
	  close SRCFILE;		# Done with this file
	  $src_info->set_build_info_string(INCLUDES => join(" ", @includes),
					 SYSTEM_INCLUDES => join(" ", @system_includes));
				# Store the information for next time.
	}
      }
    }

#
# Find the exact location of each of these include files:
#
  inc_file_loop:
    foreach my $include_fname (@includes) {
#
# Check the directories specified in the include path:
#
      foreach my $incdir (@include_dirs) {
	my $inc_finfo = file_info($include_fname, $incdir);
	if ($inc_finfo->exists_or_can_be_built) {
	  push @source_files, $inc_finfo # Remember it.
	    unless $all_sources{$inc_finfo}++; # Don't duplicate files.
	  next inc_file_loop;
	}
      }

#
# Non-system include files can also be in the same directory as the file itself.
#
      my $inc_finfo = file_info($include_fname, $src_info->{".."});
      if ($inc_finfo->exists_or_can_be_built) {
	push @source_files, $inc_finfo # Remember it.
	  unless $all_sources{$inc_finfo}++; # Don't duplicate files.
	next inc_file_loop;
      }
#
# Do the same thing for the system includes, except that we don't check files
# that they include--we assume they don't change.  We also don't list them
# explicitly as dependencies.
#
      foreach my $incdir (@Makesubs::system_include_dirs) {
	next inc_file_loop if FileInfo::file_exists(file_info($include_fname, $incdir));
      }

      $main::warn_level and
	main::print_error("warning: can't locate file $include_fname, included from ",
			  $src_info->absolute_filename, "\nInclude path is:\n  ",
			  join("\n  ",
			       map { $_->absolute_filename } @include_dirs))
	  unless $already_warned_missing{$include_fname}++;
    }

#
# Do the same thing for the system includes.  This isn't quite the same,
# because if we don't find a system include file, then we don't complain--we
# assume it's in one of the system directories.  Since @system_include_dirs
# probably isn't an exhaustive list, this keeps us from giving annoying 
# warnings about things which aren't a problem.
#
  system_inc_file_loop:
    foreach my $include_fname (@system_includes) {
      foreach my $incdir (@include_dirs) {
	my $inc_finfo = file_info($include_fname, $incdir);
	if ($inc_finfo->exists_or_can_be_built) {
				# Exists or can be built?
	  push @source_files, $inc_finfo # Remember it.
	    unless $all_sources{$inc_finfo}++; # Don't duplicate files.
	  next system_inc_file_loop;
	}
      }
				# Not found--assume it is in /usr/include, and
				# don't bother checking.
    }

  } # End loop through source files.

  $stop_at_obj and return;	# If no linking to do, ignore libraries.

#
# At this point, we've got a list of all the source files that this depends
# on.  Now, if we're going to be linking, look for the -l libs.
#
 libloop:
  foreach my $libname (@libs) {
    foreach my $libdirinfo ($build_cwd, @lib_dirs) {
      foreach (qw(la a so sa)) { # Common extensions.
	my $lib_info = file_info("lib$libname.$_", $libdirinfo);
	if ($lib_info->exists_or_can_be_built) {
	  push @obj_files, $lib_info; # Found the library.
	  next libloop;
	}
      }
    }
    
#
# Not found in the explicitly specified directories.  We probably ought to
# search through the system library directories, but the trouble is we don't
# really know where they are, and I don't know where to get that information
# reliably.  It would be nice to give a warning message that makepp doesn't
# know whether a library needs to be built or not, but I don't see how to do
# it.
#
  }

  foreach (@obj_files) {	# Mark each of the objects as a dependency.
    $rule->add_dependency(file_info($_, $build_cwd));
  }	
}

#
# This scanner exists only to allow the user to say ":scanner none" to suppress
# the default scanner.
#
sub scanner_none {
}

#
# This scanner simply moves to the next word that doesn't begin with
# - and scans again.
#
sub scanner_skip_word {
  my ($action, $rule) = @_;	# Name the arguments.

  $action =~ s/^\s+//;		# Leading whitespace messes up the regular
				# expression below.
  while ($action =~ s/^\S+\s+//) { # Strip off another word.
    $action =~ s/^[\"\'\(]//;	# Strip off leading quotes in case it's 
				# something like sh -c "cc ...".
    if ($action !~ /^-/) {	# Word that doesn't look like an option?
      $rule->scan_action($action); # Scan it now.
      last;			# Don't go any further.
    }
  }
}

#
# Special scanner so we can get config.status to work properly with
# repositories.  This is to handle an idiom from automake command files:
#
#  config.status: configure
#	/bin/sh ./config.status --recheck
#
# This obviously won't work unless config.status already exists.  So we link
# it in initially from the repository, even though it's not a dependency,
# so the command can run.  (Configure actually rm's the config.status before
# writing to it, so it's ok for it to be a soft link to another directory.)
#
# This doesn't actually seem to help, because automake's stuff is hopelessly
# complicated and depends on way more files than we can easily ferret out
# automatically.
# sub scanner_config_status {
#   my ($action, $rule) = @_;	# Name the arguments.

#   $rule->set_signature_method_default($Signature::target_newer::target_newer);
# 				# Always use the bozo-make algorithm.

#   if ($action=~ /^\s*(\S+)\s+/) { # Get the name of the config.status file.
#     my $finfo = file_info($1, $rule->build_cwd); # Access file info.
#     if ($finfo->{ALTERNATE_VERSIONS} &&	# Available in repository?
# 	!$finfo->file_exists) {	# Does not exist yet?
#       eval { $finfo->symlink($finfo->{ALTERNATE_VERSIONS}[0]); };
# 				# Try to make the file.
# 				# (This is a dirty trick.  We can't actually
# 				# use the proper move_or_link subroutine,
# 				# because that marks it as from a repository,
# 				# and the first thing that build() does is
# 				# to remove all files from repositories.)
#       $finfo->may_have_changed;	# Restat the file.
#     }
#   }
# }

#
# This array contains the list of the default scanners used for various
# command words.
#
%Makesubs::scanners =
  ("libtool" => \&scanner_libtool,
				# These words usually introduce another command
				# which actually is the real compilation command.
   "sh"      => \&scanner_skip_word,
   "purify"  => \&scanner_skip_word,
   "ignore_error" => \&scanner_skip_word,
   "noecho"  => \&scanner_skip_word,

#   "config.status" => \&scanner_config_status,
   "cc"      => \&scanner_c_compilation, # These are all the C/C++
   "gcc"     => \&scanner_c_compilation, # compilers I can think of.
   "c++"     => \&scanner_c_compilation,
   "g++"     => \&scanner_c_compilation,
   "CC"      => \&scanner_c_compilation,
   "cxx"     => \&scanner_c_compilation,
   "c89"     => \&scanner_c_compilation,
   "pcc"     => \&scanner_c_compilation,
   "kcc"     => \&scanner_c_compilation,
   "kgcc"    => \&scanner_c_compilation,
   "egcc"    => \&scanner_c_compilation,
);

###############################################################################
#
# Functions that are intended to be invoked by make expressions.  These
# all begin with the prefix "f_", which is added before we look up the
# name of the function.  These functions are called with the following
# arguments:
# a) The text after the function name in the makefile (with other macros
#    already expanded).
# b) The makefile.
# c) The line number in the makefile that this expression occured in.
#

#
# Perform a pattern substitution:
#
sub f_patsubst {
  my ($src, $dest, $words) = split(/,\s*/, $_[0]);
				# Get the arguments.
  return join(" ", TextSubs::pattern_substitution($src, $dest,
						  split_on_whitespace($words)));
}

#
# Perform a pattern substitution on file names.  This differs from patsubst
# in that it will perform correctly when alternate names for directories are
# given (as long as they precede the percent sign).  For example,
#
#  $(filesubst ./src/%.c, %.o, $(wildcard src/*.c))
#
# will work with filesubst but not with patsubst.
#
sub f_filesubst {
  my ($src, $dest, $words) = split(/,\s*/, $_[0]);
				# Get the patterns.
  my ($makefile, $makefile_line) = @_[1,2]; # Name the other arguments.
#
# First we eat away at the directories on the source until we find the
# percent sign.  We remember where this directory is.  Then we consider each 
# of the words and strip off leading directories until we reach that
# directory.  Then we run through patsubst.
#
  my $startdir = ($src =~ s@^/+@@) ? $FileInfo::root : $makefile->{CWD}; 
				# The directory we're in if there are no
				# other directories specified.

  while ($src =~ s@([^%/]+)/+@@) { # Strip off a leading directory that
				# doesn't contain the % sign.
    $startdir = file_info($1, $startdir)->dereference;
				# Move to that directory.
  }	

#
# Now eat away at the directories in the words until we reach the starting
# directory.
#  
  my @words;
  foreach (split(' ', $words)) {
    my $thisdir = (s@^/+@@) ? $FileInfo::root : $makefile->{CWD};
    while ($thisdir != $startdir &&
	   s@([^/]+)/+@@) {	# Another directory?
      $thisdir = file_info($1, $thisdir)->dereference;
    }
    push @words, $_;		# What's left is the filename relative to that
				# directory.
  }

  return join(" ", TextSubs::pattern_substitution($src, $dest, @words));
}

#
# Mark targets as phony:
#
sub f_phony {
  my ($str, $makefile, $makefile_line) = @_; # Name the arguments.

  foreach (split_on_whitespace($str)) {
    main::find_makepp_info(unquote($_), $makefile->{CWD})->{IS_PHONY} = 1;
  }

  return $str;			# Just return our argument.
}

sub f_sort;			# Must be predeclared or else $+ doesn't work.

###############################################################################
#
# Define special automatic variables:
#
sub f_target {
  defined($Makesubs::rule) || die "\$(target) or \$\@ used outside of rule\n";
  return $Makesubs::rule->{EXPLICIT_TARGETS}[0]->name($Makesubs::rule->build_cwd);
}
*f_output = *f_target;

sub f_targets {
  defined($Makesubs::rule) ||
    die "\$(targets) or \$(outputs) used outside of rule\n";
  return join(" ", relative_filenames($Makesubs::rule->{EXPLICIT_TARGETS}));
}
*f_outputs = *f_targets;

sub f_dependency {
  defined($Makesubs::rule) ||
    die "\$(dependency) or \$(input) used outside of rule\n";
  my $finfo = $Makesubs::rule->{EXPLICIT_DEPENDENCIES}[0];
  $finfo or return '';		# No dependencies.

  return $finfo->name($Makesubs::rule->build_cwd);
}
*f_input = *f_dependency;

sub f_dependencies {
  defined($Makesubs::rule) ||
    die "\$(dependencies) or \$(inputs) or \$^ used outside of rule\n";
  return join(" ", relative_filenames($Makesubs::rule->{EXPLICIT_DEPENDENCIES}));
}
*f_inputs = *f_dependencies;

sub f_sorted_dependencies {
  defined($Makesubs::rule) || die "\$(sorted_dependencies) or \$(sorted_inputs) or \$\^ used outside of rule\n";
  return Makesubs::f_sort join(" ", relative_filenames($Makesubs::rule->{EXPLICIT_DEPENDENCIES}));
}
*f_sorted_inputs = *f_sorted_dependencies;

sub f_foreach {
  defined($Makesubs::rule) && defined($Makesubs::rule->{FOREACH}) or
    return "\$(foreach)";	# Just delay the expansion.
				# This is necessary because target strings
				# may get expanded before the foreach variable
				# is defined.
#    die "\$(foreach) used outside of rule, or in a rule that has no :foreach clause\n";
  return $Makesubs::rule->{FOREACH}->name($Makesubs::rule->build_cwd);
  
}

sub f_stem {
  defined($Makesubs::rule) || die "\$(stem) or \$* used outside of rule\n";
  defined($Makesubs::rule->{PATTERN_STEM}) and 
    return $Makesubs::rule->{PATTERN_STEM};

  return f_basename(f_target);
				# If there's no stem, just strip off the 
				# target's suffix.  This is what GNU make
				# does.
}

#
# $(MAKE) needs to expand to the name of the program we use to replace a
# recursive make invocation.
#
sub f_MAKE {
  if ($main::traditional_recursive_make) { # Do it the bozo way?
    unless ($Makesubs::make_name) { # Haven't figured it out yet?
      $Makesubs::make_name = $0;	# Get the name of the program.
      unless ($Makesubs::make_name =~ m@^/@) { # Not absolute?
#
# We have to search the path to figure out where we came from.
#
	foreach (split(/:/, $ENV{'PATH'}), '.') {
	  my $finfo = file_info("$_/$0", $main::original_cwd);
	  if ($finfo->file_exists) { # Is this our file?
	    $Makesubs::make_name = $finfo->absolute_filename;
	    last;
	  }
	}
      }	
    }	
    return $Makesubs::make_name;
				# All the rest of the info is passed in the
				# MAKEFLAGS environment variable.
  } else {
    my $makefile = $_[1];	# Get the makefile we're run from.

    my $recursive_makepp = 
      file_info($main::datadir, $main::original_cwd)->absolute_filename .
	"/recursive_makepp";
				# Sometimes we can be run as ../makepp, and
				# if we didn't hard code the paths into
				# makepp, the directories may be relative.
				# However, since recursive make is usually
				# invoked in a separate directory, the
				# path must be absolute.
    return "$recursive_makepp " . join(" ", map { "$_=" . requote($makefile->{COMMAND_LINE_VARS}{$_}) } keys %{$makefile->{COMMAND_LINE_VARS}});
  }
}

#
# An internal subroutine that converts FileInfo structures to printable
# names.  Takes either a single FileInfo structure, an array of FileInfo
# structures, or a reference to an array of FileInfo structures.
#
sub relative_filenames {
  my @ret_vals;

  my $cwd = $Makesubs::rule->build_cwd;
  foreach (@_) {
    next unless defined $_;	# Skip undef things--results in a blank.
    if (ref($_) eq 'ARRAY') {
      push @ret_vals, relative_filenames(@$_);
    }
    else {
      push @ret_vals, $_->name($cwd);
    }
  }

  return @ret_vals; 
}

sub f_dir_noslash;
sub f_notdir;

#
# Now define all the cryptic one-character symbols, and anything else that
# isn't a valid subroutine name:
#
%Makesubs::perl_unfriendly_symbols =
  ("@" => \&f_target,
   "<" => \&f_dependency,
   "^" => \&f_dependencies,
   "?" => \&f_dependencies,
   "+" => \&f_sorted_dependencies,
   "*" => \&f_stem,
   "&" => sub { "" },		# Perl makefiles use this for some reason, but
				# $& is a perl variable whose value is '$', so
				# we don't want to look for the perl variable
				# $&.

   '@D' => sub { f_dir_noslash f_target },
   '@F' => sub { f_notdir f_target },
   '*D' => sub { f_dir_noslash f_stem },
   '*F' => sub { f_notdir f_stem },
   '<D' => sub { f_dir_noslash f_dependency },
   '<F' => sub { f_notdir f_dependency },
   '^D' => sub { f_dir_noslash f_dependencies },
   '^F' => sub { f_notdir f_dependencies },
  );

1;

__DATA__
#
# These subroutines are seldom used and so are autoloaded only on demand.
#

###############################################################################
#
# Makefile statements:
#

#
# Export statement.
#
sub s_export {
  my ($text_line, $makefile, $makefile_line) = @_; # Name the arguments.

  if ($text_line =~ /^\s*([^\s=\#]+)\s*=\s*(.*)$/) { # Assignment?
    $ {$makefile->{PACKAGE} . "::$1"} = $makefile->expand_text($2, $makefile_line);
				# Do the assignment.  (This is a := assignment.)
    $text_line = $1;		# Pretend just the variable name was present.
  }

  foreach (split(' ', $makefile->expand_text($text_line, $makefile_line))) {
    $makefile->{EXPORTS}{$_} = undef;
				# Mark these variables for export.  We'll
				# fill out their values later.
  }
}

#
# This subroutine does exactly the same thing as include, except that it
# doesn't die with an error message if the file doesn't exist.
#
sub s__include {
  my ($text_line, $makefile, $makefile_line) = @_;
				# Name the arguments.
  foreach (split(' ', $makefile->expand_text($text_line, $makefile_line))) {
    eval { s_include($_, $makefile, $makefile_line); };
  }

  $@ = '';			# Discard any error.
}

#
# Load one or several makefiles.
#
sub s_load_makefile {
  my ($text_line, $makefile, $makefile_line) = @_; # Name the arguments.

  my (@words) = split(' ', $makefile->expand_text($text_line, $makefile_line));

  my %command_line_vars = %{$makefile->{COMMAND_LINE_VARS}};
				# Extra command line variables.  Start out
				# with a copy of the current command line
				# variables.
#
# First pull out the variable assignments.
#
  foreach (@words) {
    if (/^(\w+)=(.*)/) {	# Found a variable?
      $command_line_vars{$1} = unquote($2);
    }
  }
#
# Now process the makefiles:
#
  foreach (@words) {
    next if /^\w+=/;		# Skip over variable assignments.
    s/^-F//;			# Support the archaic syntax that put -F
				# before the filename.
    my $mfile = file_info($_, $makefile->{CWD});
				# Get info on the file.
    my $mdir = $mfile;		# Assume it is actually a directory.
    $mfile->is_or_will_be_dir or $mdir = $mfile->{".."};
				# Default directory is the directory the
				# makefile is in.
    &Makefile::load($mfile, $mdir, \%command_line_vars); # Load the makefile.
  }
}

#
# Load from repositories:
#
sub s_repository {
  my ($text_line, $makefile, $makefile_line) = @_; # Name the arguments.

  foreach my $rdir (split(' ', $makefile->expand_text($text_line, $makefile_line))) {
				# Get a list of repository directories.
    if ($rdir =~ /^([^=]+)=(.*)$/) { # Destination directory specified?
      my $rinfo = file_info($2, $makefile->{CWD});
      my $dst_info = file_info($1, $makefile->{CWD});
      main::load_repository($rinfo, $dst_info);
    }
    else {
      my $rinfo = file_info($rdir, $makefile->{CWD});
				# Get the fileinfo structure.
      main::load_repository($rinfo, $makefile->{CWD});
				# Load all the files.
    }	
  }
}

#
# Begin a whole block of perl code.
#
sub s_perl_begin {
  my ($junk, $makefile, $makefile_line) = @_;
				# Name the arguments.
  my $perl_code;
  my $line;
  while (defined($line = &read_makefile_line)) { # Get the next line.
    last if $line =~ /^perl_end/; # Found the terminator?
    $perl_code .= $line;
  }

  eval "no strict; package $makefile->{PACKAGE};\n$perl_code";
  if ($@) {			# Try to convert the line number into
				# something that's more readable.
    my ($starting_line, $makefile_name) = ('random subroutine', 1);
    if ($makefile_line =~ /(.*):(\d+)$/) { # Get the name of the makefile and
      $makefile_name = $1;	# the starting line number, so we can fix the
      $starting_line = $2;	# error message.
    }

    my $error = $@;		# Make a modifiable copy.

    $error =~ s{\(eval \d+\)(?:\[.*?\])? line (\d+)}{"$makefile_name:" . ($starting_line+$1-1)}eg;
				# Try to fix up the line numbers.
    die $error;			# Resignal the error.
  }	
}

#
# Register an action scanner.
# Usage from the makefile: 
#    register_scanner command_word scanner_subroutine_name
#    
#
sub s_register_scanner {
  my ($text_line, $makefile, $makefile_line) = @_; # Name the arguments.

  my (@fields) = split_on_whitespace($text_line);
				# Get the words.
  @fields == 2 or die "$makefile_line: invalid register_scanner line\n";
  my $command_word = unquote($fields[1]); # Remove quotes, etc.
  my $scanner_sub = \&{$makefile->{PACKAGE} . "::$fields[2]"};
				# Get a reference to the subroutine.
  $makefile->register_scanner($command_word, $scanner_sub);
}

#
# This function allows the user to do something in the makefile like:
# sub subname {
#   ... perl code
# }
#
sub s_sub {			# Yes, we can actually have a function with
				# the name "sub".
  my ($subr_text, $makefile, $makefile_line) = @_;
				# Name the arguments.
  unless ($subr_text =~ /\}\s*$/) { # Subroutine is not entirely inline?
    $subr_text .= "\n";		# Put the newline in that got removed.
    my $line;
    while (defined($line = &read_makefile_line)) { # Get the next line.
      $subr_text .= $line;
      last if $line =~ /^\}/;	# Stop at a brace at the left margin.
    }
  }	
    
#
# At this point, $subr_text contains the text of the subroutine.  Evaluate
# it:
#
  eval "package $makefile->{PACKAGE}; sub $subr_text";
  if ($@) {			# Try to convert the line number into
				# something that's more readable.
    my ($starting_line, $makefile_name) = ('random subroutine', 1);
    if ($makefile_line =~ /(.*):(\d+)$/) { # Get the name of the makefile and
      $makefile_name = $1;	# the starting line number, so we can fix the
      $starting_line = $2;	# error message.
    }

    my $error = $@;		# Make a modifiable copy.

    $error =~ s{\(eval \d+\)(?:\[.*?\])? line (\d+)}{"$makefile_name:" . ($starting_line+$1-1)}eg;
				# Try to fix up the line numbers.
    die $error;			# Resignal the error.
  }	
}

#
# Don't export a variable to child processes.
#
sub s_unexport {
  my ($text_line, $makefile, $makefile_line) = @_;
				# Name the arguments.
  return unless $makefile->{EXPORTS};

  foreach (split(' ', $makefile->expand_text($text_line, $makefile_line))) {
				# Look at each variable listed.
    delete $makefile->{EXPORTS}{$_};
  }
}

###############################################################################
#
# Scanner subroutines:
#

#
# Scanning libtool commands is a bit tricky.  Several things we need to do:
# 1) If a file depends on a xyz.lo, it also depends on .libs/xya.o (or whatever
#    is specified in the xyz.lo file).
# 2) .la files cannot be linked in from repositories, because they have
#    absolute pathnames encoded in them.  Thus we remove repository information
#    for them.
#
sub scanner_libtool {
  my ($action, $rule) = @_;	# Name the arguments.
  &scanner_skip_word;		# Call the linker or C compiler scanner, to get
				# all of the dependencies.
#
# Search the dependency list for .lo or .la files:
#
  my @handles;
  my @lofiles;
  foreach my $finfo (values %{$rule->{ALL_DEPENDENCIES}}) {
    if ($finfo->{NAME} =~ /\.lo$/) { # Is it a libtool object flie?
      my $handle = main::build($finfo); # Queue a build for it.
      $handle and push @handles, $handle; # Remember to wait for it later.
      push @lofiles, $finfo;	# Remember to look at this later.
    }
    elsif ($finfo->{NAME} =~ /\.la$/) {	# Is it a libtool library?
      delete $finfo->{ALTERNATE_VERSIONS};
				# Never link these in from repositories.
    }
  }

#
# Search the target list for any .la files:
#
  foreach my $finfo (values %{$rule->{ALL_TARGETS}}) {
    if ($finfo->{NAME} =~ /\.la$/) {
      delete $finfo->{ALTERNATE_VERSIONS};
				# Never link these in from repositories.
    }
  }

  if (@handles) {
    my $status = main::wait_for(@handles); # Wait until all the .lo files we 
				# need actually exist.
    $status and return;		# Don't bother scanning them if they aren't
				# correctly build.
  }	
  my $build_cwd = $rule->build_cwd; # We need this in several places.

  foreach (@lofiles) {
    local *LOFILE;		# Make a local file handle.
    open(LOFILE, $_->absolute_filename) || next; # Read the file.
    my $line;
#
# We're looking for a line in the file that looks like this:
# pic_object='.libs/xyz.o'
# or
# non_pic_object='.libs/xyz.o'
#
    while (defined($line = <LOFILE>)) {	# Read a line.
      if ($line =~ /^\s*(?:non_)?pic_object=\'([^\']+)\'/) { # Found line 
				# specifying another object?
	$rule->add_dependency(file_info($1, $build_cwd));
				# Mark it as a dependency.
      }
    }
    close LOFILE;		# Done with the file.
  }

#
# The libtool command file itself is usually also a dependency (it has to be
# linked in from a repository if it doesn't exist).
#
  $action =~ /^\s*(\S+)/;	# Extract the first word.  It's the libtool
				# commnd.
  $rule->add_dependency(file_info($1, $build_cwd));
}

###############################################################################
#
# Functions invoked as make expressions:
#

#
# Return the absolute filename of all the arguments.
#
sub f_absolute_filename {
  my @files = split_on_whitespace($_[0]); # Get a list of files.
  my @ret_names;
  my $cwd = $Makesubs::rule ? $Makesubs::rule->build_cwd : $FileInfo::CWD_INFO;
  foreach (@files) {
    push @ret_names, file_info(unquote($_), $cwd)->absolute_filename;
  }	

  return join(" ", @ret_names);
}

sub f_addprefix {
  my ($prefix, $text) = split(/,\s+/, $_[0]); # Get the prefix.
  return join(" ", map { "$prefix$_" } split(' ', $text));
}

sub f_addsuffix {
  my ($suffix, $text) = split(/,\s+/, $_[0]); # Get the prefix.
  return join(" ", map { "$_$suffix" } split(' ', $text));
}

sub f_basename {
  my @ret_vals;
  foreach (split ' ', $_[0]) {
    if (m@(\.[^./\,]*)$@) {	# Match the extension, but don't look into the
				# directory for a period.
      push @ret_vals, substr($_, 0, length($_)-length($1)); # Take it off
				# (without using $PRE or $POST or a regex
				# that has lots and lots of backtracking).
    } else {
      push @ret_vals, $_;	# No extension.  Return the whole filename.
    }
  }

  return join(" ", @ret_vals);
}

sub f_dir {
  my @ret_vals;
  foreach (split ' ', $_[0]) {
    if (m@^(.*/)@) { push @ret_vals, $1; }
    else           { push @ret_vals, './'; }
  }

  return join(" ", @ret_vals);
}

sub f_dir_noslash {		# An internal routine that does the same
				# thing but doesn't return a trailing slash.
  my @ret_vals;
  foreach (split ' ', $_[0]) {
    if (m@^(.*)/@) { push @ret_vals, $1; }
    else           { push @ret_vals, '.'; }
  }

  return join(" ", @ret_vals);
}

sub f_filter {
  my ($filters, $words) = split(/,\s*/, $_[0]);

  my @filters = split(' ', $filters); # Can be more than one filter.
  foreach (@filters) {		# Convert these into regular expressions.
    s/\./\\./g;			# Protect all the periods.
    s/\%/\.\*/g;		# Replace '%' with '.*'.
    $_ = "^$_\$";		# Anchor the pattern.
  }

  my @ret_words;
 wordloop:
  foreach (split(' ', $words)) { # Now look at each word.
    foreach my $filter (@filters) {
      if (/$filter/) {		# Does it match this filter?
	push @ret_words, $_;
	next wordloop;
      }
    }
  }

  return join(" ", @ret_words);
}

sub f_filter_out {
  my ($filters, $words) = split(/,\s*/, $_[0]);

  my @filters = split(' ', $filters); # Can be more than one filter.
  foreach (@filters) {		# Convert these into regular expressions.
    s/\./\\./g;			# Protect all the periods.
    s/\%/\.\*/g;		# Replace '%' with '.*'.
    $_ = "^$_\$";		# Anchor the pattern.
  }

  my @ret_words;
 wordloop:
  foreach (split(' ', $words)) { # Now look at each word.
    foreach my $filter (@filters) {
      next wordloop if /$filter/; # Skip if it matches this filter.
    }
    push @ret_words, $_;
  }

  return join(" ", @ret_words);
}

sub f_findstring {
  my ($find, $in) = split(/,/, $_[0]);

  return (index($in, $find) >= 0) ? $find : '';
}

sub f_firstword {
  return (split(' ', $_[0]))[0] || '';
}

sub f_if {
  my ($cond, $then_expr, $else_expr) = split(/\,\s*/, $_[0]);

  if ($cond && $cond !~ /^\s+$/) { # Is the condition true?
    return $then_expr || '';
  } else {
    return $else_expr || '';
  }	
}

sub f_notdir {
  my @ret_vals;
  foreach (split ' ', $_[0]) {
    if (m@^.*/([^/]+)@) { push @ret_vals, $1; }
    else                { push @ret_vals, $_; }
  }

  return join(" ", @ret_vals);
}

#
# Return only the files in the list that are actually targets of some rule:
#
sub f_only_targets {
  my ($text, $makefile, $makefile_line) = @_; # Name the arguments.
  my @ret_files;

  my $build_cwd = $makefile->{CWD};

  foreach (split(' ', $_[0])) {
    foreach my $finfo (Glob::zglob_fileinfo($_, $build_cwd)) {
      $finfo->get_rule(1) and
	push @ret_files, $finfo->name($build_cwd);
    }
  }

  return join(" ", @ret_files);
}

#
# Return only the files in the list that are not targets of some rule:
#
sub f_only_nontargets {
  my ($text, $makefile, $makefile_line) = @_; # Name the arguments.
  my @ret_files;

  my $build_cwd = $makefile->{CWD};

  foreach (split(' ', $_[0])) {
    foreach my $finfo (Glob::zglob_fileinfo_atleastone($_, $build_cwd)) {
      $finfo->get_rule(1) or
	push @ret_files, $finfo->name($build_cwd);
    }
  }

  return join(" ", @ret_files);
}

sub f_print {
  print $_[0], "\n";		# Print the text.
  $_[0];			# Just return it verbatim.
}

sub f_shell {
  my ($str, $makefile, $makefile_line) = @_; # Name the arguments.

  chdir $makefile->{CWD};	# Make sure we're in the correct directory.
  my $shell_output =`$str`;	# Run the shell command.
  $? == 0 or
    die "error $? running shell command `$str'\n";
  $shell_output =~ s/\r?\n/ /g;	# Get rid of newlines.
  $shell_output =~ s/\s+$//;	# Strip out trailing whitespace.
  return $shell_output;
}

sub f_sort {
#
# Sort is documented to remove duplicates as well as to sort the string.
#
  my @ret_words = sort split ' ', $_[0]; # Get the words.

  my $idx = 1;			# Now remove duplicates:
  while ($idx < @ret_words) {
    if ($ret_words[$idx] eq $ret_words[$idx-1]) {
      splice @ret_words, $idx, 1; # Remove duplicated word.
    } else {
      ++$idx;			# Skip unique word.
    }
  }

  join(" ", @ret_words);
}

sub f_strip {
  return join(" ", split(' ', $_[0]));
}

sub f_subst {
  my ($from, $to, $text) = split(/,/, $_[0]);
  $from = quotemeta($from);

  my @ret_vals;
  foreach (split(' ', $text)) {
    (my $newval = $_) =~ s/$from/$to/g;
    push(@ret_vals, $newval);
  }

  join(" ", @ret_vals);
}

sub f_suffix {
  my @ret_vals;

  foreach (split ' ', $_[0]) {
    if (m@\.([^\./]*)$@) { push @ret_vals, $1; }
  }

  join(" ", @ret_vals);
}


sub f_wildcard {
  my ($line, $makefile, $makefile_line) = @_; # Name the arguments.

  my $cwd = $Makesubs::rule ? $Makesubs::rule->build_cwd : $makefile->{CWD};
				# Get the default directory.

  join(" ", map { Glob::zglob($_, $cwd) } split(' ', $line));
}

sub f_word {
  my ($wordidx, $text) = split(/,\s*/, $_[0]);

  return (split(' ', $text))[$wordidx-1] || '';
}

sub f_wordlist {
  my ($startidx, $endidx, $text) = split(/,\s*/, $_[0]);
  if ($startidx > $endidx) { my $temp = $startidx; $startidx = $endidx; $endidx = $temp; }
  return join(" ", (split(' ', $text))[$startidx-1 .. $endidx-1]);
}

sub f_words {
  my @wordlist = split(' ', $_[0]);
  return scalar(@wordlist);
}
