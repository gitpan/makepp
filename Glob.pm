package Glob;
require 5.005;			# Needs the m//gc option, which appeared in
				# 5.004, and the qr// syntax, which appeared
				# in 5.005.
use strict;

use FileInfo;

require Exporter;
@Glob::ISA = qw(Exporter);
@Glob::EXPORT = qw(chdir);	# Force our caller to use our chdir sub that
				# we inherit from FileInfo.
@Glob::EXPORT_OK = qw(zglob zglob_fileinfo $allow_dot_files wildcard_action);
use strict;

=head1 NAME

Glob -- Subroutines for reading directories easily.

=head1 USAGE

  my @file_info_structs = Glob::zglob_fileinfo("pattern"[, default dir]);
  my @filenames = Glob::zglob("pattern"[, default dir]);
  $Glob::allow_dot_files = 1;	# Enable returning files beginning with '.'.
  wildcard_action @wildcards, sub { my $finfo = $_[0]; ... };

=head1 DESCRIPTION

=head2 Glob::zglob

This subroutine performs globbing without forking off to the csh, so in
principle it may be faster than using the builtin perl glob.  It also
supports some limited extended wildcards (ideas stolen from zsh).

	*		Matches any text
	?		Matches 0 or one characters
	[range]		Matches a range just like the unix wildcards.
	**		Matches an arbitrary list of directories.  This
			is a shortcut to running "find" on the directory
			tree.  For example, "x/**/a.o" matches "x/a.o",
			"x/y/a.o", "x/y/z/a.o", etc.  Like find, it does
			not search through directories specified by
			symbolic links.

If the argument to zglob does not have any wildcard characters in it, the file
name is returned if the file exists; an empty list is returned if it does not
exist.  (This is different from the shell's usual globbing behaviour.)  If
wildcard characters are present, then a list of files matching the wildcard is
returned.  If no files match, an empty list is returned.

If you want a subroutine which returns something even if no files matched,
then call zglob_fileinfo_atleastone.  This has the same behavior as the Bourne
shell, which returns the wildcard or filename verbatim even if nothing
matches.

The second argument to C<zglob> is the default directory, which is used if you
specify a relative file name.  If not specified, uses the current default
directory.  C<zglob> doesn't repeatedly call Cwd::cwd to get the directory;
instead, it uses the FileInfo package to track the current directory.  (This
means that it overrides C<chdir> in your module to be something that stores
the current directory.)

By default, C<zglob> does not return file names beginning with '.'.
You can force it to return these files by setting $Glob::allow_dot_files=1,
or (as with the shell) by specifing a leading . in the wildcard pattern (e.g.,
".*").

C<zglob> returns a list of file names.  It uses an internal subroutine,
C<zglob_fileinfo>, which returns a list of FileInfo structures.  See the
FileInfo package for more details.

=cut

$Glob::allow_dot_files = 0;	# Don't return any files beginning with '.'.

sub zglob {
  map { $_->relative_filename($_[1]) } zglob_fileinfo(@_);
}

sub zglob_fileinfo_atleastone {
  my @files = &zglob_fileinfo;	# Get a list of files.
  if (@files == 0) {		# No files matched at all?
    @files = (file_info($_[0], $_[1] || $FileInfo::CWD_INFO));
				# Make a fileinfo structure for whatever the
				# wildcard itself (or non-existent file) was.
  }

  return @files;
}

#
# The third argument to zglob_fileinfo indicates whether to avoid following
# soft-linked directories ("**" never follows soft links, but other wildcards
# can).  Generally, you want to follow soft links, but because of some
# technical restrictions, wildcard_action can't do it right so in order to
# avoid having makefile bugs where things work some of the time but not all
# of the time, when we're called from wildcard_action we don't follow soft
# links either.
#
sub zglob_fileinfo {
  local ($_) = $_[0];		# Access the filename or wildcard.
  my $startdir = $_[1] ? file_info($_[1]) : $FileInfo::CWD_INFO;
				# Get the current directory.
  my $dont_follow_soft = $_[2];

  my $is_wildcard = 0;		# We haven't seen a wildcard yet.

  s@^/@@ and $startdir = $FileInfo::root; # If this is a rooted wildcard,
				# change to the top of the path.  Also,
				# strip out the leading /.
  my @pieces = split(/\/+/, $_); # Get the pieces of the filename, and
				# examine each one of them separately.
  my @new_candidates = ($startdir); # Directories that are possible.  At first,
				# there is only the starting directory.

  my @candidate_dirs;

  foreach (@pieces) {
    if ($dont_follow_soft) {
      @candidate_dirs = grep($_->{DIRCONTENTS} && !$_->is_symbolic_link ||
			     $_->is_or_will_be_dir,
			     @new_candidates);
    } else {
      @candidate_dirs = grep($_->{DIRCONTENTS} || $_->is_or_will_be_dir, @new_candidates);
				# Discard everything that isn't a directory,
				# since we have to look for files in it.
				# (Note that we will return files that are
				# in directories that don't exist yet.)
    }

    if ($main::implicitly_load_makefiles) { # Should wildcards trigger loading?
				# We have to do this before scanning the
				# directory, since loading the makefile
				# may make extra files appear.
      foreach my $dir (@candidate_dirs) { Makefile::implicitly_load($dir); }
    }


    @new_candidates = ();	# This will contain the files that live in
				# the candidate directories.
#
# First translate the wildcards in this piece:
#
    if ($_ eq "**") {		# Special zsh wildcard?
      foreach (@candidate_dirs) {
	push(@new_candidates, $_, find_all_subdirs_recursively($_));
      }
      next;
    }

#
# The remaining wildcards match only files within these directories.  Convert
# them to regular expressions:
#
    my ($is_wildcard, $file_regex_or_name) = wild_to_regex($_);
				# Convert to a regular expression.

#
# At this point, $file_regex contains a regular expression that corresponds
# to the wildcard.  It's possible, however, that there were wildcard characters
# but they were all preceded by a backslash.
#
    if ($is_wildcard) {		# Was there actually a wildcard?
      my $allow_dotfiles = $Glob::allow_dot_files ||
	($file_regex_or_name =~ /^\\\./);
				# Allow dot files if they are explicitly
				# specified, or if we're automatically
				# accepting them.
      my $dir;
      my $search_perl_code = eval
	("sub {\n" .
	 "  foreach my \$finfo (values %{\$dir->{DIRCONTENTS}}) {\n" .
				# Used to use "each" here instead of values,
				# but perl 5.005_3 apparently has a bug where
				# it unaccountably returns nothing unless
				# you print out the keys of DIRCONTENTS. 
				# Argh!!!!!!!!!!!!!!!!!!!!!!
	 "    next unless \$finfo->{NAME} =~ /^$file_regex_or_name\$/;\n" .
	 ($allow_dotfiles ? "" : "    next if \$finfo->{NAME} =~ /^\\./;\n") .
	 "    \$finfo->exists_or_can_be_built and\n" . # File must exist, or
	 "        push(\@new_candidates, \$finfo);\n" .
	 "  }\n" .
	 "}\n");
				# Perl code to search for all the files in a
				# directory.
      foreach (@candidate_dirs) { # Look for the file in each of the 
				# possible directories.
	$dir = $_;		# This is a hack, because foreach $dir ()
				# doesn't load up the my variable $dir in 
				# a way that the eval'd expression above
				# can access it.
	$dir->{READDIR} or $dir->read_directory; # Load the list of filenames.
				# This also correctly sets the EXISTS flag
	&$search_perl_code;	# Search for those files.
      }
      next;			# We're done with this wildcard.
    }
#
# No wildcard characters were present.  Just see if this file exists in any
# of the candidate directories.
#
    my $dir;
    foreach $dir (@candidate_dirs) {
      if ($_ eq "..") {		# Go up a directory?
	push @new_candidates, $dir->{".."};
      }
      elsif ($_ eq ".") {	# Stay in same directory?
	push @new_candidates, $dir;
      }
      else {
	$dir->{READDIR} or $dir->read_directory; # Load the list of filenames.
	my $finfo = $dir->{DIRCONTENTS}{$file_regex_or_name}; # See if this entry exists.
	$finfo && $finfo->exists_or_can_be_built &&
	  !$finfo->{IS_PHONY} and	# Not a phony target?
	    push(@new_candidates, $finfo);
      }
    }

  }

  return sort { $a->{NAME} cmp $b->{NAME} ||
		  $a->name cmp $b->name } @new_candidates;
				# Return a sorted list of matching files.
}

=head2 Glob::find_all_subdirs

  my @subdirs = Glob::find_all_subdirs($dirinfo)

Returns FileInfo structures for all the subdirectories immediately under
the given directory.  These subdirectories might not exist yet; we return
FileInfo structures for any FileInfo for which has been treated as a 
directory in calls to FileInfo::file_info.

We do not follow symbolic links.  This is necessary to avoid infinite
recursion and a lot of other bad things.

=cut
sub find_all_subdirs {
  my $dirinfo = shift @_;	# Get a fileinfo struct for this directory.

#
# First find all the directories that currently exist.  (There may be other
# files with a DIRCONTENTS field that don't exist yet; presumably these will
# become directories.)  We make sure that all real directories have a
# DIRCONTENTS array (even if it's empty).
#
  if (!$dirinfo->{SCANNED_FOR_SUBDIRS}++ &&
				# Don't do this again, because we may have
				# to stat a lot of files.
      $dirinfo->is_dir) {	# Don't even try to do this if this directory
				# itself doesn't exist yet.

    foreach (find_real_subdirs($dirinfo)) {
      $_->{DIRCONTENTS} ||= {};	# Make sure that each of the directories is
				# at least tagged as such.
    }
  }

#
# Now return a list of FileInfo structures that have a DIRCONTENTS field.
#
  my @subdirs;

  foreach my $finfo (values %{$dirinfo->{DIRCONTENTS}}) {
    next if $finfo->is_symbolic_link; # Don't return symbolic links, or else
				# we can get in trouble with infinite 
				# recursion.
    $finfo->{DIRCONTENTS} and push @subdirs, $finfo;
  }

  return @subdirs;
}

#
# This is an internal subroutine which finds all the subdirectories of a given
# directory as fast as possible.  Unlike find_all_subdirs, this will only
# return the subdirectories that currently exist; it will not return 
# subdirectories which don't yet exist but have valid FileInfo structures.
#
sub find_real_subdirs {

  my $dirinfo = shift @_;	# Get the directory to search.

#
# Find the number of expected subdirectories.  On all unix file systems, the
# number of links minus 2 is the number of expected subdirectories.  This
# means that we can know without statting any files whether there are any
# subdirectories.
#
  my $dirstat = $dirinfo->stat_array;
  my $expected_subdirs = 0;
  defined($dirstat->[3]) and	# If this directory doesn't exist, then it 
				# doesn't have subdirectories.
    $expected_subdirs = $dirstat->[3]-2;

  $expected_subdirs or return (); # Don't even bother looking if this is a
				# leaf directory.
  $dirinfo->{READDIR} or $dirinfo->read_directory;
				# Load all the files known in the directory.
#
# Here we apply a simple heuristic optimization in order to avoid statting
# most of the files in the directory.  Looking for subdirectories is
# time-consuming if we have to stat every file.  Since we know the link count
# of the parent directory, we know how many subdirectories we are looking for
# and we can stop when we find the right amount.  Furthermore, some file names
# are unlikely to be directories.  For example, files with '~' characters in
# them are usually editor backups.  Similarly, files with alphabetic
# extensions (e.g., ".c") are usually not directories.
#
# On other operating systems, this would be a lot easier since directories
# often have an extension like ".dir" that uniquely identifies them.
#
# More detailed heuristics are possible, but we have to balance the cost of
# testing the heuristics with the cost of doing the stats.
#
  my ($dirfilename, $dirfileinfo);
  my @subdirs;			# Where we build up the list of subdirectories.

#
# First pass: look for files without extensions and ~ characters.
#
  while ($expected_subdirs > 0 &&
	 (($dirfilename, $dirfileinfo) = each %{$dirinfo->{DIRCONTENTS}})) {
				# Look at each file in the directory.
    next if $dirfilename =~ /\~/; # Skip editor backups.
    next if $dirfilename =~ /[A-Za-z]\.[0-9]$/;	
				# Skip man pages.  (These can be pretty
				# expensive to stat since there's often a lot
				# of them.)  This will not skip directories
				# like "perl-5.6.0" because the period
				# must be preceded by an alphabetic char.
    next if $dirfilename =~ /.\.[A-Za-z]+$/;
				# Skip files with alphabetic extensions.  Don't
				# skip files with numeric extensions, since
				# version numbers are often placed in
				# directory names.  Note that this does not
				# skip files with a leading '.'.

    if ($dirfileinfo->is_dir) {
      push(@subdirs, $dirfileinfo) # Note this directory.
	unless $dirfilename =~ /^\./ && !$Glob::allow_dot_files;
				# Skip dot directories.
      --$expected_subdirs;	# We got one of the expected subdirs.
    }
  }

#
# Second pass: look at all files (except the ones we looked at on the previous
# pass).
#
  while ($expected_subdirs > 0 && 
	 (($dirfilename, $dirfileinfo) = each %{$dirinfo->{DIRCONTENTS}})) {
				# Look at each file in the directory that we
				# haven't looked at before.
    next unless ($dirfilename =~ /\~/ || # We skipped editor backups.
		 $dirfilename =~ /[A-Za-z]\.[0-9]$/ || # We skipped man pages.
		 $dirfilename =~ /.\.[A-Za-z]+$/);
				# We skipped files with alphabetic extensions.

    if ($dirfileinfo->is_dir) {
      push(@subdirs, $dirfileinfo) # Note this directory.
	unless $dirfilename =~ /^\./ && !$Glob::allow_dot_files;
				# Skip dot directories.
      --$expected_subdirs;	# We got one of the expected subdirs.
    }
  }

  return @subdirs;
}

=head2 Glob::find_all_subdirs_recursively

  my @subdirs = Glob::find_all_subdirs_recursively($dirinfo);

Returns FileInfo structures for all the subdirectories of the given
directory, or subdirectories of subdirectories of that directory,
or.... 

The subdirectories are returned in a breadth-first manner.  The directory
specified as an argument is not included in the list.

Subdirectories beginning with '.' are not returned unless
$Glob::allow_dot_files is true.

=cut

sub find_all_subdirs_recursively {
  my @subdirs;

  if ($Glob::allow_dot_files) {
    @subdirs = &find_all_subdirs; # Start with the list of our subdirs.
    for (my $subdir_idx = 0; $subdir_idx < @subdirs; ++$subdir_idx) {
				# Use this kind of loop because we'll be adding
				# to @subdirs.
      push(@subdirs, find_all_subdirs($subdirs[$subdir_idx]));
				# Look in this directory for subdirectories.
    }
  }
  else {			# Same code, except that we don't search
				# subdirectories that begin with '.'.
    @subdirs = grep($_->{NAME} !~ /^\./, &find_all_subdirs); 
				# Start with the list of our subdirs.
    for (my $subdir_idx = 0; $subdir_idx < @subdirs; ++$subdir_idx) {
				# Use this kind of loop because we'll be adding
				# to @subdirs.
      push(@subdirs, grep($_->{NAME} !~ /^\./, 
			  find_all_subdirs($subdirs[$subdir_idx])));
				# Look in this directory for subdirectories.
    }
    
  }	

  return @subdirs;
}

#
# This subroutine converts a wildcard to a regular expression.  If no wildcards
# were actually used, then it returns just the filename (after removing
# backslashes).
#
# Arguments:
# a) The wildcard string to convert to a regular expression.

#
# Returns:
# a) A flag indicating whether in fact there were any wildcards.
# b) The regular expression, if there was a wildcard, or the filename with
#    backslashes removed, if there were no wildcards.
#    The returned regular expression does not have a leading "^" or a trailing
#    "$".
#
sub wild_to_regex {
  my $fname = $_[0];		# Access the filename.

  if ($fname =~ /[\*\[\?]/) {	# Is it possible that there are wildcards?  If
				# not, don't bother to do the more complicated
				# parsing.
    my $is_wildcard = 0;	# Haven't seen a wildcard yet.
    my $file_regex = '';	# A regular expression to match this level.
    pos($fname) = 0;
      
  parseloop:
    while (pos($fname) < length($fname)) {
      if ($fname =~ /\G([^\\\[\]\*\?]+)/gc) { # Ordinary characters?
	$file_regex .= quotemeta($1);	# Just add to regex verbatim, with
				# appropriate backslashes.
      }

      elsif ($fname =~ /\G(\\.)/gc) { # \ + some char?
	$file_regex .= $1;	# Just add it verbatim.
      }

      elsif ($fname =~ m@\G\*\*/@gc) { # Special "**/" wildcard?
	$file_regex .= "(?:[^\\/.][^\\/]*\\/)*"; # Match any number of directories.
	$is_wildcard = 1;
      }

      elsif ($fname =~ /\G\*/gc) { # Any number of chars?
	$is_wildcard = 1;	# We've actually seen a wildcard char.
	$file_regex .= "[^\\/]*"; # Convert to proper regular expression syntax.
      }

      elsif ($fname =~ /\G\?/gc) { # Single character wildcard?
	$is_wildcard = 1;
	$file_regex .= "[^\\/]";
      }

      elsif ($fname =~ /\G\[/gc) { # Beginning of a character class?
	$is_wildcard = 1;
	$file_regex .= "[";	# Begin the character class.
      CLASSLOOP:		# Nested loop for parsing the character class.
	{
	  if ($fname =~ /\G([^\\\]]+)/gc) { $file_regex .= $1; redo CLASSLOOP; }
				# No quotemeta because we want it to
				# interpret '-' and '^' as wildcards, and those
				# are the only special characters within a
				# character class except \.
	  if ($fname =~ /\G(\\.)/gc)      { $file_regex .= $1; redo CLASSLOOP; }
	  if ($fname =~ /\G\]/gc)         { $file_regex .= "]"; }
	  else { die("$0: unterminated character class in '$fname'\n"); }
	}
      }
      else {
	die "How in the world did I get here?";
      }
    }

    if (!$is_wildcard) {	# No wildcard actually seen?
      $fname =~ s/\\(.)/$1/g;	# Unquote any backslashed characters.
      return (0, $fname);	# Just return the filename.
    }

    return (1, $file_regex);	# It's a regular expression.
  }

  $fname =~ s/\\(.)/$1/g;	# Unquote any backslashed characters.
  return (0, $fname);		# Just return the filename.
}

=head2 Glob::wildcard_action

You generally should not call this subroutine directly; it's intended to be
called from the chain of responsibility handled by main::wildcard_action.

This subroutine is the key to handling wildcards in pattern rules and
dependencies.  Usage:

  wildcard_action @wildcards, sub {
    my ($finfo, $was_wildcard_flag) = @_;
    ...
  };

The subroutine is called once for each file that matches the wildcards.  If at
some later time, files which match the wildcard are created (or we find rules
to build them), then the subroutine is called again.  (Internally, this is done
by FileInfo::publish, which is called automatically whenever a file which
didn't used to exist now exists, or whenever a build rule is specified for a
file which does not currently exist.)

You can specify non-wildcards as arguments to wildcard_action.  In this case,
the subroutine is called once for each of the files explicitly listed, even if
they don't exist and there is no build command for them yet.  Use the second
argument to the subroutine to determine whether there was actually a wildcard
or not, if you need to know.

As with Glob::zglob, wildcard_action will match files in directories which
don't yet exist, as long as the appropriate FileInfo structures have been put
in by calls to file_info().

Of course, there are ways of creating new files which will not be detected by
wildcard_action(), since it only looks at things that it expects to be
modified.  For example, directories are not automatically reread (but when
they are reread, new files are noticed).  Also, creation of new symbolic links
to directories may deceive the system.

There are two restrictions on wildcards handled by this routine.  First, it
will not soft-linked directories correctly after the first wildcard.  For
example, if you do this:

   **/xyz/*.cxx

in order to match all .cxx files somewhere in a subdirectory called "xyz", and
"xyz" is actually a soft link to some other part of the file system, then your
.cxx files will not be found.  This is only true if the soft link occurs
B<after> the first wildcard; something like "xyz/*.cxx" will work fine even if
xyz is a soft link.

Similarly, after the first wildcard specification, ".." will not work as
expected.  (It works fine if it's present before all wildcards.)  For example,
consider something like this:

   **/xyz/../*.cxx

In theory, this should find all .cxx files in directories that have a
subdirectory called xyz.  This won't work with wildcard_action (it'll work
fine with zglob_fileinfo above, however).  wildcard_action emits a warning
message in this case.

=cut

sub wildcard_action {
  my $subr = pop @_;		# Last argument is the subroutine to execute.

#
# We first call the subroutine immediately with all files that we currently
# know about that match the wildcard.  
#
 file_loop:
  foreach my $filename (@_) {
#
# Split this apart into the directories, and handle each layer separately.
# First handle any leading non-wildcarded directories.
#
    my $initial_finfo = $FileInfo::CWD_INFO; # Start at the current directory.
    my @file_pieces = split(/\/+/, $filename);
				# Break up into directories.
    if ($file_pieces[0] eq "") { # Was there a leading slash?
      $initial_finfo = $FileInfo::root; # Start at the root.
      shift @file_pieces;	# Discard the leading slash.
    }

    while (@file_pieces) {	# Loop through leading non-wildcarded dirs:
      my ($wild_flag, $name_or_regex) = wild_to_regex($file_pieces[0]);
      last if $wild_flag;	# Quit if we hit the first wildcard.
      $initial_finfo = file_info($name_or_regex, $initial_finfo);
      shift @file_pieces;	# Get rid of that piece.
    }

#
# At this point, $initial_finfo is the FileInfo entry for the file that 
# matches the leading non-wildcard directories.
#
    if (@file_pieces == 0) {	# Is there anything left?
      &$subr($initial_finfo, 0); # No.  Just call the subroutine directly,
				# and set the no-wildcard flag.
    }
    else {
#
# We have wildcards.  Convert the whole rest of the string into a regular
# expression that matches:
#
      my $idx = 0;
      while ($idx < @file_pieces) {
	if ($file_pieces[$idx] eq ".") { # Remove useless "./" components
	  splice(@file_pieces, $idx, 1); # (since they will mess up the regex).
	  next;			# Go back to top without incrementing idx.
	}

	if ($file_pieces[$idx] eq "..") { # At least give a warning message
	  warn "$0: .. is not supported after a wildcard 
  in the wildcard expression \"$filename\".
  This will only match existing files.\n";
				# Let user know this will not do what he thinks.
	}

	++$idx;
      }

      my $remaining_path = join("/", @file_pieces);
      foreach (zglob_fileinfo($remaining_path, $initial_finfo, 1)) {
	&$subr($_, 1);		# Call the subroutine on each file that matches
      }				# right now.  We give an extra argument to 
				# zglob_fileinfo that says not to match soft-
				# linked directories, so the behavior is at
				# least consistent and we do not have subtle
				# bugs in makefiles.  The reason is that 
				# we match based on the text of the 
				# filename including the directory
				# path, and (in order to make that path unique)
				# the path cannot contain soft-linked dirs.

				# Associate a wildcard checking subroutine with
				# this directory, so that any subsequent files
				# which match also cause the subroutine to
				# be  called.
      my $wild_regex = "^" . wild_to_regex($remaining_path) . "\$";
      push(@{$initial_finfo->{WILDCARD_ROUTINES}},
	   eval "sub { \$_[1] =~ /$wild_regex/ and &\$subr(\$_[0], 1); }");
				# Note that wild_to_regex has quoted all the
				# special characters (except some slashes),
				# so it is safe to interpolate in the string
				# like this.
				# $subr is a my value, so it is passed into
				# the subroutine by the closure mechanism.
      $@ and die "internal error: $@\n"; # Make sure the expression compiles
				# properly.
    }
  }

  1;				# Return true, because we accept all wildcards.
				# We are the last-chance handler in the chain
				# of responsibility for recognizing wildcards.
}

1;


__END__
