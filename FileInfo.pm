package FileInfo;
require Exporter;
use Cwd;

# $Id: FileInfo.pm,v 1.4 2003/07/15 00:50:38 grholt Exp $

#use English;
# Don't ever include this!  This turns out to slow down 
# Signature::c_compilation_md5::md5sum_c_tokens by at least three orders
# of magnitude!

@ISA = qw(Exporter);
@EXPORT = qw(file_info chdir);
@EXPORT_OK = qw(relative_filename absolute_filename traverse $CWD_INFO);

use strict;

=head1 NAME

FileInfo -- cached information about files and directories

=head1 USAGE

  use FileInfo;
  chdir($new_dir);		# Changes to new directory and keeps track
				# of the directory name in the variable
				# $CWD_INFO.
  $finfo = file_info("filename");

  $finfo->build_handle;		# Returns the handle for the process that is
				# building (or has built) the file.
  $finfo->set_build_handle($handle); # Sets the handle for the process
				# that's currently building the file.


  $build_rule = $finfo->build_rule; # Returns the rule for building the file,
				# if such a rule exists.
  $finfo->set_build_rule($rule); # Set the rule used to build this file.

  $finfo->build_info_string("key");
				# Returns a piece of information from the build
				# info file, if there is one.
  $finfo->set_build_info_string("key", "value");
  FileInfo::update_build_infos(); # Flushes build info cache to disk.

  if ($finfo->exists_or_can_be_built) {}
  my $dir_finfo = $finfo->parent; # The directory containing this file.
  $name = $finfo->absolute_filename;	# Returns absolute file name.
				# If more than one name can be used
				# (because of soft links), the shortest
				# possible one is used to avoid problems with
				# the automounter.
  $name = $finfo->relative_filename;
  $relative_fname = $finfo->relative_filename("dir");
  $relative_fname = $finfo->relative_filename($dirinfo);
				# Returns name relative to given directory.

  $finfo->may_have_changed;	# Indicate that the file may have changed on
				# disk, so invalidate (or check) cached info.

  if ($finfo->file_exists) { ... }
  $date = $finfo->file_mtime;

  @file_infos = find_file("filename"); # Find a file with a given name
				# somewhere in the directory hierarchy.
				# (See also Glob::zglob for a more powerful
				# wildcarded find procedure.)

  if ($finfo->is_dir) { ... }
  if ($finfo->is_writable) { ... }
  if ($finfo->is_executable) { ... }
  if ($finfo->is_symbolic_link) { ... }

  $finfo->unlink;		# Deletes the file.

  my $link_finfo = $finfo->dereference;
				# Dereference a symbolic link.

  @file_infos = $finfo->dir_contents;
				# If the file is a directory, this is the list
				# of FileInfo structures in that directory.
  $finfo->read_directory;	# Try to (re)read the contents of a directory.

  my $stat_array = $finfo->stat_array; # Return the array returned by stat().
  my $lstat_array = $finfo->lstat_array; # Return the array returned by lstat().
  $signature = $finfo->signature;
  $finfo->set_signature_function(\&function);
				# Set the function that is used to compute
				# the signature for this file.

  FileInfo::relative_filename("file", "dir"); # Returns relative name of file
				# with respect to the directory.
  FileInfo::absolute_filename("file"); # Returns absolute name of file.

=head1 DESCRIPTION

FileInfo is an efficient way to avoid re-statting files and rereading
directories.  For each known file, there is a FileInfo structure that
describes what is known about the file.  You may add arbitrary
additional information to the structure.

FileInfo is designed so it won't be confused by soft-linked directories.
However, it will have problems if a soft link initially points to one
directory and then is changed to point to a different directory, or if files
are referred to thruogh a symbolic link to a directory before the symbolic
link is actually created.  Generally speaking, it's not a good idea to modify
existing soft links.

FileInfo can be used alone.  Some supplemental routines useful only in the
context of makepp are found in FileInfo_makepp.pm, and in fact that file
overrides some of the routines here.

=cut

#
# Definitions for use with the stat command:
#
my $stat_dev     = 0;
my $stat_inode   = 1;
my $stat_mode    = 2;
my $stat_nlink   = 3;
my $stat_uid     = 4;
my $stat_gid     = 5;
my $stat_rdev    = 6;
my $stat_size    = 7;
my $stat_atime   = 8;
my $stat_mtime   = 9;
my $stat_ctime   = 10;
my $stat_blksize = 11;
my $stat_blocks  = 12;

my $S_IFLNK = 0120000;		# Bit in stat modes field that indicates this
				# is a symbolic link.
my $S_IFDIR = 040000;		# This is a directory.

my $is_windows = $^O eq 'cygwin'; # If on windows NT, filenames are case 
				# insensitive but case preserving.

#
# All of the information is stored in the structure below.  %file_info
# is an associative array indexed by the top-level directories.  In
# addition, for every directory that we stat, we store the device and
# inode number so we won't be confused by symbolic links with directories.
#
$FileInfo::root = bless {NAME => '',
			 SHORTEST_FULLNAME => '',
			 DIRCONTENTS => {}
			 };

#
# Here are all the possible keys that can be contained in a FileInfo
# structure.  Of course, not all FileInfo structures will have all of these
# fields.  As usual in OOP, these fields should not be explicitly accessed
# except by member functions; this documentation is provided as internals
# documentation of the FileInfo class.
#
# Key		Meaning
# ..		A reference to the FileInfo of the parent directory whose
#		DIRCONTENTS field contains this file.
# BUILD_HANDLE	A Fork::Process handle for the process that is currently 
#		building or has already built this file.
# BUILD_INFO	If build information has been loaded, this hash contains the
#		key/value pairs.  See build_info_string() and
#		set_build_info_string().
# RULE		The rule object for the file, if known.
# DIRCONTENTS	If this is a directory, this contains a reference to another
#		hash of the files in the directory.  The key for the hash
#		is the filename.  Sometimes files which aren't directories
#		can have a DIRCONTENTS field too.  This occurs when they are
#		referenced as a directory, i.e., "filename/filename".
#		Usually this is for a directory that doesn't exist yet but
#		will be created.
#		The DIRCONTENTS field is only created by the subroutine
#		mark_as_directory().  This is so the wildcard routines are
#		reliably informed that a new directory exists.  See the
#		documentation for Glob::wildcard_action for details.
# EXISTS	1 if we know the file exists (either because we lstatted it,
#		or because its name was in the directory), 0 if we know
#		it doesn't exist (because its name wasn't in the directory,
#		or the lstat failed).
# IS_PHONY	True if this has been tagged as a phony target.
# LINK_DEREF	For soft links, the cached value of the symbolic link.
# LSTAT		A reference to the array returned by lstat.
# NAME		The name of the file (without any directories).
# PUBLISHED	True if we've alerted any waiting wildcard subroutines that
#		this file exists.
# READDIR	1 if we've tried to read this directory.
# REPOSITORY_FILE
#		For files that can be imported from a repository, this field
#		contains a reference to the FileInfo struct for the file in
#		the repository.
# SCANNED_FOR_SUBDIRS
#		If this is a directory, this flag indicates that we have
#		found all of the subdirectories under the current directory,
#		i.e., we don't need to stat any more files to see if they
#		are subdirectories.
# SHORTEST_FULLNAME
#		There can be multiple names for a directory if soft links are
#		present.  Usually the shortest name is the most useful one,
#		since it's less likely to be some weird name from the
#		automounter.  When we form absolute filenames, we use the
#		shortest name so it's more portable to different machines
#		on the network.
# WILDCARD_ROUTINES
#		For a directory, this is a list of subroutines to be called
#		whenever a new file springs into existence in this directory
#		or any subdirectory.  These routines are used so that wildcards
#		can match files which didn't exist when the wildcard was
#		invoked.  See Glob::wildcard_action() and FileInfo::publish()
#		for details.
#

=head2 absolute_filename

  $str = $fileinfo->absolute_filename;
  $str = absolute_filename("relative file name");

  $str = $fileinfo->absolute_filename($dir);
  $str = absolute_filename("relative file name", $dir);

Returns the absolute file name.  The optional $dir argument specifies
where to start looking for the file (i.e., what the "." directory is).
If you don't specify this, uses the current directory.

=cut

sub absolute_filename {
  my $fileinfo = &file_info;	# Locate the file.

  my @subdirs = ();		# Build a list of subdirectories on the path.

  until (exists($fileinfo->{SHORTEST_FULLNAME})) {
    push(@subdirs, $fileinfo->{NAME});
				# Go up the tree until we find a directory
				# whose full name we know.
    $fileinfo = $fileinfo->{".."};
  }

  my $retstr = join("/", $fileinfo->{SHORTEST_FULLNAME}, reverse @subdirs);
  $is_windows and $retstr =~ s@^/([A-Za-z]:)@$1@;
				# Convert /C: to C:.  We converted the other
				# way so we could use unix file name syntax
				# everywhere.
  $retstr;
}

#
# A variant of absolute_filename that ignores symbolic links.  This is intended
# for internal use, when we need to supply a name to the operating system,
# especially when we lstat the file.
#
sub absolute_filename_nolink {
  my $fileinfo = $_[0];		# Locate the file.

  my $ret_str = $fileinfo->{NAME};

  return "/" if $fileinfo == $FileInfo::root;
				# Special case this one.
  for (;;) {
    $fileinfo = $fileinfo->{".."};
    last if $fileinfo == $FileInfo::root; # Quit when we reached the top.

    $ret_str = $fileinfo->{NAME} . "/$ret_str";
				# Add another directory.
  }

  $is_windows and $ret_str =~ s@^/([A-Za-z]:)@$1@;
				# Convert /C: to C:.  We converted the other
				# way so we could use unix file name syntax
				# everywhere.
  return "/$ret_str";
}

=head2 chdir("new dir")

  chdir("new dir")
  chdir($dirinfo)

Changes to the indicated directory, and keeps track of the change in
the variable $FileInfo::CWD_INFO.  Dies with a message if the chdir failed.

You can pass a FileInfo structure describing the directory instead of the
directory name itself if that is more convenient.

This subroutine is automatically exported into any packages that 
use FileInfo, so your chdirs will work automatically.

=cut

sub chdir {
  my $newdir = $_[0];		# Access the directory.
  my $status;
  if (ref($newdir) eq 'FileInfo') { # Were we passed a file information struct?
    return 0 if $newdir == $FileInfo::CWD_INFO; # Don't do the chdir if we're already there.
    $status = CORE::chdir($newdir->absolute_filename_nolink);
  } else {			# Passed a file name:
    $newdir = file_info($newdir); # Get the file information.
    return 0 if $newdir == $FileInfo::CWD_INFO; # Don't do the chdir if we're already there.
    $status = CORE::chdir($_[0]);
  }

  unless ($status) {
    if ($newdir->{ALTERNATE_VERSIONS}) { # Was it from a repository?
      $newdir->mkdir;           # Make it.
      $status = CORE::chdir($newdir->absolute_filename_nolink);
    }
    $status or
      die ("chdir: can't cd to directory " . $newdir->absolute_filename .
           "--$!\n");
  }

  publish($newdir);		# Make sure we know about this directory.

  $FileInfo::CWD_INFO = $newdir; # Store the new directory if that succeded.
}


=head2 dereference

  $finfo = $fileinfo->dereference;

If the file is a symbolic link, this returns a FileInfo structure for the file
it points to.  If the symbolic link points to another symbolic link, returns
what that link points to.  If the file is not a symbolic link, returns the
original FileInfo structure.

=cut
sub dereference {
  my $finfo = $_[0];		# Get the argument as a FileInfo struct.
  my $linkcount = ($_[1] || 0);
  if ($linkcount > 20) {
    die "symlink: infinite loop trying to resolve symbolic link ", $finfo->absolute_filename, "\n";
  }
  my $stat_arr = $finfo->lstat_array;	# Get the flags.
  (($stat_arr->[$stat_mode] || 0) & $S_IFLNK) != $S_IFLNK and return $finfo;
				# Not a symbolic link,
  my $link =
    ($finfo->{LINK_DEREF} ||= # Have we already dereferenced it?
     file_info(readlink($finfo->absolute_filename_nolink), $finfo->{".."}));
#
# See if using the symbolic link is a shorter way of referring to the
# file than the absolute path.  If so, use the shorter way.  This will usually
# get rid of junk from the automounter, and other stuff that may vary from
# machine to machine on the network.
#
  if (length($finfo->absolute_filename) < length($link->absolute_filename)) {
				# Found a shorter way to refer to the file?
    $link->{SHORTEST_FULLNAME} = $finfo->absolute_filename;
  }
  return $link->dereference($linkcount+1); # Get what it refers to.
}

=head2 exists_or_can_be_built

  if ($file_info->exists_or_can_be_built) { ... }

Returns true if the file exists.  This is overridden by a method of the same
name in FileInfo_makepp.pm, which returns true if the file already exists
or if we know of a way to build it.

=cut

# This is commented out because it needs to be superceded by the version in
# FileInfo_makepp.
#sub exists_or_can_be_built {
#  &file_exists;
#}

=head2 file_exists

  if ($file_info->file_exists) { ... }

Returns true (actually, returns the FileInfo structure) if the file exists,
and undef if it doesn't.

=cut

sub file_exists {
  my $fileinfo = $_[0];		# Get the file information structure.

  my $ex_flag = $fileinfo->{EXISTS};
  defined($ex_flag) and return $ex_flag ? $fileinfo : undef;
				# See if we already know whether it exists.
  $fileinfo->lstat_array;	# Stat it to see if it exists.  This will set
				# the EXISTS flag.
  return $fileinfo->{EXISTS} ? $fileinfo : undef;
}

=head2 file_info

  $finfo = file_info("filename");
  $finfo = file_info("filename", $dirinfo);

Returns the FileInfo structure for the given file.  If no FileInfo
structure exists, creates a new one for it.

If you pass a FileInfo structure to file_info, it just returns
its argument.

The optional second argument specifies a directory the file name
should be relative to.  By default, this is the current directory.

  foreach (@{$finfo->dir_contents}) {
    $_->exists_or_can_be_built or next;	# Skip if file doesn't exist.
				# (Files which don't exist can have FileInfo
				# entries, if you happened to call
				# file_info on them explicitly.)
    # do your thing here.
  }

If you want to iterate through all the files which are in a directory,
not just the ones encountered previously, then call $dirinfo->read_directory
before using the above code snippet.

=cut

sub file_info {
  my $file = $_[0];		# Access the file name.
  return $file if ref($file) eq 'FileInfo'; # Don't do anything if we were
				# already passed a FileInfo structure.

  if ($is_windows) {
    $file =~ s@^([A-Za-z]):@/$1:@; # If on windows, transform C: into /C: so it
				# looks like it's in the root directory.
    $file = lc($file);		# Switch to all lower case to avoid
				# confounds with mix case.
  }

  my $finfo = ($file =~ s@^/+@@ ? $FileInfo::root :
		 ($_[1] || $FileInfo::CWD_INFO));
				# Start at current directory or
				# the top level or specified directory.
  $file =~ s@/+$@@;		# Strip trailing slashes.

  foreach (split(/\/+/, $file)) { # Handle each piece of the filename.
#
# Also, we now know the parent is (or possibly will be) a directory, so we
# need to publish it as a directory.  This is necessary so wildcard routines
# install themselves appropriately in the directory.
#
    unless (exists($finfo->{DIRCONTENTS})) {
				# If the DIRCONTENTS field doesn't exist, then
				# we haven't checked yet whether the parent is
				# a directory or not.
      my $orig_finfo = $finfo;
      if ($finfo->is_symbolic_link) { # Follow symbolic links.
	$finfo = $finfo->dereference; # Get where it points to.
	mark_as_directory($finfo); # Remember that this is a directory.
	$orig_finfo->{DIRCONTENTS} = $finfo->{DIRCONTENTS};
				# Set the DIRCONTENTS field of the soft link
				# to point to the DIRCONTENTS of the actual
				# directory.
      } else {
	mark_as_directory($finfo); # Let the wildcard routines know that we
				# discovered a new directory.
	publish($finfo);	# Alert any wildcard routines.
      }

    }

#
# At this point, $finfo points to the the parent directory.  Now handle the 
# file:
#
    if ($_ eq "..") {		# Go up a directory?
      $finfo = $finfo->{".."} || $FileInfo::root;
				# Don't go up above the root.
    } elsif ($_ eq ".") {	# Stay in same directory?
				# Do nothing.

    } else {
      $finfo = ($finfo->{DIRCONTENTS}{$_} ||=
		bless { NAME => $_, ".." => $finfo->{LINK_DEREF} || $finfo });
				# Point to the entry for the file, or make one
				# if there is not one already.
    }

  }

  return $finfo;
}

=head2 file_mtime

  $date = $file_info->file_mtime;
  $date = file_mtime("filename");

Returns the last modification time for the given file.  If the file is a
symbolic link, this returns the modification for the file the link refers to.

=cut

sub file_mtime {
  my $stat = &stat_array;
  $stat->[$stat_mtime];		# Return the time, or undef if it doesn't
				# exist.
}

=head2 find_file

  @file_infos = find_file("filename");

Finds all files in the directory tree that have the given file name.
The files must either exist or have a build command specified for
them (this is used by PBuilder).  The file name must match exactly.
The name should not contain any directory specification.

Returns a null array if nothing is found.

See also Glob::zglob for a find procedure that will work with wildcards.
Note especially its "**" wildcard.  C<find_file("filename")> is equivalent
to (but faster than) C<zglob_fileinfo("**/filename")>, except that it will not
read any directories--it only looks at files that are currently known.

This function will not return any files located in directories that don't
exist, even if there are FileInfo structures for them.

=cut
#
# The following subroutine recursively searches a FileInfo directory tree
# looking for an entry for a file with a given name that exists or
# has a build command defined.  Arguments:
# 1) The name of the file.
# 2) The FileInfo tree to search.  If not specified, defaults to
#    the root directory.  All subdirectories of the given directory are
#    searched.
#
# Returns an array consisting of the FileInfo structures that it found.
#
sub find_file {
  my ($target_fname, $dirinfo) = @_; # Name the arguments.
  $is_windows and $target_fname = lc($target_fname);
  $dirinfo ||= $FileInfo::root; # Default to root directory if not
				# specified.

  my @ret_vals = ();		# Files found so far.
  my ($filename, $fileinfo);
  while (($filename, $fileinfo) = each %{$dirinfo->{DIRCONTENTS}}) {
				# Look at everything here.
    push(@ret_vals, find_file($target_fname, $fileinfo))
      if ($fileinfo->is_dir);	# Recursively descend into the tree.

    if ($filename eq $target_fname && # Matching file that exists or can be built?
	$fileinfo->exists_or_can_be_built) {
      push(@ret_vals, $fileinfo); # Remember it.
    }
  }

  @ret_vals;			# Return what we found, if anything.
}

=head2 is_dir

  if ($fileinfo->is_dir) { ... }

Returns true (actually, returns the fileinfo structure) if the given file is
actually a directory.  Does not return true for soft links that point to
directories.  (If you want to do that, you can call is_symbolic_link and then
follow the symbolic link using dereference.)  See under C<file_info> for how
to examine the contents of the directory.

is_dir() only returns true for directories that currently exist.  You can
create FileInfo structures for directories that don't exist yet; to check for
this kind of directory, use is_or_will_be_dir().

=cut

sub is_dir {
  my $stat = &lstat_array;	# Get the stat values.

  (($stat->[$stat_mode] || 0) & $S_IFDIR) ? $_[0] : undef;
}

=head2 is_or_will_be_dir

  if ($fileinfo->is_dir) { ... }

Returns true (actually, returns the fileinfo structure) if the given file is
actually a directory, or if it will be a directory (because file_info() was
called using it as a directory).  Also returns true for soft links that point
to directories.

=cut
sub is_or_will_be_dir {
  $_[0]->{DIRCONTENTS} and return $_[0];
  $_[0]->dereference->is_dir and return $_[0];

  return undef;
}


=head2 is_executable

  if ($finfo->is_executable) { ... }

Returns true (actually, returns the FileInfo structure) if the given file is
executable by this user.  We don't actually handle the group executable
bit correctly right now, since it's a pain to find out what groups this
user is actually in.

=cut

sub is_executable {
  my $stat = $_[0]->stat_array;	# Get the status info.
  @$stat or return undef;	# File doesn't exist.
  ($stat->[$stat_mode] & (011)) || # User or group executable?
    (($stat->[$stat_mode] & 0100) && # User executable?
      $stat->[$stat_uid] == $>) and return $_[0]; # We're the owner?
				# It's executable.
  return undef;
}

=head2 is_readable

  if ($finfo->is_readable) { ... }

Returns true if the given file or directory can be read.

=cut

sub is_readable {
  my $finfo = $_[0];
  if (exists($finfo->{IS_READABLE})) { # Use cached value.
    return $finfo->{IS_READABLE};
  }	

  $finfo->{IS_READABLE} = 0;	# Assume it won't be readable.
  my $stat = $finfo->stat_array; # Get stat info.
  $stat && (($stat->[$stat_mode] || 0) & 0x444) or return undef;
				# If no one can read the file, then it's
				# definitely not readable.
#
# Checking for file permissions is very complicated and file-system
# dependent, so we just try to open the file.
#  
  local *FH;			# Make a local file handle.
  if ($stat->[$stat_mode] & $S_IFDIR) {	# A directory?
    opendir(FH, $finfo->absolute_filename) or return undef;
    closedir FH;
  } else {
    open(FH, $finfo->absolute_filename) or return undef;
    close FH;
  }

  return $finfo->{IS_READABLE} = 1; # File is readable.
}

=head2 is_symbolic_link

  if ($finfo->is_symbolic_link) { ... }

Returns true (actually, returns the FileInfo structure) if the given file 
is a symbolic link.

=cut

sub is_symbolic_link {
  my $lstat = $_[0]->lstat_array; # Get status info.
  @$lstat &&			# File exists.
    ($lstat->[$stat_mode] & $S_IFLNK) == $S_IFLNK and # Check the mode.
      return $_[0];		# Return true.
  
  return undef;			# File is not a symbolic link.
}

=head2 is_writable

  if ($dirinfo->is_writable) { ... }

Returns true if the given directory can be written to.  Because of the
complexity of testing for permission, we test by actually trying to
write a file to that directory.

=cut

sub is_writable {
  local *FH;			# Make a local file handle.

  my $dirinfo = $_[0];		# Access the fileinfo struct.
  if (exists($dirinfo->{IS_WRITABLE})) { # Did we try this test before?
    return $dirinfo->{IS_WRITABLE}; # Use the cached value.
  }

  if ($> == 0 && $FileInfo::uid_for_check != 0) {
				# Are we running as root?
    $> = $FileInfo::uid_for_check; # Check with a different UID because root
    $) = $FileInfo::gid_for_check; # can read too much.
				# See setting of uid_for_check for an 
				# explanation of why.
    my $retval = &is_writable;	# Run the check.
    $> = $FileInfo::orig_uid;	# Restore the UID/GID.
    $) = $FileInfo::orig_gid; 
    return $retval;
  }

  my $test_fname = $dirinfo->absolute_filename_nolink . "/.makepp_testfile";
				# Try to create a file with an unlikely
				# name.  (I hope this cause no problems
				# on unusual file systems.)

  if (open(FH, "> $test_fname")) { # Can we create such a file?
    close FH;
    CORE::unlink $test_fname;	# Get rid of it.
    return $dirinfo->{IS_WRITABLE} = 1;
  }

  return $dirinfo->{IS_WRITABLE} = 0;
}
  
=head2 link_to

  $finfo->link_to($other_finfo);

Sets up $finfo to be a soft link to the file contained in $other_finfo.

=cut
sub symlink {
  my ($dest, $link_from) = @_;

  CORE::symlink($link_from->relative_filename($dest->{".."}),
		$dest->absolute_filename_nolink)
    or die "error linking " . $link_from->absolute_filename . " to " . $dest->absolute_filename . "--$!\n";
  $dest->{EXISTS} = 1;		# We know this file exists now.
}


=head2 lstat_array

   $statinfo = $fileinfo->stat_array;
   $uid = $statinfo->[4];	# Or whatever field you're interested in.

Returns the array of values returned by the C<lstat> function on the file.
The values are cached, so calling this repeatedly entails only minimal extra
overhead.

=cut

sub lstat_array {
  my $fileinfo = $_[0];		# Get the fileinfo structure.

  my $stat_arr = $fileinfo->{LSTAT}; # Get the cached value.
  if (!defined($stat_arr)) {	# No cached value?
    $stat_arr = $fileinfo->{LSTAT} = [ lstat($fileinfo->absolute_filename_nolink) ];
				# Restat the file, and cache the info.
    if ($fileinfo->{EXISTS} = (@$stat_arr != 0)) {
				# Update the EXISTS flag too.
      publish($fileinfo);	# If we now know the file exists but we didn't
				# use to know that, activate any waiting 
				# subroutines.
#
# When a file has been created or changed, it's possible that it has become
# a directory.  If this is true, then we definitely need to tag it as a 
# directory so all the wildcard routines know about it.  Otherwise we'll miss
# a lot of files.
#
      if (!$fileinfo->{DIRCONTENTS} && # Not previously known as a directory?
	  ($stat_arr->[$stat_mode] & $S_IFDIR) != 0) { # Now it's a dir?
	mark_as_directory($fileinfo); # Tell the wildcard system about it.
      }
	    
    }
  }

  return $stat_arr;
}

=head2 may_have_changed

  $finfo->may_have_changed;

Indicates that a file may have changed, so that any cached values (such as the
signature or the file time) are invalid.

=cut
sub may_have_changed {
  my $finfo = $_[0];

  my $old_time = $finfo->file_mtime;
  delete $finfo->{LINK_DEREF};
  delete $finfo->{LSTAT};
  delete $finfo->{EXISTS};
  delete $finfo->{IS_READABLE};
#
# Don't force rereading the build information if the file hasn't actually
# changed.
#
  if (($finfo->file_mtime || 0) != ($old_time || 0)) {
    delete $finfo->{SIGNATURE};
    delete $finfo->{BUILD_INFO};
    delete $finfo->{NEEDS_BUILD_UPDATE};
				# There's no build information to update.
  } else {
    delete $finfo->{IS_READABLE};
    delete $finfo->{IS_WRITABLE};
  }
}

=head2 mkdir

   $dirinfo->mkdir;

Makes the directory specified by the FileInfo structure (and any parent
directories that are necessary).

=cut
sub mkdir {
  my $dirinfo = $_[0];

  return if $dirinfo->is_dir;	# If it's already a directory, don't do
				# anything.
  $dirinfo->{".."}->mkdir;	# Make sure the parent exists.
  CORE::mkdir($dirinfo->absolute_filename_nolink, 0777);
				# Make the directory.
  $dirinfo->may_have_changed;	# Restat it.
  $dirinfo->{IS_WRITABLE} = 1;	# This directory is now writable.
  $dirinfo->{IS_READABLE} = 1;
}

=head2 parent

   $dirinfo = $finfo->parent;

Returns the directory containing the file.

=cut
sub parent {
  return $_[0]->{".."};
}

=head2 read_directory

  $dirinfo->read_directory;

Rereads the given directory so we know what files are actually in it.

=cut

sub read_directory {
  my $dirinfo = $_[0];		# Find the directory.
  local (*DIRHANDLE);		# Make a local file handle.

  my ($fname, $fileinfo);
  while (($fname, $fileinfo) = each %{$dirinfo->{DIRCONTENTS}}) {
    delete $fileinfo->{EXISTS};	# Forget what we used to know about which
				# files exist.
    delete $fileinfo->{IS_READABLE};
  }

  opendir(DIRHANDLE, $dirinfo->absolute_filename_nolink) || return;
				# Just quit if we can't read the directory.
				# This can happen for directories which don't
				# exist yet, or directories which are unreadable.

  foreach (readdir(DIRHANDLE)) {
    next if $_ eq '.' || $_ eq '..'; # Skip the standard subdirectories.
    $is_windows and $_ = lc($_);
    my $finfo = ($dirinfo->{DIRCONTENTS}{$_} ||=
		 bless { NAME => $_, ".." => $dirinfo });
				# Get the file info structure, or make
				# one if there isn't one available.
    $finfo->{EXISTS} = 1;	# Remember that this file exists.
    publish($finfo);		# Activate any wildcard routines.
  }
  closedir(DIRHANDLE);

  $dirinfo->{READDIR} = 1;	# Remember that we read this directory.
}

=head2 relative_filename

  $str = $fileinfo->relative_filename;
  $str = $fileinfo->relative_filename($dir);
  $str = relative_filename($filename);
  $str = relative_filename($filename, $dir);

Return a file name relative to the given directory, if specified.
If no directory is specified, uses the current working directory.
If the directory and the file have no directories in common (e.g.,
like "/home/mystuff/stuff" and "/usr/local/bin"), then an absolute
file name is returned.

=cut

sub relative_filename {
  my $dir = $_[1] ? file_info($_[1]) : $FileInfo::CWD_INFO;
  my $fileinfo = file_info($_[0], $dir); # Point to the file.

  $fileinfo == $FileInfo::root and return "/"; # Special case this.
  return $fileinfo->{NAME} if $fileinfo->{".."} == $dir;
				# Optimize for the special case where the
				# file is in the given directory.

#
# Find the longest common prefix in the directories.  We can't take the
# string returned from absolute_filename, since that uses symbolic links,
# and .. doesn't do what you'd expect if symbolic links are involved.  So
# we compare only the physical directories.
#
  $fileinfo == $dir and return "."; # Take care of this annoying special case
				# first.

  my @dirs1;			# Get a list of directories for the file.
  for (;;) {
    push @dirs1, $fileinfo;
    $fileinfo = $fileinfo->{".."}; # Go to the parent.
    last if $fileinfo == $FileInfo::root;	# Quit when we reach the root.
  }

  my @dirs2;			# Get a list of directories for the dir we
				# want to be relative to.
  while ($dir != $FileInfo::root) {
    push @dirs2, $dir;
    $dir = $dir->{".."};
  }

  if ($dirs2[-1] != $dirs1[-1]) { # Only thing in common is the root?
    return $_[0]->absolute_filename; # May as well use absolute filename.
  }

  while (@dirs1 && @dirs2) {	# Peel off the top level directories
    last if $dirs2[-1] != $dirs1[-1]; # until we find a difference.
    pop @dirs2;			# until we find a difference.
    pop @dirs1;
  }

  return join("/", ("..") x @dirs2, map { $_->{NAME} } reverse @dirs1);
				# Form the relative filename.
}

=head2 stat_array

   $statinfo = $fileinfo->stat_array;
   $statinfo = FileInfo::stat_array("filename");
   $uid = $statinfo->[4];	# Or whatever field you're interested in.

Returns the array of values returned by the C<stat> function on the file.  The
values are cached, so calling this repeatedly entails only minimal extra
overhead.

If the file is a symbolic link, this returns the stat values for the file the
link refers to.

=cut

sub stat_array {
  my $fileinfo = $_[0];		# Get the fileinfo structure.

  my $stat_arr = $fileinfo->{LSTAT}; # Get the cached value.
  if (!defined($stat_arr)) {	# No cached value?
    $stat_arr = $fileinfo->{LSTAT} = [ lstat($fileinfo->absolute_filename_nolink) ];
				# Restat the file, and cache the info.
    if ($fileinfo->{EXISTS} = (@$stat_arr != 0)) {
				# Update the EXISTS flag too.
      publish($fileinfo);	# Tell everyone the news that the file exists.
    }
  }
  @$stat_arr or return [];	# File does not exist.

  if (($stat_arr->[$stat_mode] & $S_IFLNK) == $S_IFLNK) { # Symbolic link?
    my $link =
      ($fileinfo->{LINK_DEREF} ||= # Have we already dereferenced it?
       file_info(readlink($fileinfo->absolute_filename_nolink), $fileinfo->{".."}));
    $stat_arr = $link->stat_array; # Get what it refers to (and if it's
				# a link, get what that refers to also).
  }

  return $stat_arr;
}

=head2 traverse

   FileInfo::traverse sub { 
      my (@finfos) = @_;

      foreach (@finfos) { ... do something }
   }

Traverses through what we know of the entire file system.  The subroutine is
called with C<@_> as an unsorted array of all the FileInfo structures that are
contained in that directory.  The subroutine is invoked on parent directories
before it is ivoked on child directories.

=cut

sub traverse {
  my ($subr, $dirinfo) = @_;

  $dirinfo ||= $FileInfo::root; # Start at root of hierarchy if not
				# specified.

  &$subr(values %{$dirinfo->{DIRCONTENTS}}); # Call subroutine on this directory.
  foreach (values %{$dirinfo->{DIRCONTENTS}}) { # Look in each subdirectory of 
				# this directory.
    $_->{DIRCONTENTS} && !$_->is_symbolic_link # Don't traverse it if it's
				# a symbolic link, because then we'll find the
				# same files twice.
      and traverse($subr, $_);
  }
}

=head2 unlink

   $fileinfo->unlink;

Removes the file and marks it in the cache as non-existent.

=cut

sub unlink {
  my $fileinfo = &file_info;	# Get the FileInfo struct.

  CORE::unlink $fileinfo->absolute_filename_nolink; # Delete the file.
  $fileinfo->{EXISTS} = 0;	# Mark the file as non-existent.  Don't
				# delete the fileinfo struct because it
				# might contain make build info or other stuff.
  delete $fileinfo->{LSTAT};	# Invalidate the stat information.
  delete $fileinfo->{SIGNATURE};
}

###############################################################################
#
# Internal subroutines (don't call these):
#

#
# This subroutine is called as soon as we discover that a file is actually
# a directory.  Knowing that something is actually a directory is very
# important for the wildcard routines, especially those with wildcards
# like "**/*.cxx".
#
# Argument: the FileInfo structure of the thing we discovered is a directory.
#
sub mark_as_directory {
  my $dirinfo = $_[0];

  $dirinfo->{DIRCONTENTS} ||= {}; # Mark as a directory.

#
# Do something here with directory wildcards....
#  
}

#
# Subroutine to activate any wildcard routines which might be waiting to hear
# about this file.  This subroutine is called whenever a file might possibly
# be new.
#
# In order to allow wildcard subroutines to be run on files created during
# execution, and not just on files which existed when the wildcard was
# first seen, we store up a subroutines in each directory to be activated
# when a new file matches a given pattern.  This subroutine is responsible for
# activating them.
#
sub publish {
  return if $_[0]->{PUBLISHED}++; # Don't do anything if we already published
				# this file.

  my $finfo = $_[0];		# Access the fileinfo struct.

  my $fname = $finfo->{NAME};	# Form the relative filename.
  my $dirinfo = $finfo->{".."};	# Find the directory that contains it.

  while ($dirinfo) {		# Go until we hit the top level.
    foreach my $wild_rtn (@{$dirinfo->{WILDCARD_ROUTINES}}) {
				# Check each wildcard match specified to start
				# in this directory.
      &$wild_rtn($finfo, $fname, 1); # See if it matches, and call the wildcard
				# action routine if it does.
    }

    $fname = $dirinfo->{NAME} . "/" . $fname;
				# Form the relative filename of the next level.
    $dirinfo = $dirinfo->{".."}; # Go up a level.
  }
}

$FileInfo::CWD_INFO = file_info(cwd);
				# Store the current directory so we know how
				# to handle relative file names.
#
# One unfortunate complication with the way we scan for include files in
# makepp is that when the user switches to root to do the "make install",
# a different set of directories is now readable.  This may cause directories
# which used to be non-writable to become writable, which means that makepp
# will scan them for include files.  This means that the list of dependencies
# may change, and therefore recompilation may be forced.  We try to get around
# this with a special purpose hack where if we're running as root, we 
# actually do the check with the UID and GID of whoever owns the directory.
#

if ($> == 0) {			# Are we running as root?
  ($FileInfo::orig_uid, $FileInfo::orig_gid) = ($>, $));
				# Save the original IDs.
  ($FileInfo::uid_for_check, $FileInfo::gid_for_check) =
    @{$FileInfo::CWD_INFO->stat_array}[$stat_uid, $stat_gid];
				# Use the UID of whoever owns the current
				# directory.
}

1;


__END__

=head1 AUTHOR

Gary Holt (holt@lnc.usc.edu)

=cut



