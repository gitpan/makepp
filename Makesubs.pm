###############################################################################
#
# This package contains subroutines which are can be called from a makefile.
# Subroutines in this package are called in two ways:
# 1) Any line which isn't a rule or an assignment and has at the left margin a
#    word is interpreted as a subroutine call to a subroutine in the makefile
#    package, or if not in the makefile package, in this package.  "s_" is
#    prefixed to the name before the perl function is looked up.
# 2) Any function that is in a make expression (e.g., $(xyz abc)) attempts to
#    call a perl function in the make package, and failing that, in this
#    package.  "f_" is prefixed to the name first.
#
# All subroutine names in this package are automatically exported to each 
# makefile package by Makefile::load.
#

package Makesubs;

use strict qw(vars subs);

use SelfLoader;

SelfLoader->load_stubs();	# Force it to load the stubs immediately.

use TextSubs;
use FileInfo;
use FileInfo_makepp;
use MakeEvent qw(wait_for when_done);
use Config;

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
  $main::has_md5_signatures and # Use the MD5 signature checking when we can.
    $rule->set_signature_method_default($Signature::c_compilation_md5::c_compilation_md5);

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
	  main::print_error("warning: invalid directory $1 mentioned in compile command")
	    unless $dir_warnings{$dirinfo->absolute_filename}++;
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
      if (/[\*\?\[]/) {		# Might be a wildcarded file?
#	push @obj_files, Glob::zglob_fileinfo($_, $build_cwd);
				# Get all the files.
      }
      else {
	push @obj_files, file_info($_, $build_cwd);
      }
    }
    elsif (main::is_cpp_source_name($_)) { # Looks like a source file?
      if (/[\*\?\[]/) {		# Might be a wildcarded file?
#	foreach my $src_info (Glob::zglob_fileinfo($_, $build_cwd)) {
#	  push @source_files, $src_info unless $all_sources{$src_info}++;
#	}
      }
      else {			# Regular old filename.
	my $src_info = file_info($_, $build_cwd);
	unless ($all_sources{$src_info}++) { # Already seen this source?
	  push @source_files, file_info($_, $build_cwd);
	}	
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
   "pgcc"    => \&scanner_c_compilation,
   "pg++"    => \&scanner_c_compilation,
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
# Infer the linker command from a list of objects.  If any of the objects
# is fortran, we use $(FC) as a linker; if any of the objects is C++, we
# use $(CXX); otherwise, we use $(CC).
#
# This function is mostly used by the default link rules (see
# builtin_rules.mk).
#
sub f_infer_linker {
  my ($text, $makefile, $makefile_line) = @_; # Name the arguments.
  my @objs = split(' ', $text);	# Get a list of objects.
#
# First build all the objs.  Until we build them, we don't actually know what
# source files went into them.  They've probably been built, but we must 
# make sure.
#
  my @build_handles;
  foreach my $obj (@objs) {
    $obj = file_info($obj, $makefile->{CWD}); # Replace the name with the
				# fileinfo.
    my $bh = main::build($obj); # Build this one.
    $bh and push @build_handles, $bh;
  }

  my $status = wait_for @build_handles;	# Wait for them all to build.
  $status and die "Error while compiling\n"; # Maybe I'll come up with a better
				# error message later.

#
# Now see what source files these were built from.  Unfortunately, the
# dependencies have been sorted, so we can't just look at the first one.
#
  my $is_fortran = 0;		# Assume it's not fortran or C++.
  my $is_cpp = 0;
  foreach my $obj (@objs) {
    foreach my $source_name(split(/\01/, $obj->build_info_string("SORTED_DEPS") || '')) {
      $source_name =~ /\.f$/ and $is_fortran = 1;
      $source_name =~ /\.(?:c\+\+|cc|cxx|C|cpp|moc)$/ and $is_cpp = 1;
    }
  }	

  my $linker = '$(LINK.c)';	# Assume we can use the ordinary C linker.
  $is_cpp and $linker = '$(LINK.cc)';
  $is_fortran and $linker = '$(LINK.f)';

  return $makefile->expand_text($linker, $makefile_line);
				# Figure out what those things expand to.
}

#
# This is an experimental function which attempts to infer the proper 
# libraries given a list of objects.  We do this by looking at what
# files are included; from the names of the include files, we try to guess
# the corresponding libraries.  This is tricky and system dependent, so I
# don't expect this to work even close to 100% ofo the time.  It's intended
# mostly for the default rule set, so that even if there's no makefile, it
# may be possible for makepp to correctly link a program.  This should help
# novices who don't want to learn what the libraries actually are.
#
# We should probably also have some platform specific knowledge about what
# extra libraries might be needed on solaris or other operating systems
# besides linux.  There should probably be an OS-specific module for each
# variant of unix.  However, since I can only test conveniently on one
# variant, things will have to be simpler for the moment.
#
sub f_infer_libraries {
  my ($text, $makefile, $makefile_line) = @_; # Name the arguments.
  my @objs = split(' ', $text);	# Get a list of objects.
#
# Make a list 
#
  unless (%Makesubs::include_to_library) { # We haven't set up the data yet?
    %Makesubs::include_to_library = ();
  }

#
# First build all the objs.  Until we build them, we don't actually know what
# source files went into them.  They've probably been built, but we must 
# make sure.
#
  my @build_handles;
  foreach my $obj (@objs) {
    $obj = file_info($obj, $makefile->{CWD}); # Replace the name with the
				# fileinfo.
    my $bh = main::build($obj); # Build this one.
    $bh and push @build_handles, $bh;
  }

  my $status = wait_for @build_handles;	# Wait for them all to build.
  $status and die "Error while compiling\n"; # Maybe I'll come up with a better
				# error message later.

#
# Now look at the include files they depend on, and see if we can get any
# useful information out of that.  This is a little tricky, since many of the
# include files are not actually listed in the dependency list since we don't
# usually scan files in /usr/include.  We get the complete list of files
# included from every source file that was scanned; hopefully this will
# contain enough of the files from /usr/include so we can guess the libraries.
#
  my %includes;			# All the files that are included.

  foreach my $obj (@objs) {
    foreach my $source_name (split(/\01/, $obj->build_info_string("SORTED_DEPS") || '')) {
      if ($source_name =~ /\.[Hh](?:xx|\+\+)?$/) { # Looks like an include file?
	$includes{$source_name} = 1; # Remember we know about this one.
      }
      my $source_info = file_info($source_name, $obj->{".."});
      foreach my $incfile (split(' ', $source_info->build_info_string("SYSTEM_INCLUDES") || ''),
			   split(' ', $source_info->build_info_string("INCLUDES") || '')) {
				# Look at every file included in this source file.
	$includes{$incfile} = 1;
      }
    }	
  }
#
# At this point, the keys of %includes are the names of all include files
# that we can find out about (since we never actually scanned include
# files in /usr/include or in other non-writable directories).  Try to
# infer from these what libraries are necessary.
#
# This is of course very heuristic.  But if it doesn't work, you shouldn't
# try to use $(infer_libraries).
#
  my %libs;			# Where we build up the libraries that we need.
  my @dirinfos;
  foreach my $dirname (split(' ', $Config{'libpth'})) {
				# Look at all the places where system
				# libraries might be.
    push @dirinfos, file_info($dirname); # Get the fileinfo for it.
    $dirinfos[-1]->{READDIR} || $dirinfos[-1]->read_directory;
				# Get a complete list of files in it.
  }

 include_file_loop:
  foreach (keys %includes) {
#
# Some special cases:
#
    if (m@^X11/@) { $libs{'X11'} = 1; next; }
    if (m@^q.*\.h$@) { $libs{'qt'} = 1; next; }
    if ($Makesubs::include_to_library{$1}) { # Is this a special builtin translation?
      my @liblist = split(' ', $Makesubs::include_to_library{$1});
      @libs{@liblist} = (1) x @liblist;
    }
    if ($_ eq 'zlib.h') { $libs{'z'} = 1; next; }
    if ($_ eq 'jpeglib.h') { $libs{'jpeg'} = 1; next }
#
# The general case.  There are several general rules here:
# o For a file xyz/abc.h, we look for a library libxyz.
# o For a file xyzlib.h or xyz_lib.h, we look for a library libxyz.
# o For a file xyz.h, we look for a library libxyz.
#
    if (m@^([^\./]+?)_?lib.h$@ || # xyzlib.h or xyz_lib.h => look for libxyz.
	m@^([^/]+)/@) {		# xyz/abc.h => look for libxyz.
      my $basename = $1;
      foreach my $dirinfo (@dirinfos) {
	if (file_info("lib$1.$Config{'dlext'}", $dirinfo)->file_exists ||
	    file_info("lib$1$Config{'_a'}", $dirinfo)->file_exists) {
				# Found a corresponding library?
	  $libs{$1} = 1;	# Indicate that we'll need that library.
	  next include_file_loop;
	}
      }
    }

    if (m@^([^\./]+)\.h$@) {	# Does it look like an ordinary .h file?
      my $basename = $1;	# Get the base name for a prospective library.
      foreach my $dirinfo (@dirinfos) {
	if (file_info("lib$1.$Config{'dlext'}", $dirinfo)->file_exists ||
	    file_info("lib$1$Config{'_a'}", $dirinfo)->file_exists) {
				# Found a corresponding library?
	  $libs{$1} = 1;	# Indicate that we'll need that library.
	  next include_file_loop;
	}
      }
    }

  }
#
# Now that we have a list of libraries that we want, add some necessary flags.
# For some stupid reason, on all linux systems the X11 libraries aren't in
# the default build path (but they are in the default runtime search path), so
# we have to put in linker options.
#
  my $libpaths = '';
  if ($libs{'X11'}) {
    if (-d '/usr/X11R6/lib') { $libpaths .= "-L/usr/X11R6/lib "; }
    elsif (-d '/usr/X11/lib') { $libpaths .= "-L/usr/X11/lib "; }
  }

#
# Qt is often not in the direct library path.  Usually if it's not, the
# variable QTDIR is set instead.
#
  if ($libs{'qt'}) {
    my $qtdir = $makefile->expand_text('$(QTDIR'); # See if it's defined.
    if ($qtdir) {
      $libpaths .= "-L$qtdir/lib "; # Search that directory.
      $Config{'archname'} =~ /linux/ and $libpaths .= "-Wl,-rpath -Wl,$qtdir/lib ";
				# Add a bit of magic to force it to be
				# searched at runtime.
    }
  }

  return "$libpaths" . join(" ", map { "-l$_" } sort keys %libs);
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

#
# Foreach is a little bit trick, since we have to support the new
# $(foreach) automatic variable, but also the old GNU make function 
# foreach.  We can tell the difference pretty easily by whether we have
# any arguments.
#
sub f_foreach {
  my ($text, $makefile, $makefile_line) = @_; # Name the arguments.
  if ($text !~ /\S/) {		# No argument?
    defined($Makesubs::rule) && defined($Makesubs::rule->{FOREACH}) or
      return "\$(foreach)";	# Just delay the expansion.
				# This is necessary because target strings
				# may get expanded before the foreach variable
				# is defined.
#    die "\$(foreach) used outside of rule, or in a rule that has no :foreach clause\n";
    return $Makesubs::rule->{FOREACH}->name($Makesubs::rule->build_cwd);
  }

#
# At this point we know we're trying to expand the old GNU make foreach
# function.  The syntax is $(foreach VAR,LIST,TEXT), where TEXT is
# expanded once with VAR set to each value in LIST.  When we get here,
# because of some special code in expand_text, VAR,LIST,TEXT has not yet
# been expanded.
#  
  my $first_comma = index_ignoring_quotes($text, ','); # Find the variable name.
  $first_comma >= 0 or 
    die "$makefile_line: $(foreach VAR,LIST,TEXT) called with only one argument\n";
  my $varname = $makefile->expand_text(substr($text, 0, $first_comma));
				# Get the name of the variable.
  $varname =~ s/^\s+//;		# Strip off leading and trailing whitespace.
  $varname =~ s/\s+$//;

  $text = substr($text, $first_comma+1); # Get rid of the variable name.
  my $second_comma = index_ignoring_quotes($text, ',');	# Find the next comma.
  $second_comma >= 0 or 
    die "$makefile_line: $(foreach VAR,LIST,TEXT) called with only two arguments\n";
  my $list = $makefile->expand_text(substr($text, 0, $second_comma));
  $text = substr($text, $second_comma+1); 

  my $ret_str = '';
  foreach (split(' ', $list)) {	# Expand text:
    local $makefile->{COMMAND_LINE_VARS}{$varname} = $_;
				# Make it a command line variable so that it
				# overrides even an environment variable.
				# The local makes it so it goes away at the
				# end of the loop.
    $ret_str .= ' ' . $makefile->expand_text($text, $makefile_line);
  }

  length($ret_str) < 1 and return ''; # No values in list.

  return substr($ret_str, 1);	# Get rid of the extra space we put in at the
				# beginning.
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
# Makefile statements.  These are all called with the following arguments:
# a) The whole line of text (with the statement word removed).
# b) The makefile this is associated with.
# c) A printable string describing which line of the makefile the statement
#    was on.
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
# Include statement:
#
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
      foreach (@{$makefile->{INCLUDE_PATH}}) {
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
  my (@include_path) = @{$makefile->{INCLUDE_PATH}};
				# Make a copy of the include path (so we can
				# modify it with -I).
#
# First pull out the variable assignments.
#
  my @makefiles;
  while (defined($_ = shift @words)) { # Any words left?
    if (/^(\w+)=(.*)/) {	# Found a variable?
      $command_line_vars{$1} = unquote($2);
    }
    elsif (/^-I(\S*)/) {	# Specification of the include path?
      unshift @include_path, ($1 || shift @words);
				# Grab the next word if it wasn't specified in
				# the same word.
    }
    else {			# Unrecognized.  Must be name of a makefile.
      push @makefiles, $_;
    }
  }
#
# Now process the makefiles:
#
  foreach (@makefiles) {
    s/^-F//;			# Support the archaic syntax that put -F
				# before the filename.
    my $mfile = file_info($_, $makefile->{CWD});
				# Get info on the file.
    my $mdir = $mfile;		# Assume it is actually a directory.
    $mfile->is_or_will_be_dir or $mdir = $mfile->{".."};
				# Default directory is the directory the
				# makefile is in.
    &Makefile::load($mfile, $mdir, \%command_line_vars, "", \@include_path,
		    $makefile->{ENVIRONMENT}); # Load the makefile.
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
# Set the default signature method for all rules in this makefile:
#
sub s_signature {
  my ($args, $makefile, $makefile_line) = @_;
  $args =~ /^\s*(\w+)\s*$/ or 
    die "$makefile_line: invalid signature statement\n";
  my $sigmethod = $1;
  if ($sigmethod eq 'default') { # Return to the default method?
    delete $makefile->{DEFAULT_SIGNATURE_METHOD}; # Get rid of any previous
				# stored signature method.
    return;
  }
  defined $ {"Signature::${sigmethod}::${sigmethod}"} or
    die "$makefile_line: invalid signature method $sigmethod\n";
  $makefile->{DEFAULT_SIGNATURE_METHOD} = $ {"Signature::${sigmethod}::${sigmethod}"};
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

#
# Find one of several executables in PATH.
#
sub f_find_program {
  my @names = split(' ', $_[0]); # Get the programs to look for.
  my ($makefile, $makefile_line) = @_[1,2]; # Access the other arguments.

  my @pathdirs = map { file_info($_, $makefile->{CWD}) }
    split(/:/, $makefile->{EXPORTS}{PATH} || $ENV{'PATH'});
				# Get the list of directories to search.
  foreach my $name (@names) {	# Find it in the path:
    foreach my $dir (@pathdirs) {
      if (file_info($name, $dir)->is_executable) {
	return $name;
      }
    }
  }

  return $names[0];		# None of the programs were executable.
}

#
# Find a file in a specified path, or in the environment variable PATH if
# nothing is specified.
#
sub f_findfile {
  my ($name, $path) = split(/\,\s*/, $_[0]); # Get what to look for, and where
				# to look for it.
  my ($makefile, $makefile_line) = @_[1,2]; # Access the other arguments.
  my @pathdirnames = split(/[:\s]+/, $path || $ENV{'PATH'});
				# Get a separate list of directories.
  my @names = split(' ', $name); # Get a list of names to find.
  foreach $name (@names) {	# Look for each one in the path:
    foreach my $dir (@pathdirnames) {
      my $finfo = file_info($name, file_info($dir, $makefile->{CWD}));
				# Get the finfo structure.
      if ($finfo->file_exists) { # Found it?
	$name = $finfo->absolute_filename; # Replace it with the full name.
	last;			# Skip to the next thing to look for.
      }
    }
  }

  return join(" ", @names);
}

sub f_findstring {
  my ($find, $in) = split(/,/, $_[0]);

  return (index($in, $find) >= 0) ? $find : '';
}

sub f_firstword {
  return (split(' ', $_[0]))[0] || '';
}

#
# The if function is unusual, because its arguments have not
# been expanded before we call it.  The if function is defined so that
# only the expression that is actually used is expanded.  E.g., if the
# if statement is true, then only the then expression is expanded, and
# any side effects of the else expression do not happen.
#
sub f_if {
  my ($text, $makefile, $makefile_line) = @_; # Name the arguments.
  my $first_comma = index_ignoring_quotes($text, ',');
				# Find the first comma.
  $first_comma >= 0 or die "$makefile_line: $(if ) with only one argument\n";
  my $cond = $makefile->expand_text(substr($text, 0, $first_comma), $makefile_line);
				# Evaluate the condition.
  $cond =~ s/^\s+//;		# Strip out whitespace on the response.
  $cond =~ s/\s+$//;

  $text = substr($text, $first_comma+1); # Get the text w/o the comma.

  my $second_comma = index_ignoring_quotes($text, ',');
				# Find the boundary between the then and the
				# else clause.
  if ($cond) {			# Is the condition true?
    my $then;
    if ($second_comma >= 0) {	# Was there an else clause?
      $then = substr($text, 0, $second_comma);
    } else {
      $then = $text;		# No else clause, then clause is the rest.
    }	
    $then =~ s/^\s+//;		# Strip out leading whitespace.
    $then =~ s/\s+$//;		# Strip out trailing whitespace.
    return $makefile->expand_text($then, $makefile_line);
    
  } else {			# Condition was false.  Extract the else
				# clause.
    $second_comma >= 0 or return ''; # No else clause.
    my $else = substr($text, $second_comma+1); # Get the text.
    $else =~ s/^\s+//;		# Strip out leading whitespace.
    $else =~ s/\s+$//;		# Strip out trailing whitespace.
    return $makefile->expand_text($else, $makefile_line);
  }	
}

#
# Usage:
#    target : $(infer_objs seed-list, list of possible objs)
#
sub f_infer_objects {
  my ($text, $makefile, $makefile_line) = @_; # Name the arguments.
  my ($seed_objs, $candidate_list) = split(/,\s*/, $text);
				# Get the arguments.

  $candidate_list or die "infer_objects called without a candidate list\n";
  $Makesubs::rule or die "infer_objects called outside of a rule\n";

  my $build_cwd = $Makesubs::rule->build_cwd;

#
# Build up a list of all the possibilities:
#
  my %candidate_objs;
  foreach my $candidate_obj (map(Glob::zglob_fileinfo_atleastone($_, $build_cwd),
				 split(' ', $candidate_list))) {
				# Get a list of all the possible objs.
    my $objname = $candidate_obj->{NAME};
    $objname =~ s/\.[^\.]+$//;	# Strip off the extension.
    if ($candidate_objs{$objname}) { # Already something by this name?
      ref($candidate_objs{$objname}) eq 'ARRAY' or
	$candidate_objs{$objname} = [ $candidate_objs{$objname} ];
				# Make into an array as appropriate.
      push @{$candidate_objs{$objname}}, $candidate_obj;
    }
    else {			# Just one obj?
      $candidate_objs{$objname} = $candidate_obj;
    }
  }	
#
# Now look at the list of all the include files.  This is a little tricky
# because we don't know the include files until we've actually built the
# dependencies.
#
  my %source_names;		# These are the names of include files for
				# which are look for the corresponding objects.

  my @build_handles;		# Where we put the handles for building objects.
  my @deps = map { Glob::zglob_fileinfo($_, $build_cwd) } split(' ', $seed_objs);
				# Start with the seed files
				# themselves.
  $main::log_level and 
    main::print_log("infer_objects called with seed objects ",
		    join(" ", map { $_->name } @deps));
  
  foreach (@deps) {
    my $name = $_->{NAME};
    $name =~ s/\.[^\.]+$//;	# Strip off the extension.
    $source_names{$name}++;	# Indicate that we already have this as a
				# source file.
  }	


  my $dep_idx = 0;

#
# Build everything, so we know what everything's dependencies are.  Initially,
# we'll only have a few objects to start from, so we build all of those, in
# parallel if possible.  (That's why the loop structure is so complicated
# here.)  Then we infer additional objects, build those in parallel, and
# so on.
#
  for (;;) {
    while ($dep_idx < @deps) {	# Look at each dependency currently available.
      my $o_info = $deps[$dep_idx]; # Access the FileInfo for this object.
      my $bh = main::build($o_info); # Start building it.
      my $handle = when_done $bh, # Build this dependency.
      sub {			# Called when the build is finished:
	$bh->status and return $bh->status;
				# Skip if an error occured.
	my @this_sources = split(/\01/, $o_info->build_info_string("SORTED_DEPS") || '');
				# Get the list of source files that went into
				# it.
	foreach (@this_sources) {
	  my $name = $_;	# Make a copy of the file.
	  $name =~ s@.*/@@;	# Strip off the path.
	    $name =~ s/\.[^\.]+$//; # Strip off the extension.
	  unless ($source_names{$name}++) { # Did we already know about that source?
	    if (ref($candidate_objs{$name}) eq 'FileInfo') { # Found a file?
	      $main::log_level and
		main::print_log(0, "infer_objects: adding ",
				$candidate_objs{$name}->name,
				" to dependency list because of $_");
	      push @deps, $candidate_objs{$name}; # Scan for its dependencies.
	    }
	    elsif (ref($candidate_objs{$name}) eq 'ARRAY') { # More than 1 match?
	      main::print_error($Makesubs::rule->source, " in infer_objects: more than one possible object for include file $_:\n  ",
				join("\n  ", map { $_->absolute_filename } @{$candidate_objs{$name}}),
				"\n");
	    }
	  }
	}	
      };

      defined($handle) and push @build_handles, $handle;
				# Is this something we need to wait for?
      ++$dep_idx;
    }

    last unless @build_handles;	# Quit if nothing to wait for.
    my $status = wait_for @build_handles; # Wait for them all to build, and 
				# try again.
    @build_handles = ();	# We're done with those handles.
    $status and last;		# Quit if there was an error.
  }	

#
# At this point, we have built all the dependencies, and we also have a
# complete list of all the objects.
#
  return join(" ", map { $_->relative_filename($build_cwd) } @deps);
}

sub f_join {
  my ($words1, $words2) = split(/,/, $_[0]);
				# Get the two lists of words.
  defined($words2) or die "$_[2]: $(join ) called with < 2 arguments\n";
  my @words1 = split(' ', $words1);
  my @words2 = split(' ', $words2);

  my $maxidx = @words1;		# Get the number of words in the output.
  $maxidx < @words2 and $maxidx = @words2;

  my @outwords;
  for (my $idx = 0; $idx < $maxidx; ++$idx) {
    push @outwords, ($words1[$idx] || '') . ($words2[$idx] || '');
				# Do the concatenation.
  }

  return join(' ', @outwords);
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

#
# Perform a pattern substitution:
#
sub f_patsubst {
  my ($src, $dest, $words) = split(/,\s*/, $_[0]);
				# Get the arguments.
  return join(" ", TextSubs::pattern_substitution($src, $dest,
						  split_on_whitespace($words)));
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

###############################################################################
# 
# Special variables that are implemented as make functions.  (Implementing them
# as functions with no arguments makes it possible to override them with
# a variable assignment, yet by default they're available for every makefile.)
#

sub f_AR { return "ar"; }
sub f_ARFLAGS { return "rv"; }

sub f_AS { return "as"; }

#
# C compiler.  We look for a good C compiler to use.
#
sub f_CC {
  my ($makefile, $makefile_line) = @_[1,2]; # Name the arguments.
  my $ccname = f_find_program("gcc pgcc egcc c89 cc", @_[1,2]) 
				# "gcc" is first so CFLAGS knows whether to
				# add the special gcc flags.
    || 'cc';			# If not found, we have to expand to something.
  $ {$makefile->{PACKAGE} . "::CC"} = $ccname;
				# Cache the value, so we can look at it when
				# we have to expand CFLAGS.
  return $ccname;
}

sub f_CFLAGS {
  my ($makefile, $makefile_line) = @_[1,2]; # Name the arguments.

  if (($ {$makefile->{PACKAGE} . "::CC"} || 'cc') =~ /gcc$/) {
				# Is this the GNU compiler?
    return "-g -Wall";		# Enable warnings.
  }
  else {
    return "-g";		# Just enable debug.  (This is different
				# from standard make, but I can't imagine
				# why you wouldn't want it.)
  }
}

sub f_COMPILE_dot_C { return &f_COMPILE_dot_cc; }
sub f_COMPILE_dot_F { return $_[1]->expand_text('$(FC) $(FFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c', $_[2]); }
sub f_COMPILE_dot_S { return $_[1]->expand_text('$(CC) $(ASFLAGS) $(CPPFLAGS) $(TARGET_MACH) -c', $_[2]); }
sub f_COMPILE_dot_c { return $_[1]->expand_text('$(CC) $(CFLAGS) $(CPPFLAGS) $(INCLUDES) $(TARGET_ARCH) -c', $_[2]); }
sub f_COMPILE_dot_cc { return $_[1]->expand_text('$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(INCLUDES) $(TARGET_ARCH) -c', $_[2]); }
sub f_COMPILE_dot_cpp { return &f_COMPILE_dot_cc; }
sub f_COMPILE_dot_f { return $_[1]->expand_text('$(FC) $(FFLAGS) $(TARGET_ARCH) -c', $_[2]); }
sub f_COMPILE_dot_s { return $_[1]->expand_text('$(AS) $(ASFLAGS) $(TARGET_MACH)', $_[2]); }


#
# C++ compiler.  We look for a good C++ compiler to use.
#
sub f_CXX {
  my ($makefile, $makefile_line) = @_[1,2]; # Name the arguments.
  my $cxxname = f_find_program("g++ c++ pg++ cxx CC", @_[1,2]) 
    || 'c++';			# If not found, we have to expand to something.
  $ {$makefile->{PACKAGE} . "::CXX"} = $cxxname;
				# Cache the value, so we can look at it when
				# we have to expand CFLAGS.
  return $cxxname;
}

sub f_CXXFLAGS {
  my ($makefile, $makefile_line) = @_[1,2]; # Name the arguments.

  if (($ {$makefile->{PACKAGE} . "::CC"} || 'c++') =~ /[gc]\+\+$/) {
				# Is this the GNU compiler?
    return "-g -Wall";		# Enable warnings.
  }
  else {
    return "-g";		# Just enable debug.  (This is different
				# from standard make, but I can't imagine
				# why you wouldn't want it.)
  }
}

sub f_CPP { return &f_CC . " -E"; } # C preprocessor.

sub f_FC { return &f_F77; }

sub f_F77 {
  my ($makefile, $makefile_line) = @_[1,2]; # Name the arguments.
  my $f77name = f_find_program("f77 g77 fort77", @_[1,2]) 
    || 'f77';			# If not found, we have to expand to something.
  $ {$makefile->{PACKAGE} . "::F77"} = $f77name;
				# Cache the value, so we can look at it when
				# we have to expand CFLAGS.
  return $f77name;
}

sub f_LD { return "ld"; }

sub f_LEX { return f_find_program("lex flex", @_[1,2]); }
sub f_LEX_dot_l { return $_[1]->expand_text('$(LEX) $(LFLAGS) -t', $_[2]); }

sub f_LINK_dot_C { return &f_LINK_dot_cc; }
sub f_LINK_dot_S { return $_[1]->expand_text('$(CC) $(ASFLAGS) $(CPPFLAGS) $(LDFLAGS) $(TARGET_ARCH)', $_[2]); }
sub f_LINK_dot_c { return $_[1]->expand_text('$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) $(TARGET_ARCH)', $_[2]); }
sub f_LINK_dot_cc { return $_[1]->expand_text('$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS) $(TARGET_ARCH)', $_[2]); }
sub f_LINK_dot_cpp { return &f_LINK_dot_cc; }
sub f_LINK_dot_f { return $_[1]->expand_text('$(FC) $(FFLAGS) $(LDFLAGS) $(TARGET_ARCH)', $_[2]); }
sub f_LINK_dot_o { return $_[1]->expand_text('$(CC) $(LDFLAGS) $(TARGET_ARCH)', $_[2]); }
sub f_LINK_dot_s { return $_[1]->expand_text('$(CC) $(ASFLAGS) $(LDFLAGS) $(TARGET_ARCH)', $_[2]); }


#
# $(MAKE) needs to expand to the name of the program we use to replace a
# recursive make invocation.  We pretend it's a function with no arguments.
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
    return $Config{'bin'} . "/perl " . $Makesubs::make_name . " --recursive_makepp";
				# All the rest of the info is passed in the
				# MAKEFLAGS environment variable.
				# The --recursive option is just a flag that
				# helps the build subroutine identify this as
				# a recursive make command.  It doesn't 
				# actually do anything.
  } else {
    my $makefile = $_[1];	# Get the makefile we're run from.

    my $recursive_makepp = $Config{'bin'} . "/perl " .
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


sub f_MAKE_COMMAND { return &f_MAKE; }

sub f_MAKEINFO { return "makeinfo"; }

sub f_RM { return "rm -f"; }

sub f_YACC {
  my $yacc = f_find_program("yacc bison", @_[1,2]);
				# Pick yacc if available, otherwise bison.
  $yacc eq "bison" and $yacc .= " -y"; # Go to yacc compatibility mode.
  return $yacc;
}

sub f_YACC_dot_y {
  return $_[1]->expand_text('$(YACC) $(YFLAGS)', $_[2]);
}
