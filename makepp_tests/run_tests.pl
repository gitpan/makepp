#!/usr/bin/perl
# $Id: run_tests.pl,v 1.5 2003/07/07 15:24:01 grholt Exp $
#
# This script runs the specified tests and reports their result.
# Usage:
#    run_tests.pl test1 test2 test3
#
# If no arguments are specified, defaults to *.test.
#
# Each test is a tar file that contains the following:
#   A makefile or makeppfile
#   A subdirectory called "answers" which contains a copy of all target
#     files.
#   [Optional] A script called makepp_test_script which is run to do the
#     test.  The script takes one argument which is the path to makepp.
#     If this file does not exist, then we execute the statement
#        perl /path/to/makepp
#     so makepp picks up the default makefiles.
#   [Optional] A script called "is_relevant" which returns true (0) if the test
#     is relevant on this platform, and false (1) if not.  If absent, we assume
#     the test is relevant.  The environment variable PERL contains the path
#     to the perl distribution we are using.
#
# Makepp is run, and then each target file in the "answers" directory is
# compared with the corresponding file in the top level directory.  If there
# is no difference, the test passed; if there is a difference, the test
# failed.
#
# Furthermore, the "answers" directory contains a file called "n_files".
# This file contains a number which is the number of files that should have
# been built for this test.  (This allows us to find bugs where makepp
# rebuilds too often.)
#

use Cwd;

$old_cwd = cwd;			# Remember where we were so we can cd back
				# here.

@ARGV or @ARGV = <*.test>;	# Get a list of arguments.

$n_failures = 0;

test_loop:
foreach $tarfile (@ARGV) {
  ($testname = $tarfile) =~ s/\.test$//;

  -d "tdir" and system("rm -rf tdir"); # Get rid of junk from previous run.
  mkdir "tdir", 0755 || die "$0: can't make directory tdir--$!\n";
				# Make a directory.
  chdir "tdir" || die "$0: can't cd into tdir--$!\n";

  eval {
    $^O eq 'cygwin' && $testname =~ /_unix/ and die "skipped\n";
                                # Skip things that will cause errors on cygwin.
    system("tar xf ../$tarfile") &&	# Extract the tar file.
      die "$0: can't extract testfile $tarfile\n";
    if (-x "is_relevant") {
      system("./is_relevant ../../makepp > /dev/null 2>&1") && die "skipped\n";
    }
    if (-x "makepp_test_script") { # Shell script to execute?
      system("./makepp_test_script ../../makepp > ../$tarfile.log 2>&1") && # Run it.
	die "$0: makepp returned error status\n";
    } else {
      system("\${PERL-perl} ../../makepp > ../$tarfile.log 2>&1") && # Run makepp.
	die "$0: makepp returned error status\n";
    }
#
# Now look at all the final targets:
#
    local *DIR;			# Make a local directory handle.
    if (opendir(DIR, "answers")) { # Use opendir() instead of globbing because
				# 5.005's globbing can't handle filenames with
				# spaces.
      while ($tfile = readdir(DIR)) {	# Read the next file.
	next if $tfile =~ /^\./;	# Skip . and ...
	next if $tfile eq "n_files"; # Skip the special file.
	local $/ = undef;		# Slurp in the whole file at once.
	open(TFILE, "answers/$tfile") || die "$0: can't open tdir/$tfile--$!\n";
	$tfile_contents = <TFILE>; # Read in the whole thing.
	close TFILE;
	$tfile_contents =~ s/\r//g; # For cygwin, strip out the extra CRs.

	$mtfile = $tfile;		# Get the name of the actual file.
	open(MTFILE, $mtfile) || die "$mtfile\n";
	my $mtfile_contents = <MTFILE>; # Read in the whole file.
	close MTFILE;
	$mtfile_contents =~ s/\r//g; # For cygwin, strip out the extra CRs.
	$mtfile_contents eq $tfile_contents or die "$mtfile\n";
      }	
      closedir DIR;
    }
#
# See if the correct number of files were built:
#
    my $n_files_updated;
    open(LOGFILE, ".makepp_log") or die ".makepp_log\n";
    while (defined($_ = <LOGFILE>)) {
      if (/^(\d+) files updated/) {
	$n_files_updated = $1;	# Found the count of files changed.
	last;
      }
    }	
    close LOGFILE;
    unlink ".makepp_log";	# Get rid of the log file so we don't
				# get confused if the next test doesn't make
				# a log file for some reason.
    defined($n_files_updated) or die ".makepp_log\n";
    
    open(N_FILES, "answers/n_files") or die "answers/n_files\n";
    $_ = <N_FILES>;
    close N_FILES;
    /^(\d+)/ or die "answers/n_files\n";
    $1 == $n_files_updated or die "n_files\n";

#
# Also search through the log file to make sure there are no perl messages
# like "uninitialized value" or something like that.
#
    if (open LOGFILE, "../$tarfile.log") {
      while (defined($_ = <LOGFILE>)) {
	/at \S+ line \d+/ and die "../$tarfile.log\n";
	/generated error/ and die "../$tarfile.log\n";
      }
    }
    close LOGFILE;
  };

  if ($@) {
    if ($@ =~ /skipped/) {      # Skip this test?
      print "skipped $testname\n";
    } elsif ($@ =~ /^\S+$/) {	# Just one word?
      my $loc = $@;
      $loc =~ s/\n//;		# Strip off the trailing newline.
      print "FAILED $testname (at $loc)\n";
      ++$n_failures;
    } else {
      print "FAILED $testname: $@";
      ++$n_failures;
    }	
  } else {
    print "passed $testname\n";
  }

  -x "cleanup_script" and system("./cleanup_script");
  chdir $old_cwd;		# Get back to the old directory.
  system("rm -rf tdir");	# Get rid of the test directory.
}

exit $n_failures;
