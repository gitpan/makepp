# $Id: exact_match.pm,v 1.1.1.1 2002/01/10 07:04:35 grholt Exp $
package Signature::exact_match;

use strict;

use Signature;

@Signature::exact_match::ISA = qw(Signature);

=head1 NAME

Signature::exact_match -- a signature class that requires that dependencies have exactly the same signature they did on the last build

=head1 USAGE

This is the default signature class.  You don't have to do anything to use it
unless you override it from the command line.  You can specify it explicitly
in the Makefile using

   target : dependencies
	  : signature exact_match
	$(commands_to_build)


=head1 DESCRIPTION

This class forces a recompilation if any of the following are different from
the last time this particular rule was executed:

=over 4

=item *

The build command or the directory from which it is executed.

=item *

The architecture this is running on (e.g., i386, sparc, alpha, etc.).

=item *

The signatures of each dependency.

=back

Returns undef if no build is necessary, or a true value if a build is
necessary.

=cut

my $last_compilation_warning_x86 = 0;

$Signature::exact_match::exact_match = bless {}; # Make the singleton object.

sub build_check {
  shift @_;			# Get rid of dummy argument.

  my ($tinfo, $command_string, $cwd, $sorted_dep_str, $dep_sig_str) = @_;

#
# Do the default, which is exact matching.  Note that we don't have to
# check the signature, because if it doesn't match, the build info file
# will be invalid anyway.  See FileInfo::load_build_info.
#  
  if ($command_string ne ($tinfo->build_info_string("COMMAND") || '')) {
    if ($main::log_level) {
      my ($last_cmd, $this_cmd);
      ($last_cmd = $tinfo->build_info_string("COMMAND") || '') =~ s/\n/\\n/g;
      ($this_cmd = $command_string) =~ s/\n/\\n/g;
				# Make the strings more printable.
      main::print_log("Rebuild ", $tinfo->name, " because last build command (\"$last_cmd\") differs from current command (\"$this_cmd\")");
    }	
    return 1;
  }

#
# We used to make sure the build cwd is the same.  We don't check this any
# more, because there are evidently a lot of programs (the linux kernel is
# a horrifying example of this) where the same file can be built from multiple
# makefiles, with different CWDs.  Usually this is for files whose actions
# are just "echo" or "touch", things which don't care about the current
# directory.
#
# Generally speaking, if the build cwd changes, so does the command, so 
# it's probably ok not to bother checking the build cwd.
#

#  if ($cwd ne ($tinfo->build_info_string("CWD") || '')) {
#    $main::log_level and
#      main::print_log("Rebuild ", $tinfo->name, " because build cwd changed\n");
#    return 1;
#  }

  if ($main::architecture ne ($tinfo->build_info_string("ARCH") || '')) {
#
# The pentium architectures are all more or less equivalent, but have different
# architecture flags.  Give a warning (so at least the user isn't surprised
# about recompilation).
#
    if ($main::architecture =~ /^i[34567]86-linux/ &&
	$tinfo->build_info_string("ARCH") =~ /^i[34567]86-linux/) {
      unless ($last_compilation_warning_x86++) {
	$main::warn_level and
	  main::print_error("warning: last compilation was on the ",
			    $tinfo->build_info_string("ARCH"),
			    " architecture, and this is on $main::architecture.
  These are technically different and force a recompilation of everything,
  but this may not be what you want.  The difference is most likely caused
  by running a different copy of perl.");
      }
    }
    $main::log_level and
      main::print_log("Rebuild ", $tinfo->name, " because last build was on ",
		      $tinfo->build_info_string("ARCH") || '',
		      " and this is on ", $main::architecture);
    return 1;
  }

  my $last_dep_sigs = $tinfo->build_info_string("DEP_SIGS") || '';

  if ($last_dep_sigs ne $dep_sig_str) {
				# Is anything different?  (This will be
				# different not only if a file has changed,
				# but also if the dependency list has changed,
				# so we don't want to check the dependency
				# list directly.  In fact, checking it can
				# cause problems when we get files out of
				# a repository.)
    if ($main::log_level) {	# Do we have to figure out what is different?
      my $last_dep_str = $tinfo->build_info_string("SORTED_DEPS") || '';
      my (%cur_deps, %last_deps);
      @cur_deps{split(/\01/, $sorted_dep_str)} = split(/\01/, $dep_sig_str);
      @last_deps{split(/\01/, $last_dep_str)} = split(/\01/, $last_dep_sigs);

      foreach (keys %cur_deps, keys %last_deps) {
	next if (($cur_deps{$_} || '') eq ($last_deps{$_} || ''));
	
	main::print_log("Rebuild ", $tinfo->name, " because $_ ",
			($cur_deps{$_} ? 
			 ($last_deps{$_} ? "changed" :
			  "was not previously a dependency") :
			 "is no longer a dependency"));
      }
    }
    return 1;
  }

  return undef;		# No build necessary.
 
}

1;
