# $Id: target_newer.pm,v 1.1.1.1 2002/01/10 07:04:35 grholt Exp $
package Signature::target_newer;

use Signature;

@ISA = qw(Signature);

=head1 NAME

Signature::target_newer -- a signature class that uses the traditional Make algorithm

=head1 USAGE

This is the default signature class for a few special cases, e.g., for rules
that build Makefile or Makefile.in.  Otherwise, if you want to use it, you
must specify it on the command line or explicitly in rules:

   target : dependencies
	  : signature exact_match
	$(commands_to_build)

=head1 DESCRIPTION

This class forces a recompilation if the target is older than any of its
dependencies.  It also does not require the command to be the same as on the
last build, nor does it it make sure that the architecture is the same.  This
is generally not a reliable way to build things, but it is occasionally useful
for special purpose things where a target may be modified by some commands not
executed under make's control.  For example, if you want your makefile to pull
the latest version out of an RCS archive, but only if the RCS archive is more
recent, you could have a rule like this:

   %: %,v
    : signature target_newer
	co $(FIRST_DEPENDENCY)

If you did not add the "C<:signature target_newer>", the rule would not work as
expected.  If you checked the file out of the RCS archive, then modified it,
B<makepp>'s default rule would notice that the file's signature did not match
the signature from the last build, so it would check it out for you again.

Note that this rule will not work at all if you change the default signature
from being a timestamp to being an MD5 checksum or something like that.  So
don't do that.

Note also that repositories will not work (currently) if you use the
traditional algorithm.

=cut

$target_newer = bless {};	# Make the singleton object.

sub build_check {
  shift @_;			# Get rid of the bogus argument.

  my ($tinfo, $command_string, $cwd, $sorted_dep_str, $dep_sig_str) = @_;

  my $tsig = $tinfo->signature;	# Get the timestamp on the target.
  unless ($tsig) {		# If there's no target, then we need to
				# rebuild.
    $main::log_level and
      main::print_log("Rebuild because target doesn't exist");
    return 1;
  }

  my @dep_sigs = split(/\01/, $dep_sig_str); # Get timestamps on dependencies.

  for (my $dep_idx = 0; $dep_idx < @dep_sigs; ++$dep_idx) {
    if ($tsig < ($dep_sigs[$dep_idx] || 0)) { # Found something older?
      if ($main::log_level) {	# Do we have to justify ourselves?
	my @deps = split(/\01/, $sorted_dep_str);
	$main::log_level and
	  main::print_log("Rebuild because $deps[$dep_idx] is newer");
      }
      return 1;
    }
  }

  main::print_log("Rebuild of ", $tinfo->name, " not needed");
  return undef;			# No rebuild necessary.
}

1;
