=head1	NAME

profiler -- finds out how long functions run and how often they are called.

=head1 USAGE

    cp -r <makepp-root> /tmp/makeppcopy
    cd /tmp/makeppcopy
    perl profiler.pm [-r fn-name-beginning-regexp] [Module ...]
    cd ...
    /tmp/makeppcopy/makepp ...
    cat profiler.log

=head1 DESCRIPTION

When called as a script, this modifies every source of makepp, adding a my
variable to each named sub.  If a I<fn-name-beginning-regexp> is given, only
functions whose name beginning match it get instrumented.  If a list of
modules is given, only those are instrumented.  However makepp itself is always
instrumented, since that sets up the profiler and outputs the results.

At makepp runtime this variable records the hires time, and its destructor
stores the difference minus the time of any other makepp subs it called.  The
number of calls is also stored.

Independently each sub also records the above info per different caller it got
invoked from.

This information is output as a delta for the top cases roughly every
PROFMPP_INTERVAL seconds and as a total at the end of the run.

Four environment variables control the relevancy and granularity:

  $PROFMPP_MINTIME		# .5 seconds
  $PROFMPP_MINCOUNT		# 100 calls
  $PROFMPP_CHILDFRACTION	# 100
  $PROFMPP_INTERVAL		# 60 seconds

Values which are less than these per time interval or in the final tally are
not considered.  Child processes only have a final tally, and the time and
count are 1/PROFMPP_CHILDFRACTIONth of these values there.

=head1 BUGS

This counts real time -- I don't know how to get the CPU time, since times()
is far too coarse.  It is only shown indicatively per interval.

=cut



if ( !caller ) {		# Called as a script.
  undef $/;
  my $re = '';
  if( @ARGV > 1 && $ARGV[0] eq '-r' ) {
    shift;
    $re = '(?=' . shift() . ')';
  }
  for my $file ( 'makepp', @ARGV ? @ARGV : <*.pm */*.pm> ) {
    next if $file eq 'profiler.pm' or $file eq 'Dump.pm';
    open my $fh, '+<', $file;
    $_ = <$fh>;
    if( $file eq 'makepp' ) {
      s/(?=use Make)/use profiler;\n/;
    } elsif( $file eq 'Mpp/Event.pm' or $file eq 'Mpp/Rule.pm' ) {
      s/(?=# In the child)/&profiler::reset_child;/;
      s/(?=\bexec\b|POSIX::_exit)/&profiler::output_child;/g;
    } elsif( $file eq 'Mpp/Glob.pm' ) {
      s/(\("sub \{)/$1 my \\\$__profiler = new profiler;/;
    }
    # Instrumentize all nontrivial subs.  Expect constants to be all UC.
    s/(^\ *sub\ $re:*\w+[a-z]\w*(?:\(.*\))?\ \{)
      (?!(?:\s*\#.*\n)*\s*(?:return\b\s*)?
	(?:\$_\[\d\]->\{[^ ]+\} (?:\s*[|&=]{1,3}\s*)?)?
	(?:\d* | undef | '[^']*' | "[^"]*" | [\$\@%&]?[][{}\w:->'.]+)? \s*;?
	(?:\s*\#.*\n)*\s*\})/$1my \$__profiler = new profiler;/gmox;
    seek $fh, 0, 0;		# Back to beginning.
    print $fh $_;
  }
  exit;
}



# This is the actual module.

package Mpp::profiler;

use Time::HiRes qw'gettimeofday tv_interval';
my @times = (0, 0, 0, 0);

my $mintime = ($ENV{PROFMPP_MINTIME} || .5) + 0;
my $mincount = ($ENV{PROFMPP_MINCOUNT} || 100) + 0;
my $childfraction = ($ENV{PROFMPP_CHILDFRACTION} || 100) + 0;
my $interval = ($ENV{PROFMPP_INTERVAL} || 60) + 0;

open my $log, '> profiler.log';
print $log "PROFMPP_MINTIME=$mintime PROFMPP_MINCOUNT=$mincount PROFMPP_INTERVAL=$interval\n";

open my $log_children, '> profiler-children.log';
print $log_children "PROFMPP_MINTIME=$mintime PROFMPP_MINCOUNT=$mincount PROFMPP_CHILDFRACTION=$childfraction\n";

my $pid = $$;
my $child_pid = 0;
my @last = gettimeofday;
my @subtime = (0, 0);

# Time::HiRes is good for getting the time, but not so good at operating on
# it, as it converts precise integers into wobbly floats.  So we add and
# subtract s and us separately (never mind if we get more than a second worth
# of us -- that becomes added as a float when displaying).  Since this gives us
# two values to operate on, plus a third for the counter, we only store the
# index into heaps within the hashes.  At the index there are the three
# corresponding values, first the seconds [i], then the microseconds [i + 1]
# and the counter [i + 2].
my( %interval, %intervalpair, @intervalheap, %total, %totalpair, @totalheap,
    $check );

sub new {
  push @subtime, 0, 0;		# Accumulators (s & us) for profilees callees.
  bless [gettimeofday];
}

sub display(\%\@$$) {
  my( $hash, $heap, $time, $n) = @_;
  my $msg = $time ? '%13.6fs	' : '%6d x	';
  my $limit = $time ? $mintime : $mincount;
  if( $time ) {
    $hash = {%$hash};
    $_ = $heap->[$_] + $heap->[$_ + 1] / 1_000_000 for values %$hash;
  } else {
    # Since we display time first, we can now destroy the hash.
    $_ = $heap->[$_ + 2] for values %$hash;
  }
  my @elts =
    sort { $hash->{$b} <=> $hash->{$a} || $a cmp $b }
    grep { $hash->{$_} >= $limit }
    keys %$hash;
  printf $log "$msg$_\n", $hash->{$_} for
    @elts > $n ? @elts[0..$n] : @elts;
}


# DESTROY is documented as not being called at a predictable time, but an
# example so small it wouldn't need garbage collection shows that it seems to
# get called each time when leaving the function.
sub DESTROY {
  my( $secs, $usecs ) = gettimeofday;
  $usecs -= $_[0][1] + pop @subtime;
  $secs -= $_[0][0] + pop @subtime;
  my $profilee = (caller 2)[3] or # Destructor gets called in an additional eval frame.
    goto end;			# Just to be sure we have a function.
  my $pair = (caller 3)[3];
  $pair .= ((caller 4)[3] || '(top)') if $pair && $pair eq '(eval)';
  $pair = $profilee . ' <- ' . ($pair || '(top)');

  if( !exists $total{$profilee} ) {
    $total{$profilee} = @totalheap;
    $totalpair{$pair} = @totalheap + 3;
    push @totalheap, $secs, $usecs, 1, $secs, $usecs, 1;
  } else {
    my $i = $total{$profilee};
    $totalheap[$i] += $secs;
    $totalheap[$i + 1] += $usecs;
    $totalheap[$i + 2]++;
    if( !exists $totalpair{$pair} ) {
      $totalpair{$pair} = @totalheap;
      push @totalheap, $secs, $usecs, 1;
    } else {
      $i = $totalpair{$pair};
      $totalheap[$i] += $secs;
      $totalheap[$i + 1] += $usecs;
      $totalheap[$i + 2]++;
    }
  }

  # Don't output in child process if by chance PROFMPP_INTERVAL elapses.
  if( $$ == $pid ) {
    if ( !exists $interval{$profilee} ) {
      $interval{$profilee} = @intervalheap;
      $intervalpair{$pair} = @intervalheap + 3;
      push @intervalheap, $secs, $usecs, 1, $secs, $usecs, 1;
    } else {
      my $i = $interval{$profilee};
      $intervalheap[$i] += $secs;
      $intervalheap[$i + 1] += $usecs;
      $intervalheap[$i + 2]++;
      if ( !exists $intervalpair{$pair} ) {
	$intervalpair{$pair} = @intervalheap;
	push @intervalheap, $secs, $usecs, 1;
      } else {
	$i = $intervalpair{$pair};
	$intervalheap[$i] += $secs;
	$intervalheap[$i + 1] += $usecs;
	$intervalheap[$i + 2]++;
      }
    }

    # Every 1000 calls check if we haven't output something in PROFMPP_INTERVAL s.
    if( !(++$check % 1000) && $interval < tv_interval \@last ) {
      @last = gettimeofday;
      my $header = '>>> ' . localtime;
      print Mpp::LOG_FILE $header, ": Flushing profiler\n";
      print "makepp: Flushing profiler\n";
      my @curtimes = times;
      printf $log "%s\n%.2fs user  %.2fs system   children: %.2fs user  %.2fs system\nTop 20 relevant functions:\n",
	$header, map { $curtimes[$_] - $times[$_] } 0..3;
      @times = @curtimes;
      display %interval, @intervalheap, 1, 19;
      display %interval, @intervalheap, 0, 19;

      print $log "Top 50 relevant functions when called from <- caller:\n";
      display %intervalpair, @intervalheap, 1, 49;
      display %intervalpair, @intervalheap, 0, 49;

      # Reset counters.
      %interval = %intervalpair = @intervalheap = ();
    }
  }
 end:
  # This displaying was a lot to do that we don't want to profile.  By getting
  # two intervals, report it as the profilees time to the caller, whereas the
  # profilee didn't count it for itself above.
  ( $secs, $usecs ) = gettimeofday;
  $subtime[-2] += $secs - $_[0][0];
  $subtime[-1] += $usecs - $_[0][1];	# Add total to caller's children
					# (which that will then subtract from
					# its own).
}

sub output {
  printf $log
    ">>> %s total\n%.2fs user  %.2fs system   children: %.2fs user  %.2fs system\nTop 100 relevant functions:\n",
    scalar( localtime ), times;
  display %total, @totalheap, 1, 99;
  display %total, @totalheap, 0, 99;

  print $log "Top 200 relevant functions when called from <- caller:\n";
  display %totalpair, @totalheap, 1, 199;
  display %totalpair, @totalheap, 0, 199;

  close $log;
}


sub reset_child {
  $child_pid = $$;
  # Don't care about the intervals, just free the memory.
  %interval = %intervalpair = @intervalheap = %total = %totalpair = @totalheap = ();
  $mintime /= $childfraction;
  $mincount = int $mincount / $childfraction;
}

sub output_child {
  return if $$ != $child_pid;	# Not direct child process.
  undef $log;
  open $log, '>>&', $log_children;
  seek $log, 0, 2;		# EOF
  &output;
}

END { &output }

1;
