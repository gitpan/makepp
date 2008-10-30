#! /usr/bin/perl -w

chdir 't';			# failure ignored, when called from here

my %c;				# CC=gehtnich CXX=gehtnich ./run_all.pl
@c{qw(
c_compilation.test
log_graph.test
md5.test
additional_tests/2003_10_11_idash.test
additional_tests/2003_11_25_wild.test
additional_tests/2004_02_19_repository_change.test
additional_tests/2004_03_12_condscan.test
additional_tests/2004_03_24_scanner_c_lib.test
additional_tests/2004_11_02_repository_rmstale.test
additional_tests/2004_12_06_scancache.test
additional_tests/2004_12_17_idl.test
additional_tests/2005_03_31_scanfail.test
additional_tests/2005_07_12_build_cache_cp.test
additional_tests/2006_12_07_scan_order.test
)} = ();

BEGIN {
  if( $^O =~ /^MSWin/ ) {
    require Win32API::File;
    Win32API::File::SetErrorMode( &Win32API::File::SEM_FAILCRITICALERRORS | &Win32API::File::SEM_NOOPENFILEERRORBOX );
  }
}

use Config;

$0 =~ s!.*/!!;
my $makepp = @ARGV && $ARGV[0] =~/\bm(?:ake)?pp$/ && shift;
if( @ARGV && $ARGV[0] eq '-?' ) { print <<EOF; exit }
$0\[ options in following order][ -- run_tests options][ tests]
    -T  run_tests.pl -dvs rather than default -ts
    -b  Add all build_cache tests to list.
    -c  Select only those which use the C compiler.
    -C  Select none of those which use the C compiler.
    -R  Add all repository tests to list.
    -S  None of the stress_tests.

    If no tests are given, runs all in and below the current directory.
EOF
$0 =~ s!all\.t!tests.pl!;
my $T = @ARGV && $ARGV[0] eq '-T' and shift;
my $b = @ARGV && $ARGV[0] eq '-b' and shift;
my $c = @ARGV && $ARGV[0] eq '-c' and shift;
my $C = @ARGV && $ARGV[0] eq '-C' and shift;
my $R = @ARGV && $ARGV[0] eq '-R' and shift;
my $S = @ARGV && $ARGV[0] eq '-S' and shift;

my @opts;
if( @ARGV && $ARGV[0] eq '--' ) {
  shift;
  push @opts, shift while @ARGV && $ARGV[0] =~ /^-/;
}

push @ARGV, <*build_cache*.test */*build_cache*.test> if $b;
push @ARGV, <*repository*.test */*repository*.test> if $R;

@ARGV = @ARGV ?
  map { /\.test$/ ? $_ : "$_.test" } @ARGV :
  <*.test */*.test>;
@ARGV = grep exists $c{$_}, @ARGV if $c;
@ARGV = grep !exists $c{$_}, @ARGV if $C;
@ARGV = grep !/stress_tests/, @ARGV if $S;

unshift @ARGV, @opts, $T ? '-dvs' : '-ts';
print "$0 @ARGV\n" if $ENV{DEBUG};

if( $ENV{AUTOMATED_TESTING} ) {
  system $^X, $0, @ARGV;
} else {
  exec $^X, $0, @ARGV;
}

sub mail {
  my $a = 'occitan@esperanto.org';
  if( open MAIL, "| exec 2>/dev/null; mailx -s$_[0] $a || mail -s$_[0] $a || /usr/lib/sendmail $a || mail $a" ) {
    print MAIL "$_[0]\n";
    my %acc;
    $Config{$_} && push @{$acc{$Config{$_}}}, $_ for sort keys %Config;
    print MAIL "@{$acc{$_}} => $_\n" for sort keys %acc;
    1;
  }
}

# CPAN testers don't send success or error details
my $v = sprintf $Config{ptrsize} == 4 ? 'V%vd' : 'V%vd-%dbits', $^V, $Config{ptrsize} * 8;
my $perltype =
  $Config{cf_email} =~ /(Active)(?:Perl|State)/ ? $1 :
  $Config{ldflags} =~ /(vanilla|strawberry|chocolate)/i ? ucfirst lc $1 :
  '';
$v .= "-$perltype" if $perltype;
(my $arch = $Config{myarchname}) =~ tr/ ;&|\\'"()[]*\//-/d; # clear out shell meta chars
if( !<$v/*.failed> ) {
  mail "SUCCESS-$arch-$v";
} elsif( mail "FAIL-$arch-$v" ) {
  open SPAR, '-|', $^X, 'spar', '-d', '-', $v;
  undef $/;
  print MAIL "\nbegin 755 $arch-$v.spar\n" . pack( 'u*', <SPAR> ) . "\nend\n";
}
