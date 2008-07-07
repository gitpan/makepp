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
    Win32API::File::SetErrorMode( Win32API::File::SEM_FAILCRITICALERRORS | Win32API::File::SEM_NOOPENFILEERRORBOX );
  }
}

use Config;

my $bits = $Config{ptrsize} == 8 ? '-64bit' : '';

$0 =~ s!.*/!!;
$0 =~ s!all\.t!tests.pl!;
my $T = @ARGV && $ARGV[0] eq '-T' and shift;
my $n = @ARGV && $ARGV[0] =~ s/^-n// && shift;
my $b = @ARGV && $ARGV[0] eq '-b' and shift;
my $c = @ARGV && $ARGV[0] eq '-c' and shift;
my $C = @ARGV && $ARGV[0] eq '-C' and shift;
my $R = @ARGV && $ARGV[0] eq '-R' and shift;
my $makepp = @ARGV && $ARGV[0] =~/\bmakepp$/ && shift;

push @ARGV, <*build_cache*.test */*build_cache*.test> if $b;
push @ARGV, <*repository*.test */*repository*.test> if $R;

@ARGV = @ARGV ?
  map { /\.test$/ ? $_ : "$_.test" } @ARGV :
  <*.test */*.test>;
@ARGV = grep exists $c{$_}, @ARGV if $c;
@ARGV = grep !exists $c{$_}, @ARGV if $C;
unshift @ARGV, $makepp if $makepp;
print "@ARGV\n" if $ENV{DEBUG};

unshift @ARGV, $T ? qw(-d -v) : '-t';
print "$n " if $n && $T;

if( $^O =~ /^MSWin/ ) {
  system $^X, $0, @ARGV;
} elsif( !fork ) {
  do $0;
  die "run_tests didn't exit--$@\n" if $@ and $@ !~ /Invalid argument at run_tests.pl line 169/;
  exit 1;
}

my $v = sprintf $Config{ptrsize} == 4 ? 'V%vd' : 'V%vd-%dbits', $^V, $Config{ptrsize} * 8;
$v .= "-$n" if $n;

wait;
unless( $T ) {
  my $ret = $? ? 1 : 0;
  my @failed = <*.failed */*.failed>;
  push @failed, map substr( $_, 0, -6 ) . 'log', @failed;
  if( -d 'tdir' ) {
    push @failed, 'tdir';
    my @logs = <*.log */*.log>;
    push @failed, $logs[-1] if @logs;
  }
  if( @failed && (open MAIL, "|mail -s FAIL-$^O-$v occitan\@esperanto.org" or open MAIL, '|mail occitan@esperanto.org') ) {
    print MAIL "FAIL-$^O-$v\n";
    $Config{$_} && printf MAIL "%-30s => $Config{$_}\n", $_ for sort keys %Config;
    open SPAR, '-|', $^X, 'spar', '-d', '-', @failed;
    undef $/;
    print MAIL "\nbegin 755 errors.spar\n" . pack( 'u*', <SPAR> ) . "\nend\n";
    close MAIL;
  }
  exit $ret;
}

if( -d $v ) {
  require File::Path;
  eval { File::Path::rmtree $v } && last
    or $_ < 9 && select undef, undef, undef, .1
    for 0..9;
  die $@ if $@;
}
mkdir $v or warn $!;

for( map { s/\.test$// ? grep -e, "$_.log", "$_.failed" : () } @ARGV ) {
    rename "$1/$_", $_ if s!^(.*/)!!; # Some systems don't mv from one dir to another
    rename $_, "$v/$_";
}
rename 'tdir', "$v/tdir" if -d 'tdir';
