#! /usr/bin/perl

sub c() {
  open my $fh, '<', $_;
  local $_;
  while( <$fh> ) {
    return 1 if /\bg?cc\b/i || /\.c\b/;
  }
}

use Config;

my $bits = $Config{ptrsize} == 8 ? '-64bit' : '';

$0 =~ s/all/tests/;
my $c = @ARGV && $ARGV[0] eq '-c' and shift;
@ARGV or @ARGV = <*.test */*.test>;
@ARGV = grep c, @ARGV if $c;
print "@ARGV\n" if $ENV{DEBUG};

if( !fork ) {
  unshift @ARGV, '-d', '-v';
  do $0;
  die "run_tests didn't exit--$@\n" if $@ and $@ !~ /Invalid argument at run_tests.pl line 169/;
  exit 1;
}
wait;

my $v = sprintf $Config{ptrsize} == 4 ? 'V%vd' : 'V%vd-%dbits', $^V, $Config{ptrsize} * 8;

use File::Path;
rmtree $v;
mkdir $v;

my @save = ((map { s/\.test$// ? grep -e, "$_.log", "$_.failed" : () } @ARGV),
    (-d 'tdir' ? 'tdir' : ()));
exec mv => @save, $v if @save;
