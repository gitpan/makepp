=head1 SYNOPSIS

  use Dump;

This will Dump makepp's $Mpp::File::CWD_INFO in makepp's end handler.

  use Dump ();
  Dump::Dump;
  Dump::Dump $Mpp::File::root;
  Dump::Dump @makefiles;

This will Dump makepp's $Mpp::File::CWD_INFO and then @makefiles to the file
.makepp_dump.pl.  Hashes are sorted sensibly, so they can be compared
before/after.

  $Dump::eliminate (default: qr/^ENVIRONMENT$|^\.makepp/)

is a regexp which governs the hash keys to eliminate.  Additionally '..' is
eliminated if FULLNAME occurs in a hash.

  $Dump::slash_dircontents (default: 1)

Replace {DIRCONTENTS} with a slash.

  $Dump::time

Replace file timestamps with word time.

=cut

package Mpp:Dump;

our $compress_siglist = 1;
our $eliminate = qr/^ENVIRONMENT$|^\.makepp/;
our $slash_dircontents = 1;
our $time;

use Data::Dumper;
use Mpp::File;

open DUMP, '> makepp_dump.spec';
my $oldfh = select DUMP; $| = 1; select $oldfh;
my $sep = '# ';

sub sorter($) {
  my @keys = keys %{$_[0]};
  @keys = grep !/^\.\.$/, @keys if exists $_[0]{FULLNAME};
  @keys = grep !/$eliminate/, @keys if $eliminate;
  my( @names, @dirs, @others );
  for( sort @keys ) {
    if( ref( $_[0]{$_} ) eq 'HASH' and $_[0]{$_}{DIRCONTENTS} ) {
      push @dirs, $_;
    } elsif( /NAME/ ) {
      push @names, $_;
    } else {
      push @others, $_;
    }
  }
  unshift @others, @names, @dirs;
  unshift @others, 'iSaFiLeInFo' if ref( $_[0] ) eq 'Mpp::File';
  \@others;
}

sub Dump(@) {
  local $Data::Dumper::Indent = 1;
  local $Data::Dumper::Quotekeys = 0;
  local $Data::Dumper::Sortkeys = \&sorter;

  print DUMP $sep, scalar localtime, ": @{[caller]}\n";
  $sep = "\f\n# ";

  @_ = $Mpp::File::CWD_INFO unless @_;
  for( &Dumper ) {
    s/bless\( \{\n +iSaFiLeInFo => undef,/Mpp::File {/g;
    s/\}, 'Mpp::File' \)/}/g;
    s/([[{])\n +(.+)\n +([]}])/$1 $2 $3/g;
    while( / LSTAT => \[$/gm ) {
      1 while s/\G(.*)\s+(?=[\d'\]])/$1 /gm;
    }
    s/'(-?\d{1,9})'/$1/;
    s('([\d,a-z+/]+\ca[\d,a-z+/\ca]+)'){
      my $s = $1;
      'q!Dump compressed siglist: ' . (length $s) . ' chars, ' .
	($s =~ tr/\ca//d + 1) . ' elements, ' . ($s =~ tr/,//d) . ' with comma!'
    }eig
      if $compress_siglist;
    s/'\n'/"\\n"/g;
    s/\n'/'."\\n"/g;
    s!^((?: {8})+)!"\t" x (length( $1 ) / 8)!gem;
    s!\{DIRCONTENTS\}!/!g if $slash_dircontents;
    s!(\bLSTAT => .+,) \d+ (\],?)$!$1 time $2!gm if $time;
    print DUMP;
  }
}

sub import(@) {
  # Call Dump without Mpp::perform's arguments
  my $cwd = $Mpp::File::CWD_INFO;
  eval 'END { Dump $cwd }';
}

1;
