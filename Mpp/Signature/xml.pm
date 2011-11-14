# $Id: xml.pm,v 1.4 2009/02/09 22:07:39 pfeiffer Exp $
use strict;
package Mpp::Signature::xml;

eval 'use XML::Parser';		# if not available, eval below will fail and fall back to md5
use Digest::MD5;

our @ISA = qw(Mpp::Signature);

=head1 NAME

Mpp::Signature::xml -- a signature class that ignores changes to whitespace and comments

=head1 DESCRIPTION

Very similar to Mpp::Signature::c_compilation_md5, except that it recognizes
different filenames.

=cut

our $xml = bless \@ISA;		# Make the singleton object.

our $space;			# Is whitespace important?

# expect	tag => [{attrs}, content ...]
sub flatten {
  my $res = "\cA$_[0]";
  for my $attr ( sort keys %{$_[1][0]} ) {
    $res .= "\cB$attr\cC$_[1][0]{$attr}";
  }
  for( my $i = 1; $i < @{$_[1]}; $i += 2 ) {
    if( $_[1][$i] ) {
      $res .= flatten( $_[1][$i], $_[1][$i+1] );
    } else {
      my $str = $_[1][$i+1];
      if( $space ) {
	$res .= "\cD$str";
      } else {
	$str =~ s/\A\s+//;
	$str =~ s/\s+\Z//;
	$str =~ s/\s+/ /;
	$res .= "\cD$str" if length $str;
      }
    }
  }
  "$res\cZ";
}

sub signature {
  my $finfo = $_[1];		# Name the argument.
  local $space = 1 if ref( $_[0] ) =~ /xml_space/;
  my $key = $space ? 'XML_SPACE_MD5_SUM' : 'XML_MD5_SUM';
  my $sum = Mpp::File::build_info_string $finfo, $key;

  unless( $sum ) {	       # Don't bother resumming if we know the answer.
    my $tree = eval { XML::Parser->new( Style => 'Tree' )->parsefile( Mpp::File::absolute_filename $finfo ) };
    if( $@ ) {			 # Not valid xml.
      require Mpp::Signature::md5; # Make sure the MD5 signature module is loaded.
      $sum = Mpp::Signature::md5::signature( $Mpp::Signature::md5::md5, $finfo );
    } else {
      $sum = Digest::MD5::md5_base64 flatten @$tree;
      Mpp::File::set_build_info_string $finfo, $key, $sum;
    }
  }
  $sum;
}

1;
