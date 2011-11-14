# $Id: xml.pm,v 1.4 2009/02/09 22:07:39 pfeiffer Exp $
use strict;
package Mpp::Signature::xml_space;

use Mpp::Signature::xml;

our @ISA = qw(Mpp::Signature::xml);

our $xml_space = bless \@ISA;	# Make the singleton object.
