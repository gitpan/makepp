# $Id: Basic.pm,v 1.5 2006/12/22 02:18:37 topnerd Exp $

=head1 NAME

CommandParser::Basic - The simplest possible makepp command parser

=head1 DESCRIPTION

Does nothing except add a dependency on the executable if it has a relative
path, which is implemented in the base class.

=cut

use strict;
package CommandParser::Basic;

use CommandParser;
our @ISA = qw/CommandParser/;
use TextSubs ();

*xparse_command = \&TextSubs::CONST1;

1;
