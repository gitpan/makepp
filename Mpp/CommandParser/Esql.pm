# $Id: Esql.pm,v 1.18 2011/01/16 17:10:41 pfeiffer Exp $

=head1 NAME

Mpp::CommandParser::Esql - makepp command parser for various Esql preprocessors

=head1 DESCRIPTION

Parses an Embedded SQL preprocessor or compile command for implicit
dependencies.

This is essentially identical to Gcc.pm, but for some extra args, because some
precompilers can call the compiler and produce an executable.  Currently it
does not handle options or file extensions indicating an embedding language
other than C/C++.

=cut

use strict;
package Mpp::CommandParser::Esql;

use Mpp::CommandParser::Gcc;
our @ISA = 'Mpp::CommandParser::Gcc';

use Mpp::Text;
use Mpp::File;

=head2 Suffixes

Most preprocessors, except the exotic syntax of DB2 and Oracle, differ mostly
in the file suffixes they handle.  A few preprocessors agree that input files
end with F<.ec> and EXEC SQL INCLUDE file without a suffix gets .h appended.
So these are the defaults.  This class has 3 hash variables with which you can
override that, each keyed by the command without directory and on Windows
without F<.exe>:

=over

=item %suffix_re

A regexp to recognize input file arguments.

=item %suffixes

If the preprocessor accepts file arguments without their suffix, this is the
array of suffixes to try for finding the file.  It should B<not> start with a
leading slash C</>.

=item %inc_suffixes

This is the array of suffixes to try for finding the EXEC SQL INCLUDE file.
It should start with a leading slash C</>, meaning to try these suffixes only
for an argument that does not have a suffix.

=back

=cut

my $suffix_re = qr/\.ec$/;	# most common case
my %suffix_re =
 (apre => qr/\.sc$/,
  ecpg => qr/\.\w+$/,
  esqlc => qr/\.(?:dcl|sc)$/,
  gpre => qr/\.e(?:xx)?$/);

my %suffixes =
 (gpre => [qw(.e .exx)]);

my $inc_suffixes = [qw(/ .h)];	# standard case
my %inc_suffixes =
 (db2 => [qw(/ .sqC .sqx .sqc .hpp .h)]);

*factory = \&Mpp::Subs::p_esql_compilation;

sub new {
  my $self = &Mpp::CommandParser::Gcc::new_no_gcc;
  require Mpp::Scanner::Esqlc;
  $self->{SCANNER} = new Mpp::Scanner::Esqlc($self->rule, $self->dir);
  $self;
}

sub parse_arg {
  my( undef, undef, $words, $files ) = @_;
  my $scanner = $_[0]{SCANNER};
  for( $_[1] ) {
    if( $_[0]{CMD} eq 'proc' ) { # Oracle Pro*C
      if( s/^(sys_?)?include=//i ) {
	my $sys = $1;
	for( /^\((.+)\)$/ ? split( ',', $1 ) : $_ ) {
	  $scanner->add_include_dir( user => $_ );
	  $scanner->add_include_dir( sys => $_ ) if $sys;
	}
      } elsif( s/^iname=//i ) {
	push @$files, /\.[^\/]*$/ ? $_ : "$_.pc";
      } elsif( s/^define=//i ) {
	unshift @$words, "-D$_";
      } elsif( Mpp::is_windows ? s/^config=(?=(\/|[a-z]:)?)//i : s/^config=(?=(\/)?)//i ) {
	$_[0]->add_simple_dependency( $_ );
	substr $_, 0, 0, &Mpp::CommandParser::dirinfo->{FULLNAME} . '/' unless defined $1;
	Mpp::Subs::prebuild file_info( $_, $_[0]{RULE}{MAKEFILE}{CWD} ), $_[0]{RULE}{MAKEFILE}, $_[0]{RULE}{RULE_SOURCE};
	# Might be a generated file.
	if( open my $fh, $_ ) {
	  while( <$fh> ) {
	    chomp;
	    parse_arg( $_[0], $_, $words, $files );
	  }
	} else {
	  warn "config file `$_' not found";
	}
      } elsif( !/^\w+=/ ) {
	push @$files, /\.[^\/]*$/ ? $_ : "$_.pc";
      }
    } elsif( $_[0]{SUFFIXES} && Mpp::is_windows ? /(?:^|[\\\/])[^.]+$/ : /(?:^|\/)[^.]+$/ ) {
      for my $suffix ( @{$_[0]{SUFFIXES}} ) {
	if( Mpp::File::exists_or_can_be_built file_info "$_$suffix", $_[0]{RULE}{MAKEFILE}{CWD} ) {
	  push @$files, "$_$suffix";
	  last;
	}
      }
    }
  }
}

sub parse_opt {
  my $words = $_[2];
  for( $_[1] ) {
    if( s/^E(?=[DU].+)// || s/^Y// ) {	# Informix or Yard
      unshift @$words, "-$_";
    }				# else ignore unknown option.
  }
}

sub xparse_command {
  my $scanner = $_[0]{SCANNER};
  my( $cmd ) = $_[1][0] =~ (Mpp::is_windows ? /(\w+)(?:\.exe)?$/ : /(\w+)$/);
  $scanner->add_include_suffix_list( user => $inc_suffixes{$cmd} || $inc_suffixes );

  if( $cmd eq 'db2' ) {		# Special case this unusual syntax
    my $file = $_[1][1];
    return 0 if $file !~ s/^pre(?:compile|p)\s*//i; # other subcommand
    if( $file ) {
      $file =~ s/\s.*//;
    } else {
      $file = $_[1][2];
    }
    for( split ':', $ENV{DB2INCLUDE} || '' ) {
      $scanner->add_include_dir( user => $_ );
      $scanner->add_include_dir( sys => $_ );
    }
    return $scanner->scan_file( $_[0], c => $file ) || undef;
  }

  $_[0]{CMD} = $cmd;
  $_[0]{SUFFIXES} = $suffixes{$cmd};
  $_[0]{REGEXP} = $suffix_re{$cmd} || $suffix_re
    if $cmd ne 'proc';		# would match iname=x.pc -- parse_arg takes care
  goto &Mpp::CommandParser::Gcc::xparse_command;
}

sub input_filename_regexp { $_[0]{REGEXP} }

1;
