package Signature::c_compilation_md5;

use Signature::exact_match;
use Digest::MD5;

@ISA = qw(Signature::exact_match);

=head1 NAME

Signature::c_compilation_md5 -- a signature class that ignores changes to whitespace and comments

=head1 DESCRIPTION

Unlike the usual signature class, this class computes an MD5 checksum of all
source files, excluding whitespace and comments.  Your source files may
change, but if you use this signature class, makepp will be smart enough to
realize that certain changes don't matter.

=cut

$c_compilation_md5 = bless {};	# Make the singleton object.

#
# The only subroutine we need to override is the signature method; we use
# exact matching of MD5 signatures.
#
sub signature {
  my $finfo = $_[1];		# Name the argument.

  if (main::is_cpp_source_name($finfo->{NAME}) && # Does this look like a source file?
      $finfo->{".."}->is_writable) { # From a writable directory?
    my $stored_cksum = $finfo->build_info_string("C_MD5_SUM");
    return $stored_cksum if $stored_cksum; # Do not bother rescanning if
				# we have already scanned the file.
    $stored_cksum = md5sum_c_tokens($finfo->absolute_filename);
				# Scan the file.
    $finfo->set_build_info_string("C_MD5_SUM", $stored_cksum);
				# Store the checksum so we don't have to do
				# that again.
    return $stored_cksum;
  }

  return $finfo->signature;	# Use the default signature function.
}

#
# This is the function that does the work of scanning C or C++ source code,
# breaking it into tokens, and computing the MD5 checksum of the tokens.
# Argument is the file name to scan.
#
sub md5sum_c_tokens {
  my $fname = $_[0];		# Name the argument.

  local *INFILE;		# Make a local file handle.

  $main::warn_level and
    print "Computing MD5 sum of $fname\n";

  if (open(INFILE, $fname)) {	# File exists?
    local $/ = undef;		# Slurp in the whole file at once.
    local $_ = <INFILE>;	# Read it all.
    close INFILE;

    pos($_) = 0;		# Start scanning at position 0.
    my $ctx = Digest::MD5->new;	# Make a context.
    while (pos($_) < length($_)) {
      /\G\s+/gc and next;	# Skip over whitespace.
      m@\G//.*@gc and next;	# Skip over C++ comments.
      m@\G/\*.*?\*/@sgc and next; # Skip over C comments.

      my $token;

      if (/\G(\w+|\+\+|--|\<\<?=?|\>\>?=?|[-+|&*\/~^%]=?)/gc) {
				# An ordinary name or a 2 or 3 char token?
	$token = $1;
      }

      elsif (/\G\"/gc) {	# Quoted string?
	my $quotepos = pos($_)-1; # Remember where the string started.
	while (pos($_) < length($_)) {
	  /\G[^\\\"]+/gc and next; # Skip over everything between the quotes.
	  /\G\"/gc and last;	# Found the closing quote.
	  /\G\\./sgc and next;	# Skip over characters after a backslash.
	  die "How did I get here?";
	}
	$token = substr($_, $quotepos, pos($_)-$quotepos);
				# Add the string to the checksum.
      }

      elsif (/\G\'/gc) {	# Single quote expression?
	my $quotepos = pos($_)-1; # Remember where the string started.
	while (pos($_) < length($_)) {
	  /\G[^\\\']+/gc and next; # Skip over everything between the quotes.
	  /\G\'/gc and last;	# Found the closing quote.
	  /\G\\./sgc and next;	# Skip over characters after a backslash.
	  die "How did I get here?";
	}
	$token = substr($_, $quotepos, pos($_)-$quotepos);
				# Add the string to the checksum.
      }
      else {
	$token = substr($_, pos($_), 1); # Must be a single char token.
	++pos($_);
      }

#      $| = 1;
#      print " $token";
      $ctx->add(" $token");	# Add the token with a space before it to
				# keep tokens from running into each other.
    }

    return $ctx->hexdigest;
  }
  else {
    return '';
  }
}

1;
