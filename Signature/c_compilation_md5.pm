# $Id: c_compilation_md5.pm,v 1.3 2003/07/18 21:11:08 grholt Exp $
package Signature::c_compilation_md5;

use Signature::exact_match;
use Digest::MD5;
use TextSubs;

@ISA = qw(Signature::exact_match);

=head1 NAME

Signature::c_compilation_md5 -- a signature class that ignores changes to whitespace and comments

=head1 DESCRIPTION

Unlike the usual signature class, this class computes an MD5 checksum of all
source files, excluding whitespace and comments.  Your source files may
change, but if you use this signature class, makepp will be smart enough to
realize that certain changes don't matter.

More specifically:

=over 4

=item *

Comments are treated as if they were a single space.

=item *

Multiple spaces and tabs are collapsed to a single space (unless they are
inside quoted strings).

=item *

Spaces and tabs before a newline are ignored.

=item *

Newlines affect the signature.  This means that if you insert some lines in
the file, even if they were only comments, recompilation will occur.  Strictly speaking, recompilation is not necessary in this case, but makepp will recompile anyway to avoid messing up line numbers in the debugger.

=back

What this means is that you can freely add or change comments in your code, or
reindent your code, and as long as you don't affect the line numbers, there
will be no recompilation.

=cut

$c_compilation_md5 = bless {};	# Make the singleton object.

#
# The only subroutine we need to override is the signature method; we use
# exact matching of MD5 signatures.
#
sub signature {
  my $finfo = $_[1];		# Name the argument.

  is_object_or_library_name($finfo->{NAME}) and
                                # Looks like some kind of a binary file?
    return $finfo->signature;   # Use default signature function.

  !$finfo->{".."}->is_writable and # Not a writable directory--don't bother
    return $finfo->signature;   # scanning.

  !is_cpp_source_name($finfo->{NAME}) && # Unrecognized suffix?
    (!$finfo->file_exists ||    # File doesn't exist yet?
     -B $finfo->absolute_filename) and # Binary file?
       return $finfo->signature; # Don't use MD5 scanning.
    
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

#
# This is the function that does the work of scanning C or C++ source code,
# breaking it into tokens, and computing the MD5 checksum of the tokens.
# Argument is the file name to scan.
#
sub md5sum_c_tokens {
  my $fname = $_[0];		# Name the argument.

  local *INFILE;		# Make a local file handle.

#  $main::warn_level and
#    print "Computing commentless MD5 sum of $fname\n";

  if (open(INFILE, $fname)) {	# File exists?
    local $/ = undef;		# Slurp in the whole file at once.
                                # (This makes it easier to handle C-style
                                # comments.)
    local $_ = <INFILE>;	# Read it all.
    close INFILE;

    pos($_) = 0;		# Start scanning at position 0.
    my $ctx = Digest::MD5->new;	# Make a context.

    my $space_pending = 0;      # No space being held.
    while (pos($_) < length($_)) {
      m@\G//.*@gc and next;	# Skip over C++ comments.
      if (/\G\n/gc) {
        $token = "\n";
        $space_pending = 0;     # Strip out any spaces before the newline.
      }
      elsif (/\G[ \t]+/gc) {
        $space_pending = 1;     # Remember that there's a space before the
                                # next token.
        next;
      } 
      elsif (m@\G/\*(.*?)\*/@sgc) { # C comment?
        my $n_newlines = ($1 =~ tr/\n//); # Count # of newlines.
        if ($n_newlines == 0) { # No newlines at all?
          $space_pending = 1;   # Treat as a space.
          next;
        }
        $space_pending = 0;     # Ignore spaces before the newline.
        $token = ("\n" x $n_newlines);
                                # Replace with same number of newlines, or
                                # at least a single space.
      }

      elsif (/\G(\w+|\+\+|--|\<\<?=?|\>\>?=?|[-+|&*\/~^%]=?)/gc) {
				# An ordinary name or a 2 or 3 char token?
	$token = $1;
      }

      elsif (/\G\"/gc) {	# Quoted string?
	my $quotepos = pos($_)-1; # Remember where the string started.
	while (pos($_) < length($_)) {
	  /\G[^\\\"]+/sgc and next; # Skip over everything between the quotes.
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
      $space_pending and $ctx->add(' ');
      $ctx->add($token);        # Add the token into the checksum.
    }

    return $ctx->hexdigest;
  }
  else {
    return '';
  }
}

1;
