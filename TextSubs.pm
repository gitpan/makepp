package TextSubs;
require Exporter;
@ISA = qw(Exporter);

@EXPORT = qw(index_ignoring_quotes split_on_whitespace split_on_colon unquote
	     requote format_exec_args whitespace_len hash_neq);

#
# This module contains a few subroutines for manipulating text, mostly for
# dealing with quoted strings and make expressions.
#

=head2 pattern_substitution

  @pieces = pattern_substitution($pattern, $dest, @words)

Performs a pattern substitution like the C<$(patsubst )> function (in fact,
C<$(patsubst )> is implemented using this.  $pattern contains a C<%> as a
wildcard, and $dest contains a matching C<%>.  The substitution is applied to
each word in @words, and the result returned as an array.

For example:

  @pieces = pattern_substitution("%.c", "%.o", "file1.c", "file2.c")

returns ("file1.o", "file2.o").

=cut
sub pattern_substitution {
  my ($src, $dest, @words) = @_; # Name the arguments.
  my $percent_pos = index($src, '%'); # Find the percent char.
  $percent_pos < 0 and
    die "\$(patsubst ...) called with '$src' as first argument\n";

  my $src_prefix = substr($src, 0, $percent_pos);
  my $src_suffix = substr($src, $percent_pos+1);

  $dest =~ /\%/ or
    die "\$(patsubst ...) called with '$dest' as second argument\n";

  my @ret_words;
  foreach (@words) {
    my $stem_len = length($_) - length($src_prefix) - length($src_suffix);
    if ($stem_len >= 0 &&	# Make sure prefix & suffix don't overlap.
	substr($_, 0, length($src_prefix)) eq $src_prefix &&
	substr($_, length($_)-length($src_suffix)) eq $src_suffix) {
      my $pattern_stem = substr($_, length($src_prefix),
			     length($_)-length($src_prefix)-length($src_suffix));
      my $dest_copy;
      ($dest_copy = $dest) =~ s/\%/$pattern_stem/g;
				# Replace all occurences of % with the stem.
      push @ret_words, $dest_copy;
				# Save the resulting word(s).  There may be
				# more than one if $dest contains spaces.
      defined($Makesubs::rule) and
	$Makesubs::rule->{PATTERN_STEM} = $pattern_stem;
				# Set it up so $* can return the stem.
     }
  }
  
  return @ret_words;
}

=head2 index_ignoring_quotes

  my $index = index_ignoring_quotes($string, "substr");

Works like C<index($string, "substr")>, except that the substring may not be
inside quotes or a make expression.

=cut
sub index_ignoring_quotes {
  my $substr = $_[1];
#  local *_ = \$_[0];		# Put the string in $_ (without copying it).
				# (Just doing local $_ = $_[0] actually
				# duplicates the string in memory.)
  local $_ = $_[0];		# Above doesn't work for perl 5.005.

  pos($_) = 0;			# Start at the beginning.

 mainloop:
  for (;;) {
    my $last_pos = pos($_);
    if (/\G([^\"\'\\\$]+)/gc) {	# Just ordinary characters?
      my $idx = index($1, $substr); # See if it's in those characters.
      $idx >= 0 and return $last_pos + $idx;
    }

    return -1 if length($_) <= pos($_); # End of string?  That means no match.
				# For reasons that I don't understand, testing
				# for /\G\z/gc doesn't work here.

    if (/\G\$/gc) {		# Make expression?
      &skip_over_make_expression;
      next;
    }

    if (/\G\\/gc) {		# Backslash to protect something?
      ++pos($_);		# Skip whatever it is.
      next;
    }

    if (/\G\"/gc) {		# Double quoted string?
      &skip_over_dquote;
      next;
    }

    if (/\G\'/gc) {		# Single quoted string?
      &skip_over_squote;
      next;
    }

    die "How in the world did I get here?";

  }
}

=head2 split_on_whitespace

  @pieces = split_on_whitespace($string);

Works just like 

  @pieces = split(' ', $string)

except that whitespace inside quoted strings is not counted as whitespace.
This should be called after expanding all make variables; it does not know
anything about things like "$(make expressions)".

There are three kinds of quoted strings, as in the shell.  Single quoted
strings are terminated by a matching single quote.  Double quoted strings are
terminated by a matching double quote that isn't escaped by a backslash.
Backquoted strings are terminated by a matching backquote that isn't escaped
by a backslash.

At the moment, we don't properly handle shell expressions like 

   $(echo xyz)

Unfortunately, this will be split into separate words.

=cut
sub split_on_whitespace {
  my @pieces;

#  local *_ = \$_[0];		# Put the string in $_ (without copying it).
				# (Just doing local $_ = $_[0] actually
				# duplicates the string in memory.)
  local $_ = $_[0];		# The above causes perl 5.005 to barf.

  pos($_) = 0;			# Start at the beginning.
  /^\s+/gc;			# Skip over leading whitespace.
  my $last_pos = pos($_);

 mainloop:
  for (;;) {
    /\G[^\s\"\'\`\\]+/gc;		# Skip over irrelevant things.

    last if length($_) <= pos($_); # End of string.

    my $cur_pos = pos($_);	# Remember the current position.
    if (/\G\s+/gc) {		# Found some whitespace?
      push(@pieces, substr($_, $last_pos, $cur_pos-$last_pos));
      $last_pos = pos($_);	# Beginning of next string is after this space.
      next;
    }

    if (/\G\\/gc) {		# Backslash to protect something?
      ++pos($_);		# Skip whatever it is.
      next;
    }

    if (/\G\"/gc) {		# Double quoted string?
      while (pos($_) < length($_)) {
	next if /\G[^\\\"]+/gc;	# Skip everything except quote and \.
	/\G\\/gc and ++pos($_);	# Skip char after backslash.
	/\G\"/gc and next mainloop; # We've found the end of the string.
      }
      next;			# If we get here, there was no terminating
				# quote.
    }

    if (/\G\'/gc) {		# Single quoted string?
      /\G[^\']+/gc;		# Skip until end of quoted string.
      /\G\'/gc;			# Skip concluding single quote.
      next;
    }

    if (/\G\`/gc) {		# Back quoted string?
      while (pos($_) < length($_)) {
	next if /\G[^\\\`]+/gc;	# Skip everything except quote and \.
	/\G\\/gc and ++pos($_);	# Skip char after backslash.
	/\G\`/gc and next mainloop; # We've found the end of the string.
      }
      next;			# If we get here, there was no terminating
				# quote.
    }

    die "How in the world did I get here?";

  }

  if (length($_) > $last_pos) {	# Anything left at the end of the string?
    push @pieces, substr($_, $last_pos);
  }

  return @pieces;
}

=head2 split_on_colon

  @pieces = split_on_colon("string";

This subroutine is equivalent to

  @pieces = split(/:+/, "string");

except that colons inside double quoted strings or make expressions are passed
over.  Also, a semicolon terminates the expression; any colons after a
semicolon are ignored.  This is to support parsing of this horrible rule:

  $(srcdir)/cat-id-tbl.c: stamp-cat-id; @:

=cut

sub split_on_colon {
  my @pieces;

  local *_ = \$_[0];		# Put the string in $_ (without copying it).
				# (Just doing local $_ = $_[0] actually
				# duplicates the string in memory.)

  my $last_pos = 0;
  pos($_) = 0;			# Start at the beginning.

 mainloop:
  for (;;) {
    /\G[^;:\"\'\\\$]+/gc;	# Skip over irrelevant stuff.
    last if length($_) <= pos($_); # End of string?
				# For reasons that I don't understand, testing
				# for /\G\z/gc doesn't work here.

    if (/\G(:+)/gc) {		# Found our colon?
      push(@pieces, substr($_, $last_pos, pos($_)-$last_pos-length($1)));
      $last_pos = pos($_);	# Beginning of next string is after this space.
      next;
   }

    if (/\G\$/gc) {		# Make expression?
      &skip_over_make_expression;
      next;
    }

    if (/\G\\/gc) {		# Backslash to protect something?
      ++pos($_);		# Skip whatever it is.
      next;
    }

    if (/\G\"/gc) {		# Double quoted string?
      &skip_over_dquote;
      next;			# If we get here, there was no terminating
				# quote.
    }

    if (/\G\'/gc) {		# Single quoted string?
      &skip_over_squote;
      next;
    }

    if (/\G;/gc) {		# Found end of the rule?
      pos($_) = length($_);	# Don't look for any more colons.
      next;
    }

    die "How in the world did I get here?";

  }

  if (length($_) > $last_pos) {	# Anything left at the end of the string?
    push @pieces, substr($_, $last_pos);
  }

  return @pieces;
}

#
# This routine is used to skip over a make expression.  A make expression
# is a variable, like "$(CXX)", or a funtion, like $(patsubst %.o, %.c, sdaf).
#
# The argument should be passed in the global variable $_ (not @_, as usual),
# and pos($_) should be the character immediately after the dollar sign.
# On return, pos($_) is the first character after the end of the make
# expression.
#
sub skip_over_make_expression {
  if (/\G\(/gc) {		# Does the expression begin with $(?
    for (;;) {
      /\G[^\"\'\$\)]+/gc;	# Skip over irrelevant things.
      last if /\G\)/gc;		# Quit if closing parens.
      last if length($_) <= pos($_); # Quit if end of string.  (Testing for \z
				# seems unreliable.)

      if (/\G\$/gc) {		# A nested make expression?
	&skip_over_make_expression;
	next;
      }

      if (/\G\"/gc) {		# Double quoted string?
	&skip_over_dquote;
	next;
      }

      if (/\G\'/gc) {		# Single quoted string?
	&skip_over_squote;
	next;
      }

      die "How did I get here?";
    }	
  }
  elsif (/\G\{/gc) {		# Does the expression begin with ${?
    for (;;) {
      /\G[^\"\'\$\}]+/gc;	# Skip over irrelevant things.
      last if /\G\}/gc;		# Quit if closing parens.
      last if length($_) <= pos($_); # Quit if end of string.  (Testing for \z
				# seems unreliable.)

      if (/\G\$/gc) {		# A nested make expression?a
	&skip_over_make_expression;
      }

      if (/\G\"/gc) {		# Double quoted string?
	&skip_over_dquote;
	next;
      }

      if (/\G\'/gc) {		# Single quoted string?
	&skip_over_squote;
      }

      die "How did I get here?";
    }	
  }
  else {
    ++pos($_);			# Must be a single character variable.  Just
				# skip over it.
  }	
}

#
# This subroutine is used to skip over a double quoted string.  A double
# quoted string may have a make expression inside of it; we also skip over
# any such nested make expressions.
#
# The argument should be passed in the global variable $_ (not @_, as usual),
# and pos($_) should be the character immediately after the dollar sign.
# On return, pos($_) is the first character after the closing quote.
#
sub skip_over_dquote {
  for (;;) {
    /\G[^\"\\\$]+/gc;		# Skip over irrelevant characters.

    /\G\"/gc and last;		# Found the closing quote.
    last if length($_) <= pos($_); # Quit if end of string.  (Testing for \z
				# seems unreliable.)


    if (/\G\\/gc) {		# Skip over the character following a
      ++pos($_);		# backslash.
      next;
    }	

    if (/\G\$/gc) {		# Skip over nested make expressions.
      &skip_over_make_expression;
      next;
    }

    die "How did I get here?";
  }	
}

#
# This subroutine is used to skip over a single quoted string.  A single
# quoted string may have a make expression inside of it; we also skip over
# any such nested make expressions.  The difference between a single and double
# quoted string is that a backslash is used to escape special chars inside
# a double quoted string, whereas it has no meaning in a single quoted string.
#
# The argument should be passed in the global variable $_ (not @_, as usual),
# and pos($_) should be the character immediately after the dollar sign.
# On return, pos($_) is the first character after the closing quote.
#
sub skip_over_squote {
  for (;;) {
    /\G[^\'\\\$]+/gc;		# Skip over irrelevant characters.

    /\G\'/gc and last;		# Found the closing quote.
    last if length($_) <= pos($_); # Quit if end of string.  (Testing for \z
				# seems unreliable.)

    if (/\G\\/gc) {		# Skip over the character following a
      ++pos($_);		# backslash.
      next;
    }	

    if (/\G\$/gc) {		# Skip over nested make expressions.
      &skip_over_make_expression;
      next;
    }

    die "How did I get here?";
  }	
}

=head2 unquote

  $text = unquote($quoted_text)

Removes quotes and escaping backslashes from a name.  Thus if you give it as
an argument
    \""a bc"'"'

it will return the string

    "a bc"

You must already have expanded all of the make variables in the string.
unquote() knows nothing about make expressions.

=cut

sub unquote {
  local *_ = \$_[0];		# Put the string in $_ (without copying it).
				# (Just doing local $_ = $_[0] actually
				# duplicates the string in memory.)
  my $ret_str = '';

  pos($_) = 0;			# Start at beginning of string.
  for (;;) {
    /\G([^\"\'\\]+)/gc and $ret_str .= $1; # Skip over ordinary characters.
    last if length($_) <= pos($_);

    if (/\G\"/gc) {		# Double quoted section of the string?
      for (;;) {
	/\G([^\"\\]+)/gc and $ret_str .= $1; # Skip over ordinary chars.
	/\G\"/gc and last;	# End of string.
	/\G\\(.)/sgc and $ret_str .= $1; # Handle quoted chars.
	length($_) <= pos($_) and last;	# End of string w/o matching quote.
      }
    }

    elsif (/\G\\([0-7]{1,3})/gc) { # Octal character code?
      $ret_str .= chr(oct($1));	# Convert the character to binary.
    }
    elsif (/\G\\([^\*\[\]\?])/sgc) { # Character escaped with backslash?
				# Don't weed out backslashed wildcards here,
				# because they're recognized separately in
				# the wildcard routines.
      $ret_str .= $1;		# Put it in verbatim.
    }
    elsif (/\G\\(.)/sgc) {	# Backslashed wildcard char?
      $ret_str .= "\\$1";	# Leave the backslash there.
    }

    elsif (/\G\'/sgc) {		# Single quoted string?
      /\G([^\']+)/gc and $ret_str .= $1; # Copy up to terminating quote.
      unless (/\G\'/gc ||
	      length($_) <= pos($_) and last) { # End of string w/o matching quote.
	die "How did I get here?";
      }
    }
  }

  return $ret_str;
}

=head2 requote

  $quoted_text = requote($unquoted_text);

Puts quotes around the text, and escapes any quotes inside the text, so
that calling unquote() on $quoted_text will return the same string as
$unquoted_text.

=cut
sub requote {
  local $_ = $_[0];		# Get the string.
  s/\\/\\\\/g;			# Protect all backslashes.
  s/\"/\\\"/g;			# Escape any double quotes.
  s{([\0-\037])}{sprintf("\\%o", ord($1))}eg; # Protect any binary characters.
  qq["$_"];			# Return the quoted string.
}

#
# Perl contains an optimization where it won't run a shell if it thinks the
# command has no shell metacharacters.  However, its idea of shell
# metacharacters is a bit too limited, since it doesn't realize that something
# like "XYZ=abc command" does not mean to execute the program "XYZ=abc".  
# Also, perl's system command doesn't realize that ":" is a valid shell
# command.  So we do a bit more detailed check for metacharacters and 
# explicitly pass it off to a shell if needed.
#
# This subroutine takes a shell command to execute, and returns an array
# of arguments suitable for exec() or system().
#
sub format_exec_args {
  local ($_) = @_;		# Access the string.

  if (/[:=\(\)\{\};\>\<\"\'\`\&\*\?\\]/ || # Any shell metachars?
      /^\s*(?:exit|\.|source|eval|exec|test)\b/) { # Special commands that only
				# the shell can execute?
    return ("/bin/sh", "-c", $_);
  } else {			# Let perl do its optimization.
    return ($_);		# Execute the command.
  }
}

#
# Compute the length of whitespace when it may be composed of spaces or tabs.
# Usage:
#	$len = whitespace_len($string);
#
# If the string is not all tabs and spaces, returns the length of the 
# whitespace up to the first non-white character.
#
sub whitespace_len {
  local $_ = $_[0];		# Access the string.
  my $white_len = 0;

  pos($_) = 0;			# Start at the beginning of the string.

  while (pos($_) < length($_)) {
    if (/\G( +)/gc) {		# Spaces?
      $white_len += length($1);
      next;
    }
    elsif (/\G\t/gc) {		# Tab?
      $white_len = 8*int(($white_len+8)/8); # Move to the next tab stop.
    }
    else {			# Not a whitespace character?
      return $white_len;
    }
  }

  return $white_len;
}

=head2 hash_equal

  if (hash_neq(\%a, \%b)) { ... }

Returns true (actually, returns the first key encountered that's different) if
the two associative arrays are unequal, and '' if not.

=cut

sub hash_neq {
  my ($a, $b) = @_;
#
# This can't be done simply by stringifying the associative arrays and
# comparing the strings (e.g., join(' ', %a) eq join(' ', %b)) because
# the order of the key/value pairs in the list returned by %a may differ
# even for identical hashes (if two keys happen to land in the same bucket).
#
  my %a_not_b = %$a;		# Make a modifiable copy of one of them.
  foreach (keys %$b) {
    !exists($a_not_b{$_}) and return $_ || '0_'; # Must return a true value.
    $a_not_b{$_} eq $b->{$_} or return $_ || '0_';
    delete $a_not_b{$_};	# Remember which things we've compared.
  }

  if (scalar %a_not_b) {	# Anything left over?
    return (%a_not_b)[0] || '0_'; # Return the first key value.
  }
  return '';			# No difference.
}

1;
