package Makefile;

use IO::File;

use Glob;
use MakeEvent qw(when_done wait_for);
use TextSubs;
use Makesubs;
use FileInfo;
use FileInfo_makepp;

use strict qw(vars subs);

=head1 NAME

Makefile -- an object that parses makefiles and stores their relevant info

=head1 USAGE

  $makefile = Makefile::load("filename_or_dir_nae");
  $makefile = Makefile::load($fileinfo_or_dirinfo);

  $makefile->expand_string("$(STRING) $(WITH) $(MAKE) $(VARIABLES) $(OR FUNCTIONS)");
  $makefilename = $makefile->name("dir");

=head1 DESCRIPTION

The Makefile package is responsible for parsing a makefile, and
subsequently for holding all relevant information from the makefile,
such as variable definitions.

A new Makefile class may be created at any time, whenever you discover
that it is available.  The Makefile constructor parses the makefile, and
integrates all of its rules into the makepp's memory database.

=cut

$Makefile::package_seed = '000';

$Makefile::global_command_line_vars = undef;
				# The command line variables that were 
				# specified on the top level command line
				# (as opposed to additional ones that may
				# have been specified on the load_makefile
				# or recursive make lines).

#
# Targets that we ignore:
#
foreach (qw(.SUFFIXES .DEFAULT .PRECIOUS .INTERMEDIATE .SECONDARY
	    .DELETE_ON_ERROR .IGNORE .SILENT .EXPORT_ALL_VARIABLES 
	    .NOEXPORT .POSIX)) {
  $Makefile::ignored_targets{$_} = 1;
				# These targets should be ignored.  In fact,
				# they will be even if we didn't do this, but
				# if they happen to be the first target in the
				# file we don't want to make them the default
				# target.
}

=head2 expand_text("string", $makefile_line)

  my $expanded_string = $makefile->expand_text("string with make variables",
			                       $makefile_line);

Expands all the make variables and expressions in the text and returns the
result.  If an error occurs, die's with the error message prefixed by
C<$makefile_line>.

=cut
sub expand_text { 
  my $self = $_[0];
  my $makefile_line = $_[2];
#  local *_ = \$_[1];		# Get the string into $_.  This gets the
				# string without making a copy (by experiment,
				# local $_ = $_[1] makes a copy).
				# Note that this messes up @_, so we have
				# to do this after getting the other arguments.
  local $_ = $_[1];		# The above somehow doesn't work reliably
				# (it works sometimes, and I can't figure
				# out exactly when it doesn't work).

  my $ret_str = '';
  my @cur_words = ('');		# The word we are currently expanding.  There
				# may be more than one if we're in the middle
				# of RC expansion.  For example, if X = a b c
				# and we're expanding q$(X)r, then
				# @cur_words will contain just one element
				# when we see the q.  When we process the $(X)
				# @cur_words will be (qa, qb, qc).  Then when
				# we see the r, it turns into (qar, qbr, qcr).
				# When we see a space, it is reset.

  pos($_) = 0;			# Suppress a warning message.
  while (pos($_) < length($_)) {
    if (/\G([\s,:;\{\[\(\)\]\}=\#\`\"\'@]+)/gc) {	# Word separators?
      $ret_str .= join(" ", @cur_words);
				# Store the accumulated words.
      @cur_words = ('');
      $ret_str .= $1;		# Put in the original punctuation.  
    }
    elsif (/\G([^\$\s,:;\{\[\(\)\]\}=\#\`\"\'@]+)/gc) { # Text of a word?
      foreach (@cur_words) { $_ .= $1; } # Append to each word fragment
				# we're holding.
    }
    elsif (/\G\$\$/gc) {	# Double dollar sign?
      foreach (@cur_words) { $_ .= "\$"; } # Replace with a single one.
    }
    elsif (/\G\$/gc) {		# Something to expand?
#
# Get the whole text of the expression to expand, and expand any nested
# parts of it.
#      
      my $oldpos = pos($_);	# Remember where the expression starts.
      &TextSubs::skip_over_make_expression; # Find the end of it.
      my $newpos = pos($_);	# For some obscure reason, the following
				# messes up pos($_).
      my $expr = $self->expand_text(substr($_, $oldpos, $newpos-$oldpos),
				    $makefile_line);
				# Get the string to expand, and expand any
				# nested make expressions.
      if ($expr =~ s/^\(//) { $expr =~ s/\)$//; } # Strip off the surrounding
      elsif ($expr =~ s/^\{//) { $expr =~ s/\}$//; } # braces or parentheses.
      
      my @exp_words = split_on_whitespace($self->expand_expression($expr, $makefile_line));

      if (@exp_words == 1) {	# Optimize for the most common case.
				# Treat as a single word, and append to each
				# current word.
	foreach my $word (@cur_words) { $word .= $exp_words[0]; }
      }
      elsif (@exp_words > 1) {	# We have to do a real cartesian product.
	my (@old_words) = @cur_words; # Make a copy of the old things.
	@cur_words = ();
	foreach my $old_word (@old_words) {
	  foreach (@exp_words) {
	    push @cur_words, "$old_word$_";
	  }
	}
      }

      pos($_) = $newpos;	# Reset the position after the make expression.
    }
    else {
      die "$makefile_line: internal error parsing $_ at position " . pos($_);
    }
  }

  $ret_str .= join(" ", @cur_words); # Store the last word(s), if any.
  $ret_str;			# Return all the words.
}


#
# This is a helper routine which is used for expanding a variable expression.
# Arguments:
# a) The makefile.
# b) The expression to expand.  This should have no nested make expressions.
#    This expression should have had the surrounding parentheses removed.
#    For example, if expand_text() was called on the string
#    "x = $(patsubst %.o, %.c, stuff)" then the string that we actually
#    will see is "patsubst %.o, %.c, stuff".
# b) The makefile line number (for error messages only).
#
sub expand_expression {
  my ($self, $expr, $makefile_line) = @_; # Name the arguments.

  my $result;
  if ($expr =~ /^([-\w]+)\s+(.*)/s) { # Does it begin with a leading word, so it
				# could be a function?
    local $main::makefile = $self; # Pass the function a reference to the
				# makefile.
    my ($rtn, $rest_of_line) = ($1, $2);
    $rtn =~ s/-/_/g;		# Convert - into _ so it's more perl friendly.
    $result = eval { local $_;	# Prevent really strange head-scratching errors.
		     &{$self->{PACKAGE} . "::f_$rtn"}($rest_of_line, $self, $makefile_line); };
				# Call the function.
    $@ and die "$makefile_line: error expanding \"$expr\":\n$@\n";
  }
  elsif ($expr =~ /^\s+(.*)$/s) { # Did it begin with a space?
				# This is just a trigger for rc-style 
				# expansion, so we should return the text
				# verbatim.
    $result = $1;
  }
  elsif ($expr =~ /^([^\s:\#=]+):([^=]+)=([^=]+)$/) {
				# Substitution reference (e.g., "x:%.o=%.c")?
    my ($var, $from, $to) = ($1, $2, $3); # Save the variables.
    $from =~ /%/ or $from = "%$from";	# Use the full GNU make style
    $to =~ /%/ or $to = "%$to"; # substitution reference.

    $result = join(" ", TextSubs::pattern_substitution($from, $to,
						       split_on_whitespace($self->expand_expression($var, $makefile_line))));
  }
  else {			# Must be a vanilla variable to expand.
    if ($Makesubs::perl_unfriendly_symbols{$expr}) { # Is it one of the 1-char
				# symbols like "$@" that conflict with perl
				# variables?
      $result = eval { &{$Makesubs::perl_unfriendly_symbols{$expr}} };
      $@ and die "$makefile_line: $@\n";
    }
    else {
      my $perl_vname = $self->{PACKAGE} . "::$expr";
				# The name of the perl variable.

      $result = $self->{COMMAND_LINE_VARS}{$expr};
				# Try to get it from the command line.
      !defined($result) && $main::environment_override and
	$result = $ENV{$expr};
				# Get from environment if that's supposed to
				# override what's in the makefile.
				# (Don't use ||= since that will wipe out
				# a variable which is defind as "0".)
      if (!defined($result)) {	# No value yet?
	$result = $$perl_vname;	# Get from the makefile.
	defined($result) && $self->{VAR_REEXPAND}{$expr} and
	  $result = $self->expand_text($result, $makefile_line);
				# If it was't a := variable, then we need to
				# expand the variable's text.
      }
      !defined($result) && !$main::environment_override and
	$result = $ENV{$expr};
				# Get from environment if we didn't already
				# try to do that.
#
# If it's not a variable, maybe it's a function with no arguments.  See if
# there are any such functions.
#
      if (!defined($result)) {	# No result yet?
	my $rtn = $self->{PACKAGE} . "::f_$expr"; # The name of the routine.
	if (defined(*{$rtn}{CODE})) { # Defined in the makefile?
	  $perl_vname =~ s/-/_/g; # Convert - to _ so it's more perl friendly.
	  eval { local $_; $result = &{$rtn}('', $self, $makefile_line); };
				# Call the function.
				# Protect $_ so a careless function writer
				# won't have headscratching errors.
	  $@ and die "$makefile_line: $@\n"; # Forward any errors after tagging
				# them with the line.
	}
      }
    }      
      
    !defined($result) and $result = '';	# Avoid perl's warning messages.
  }

  return $result;
}

#
# Attempt to find a makefile in a directory.  Returns a fileinfo structure
# for the makefile if it found one, otherwise returns undef.
# Argument: the FileInfo structure for the directory.
#
sub find_makefile_in {
  my $dir = $_[0]->dereference;	# Resolve a soft link on the directory.  This
				# can be important if this is the first time
				# we've seen this directory.

  local $main::implicitly_load_makefiles = 0;
				# Don't let this trigger a makefile load.

  my $trial_makefileinfo;
  foreach my $names (qw(Makeppfile makefile Makefile)) {
    $trial_makefileinfo = file_info($names, $dir);
    $trial_makefileinfo->exists_or_can_be_built and
      return $trial_makefileinfo;
  }
}

#
# This subroutine is called when we have discovered a new directory and we
# want to load a makefile implicitly from it, if we haven't already.
#
# Argument: the FileInfo structure for the relevant directory.
#
sub implicitly_load {
  $main::implicitly_load_makefiles or return;
				# Don't do anything if we don't implicitly
				# load makefiles from directories.

  my $dirinfo = $_[0];

  exists($dirinfo->{ALREADY_LOADED}) and return;
				# Already tried to load something.
  $dirinfo->{ALTERNATE_VERSIONS} ||
    $dirinfo->is_writable or
      return;			# If the directory isn't writable, don't
				# try to load from it.  (Directories from
				# repositories will always be writable since
				# we're going to create them.)

  eval { Makefile::load($dirinfo, $dirinfo,
			$Makefile::global_command_line_vars); };
				# Try to load the makefile.
  $dirinfo->{ALREADY_LOADED} ||= undef;
				# Remember that we tried to load something,
				# even if we failed.
  if ($@ &&			# Some error?
      $@ !~ /can\'t find a makefile in directory/) { # Unrecognized error?
    die $@;			# Resignal the error.
  }
}

=head2 load("makefile", $default_dir, $command_line_vars)

Makes a new makefile object.  The argument is the makefile to load, or else
a directory that may contain the makefile.  Exits with die if no such
makefile exists, or if there is a fatal parse error.  Otherwise, returns
the Makefile object.

If you do not specify the default directory, then directory containing the
makefile is assumed.

If the makefile has already been loaded, then this does not reload the
makefile; it returns the old makefile object.

$command_line_vars is a reference to a hash containing the names and values of
all variables which were specified on the command line.

If there is a target in the Makefile for the Makefile itself, the makefile is
remade and then reread.  Makefile::load does not return until the makefile
has been rebuilt.

=cut

sub load {
  my $minfo = &file_info;	# Get the FileInfo struct for the
				# makefile.
  my $mdinfo = $_[1];		# Get the directory, if specified.
  my $command_line_vars = $_[2]; # Get any extra command line variables.
  $Makefile::global_command_line_vars ||= $command_line_vars;
				# If these are the top level variables,
				# remember them in case we have to load
				# other makefiles implicitly.

  if ($minfo->is_or_will_be_dir) { # Is this a directory rather than a file?
    $mdinfo ||= $minfo;		# Save pointer to the directory.
    $mdinfo = $mdinfo->dereference; # Resolve a soft link on the directory.
    $mdinfo->{ALREADY_LOADED} ||= undef; # Indicate that we're trying to load a
				# makefile from this directory.
				# This prevents recursion with implicitly
				# loading a makefile.
    my $makefile_candidate = find_makefile_in($minfo);	# Find a makefile.
    $makefile_candidate or
      die "can't find a makefile in directory " . $minfo->absolute_filename . "\n";
    $minfo = $makefile_candidate;
  } else {
    $mdinfo ||= $minfo->{".."};	# Default directory is what contains the makefile.
    $mdinfo = $mdinfo->dereference; # Resolve a soft link on the directory.
    $mdinfo->{ALREADY_LOADED} ||= undef; # Indicate that we're trying to load a
				# makefile from this directory.
  }

  if ($minfo->{ALREADY_LOADED}) { # Did we already read this makefile?
#
# Make sure we're loaded with exactly the same commands:
#
    $minfo->{ALREADY_LOADED}{CWD} != $mdinfo and
      die "makefile " . $minfo->absolute_filename . " loaded with two different default directories:\n  " . $minfo->{ALREADY_LOADED}{CWD}->absolute_filename . "\n  " . $mdinfo->absolute_filename . "\n";

#
# Check the command line variable overrides:
#
    if (join("\01", %$command_line_vars) ne
	join("\01", %{$minfo->{ALREADY_LOADED}{COMMAND_LINE_VARS}})) {
      die "makefile " . $minfo->absolute_filename . " loaded with two different sets of
  command line variables.  You may have invoked make recursively with two
  different sets of command line variables.  If this is not a mistake, then
  add --traditional-recursive-make to the command line.
  Or, it may be that the makefile was implicitly loaded when it should not
  have been.  In this case, add an explicit load_makefile statement with the
  correct variable settings, or else disable implicit loading with the
  --noimplicit-load option on the command line.\n";
    }

    return $minfo->{ALREADY_LOADED}; # Just reuse the old definition, and 
				# do not reload.
  }

  

  $mdinfo->{MAKEFILE} and
    die "attempt to load two makefiles (" . $mdinfo->{MAKEFILE}->absolute_filename . " and " . $minfo->absolute_filename . ")
  with the same default directory.  This is not supported unless you add
  the --traditional-recursive-make option to the command line.\n";


  print "Loading makefile ", $minfo->name, "\n"
    unless $main::quiet_flag;
  $main::log_level and
    main::print_log("Loading makefile ", $minfo->name,
		    " with default directory ", $mdinfo->name);

  my $mpackage = "makefile_" . $Makefile::package_seed++;
				# Make a unique package to store variables and
				# functions from this makefile.

#
# Export all subroutines from the Makesubs package into the given package, so 
# the subroutines can be used directly.  This does not export data variables.
#
  foreach my $makesub (keys %Makesubs::) {
    my $coderef = *{"Makesubs::$makesub"}{CODE}; # Is this a subroutine?
    $coderef and *{$mpackage . "::$makesub"} = $coderef;
  }
  *{$mpackage . "::rule"} = *Makesubs::rule;
				# Also pass in the $rule symbol.

  my $self = bless { MAKEFILE => $minfo,
		     PACKAGE => $mpackage,
		     CWD => $mdinfo,
		     COMMAND_LINE_VARS => $command_line_vars,
		   };
				# Allocate our info structure.
  $minfo->{ALREADY_LOADED} = $self; # Don't try to load this file again.

  %{$mpackage . "::scanners"} = %Makesubs::scanners;
				# Make a copy of the scanners array (so we can
				# modify it without affecting other makefiles).

  $ {$mpackage . "::makefile"} = $self;	# Tell the makefile subroutines
				# about it.


#
# Now make sure the makefile itself is up to date.  This is actually fairly
# difficult to do cleanly, because loading the makefile to see whether there
# is a command to rebuild the makefile also pollutes the FileInfo hierarchy
# with a bunch of rules that might not be valid any more, and there's no
# really clean way to remove them.
#
# So we resort to an elegant hack, possible only on unix: we load the makefile
# in a subprocess, rebuild the makefile if necessary, and then have the
# parent process load the makefile.  This way, the parent's FileInfo hierarchy
# doesn't contain any possible bogus rules.
#
# If this code gets ported to windows, well, we'll have to figure out 
# something else.  Probably rerun the whole perl script as a subprocess.
#
  wait_for main::build($minfo, 0); # Build the makefile, using what rules we
				# know from outside the makefile.  This may
				# also load it from a repository.

  delete $minfo->{BUILD_HANDLE}; # Get rid of the build handle, so we avoid
				# the error message that we built the file
				# before we saw the rule.

  chdir $mdinfo;		# Get in the correct directory for wildcard
				# action routines.

  if ($main::remake_makefiles && # This often causes problems, so we provide
				# a way of turning it off.
      !$minfo->build_info_string("DOES_NOT_REMAKE_SELF")) {
				# If we don't already know (from the last
				# time we read this makefile) that it doesn't
				# rebuild itself.
    wait_for new MakeEvent::Process sub { # Execute in a child process:
      $main::quiet_flag = 1;	# Don't print anything about loading makefiles,
				# because we load them twice and that could
				# be confusing.
      $main::default_signature_method = $Signature::target_newer::target_newer;
				# Use the target_newer technique for rebuilding
				# makefiles, since makefiles are often modified
				# by programs like configure which aren't
				# under the control of make.
      $main::rebuilding_makefile = 1;
				# Remember that we're rebuilding the makefile.
				# This marks all targets that are actually
				# built, so that if we try to rebuild them
				# later we also use the target_newer signature
				# method.
      $main::recursive_make_socket = undef;
      $main::recursive_make_socket_name = undef;
      delete $ENV{'MAKEPP_SOCKET'};
				# We'll need a new recursive make socket.

      $self->read_makefile($minfo);
				# Read the makefile.
      wait_for main::build($minfo, 0); # Try to rebuild the makefile.
      $FileInfo::made_temporary_link = 0; # Don't delete files we've linked
				# in from the repository.
      exit 0;			# No error.
    } and			# Something went wrong?
      die "can't find or build " . $minfo->absolute_filename . "\n";
  }

#
# At this point, we have an up-to-date makefile.  Read it in:
#
  $self->read_makefile($minfo); # Read this makefile again.

  $minfo->{BUILD_HANDLE} = undef; # Don't try to rebuild this makefile again.
				# This can cause oodles of problems.  Often
				# there are missing targets for commands that
				# rebuild the makefile, and this causes lots of
				# unnecessary rebuilds because a target changes
				# on one rebuild but we don't notice it again
				# until the next time around.
#  print "Finished loading ", $minfo->name, "\n"
#    unless $main::quiet_flag;

#
# Build up the MAKEFLAGS variable:
#
  if ($main::traditional_recursive_make) {
    my @words =			# Pass commnd line variables down.
      map { "$_=" . requote($command_line_vars->{$_}) } keys %$command_line_vars;
    $main::hard_tabs and
      push @words, '--hard-tabs';
    $main::keep_going and
      push @words, '-k';
    $main::sigmethod_name and
      push @words, "-m $main::sigmethod_name";
    $main::implicitly_load_makefiles or
      push @words, "--noimplicit-load";
    $main::log_level or
      push @words, "--nolog";
    $main::percent_subdirs and
      push @words, "--percent-subdirs";
    $main::quiet_flag and
      push @words, "-q";
    $main::logfile eq '&STDOUT' and
      push @words, "-v";
    $main::traditional_recursive_make and
      push @words, '--traditional-recursive-make';

    $ {$mpackage . "::MAKEFLAGS"} = join(" ", @words);
				# Set the variable.
    $self->{EXPORTS}{MAKEFLAGS} = 1; # Export it to the environment.
  }
#
# Fetch the values of exported variables so we can quickly change the
# environment when we have to execute a rule:
#
  if ($self->{EXPORTS}) {	# Are there any?
    foreach (keys %{$self->{EXPORTS}}) {
      $self->{EXPORTS}{$_} = $self->expand_text("\$($_)", $minfo->name);
    }	
  }


#
# Remember for later whether this makefile remakes itself, so that if it
# doesn't, we don't have to bother checking when we load it.
#
  my $self_build_rule = $minfo->get_rule; # Does this makefile have a rule for
				# building itself?
  if ($self_build_rule && $self_build_rule->makefile == $self) {
    $minfo->set_build_info_string("DOES_NOT_REMAKE_SELF", "0");
  }
  else {
    $minfo->set_build_info_string("DOES_NOT_REMAKE_SELF", "1");
  }

  return $self;
}

#
# Parse a rule definition.  Arguments:
# a) The makefile.
# b) The line in the makefile (for error messages).
# c) The target string.
# d) The dependency string.
# e) Any other : modifiers that were present on the line after the
#    dependency string.
#
sub parse_rule {
  my ($self, $makefile_line, $target_string, @after_colon) = @_;
				# Name the arguments.

  $target_string =~ s/^(\s*)//;	# Strip out leading whitespace in the target.
  my $target_whitespace_len = whitespace_len($1);
  my $first_action_indent;
  my $last_line_was_blank;
  my $action = '';		# No actions seen yet.
#
# Unfortunately, due to some bozo design, the first line of a rule
# may be on the same line as the dependencies if it is separated by
# a semicolon, like this:
#
# x.o: x.c; @echo this is a stupid syntax
#	$(CC) $< -o $@
#
  my $idx = index_ignoring_quotes($after_colon[-1], ";");
  if ($idx >= 0) {		# Do we have this abhorrent syntax?
    $action = substr($after_colon[-1], $idx+1);
    $after_colon[-1] = substr($after_colon[-1], 0, $idx);

    print STDERR "$makefile_line: warning: obsolete syntax with action on same line as dependencies\n"
      unless $main::nowarn_obsolete;
				# Strongly discourage this stupid syntax.
    $action =~ s/^\s+//;	# Strip out any leading space.  If the action
				# is entirely blank (as happens in some
				# makefiles), this will eliminate it.
  }	

#
# Get all the modifiers, and the actions for the rule (if any).
#
  local $_;

  while (defined($_ = &read_makefile_line_stripped(1))) {
				# Get the next line.
    if (/^\#/ || /^\s*$/) {	# Blank line or comment at right margin?
      $last_line_was_blank = 1;
      next;			# Skip the blank lines.
    }
    next if /^\s*\#/;		# Skip commented out lines.
    s/^(\s+)// or last;		# Strip out leading whitespace.  If there 
				# wasn't any, then this is the first line of
				# the next rule or assignment.
    my $whitespace_len = whitespace_len($1);

#
# Note that we have to be able to handle weird indentation schemes.  Make
# requires that all rules begin with a tab character.  We don't do this
# since there's no way visually to tell a tab from 8 spaces, but we do have
# to properly parse things like this:
#
# ifneq ($X,y)
#   target: dependencies
#	actions
#
#   ifneq ($Y,y) 
#     X = 3
#   endif
#   target: dependencies
# endif
#
# target1: dependencies
# ifneq ($Z,y)
#	action1
# else
#	action2
# endif
#
# Note that the ifneq/else/endif lines are never seen by this function since
# they are handled by read_makefile_line_stripped.
#
# So our rules are a bit complicated.  Here is the current set of rules:
# 1) If the line is of the same indentation as the target or less, it ends
#    the action lines.
# 2) If the line is more indented than the target line, but less indented
#    than any previous action lines, it ends the rule (unless it's indented
#    by more than 8 spaces--necessary for ugly backward compatibility).
# 3) If we have seen a blank line, or a comment line that begins at the
#    right margin, then an action line must be indented at least 8 spaces
#    (one hardware tab).  This is to solve most problems like this:
#
#	 all: xyz pdq
#
#	 ifdef something
#	  X = 3
#	 endif
#
#    Usually people put enough whitespace in their makefiles so this works.
#
    if ($whitespace_len < 8 &&
	($whitespace_len <= $target_whitespace_len ||
	 defined($first_action_indent) && $whitespace_len < $first_action_indent ||
	 $last_line_was_blank)) {
      $_ = $1 . $_;		# Put the whitespace back (in case it's the
				# next target).
      last;			# We've found the end of this rule.
    }
    if (/^:\S/ ||		# A colon modifier?
	/^:\s+((?:foreach|signature|scanner).*)/) { # One of the ones we allow
				# with a space?
      push @after_colon, $1;
    }
    else {			# Not a colon modifier?
      $action .= $_;		# Must be an action for the rule.
    }

    $first_action_indent ||= $whitespace_len; # If this was the first line,
				# remember its indentation.
    $last_line_was_blank = 0;	# This line was not blank.
  }

  $_ and unread_makefile_line($_); # We read too far, so put this
				# line back.

#
# Pull off the : modifiers.
#
  my $foreach;
  my $signature;
  my $scanner;

  while (@after_colon > 1) {	# Anything left?
    if ($after_colon[-1] =~ /^\s*foreach\s+(.*)$/) {
      $foreach and die "$makefile_line: multiple :foreach clauses\n";
      $foreach = $self->expand_text($1, $makefile_line);
      pop @after_colon;
    }
    elsif ($after_colon[-1] =~ /^\s*signature\s+(\w+)/) { # Specify signature class?
      $signature and die "$makefile_line: multiple :signature clauses\n";
      $signature = $self->expand_text($1, $makefile_line);
      defined($ {"Signature::${signature}::$signature"}) or
	die "$makefile_line: invalid signature class $signature\n";
      $signature = $ {"Signature::${signature}::$signature"};
      pop @after_colon;
    }
    elsif ($after_colon[-1] =~ /^\s*scanner\s+(.*)$/) { # Specify scanner class?
      $scanner and die "$makefile_line: multiple :scanner clauses\n";
      $scanner = $self->expand_text($1, $makefile_line);
      defined(*{$self->{PACKAGE} . "::scanner_$scanner"}{CODE}) or
	die "$makefile_line: invalid scanner $scanner\n";
      $scanner = *{$self->{PACKAGE} . "::scanner_$scanner"}{CODE};
      pop @after_colon;
    }
    else {			# Something we don't recognize?
      last;
    }
  }

#
# Now process the pieces of the rule.  We recognize several different kinds
# of rules:
#
# 1) .c.o:			# The old-style suffix rule.
# 2) %.o : %.c			# GNU make's pattern rules.
# 3) a.o b.o c.o : %.o : %.c	# GNU make's static pattern rules.
# 4) %.o : %.c :foreach abc.c def.c # Our static pattern rule.
# 5) $(patsubst %.c, %.o, $(FOREACH)) : $(FOREACH) :foreach *.c
#
# The first four forms are often more convenient to type, but they all
# get converted into the fifth form for internal use because it is the
# most powerful.  (Note that additional dependencies, possibly depending on
# $<, may be added to the fourth form.)
#
  my $expanded_target_string = $self->expand_text($target_string, $makefile_line);
				# Expand the target string now.  We reexpand
				# it later so that it works properly if it
				# contains a $(foreach).
#
# First check for an old-style suffix rule and convert this into a GNU make
# pattern rule (type 2).
#
  if ($expanded_target_string =~ /^\s*\.([-+\w]+)\.([-+\w]+)\s*$/) {
				# One of the old suffix rules?
    $expanded_target_string = $target_string = "%.$2";
				# Convert it to a new-style pattern rule.
    $after_colon[0] = "%.$1 $after_colon[0]";
  }

#
# Convert GNU make's static pattern rules into something we like better.
# If the rule was
#    a.o b.o c.o : %.o : %.c
# then we treat it as if it were written:
#    $(foreach) : $(patsubst %.o, %.c, $(foreach)) : foreach a.o b.o c.o
#
  if (@after_colon == 2) {
    $foreach and die "$makefile_line: :foreach and GNU static pattern rule are incompatible\n";
    $foreach = $target_string;
    $after_colon[1] =~ /%/ && $after_colon[0] =~ /%/ or 
      die "$makefile_line: no pattern in static pattern rule\n";
    (@after_colon) = "\$(filesubst $after_colon[0], $after_colon[1], \$(foreach))";
    $target_string = "\$(foreach)";
  }

  @after_colon == 1 or die "$makefile_line: extra :\n";
				# At this point, the only thing we haven't
				# interpreted after the colon should be the
				# dependency string.
  my @deps = split_on_whitespace($after_colon[0]);
				# Separate the dependencies.  We have to treat
				# the first one specially.
#
# Handle GNU make's regular pattern rules.  We convert a rule like
#   %.o: %.c
# into this:
#   $(filesubst %.c, %.o, $(foreach)) : $(foreach) : foreach **/*.c
#
  if (index_ignoring_quotes($expanded_target_string, "%") >= 0) { # Pattern rule?
    index_ignoring_quotes($deps[0], "%") >= 0 or
      die "$makefile_line: target has % wildcard but no dependencies.
  This is currently not supported.\n";

    unless ($foreach) {		# No foreach explicitly specified?
      $foreach = $deps[0];	# Add one, making wildcard from first dep.
      if ($main::percent_subdirs) { # % searches subdirs?
	$foreach =~ s@^\%@**/*@ or # Convert leading % to **/*.
	  $foreach =~ s/\%/*/;	# Convert nested % to just a *.
      } else {
	$foreach =~ s/\%/\*/;	# Convert percent to a wildcard.
      }	
    }	
  }

  if ($foreach) {		# Is there a foreach clause?
#
# Handle our static pattern rule, with the % modifiers:
#
    if (index_ignoring_quotes($expanded_target_string, "%") >= 0) { # Using the % syntax?

      $target_string = "\$(filesubst $deps[0], $target_string, \$(foreach))";
      foreach (@deps[1..@deps-1]) { # Handle any extra dependencies:
	index_ignoring_quotes($_, "%") >= 0 and
	  $_ = "\$(filesubst $deps[0], $_, \$(foreach))";
      }
      $deps[0] = "\$(foreach)";	# This had better match the wildcard specified
				# in the foreach clause.  I don't know of
				# any way to check that.
      $after_colon[0] = join(" ", @deps);
    }

    chdir $self->{CWD};		# Make sure we're in the correct directory,
				# or everything will be all messed up.
    Glob::wildcard_action map(unquote($_), split_on_whitespace($self->expand_text($foreach, $makefile_line))),
    sub {
#
# This subroutine is called once for each file that matches the foreach clause.
#
      my ($finfo, $was_wildcard_flag) = @_;
				# Get the arguments.

      my $pattern_level = $was_wildcard_flag ?
	($finfo->{PATTERN_LEVEL} || 0) + 1 : 0;
				# Count the number of successive pattern rules
				# applied to produce this file.
      return if $pattern_level > 3; # Don't keep on applying pattern rules
				# to their own output.

      my $rule = new Rule($target_string, $after_colon[0], $action, $self, $makefile_line);
				# Make the rule.
      local $Makesubs::rule = $rule; # Put it so $(FOREACH) can properly expand.
      $signature and $rule->set_signature_method($signature);
      $scanner and $rule->{ACTION_SCANNER} = $scanner;
      $rule->{FOREACH} = $finfo; # Remember what to expand $(FOREACH) as.
      $rule->{PATTERN_LEVEL} = $pattern_level if $was_wildcard_flag;
				# Mark it as a pattern rule if it was actually
				# done with a wildcard.

      my @targets = split_on_whitespace($self->expand_text($target_string, $makefile_line));
				# Get the targets for this rule.

      foreach (@targets) {
	my $tinfo = main::find_makepp_info(unquote($_), $self->{CWD}); # Access the target object.
	$tinfo->set_rule($rule); # Update its rule.  This will be ignored if
				# it is overriding something we shouldn't
				# override.
	$was_wildcard_flag or	# If there was no wildcard involved, this is
				# a candidate for the first target in the file.
	  $self->{FIRST_TARGET} ||= $tinfo;
				# Remember what the first target is, in case
				# no target was specified on the command
				# line.
      }
    };				# End subroutine called on every file that
				# matches the wildcard.
  }
  else {
#
# This rule is not a pattern rule.  If there is an action, then it's
# a non-pattern rule; otherwise, we're just adding extra dependencies to
# certain files.
#
    my $expanded_target = $self->expand_text($target_string, $makefile_line);
    my @targets = split_on_whitespace($expanded_target);
				# Get the list of targets.
    if (@targets eq 1) {	# Check for some special targets.
      return if $Makefile::ignored_targets{$targets[0]};
    }

    if (length($action) > 0) {	# Is this actually a rule?
#
# If the action string mentions $@, then (for backward compatibility with
# bozo make) we assume that the command must be executed once for each
# target.  This is used frequently in makefiles (especially
# those generated by automake).  For example,
#
# all-recursive install-data-recursive [other targets] :
#	for dir in $(SUBDIRS); do cd $dir; $(MAKE) $@; cd ..; done
#  
# If there is no mention of $@, then we assume that the same rule makes
# all of the dependencies at once.
#
# y.tab.c y.tab.h: parser.y
#	yacc -d $<
#
      my @target_exprs = ($expanded_target); # Assume only one target.

      if ($action =~ /\$\@/ &&	# Does it include the old $@ target?
	  $action !~ /\$[\(\{](?:outputs|targets)[\)\}]/) {
				# And it doesn't include something that refers
				# to all targets at once?
	@target_exprs = @targets; # Apply rule independently to each target.
      }

      foreach my $tstring (@target_exprs) {
	my $rule = new Rule($tstring, $after_colon[0], $action, $self, $makefile_line);
      
	$signature and $rule->set_signature_method($signature);
	$scanner and $rule->{ACTION_SCANNER} = $scanner;
	foreach (split_on_whitespace($tstring)) {
	  my $tinfo = main::find_makepp_info(unquote($_), $self->{CWD}); # Access the target object.
	  $tinfo->set_rule($rule); # Update its rule.
	  $self->{FIRST_TARGET} ||= $tinfo;
				# Remember what the first target is, in case
				# no target was specified on the command
				# line.
	}
      }
    }
    else {			# This is just adding dependencies for a 
				# specific file.  We have to expand the
				# dependency list now:
      my @dependencies = map(main::find_makepp_info(unquote($_), $self->{CWD}),
			     split_on_whitespace($self->expand_text($after_colon[0], $makefile_line)));

      if (@targets == 1 and $targets[0] =~ /^\s*\.PHONY\s*$/) {
				# Mark other targets as phony?
	foreach (@dependencies) {
	  $_->{IS_PHONY} = 1; # Mark as phony.
	}	
	return;
      }
      foreach (@targets) {
	my $tinfo = main::find_makepp_info(unquote($_), $self->{CWD});
	$tinfo->set_additional_dependencies($after_colon[0], $self, $makefile_line);
	$self->{FIRST_TARGET} ||= $tinfo;
				# Remember what the first target is, in case
				# no target was specified on the command
				# line.

      }
    }
  }				# End if not a pattern rule.

}

#
# Actually read a makefile.
#
# Arguments:
# a) The makefile structure to fill out (usually set up by Makefile::load).
# b) The FileInfo structure for the makefile.
#
sub read_makefile {
  my ($self, $minfo) = @_;

  local $_;			# Don't mess up caller's $_.

  local $Makefile::makefile = $self; # Pass this into the routines that
				# handle ifeq/ifneq.

  local $Makefile::makefile_name = $minfo->absolute_filename;
				# Get the name of the file (and pass this
				# to all subroutines we call).

  local $Makefile::makefile_fh = new IO::File $Makefile::makefile_name;
				# Open up the file.  Using local makes this
				# handle temporarily accessible to every
				# subroutine we call.
  local $Makefile::hold_line;	# Nothing in the hold area yet.

  local $Makefile::last_conditional_start;
				# Don't mess up error messages from parent
				# makefile.
  my $mpackage = $self->{PACKAGE}; # Access the package.

  defined $Makefile::makefile_fh or
    die "can't open makefile " . $minfo->absolute_filename . "--$!\n";

 makefile_line:
  while (defined($_ = read_makefile_line_stripped())) { # Read a line at a time.
    next if /^\s*$/;		# Skip blank lines.

    my $makefile_line = $Makefile::makefile_name . ":$.";
				# The line name to use for error messages.

    if (/^\s*([^\s:\#\=]+)\s*([:+?!]?)=\s*(.*?)\s*$/) { # Variable assignment?
      $1 eq "MAKE" && $main::warn_level and
	main::print_error("warning: MAKE redefined, recursive make won't work as expected");
      if ($2 eq "+") {		# Append?
	$ {$mpackage . "::$1"} .= ' ' . 
	  (exists($self->{VAR_REEXPAND}{$1}) ? $3 : # Was it a regular =?
	   $self->expand_text($3, $makefile_line));
				# Expand the RHS if it was set with := 
				# previously.
      } elsif ($2 eq ":") {	# Assignment with immediate evaluation of rhs?
	$ {$mpackage . "::$1"} = $self->expand_text($3, $makefile_line);
	delete $self->{VAR_REEXPAND}{$1}; # Don't expand this text again.
      } elsif ($2 eq "!") {	# Run command through shell to evaluate?
	$ {$mpackage . "::$1"} = Makesubs::f_shell($self->expand_text($3, $makefile_line), $self, $makefile_line);
      } elsif ($2 eq "?") {	# Assign only if not previously assigned?
	if (!defined($ {$mpackage . "::$1"}) &&
	    !defined($main::command_line_variables{$1}) &&
	    !defined($ENV{$1})) {
	  $ {$mpackage . "::$1"} = $3;
	  $self->{VAR_REEXPAND}{$1} = 1; # Reexpand when invoked.
	}
      } else {
	$ {$mpackage . "::$1"} = $3;
	$self->{VAR_REEXPAND}{$1} = 1; # Remember to expand this variable's
				# contents when it's invoked.
      }
      next;
      
    }
#
# It's not an assignment. Check for a rule of some sort.  Basically, we just
# look for a colon, but this is somewhat tricky because there may be extra
# colons inside quotes or variable expansions.
#
    my @pieces = split_on_colon($_);
    if (@pieces > 1) {		# Was there a colon somewhere?
      $self->parse_rule($makefile_line, @pieces);
      next;
    }

#
# It's not a rule, either.  Check for a statement, like
# "include xyz.mk".
#
    if (/^\s*([-\w]+)(.*)$/) {	# Statement at beginning of line?
      my ($rtn, $rest_of_line) = ($1, $2);
      $rtn =~ s/-/_/g;		# Make routine names more perl friendly.
      if (defined(*{$self->{PACKAGE} . "::s_$rtn"})) { # Function from makefile?
	eval { &{$self->{PACKAGE} . "::s_$rtn"}($rest_of_line, $self, $makefile_line); };
				# Try to call it as a subroutine.
	$@ and die "$makefile_line: error handling $rtn statement\n$@\n";
      }	else {
	die "$makefile_line: unknown statement $rtn\n";
      }
      next;
    }

    die "$makefile_line: syntax error\n";
  }
}

#
# Register a scanner.  Arguments:
# a) The makefile.
# b) The word in the command to match.
# c) A reference to the subroutine.
#
sub register_scanner {
  my ($self, $word, $subr) = @_;

  $ {$self->{PACKAGE} . "::scanners"}{$word} = $subr;
}

#
# This subroutine reads a single line from the makefile 
# which has been opened in $Makefile::makefile_fh.  It works along
# with unread_makefile_line so you can temporarily put back lines if
# you've read too far.
#
@Makefile::hold_lines = undef;	# Nothing in the hold area yet.

sub read_makefile_line {
  if (defined $Makefile::hold_line) { # Was anything unread?
    my $ret = $Makefile::hold_line; # Get it.
    $Makefile::hold_line = undef; # Don't return it twice.
    return $ret;
  }

  return scalar <$Makefile::makefile_fh>; # Read another line.
}

#
# Read a line from this makefile, strip comments, and handle line
# continuations ('\' at end of line) and GNU make style conditionals.
#
# If you pass a true argument, then comments are not stripped but line
# continuations are handled.
#
sub read_makefile_line_stripped {
  my $line;

  for (;;) {			# Loop until we get a whole line.
    my $next_line = &read_makefile_line; # Get the next line.
    !defined $next_line and last;

    $next_line =~ s/^\s+// unless !defined $line;
				# Strip out leading whitespace from line
				# continuations.
    unless ($_[0]) {
      next if $next_line =~ /^\#/; # Skip it if it begins with a comment.
      $next_line =~ s/\s+\#.*//; # Strip out comments.
    }
    $line .= $next_line;	# Append it to the current line.
    last unless $line =~ s/\\\s*$/ /; # Quit unless there's a trailing \.
				# Note that the trailing backslash has to be
				# replaced by whitespace to conform with
				# some makefiles I have seen.
  }

#
# Handle GNU make's conditionals:
#
  defined($line) or return undef; # No point checking at end of file.

  if ($line =~ s/^\s*if(eq|neq|def|ndef)\b//) {
				# Looks like an if statement?
    $Makefile::last_conditional_start = $.;
				# Remember what line this was on so we can
				# give better error messages.
    my $cond = $1;		# Remember what the condition was.
    $line = $Makefile::makefile->expand_text($line, $.);
				# Expand away all the variables.
    $line =~ s/^\s+//;		# Strip leading whitespace.
    $line =~ s/\s+$//;		# Strip trailing whitespace.

    my $truthval;
    if ($cond =~ /def$/) {	# See whether something is defined?
      $truthval = exists $main::command_line_variables{$line} ||
	exists $ {$Makefile::makefile->{PACKAGE} . "::"}{$line} ||
	  exists $ENV{$line};
				# See if it was defined on the command line,
				# in the makefile, or in the environment.
    }	
    elsif ($cond =~ /eq$/) {	# Check for string equality?
      if ($line =~ /^\((.*?),(.*)\)$/) { # Parenthesized syntax?
	$truthval = $1 eq $2;
      }
      else {
	my ($first, $second) = split_on_whitespace($line);
				# Split on whitespace except whitespace inside
				# the quotes.
	defined($first) or $first = '';
	defined($second) or $second = '';
	$truthval = (unquote($first) eq unquote($second));
				# Remove the quotes and compare.
      }	
    }
    $cond =~ /^n/ and $truthval = !$truthval;
				# Check for negated condition.
    if (!$truthval) {		# Was it true?
      &skip_makefile_until_else_or_endif;
				# No.  Skip a big chunk.
    }
    goto &read_makefile_line_stripped; # Now return a line.
  }
  elsif ($line =~ /^\s*else\s*(?:\#|$)/) { # Else clause for an if?
    &skip_makefile_until_else_or_endif;
				# If we're here, the condition must have been
				# true, so we know the else part must be false.
				# Skip until we see the endif.
    goto &read_makefile_line_stripped; # Return the next line.
  }
  elsif ($line =~ /^\s*endif\s*(?:\#|$)/) { # End of an if?
    goto &read_makefile_line_stripped; # Return the next line.
  }

  return $line;
}	

#
# Skip until we find a line containing else or endif.  This is used to skip
# over the false part of an if statement.
#
sub skip_makefile_until_else_or_endif {
  my $endif_expected = 1;	# We return on the first endif, unless we see
				# a nested if in the mean time.

  for (;;) {
    my $line = <$Makefile::makefile_fh>; # Read another line.
    !defined($line) and
      die "$Makefile::makefile_name:$Makefile::last_conditional_start: end of makefile inside conditional\n";
    while ($line =~ s/\\\s*$/ /) {
      my $nextline = <$Makefile::makefile_fh>; # Handle continuations, because
				# we don't want to find an else inside an
				# action.
      last if !defined($nextline);
      $line .= $nextline;
    }

    if ($line =~ /^\s*if(?:eq|neq|def|ndef)\b/) {
      ++$endif_expected;	# Need another endif.
    }	
    elsif ($line =~ /^\s*else\s*(?:\#|$)/) {
      return if $endif_expected == 1; # Found the matching else for the
				# current conditional.
    }
    elsif ($line =~ /^\s*endif\s*(?:\#|$)/) {
      return if --$endif_expected == 0;	# Found the last expected endif.
    }	
  }

}

#
# Put a line back, so it can be read again:
#
sub unread_makefile_line {
  $Makefile::hold_line = $_[0];
}

1;
