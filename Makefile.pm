# $Id: Makefile.pm,v 1.7 2003/07/18 21:13:13 grholt Exp $
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

  $makefile->expand_text("$(STRING) $(WITH) $(MAKE) $(VARIABLES) $(OR FUNCTIONS)");
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
foreach (qw(.DEFAULT .PRECIOUS .INTERMEDIATE .SECONDARY
	    .DELETE_ON_ERROR .IGNORE .SILENT .EXPORT_ALL_VARIABLES 
	    .NOEXPORT .POSIX)) {
  $Makefile::ignored_targets{$_} = 1;
				# These targets should be ignored.  In fact,
				# they will be even if we didn't do this, but
				# if they happen to be the first target in the
				# file we don't want to make them the default
				# target.
}

=head2 expand_text($makefile, "string", $makefile_line)

  my $expanded_string = $makefile->expand_text("string with make variables",
			                       $makefile_line);

Expands all the make variables and expressions in the text and returns the
result.  If an error occurs, die's with the error message prefixed by
C<$makefile_line>.

=cut
sub expand_text { 
  $_[1] =~ /\$/ or return $_[1]; # No variables ==> no substitution, so exit
                                # immediately to avoid consuming CPU time.

  my $self = $_[0];
  my $makefile_line = $_[2];
#  local *_ = \$_[1];		# Get the string into $_.  This gets the
				# string without making a copy (by experiment,
				# local $_ = $_[1] makes a copy).
				# Note that this messes up @_, so we have
				# to do this after getting the other arguments.
  local $_ = $_[1];		# The above somehow does not work reliably
				# (it works sometimes, and I do not understand
				# exactly when it does not work).
  my $ret_str = '';
  pos($_) = 0;			# Suppress a warning message.

  my $rc_substitution = $ {$self->{PACKAGE} . "::rc_substitution"};
  !defined($rc_substitution) and $rc_substitution = $main::rc_substitution;
  if ($rc_substitution) {
#
# Code for handling rc-style substitution (the default):
#
    my @cur_words = ('');	# The word we are currently expanding.  There
				# may be more than one if we are in the middle
				# of RC expansion.  For example, if X = a b c
				# and we are expanding q$(X)r, then
				# @cur_words will contain just one element
				# when we see the q.  When we process the $(X)
				# @cur_words will be (qa, qb, qc).  Then when
				# we see the r, it turns into (qar, qbr, qcr).
				# When we see a space, it is reset.

    while (pos($_) < length($_)) {
      if (/\G([\s,:;\{\[\(\)\]\}=\#\`\"\'@]+)/gc) {	 # Word separators?
	$ret_str .= join(" ", @cur_words);
				# Store the accumulated words.
	@cur_words = ('');
	$ret_str .= $1;		# Put in the original punctuation.  
      } elsif (/\G([^\$\s,:;\{\[\(\)\]\}=\#\`\"\'@]+)/gc) {  # Text of a word?
	foreach (@cur_words) {
	  $_ .= $1;
	}			# Append to each word fragment we are holding.
      } elsif (/\G\$\$/gc) {	# Double dollar sign?
	foreach (@cur_words) {
	  $_ .= "\$";
	}			# Replace with a single one.
      } elsif (/\G\$/gc) {	# Something to expand?
#
# Get the whole text of the expression to expand, and expand any nested
# parts of it.
#      
	my $oldpos = pos($_);	# Remember where the expression starts.
	&TextSubs::skip_over_make_expression; # Find the end of it.
	my $newpos = pos($_);	# For some obscure reason, the following
				# messes up pos($_).
	my $expr = substr($_, $oldpos, $newpos-$oldpos);
				# Get the expression to expand.
	if ($expr =~ s/^\(//) {
	  $expr =~ s/\)$//;
	}			# Strip off the surrounding
	elsif ($expr =~ s/^\{//) {
	  $expr =~ s/\}$//;
	}			# braces or parentheses.

	if ($expr !~ /^\s*(?:if|foreach)\b/) { # Not one of the special
				# expressions that can't be expanded
				# immediately?
	  $expr = $self->expand_text($expr, $makefile_line);
				# Expand any nested make expressions.
	}
	
	my @exp_words = split_on_whitespace($self->expand_expression($expr, $makefile_line));
	
	if (@exp_words == 1) {	# Optimize for the most common case.
				# Treat as a single word, and append to each
				# current word.
	  foreach my $word (@cur_words) {
	    $word .= $exp_words[0];
	  }
	} elsif (@exp_words > 1) { # We have to do a real cartesian product.
	  my (@old_words) = @cur_words; # Make a copy of the old things.
	  @cur_words = ();
	  foreach my $old_word (@old_words) {
	    foreach (@exp_words) {
	      push @cur_words, "$old_word$_";
	    }
	  }
	}
	
	pos($_) = $newpos;	# Reset the position after the make expression.
      } else {
	die "$makefile_line: internal error parsing $_ at position " . pos($_);
      }
    }
    
    $ret_str .= join(" ", @cur_words); # Store the last word(s), if any.
    return $ret_str;		# Return all the words.
  }
  else {
#
# Code for handling the traditional substitution style (needed for some
# legacy makefiles, usually those that depend on leading/trailing whitespace).
#
    while (pos($_) < length($_)) {
      if (/\G([^\$])/gc) {	# Text w/o variables?
	$ret_str .= $1;		# Just append it.
      }
      elsif (/\G\$/gc) {	# Got a dollar sign.
#
# Get the whole text of the expression to expand, and expand any nested
# parts of it.
#      
	my $oldpos = pos($_);	# Remember where the expression starts.
	&TextSubs::skip_over_make_expression; # Find the end of it.
	my $newpos = pos($_);	# For some obscure reason, the following
				# messes up pos($_).
	my $expr = substr($_, $oldpos, $newpos-$oldpos);
				# Get the expression.

	if ($expr =~ s/^\(//) {
	  $expr =~ s/\)$//;
	}			# Strip off the surrounding
	elsif ($expr =~ s/^\{//) {
	  $expr =~ s/\}$//;
	}			# braces or parentheses.

	if ($expr !~ /^\s*(?:if|foreach)\b/) { # Not one of the special
				# expressions that can't be expanded
				# immediately?
	  $expr = $self->expand_text($expr, $makefile_line);
				# Expand any nested make expressions.
	}

	$ret_str .= $self->expand_expression($expr, $makefile_line);
				# Do the expansion.
	pos($_) = $newpos;	# Move to after the make expression.
      }	
      else {
	die "$makefile_line: internal error parsing $_ at position " . pos($_);
      }	
    }

    return $ret_str;
  }
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
  if ($expr =~ /^([-\w]+)\s+(.*)/s) { # Does it begin with a leading word, so 
				# it could be a function?
    local $main::makefile = $self; # Pass the function a reference to the
				# makefile.
    my ($rtn, $rest_of_line) = ($1, $2);
    $rtn =~ s/-/_/g;		# Convert - into _ so it's more perl friendly.
    $rtn =~ s/\./_dot_/g;
    my $code = *{$self->{PACKAGE} . "::f_$rtn"}{CODE};
				# See if it's a known function.
    if ($code) {
      $result = eval {		# Evaluate the function.
	local $_;		# Prevent really strange head-scratching errors.
	&$code($rest_of_line, $self, $makefile_line); 
      };
    } else {
      die "$makefile_line: unknown function $rtn\n";
    }

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
      my $expand_again = 1;     # Assume it was an = variable, not a :=
                                # variable.
                                # Note that we actually do want to reexpand
                                # variables gotten from the comand line (gcc
                                # 2.95.2's build procedure depends on this) and
                                # from the environment.

      {                         # This isn't a real loop; it merely defines
                                # where "last" actually goes to.
        $result = $self->{COMMAND_LINE_VARS}{$expr};
				# Try to get it from the command line.
        defined($result) and last;
        if ($main::environment_override) { # Environment variables override
                                # makefile variables?
          $result = $self->{ENVIRONMENT}{$expr};
          defined($result) and last;
        }

                                # Check for target-specific variables.  The
                                # array target_specific is set up by the
                                # rule when it's expanding the action.
        $result = $Makefile::target_specific->{$expr};
        if (defined($result)) {
          exists($Makefile::target_specific_reexpand->{$expr}) or
            $expand_again = 0;  # It was a := variable.
          last;
        }

        $result = $ {$self->{PACKAGE} . "::$expr"};
                                # Get from the makefile.
        if (defined($result)) { # Did we find it?
          exists($self->{VAR_REEXPAND}{$expr}) or 
            $expand_again = 0;  # It was a := variable.
          last;
        }

        if (!$main::environment_override) { # Didn't already look at environment?
          $result = $self->{ENVIRONMENT}{$expr};
				# Get from environment if we didn't already
				# try to do that.
          defined($result) and last;
        }
#
# If it's not a variable, maybe it's a function with no arguments.  See if
# there are any such functions.
#
	my $perl_fname = $self->{PACKAGE} . "::f_$expr"; # Name of the function.
	$perl_fname =~ s/-/_/g;	# Convert - to _ so it's more perl friendly.
	$perl_fname =~ s/\./_dot_/g;
	if (defined(*{$perl_fname}{CODE})) { # Defined in the makefile?
	  eval { 
	    local $_;		# Causes very weird errors if $_ is messed up.
	    $result = &{$perl_fname}('', $self, $makefile_line); 
	  };
	  $@ and die "$makefile_line: $@\n"; # Forward any errors after tagging
				# them with the line.
          $expand_again = 0;    # Don't perform another expansion on this.
          last;
	}

        $result = '';           # Variable not found--substitute blank.
        $expand_again = 0;
      }
                                # "last" above breaks to here:
      $expand_again and
        $result = $self->expand_text($result, $makefile_line);
                                # Reexpand any variables inside.
    }
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

  exists($dirinfo->{MAKEINFO}) and return;
				# Already tried to load something.
  $dirinfo->is_writable ||      # Directory already exists?
    ($dirinfo->{ALTERNATE_VERSIONS} && !$dirinfo->{EXISTS}) or
      return;			# If the directory isn't writable, don't
				# try to load from it.  (Directories from
				# repositories will always be writable since
				# we're going to create them, except if there
                                # is already an unwritable directory there.)
#
# See if this directory or any of its parents is marked for no implicit
# loading.
#
  for (my $pdirinfo = $dirinfo; $pdirinfo; $pdirinfo = $pdirinfo->{".."} || '') {
    $pdirinfo->{NO_IMPLICIT_LOAD} and return;
  }

  eval { Makefile::load($dirinfo, $dirinfo,
			$Makefile::global_command_line_vars,
			"",
			\@main::makepp_include_path,
			\%main::global_ENV); };
				# Try to load the makefile.
  $dirinfo->{MAKEINFO} ||= undef;
				# Remember that we tried to load something,
				# even if we failed.
  if ($@ &&			# Some error?
      $@ !~ /can\'t find a makefile in directory/) { # Unrecognized error?
    die $@;			# Resignal the error.
  }
}

=head2 load("makefile", $default_dir, $command_line_vars, $makecmdgoals, $include_path, $environment)

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

$makecmdgoals is the value of $(MAKECMDGOALS) for this makefile.

include_path is an array of FileInfo structures for directories that the
include statement should search.

$environment is a hash containing the environment for this particular
makefile.

If there is a target in the Makefile for the Makefile itself, the makefile is
remade and then reread.  Makefile::load does not return until the makefile
has been rebuilt.

=cut

sub load {
  my $minfo = &file_info;	# Get the FileInfo struct for the
				# makefile.
  my ($mdinfo, $command_line_vars, $makecmdgoals, $include_path, $env) =
    @_[1..5];			# Name the other arguments.
  my %this_ENV = %$env;		# Make a modifiable copy of the environment.
  delete $this_ENV{'MAKEPP_SOCKET'}; # Get rid of our special variables.
				# (This gets put back into the environment
				# later by Rule::execute, but we don't want
				# it here when we're making comparisons.)
  delete $this_ENV{'SHLVL'};	# This variable gets incremented by the
				# shell and can cause unnecessary makefile
				# reloads.
  delete $this_ENV{'OLDPWD'};	# Another variable that can cause unnecessary
				# reloads.
  delete $this_ENV{'_'};	# Don't know what this one does, but it too
				# seems to cause problems.

  $Makefile::global_command_line_vars ||= $command_line_vars;
				# If these are the top level variables,
				# remember them in case we have to load
				# other makefiles implicitly.

  if ($minfo->is_or_will_be_dir) { # Is this a directory rather than a file?
    $mdinfo ||= $minfo;		# Save pointer to the directory.
    $mdinfo = $mdinfo->dereference; # Resolve a soft link on the directory.
    $mdinfo->{MAKEINFO} ||= undef; # Indicate that we're trying to load a
				# makefile from this directory.
				# This prevents recursion with implicitly
				# loading a makefile.
    my $makefile_candidate = find_makefile_in($minfo);	# Find a makefile.
#
# If there's no makefile, then load the default makefile from that
# directory.
#
    $makefile_candidate or
      $makefile_candidate = file_info("$main::datadir/makepp_default_makefile.mk");
#    $makefile_candidate or
#      die "can't find a makefile in directory " . $minfo->absolute_filename . "\n";
    $minfo = $makefile_candidate;
  }
  else {
    $mdinfo ||= $minfo->{".."};	# Default directory is what contains the makefile.
    $mdinfo = $mdinfo->dereference; # Resolve a soft link on the directory.
    $mdinfo->{MAKEINFO} ||= undef; # Indicate that we're trying to load a
				# makefile from this directory.
				# This prevents recursion with implicitly
				# loading a makefile.
  }

  my $mpackage;
  my $self;
  if ($mdinfo->{MAKEINFO}) {	# Was there a previous makefile?
    $self = $mdinfo->{MAKEINFO}; # Access the old structure.
    my $var_changed;		# What actually changed to cause a reload.
    if ($self->{MAKEFILE} == $minfo) {
				# Attempt to reload the same makefile?
				# If the variables and include path are the
				# same, no need to reload.  Otherwise, we'll
				# have to reload.
      $var_changed = hash_neq($command_line_vars, $self->{COMMAND_LINE_VARS}) ||
	hash_neq(\%this_ENV, $self->{ENVIRONMENT});
				# Did any variables change?
      unless ($var_changed) {
	join(' ', @$include_path) eq join(' ', @{$self->{INCLUDE_PATH}}) or
	  $var_changed = 'include path';
      }
      $var_changed or return $mdinfo->{MAKEINFO};
				# No need to reload the makefile--just reuse
				# what we've got.
    }	
    else {
      die "attempt to load two makefiles (" . $mdinfo->{MAKEINFO}{MAKEFILE}->absolute_filename . " and " . $minfo->absolute_filename . ")
  with the same default directory.  This is not supported unless you add
  the --traditional-recursive-make option to the command line.\n";

    }      
#
# We're reloading this makefile.  Clean out all the old definitions, and set
# up a few variables:
#
    $self->{ENVIRONMENT} = \%this_ENV; # Store the new environment.
    $self->{COMMAND_LINE_VARS} = $command_line_vars;
    $self->{INCLUDE_PATH} = [ @$include_path ];
    ++$self->{LOAD_IDX};	# Invalidate all the rules from the last time 
				# we loaded this makefile.  (See code in
				# FileInfo::set_rule.)

    $mpackage = $self->{PACKAGE};
    foreach (keys %{$mpackage . "::"}) { delete $ {$mpackage . "::"}{$_}; }
				# Wipe the whole package.

    my $msg = "Reloading makefile " . $minfo->name;
    print "$msg\n" unless $main::quiet_flag;
    $main::log_level and
      main::print_log($msg, " (because of $var_changed) with default directory ", $mdinfo->name);
  }
  else {			# Loading a new makefile:
    if ($minfo->{NAME} eq 'makepp_default_makefile.mk') {
#    print "Loading default makefile for directory ", $mdinfo->name, "\n"
#      unless $main::quiet_flag;
      $main::log_level and
	main::print_log("Loading default makefile for directory ", $mdinfo->name);
    }
    else {
      my $msg = "Loading makefile " . $minfo->name;
      print "$msg\n" unless $main::quiet_flag;
      $main::log_level and
	main::print_log($msg, " with default directory ", $mdinfo->name);
    }

    $mpackage = "makefile_" . $Makefile::package_seed++;
				# Make a unique package to store variables and
				# functions from this makefile.

    $self = bless { MAKEFILE => $minfo,
		    PACKAGE => $mpackage,
		    CWD => $mdinfo,
		    COMMAND_LINE_VARS => $command_line_vars,
		    INCLUDE_PATH => [ @$include_path ],
		    ENVIRONMENT => \%this_ENV,
		    LOAD_IDX => 0 # First time this has been loaded.
		  };
				# Allocate our info structure.
  }

#
# Export all subroutines from the Makesubs package into the given package, so 
# the subroutines can be used directly.
#
  foreach my $makesub (keys %Makesubs::) {
    my $coderef = *{"Makesubs::$makesub"}{CODE}; # Is this a subroutine?
    $coderef and *{$mpackage . "::$makesub"} = $coderef;
  }
  *{$mpackage . "::rule"} = *Makesubs::rule;
				# Also pass in the $rule symbol.
  $ {$mpackage . "::MAKECMDGOALS"} = $makecmdgoals; # Set up the special
				# MAKECMDGOALS variable.

  $mdinfo->{MAKEINFO} = $self;	# Remember for later what the makefile is.

  %{$mpackage . "::scanners"} = %Makesubs::scanners;
				# Make a copy of the scanners array (so we can
				# modify it without affecting other makefiles).

  $ {$mpackage . "::makefile"} = $self;	# Tell the makefile subroutines
				# about it.

#
# We used to fork here, load the makefile once, rebuild the makefile if
# necessary, and then finally load the makefile in the parent process.  This
# avoids polluting the FileInfo hierarchy with old rules that don't exist in
# the up-to-date makefile.  It's a bit slow, however, and since we now allow
# makefiles to be reloaded and overwritten if the command line arguments or
# environment variables change, it seemed unnecessarily conservative to allow
# it to do it the old way.
#
  if ($minfo->{NAME} ne 'makepp_default_makefile.mk') {
    wait_for main::build($minfo); # Build the makefile, using what rules we
				# know from outside the makefile.  This may
				# also load it from a repository.
    delete $minfo->{BUILD_HANDLE}; # Get rid of the build handle, so we avoid
				# the error message that we built the file
				# before we saw the rule.
  }

  chdir $mdinfo;		# Get in the correct directory for wildcard
				# action routines.

#
# Read in the makefile:
#
  if ($this_ENV{'MAKEFILES'}) {	# Supposed to pre-load some files?
    foreach (split(' ', $this_ENV{'MAKEFILES'})) {
      my $finfo = file_info($_, $mdinfo);
      eval {$self->read_makefile($finfo); };
      if ($@) {
	main::print_error("warning: error reading ", $finfo->name, " (listed in \$MAKEFILES):\n$@");
      }	
    }
  }
  $self->read_makefile($minfo); # Read this makefile again.
  $self->read_makefile(file_info("$main::datadir/makepp_builtin_rules.mk"))
    unless $ {$mpackage . "::makepp_no_builtin"} ||
      !$main::builtin_rules;

#
# Build up the MAKEFLAGS variable:
#
  if ($main::traditional_recursive_make) {
    my @words =			# Pass commnd line variables down.
      map { "$_=" . requote($command_line_vars->{$_}) } keys %$command_line_vars;
    $main::keep_going and
      push @words, '-k';
    $main::sigmethod_name and
      push @words, "-m $main::sigmethod_name";
    $main::implicitly_load_makefiles or
      push @words, "--noimplicit-load";
    $main::log_level or
      push @words, "--nolog";
    $main::rc_substitution or
      push @words, "--norc-substitution";
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
# environment when we have to execute a rule.  When the export statement was
# seen, we put the names of the variables into a hash with a null value;
# now replace that null value with the actual value.
#
  if ($self->{EXPORTS}) {	# Are there any?
    foreach (keys %{$self->{EXPORTS}}) {
      $self->{EXPORTS}{$_} = $self->expand_text("\$($_)", $minfo->name);
    }	
  }

#
# Now see if the makefile is up to date.  If it's not, we just wipe it out
# and reload.  This may leave some bogus rules lying around.  Oh well.
# This must be done after setting up the EXPORTS variables above, because
# makefile rebuilding might depend on that.
#
  if ($main::remake_makefiles && # This often causes problems, so we provide
				# a way of turning it off.
      $minfo->{NAME} ne 'makepp_default_makefile.mk') {
    my $old_n_files = $main::n_files_changed;
    {
      local $main::default_signature_method = $Signature::target_newer::target_newer;
				# Use the target_newer technique for rebuilding
				# makefiles, since makefiles are often modified
				# by programs like configure which aren't
				# under the control of make.
      wait_for main::build($minfo) and # Try to rebuild the makefile.
	die "can't find or build " . $minfo->absolute_filename . "\n";
    }
    if ($old_n_files != $main::n_files_changed) {
				# Did we change anything?
      $self->{ENVIRONMENT} = { I_rebuilt_it => "FORCE RELOAD"};
				# Wipe out the environment, so we force a
				# reload.
      local $main::remake_makefiles = 0; # Don't try to keep on remaking the
				# makefile.
      return load(@_);		# Call ourselves with the same arguments to
				# force rereading the makefile.
    }
  }

#  print "Finished loading ", $minfo->name, "\n"
#    unless $main::quiet_flag;

  return $self;
}

#
# Parse a potential assignment statement.  Arguments:
# a) The makefile.
# b) The text of the assignment up to but not including the '='.  If it was
#    a += assignment, this text will end with a '+', and similarly for
#    := and ?= and !=.
# c) The text of the assignment after the =.
# d) The makefile line number (for error messages).
#
# Returns true if this is actually an assignment, false otherwise.
#
sub parse_assignment {
  my ($self, $var_name, $var_value, $makefile_line) = @_;
				# Name the arguments.
  my $assignment_type = '';	# Assume it's just an ordinary =.
  $var_name =~ s/([\+\:\?\!])$// and $assignment_type = $1;
				# Pull off the character before the equals
				# sign if it's part of the assignment token.
  $var_name = $self->expand_text($var_name, $makefile_line);
				# Make sure we can handle indirect assignments 
				# like x$(var) = value.
  $var_name =~ s/^\s+//;	# Strip leading whitespace.
  $var_name =~ s/\s+$//;	# Strip trailing whitespace.
  $var_value =~ s/^\s+//;	# Strip out leading whitespace.
  $var_value =~ s/\s+$//;	# Strip out trailing whitespace.


  if ($var_name =~ /:/) {       # If there's a : on the LHS, it's probably a
                                # target-specific variable assignment.
#
# It's a target-specific assignment, like this:
#   target1 target2: VAR = val
# or
#   target1 target2: VAR := val
# or
#   target1 target2: VAR += val
#
    my ($targets, @extra_junk);
    ($targets, $var_name, @extra_junk) = split_on_colon($var_name);
                                # Get the targets for which this variable
                                # applies.
    @extra_junk and return undef; # Not a valid target-specific assignment.
    $var_name =~ s/^\s+//;	# Strip leading whitespace (again).
    $var_name =~ s/\s+$//;	# Strip trailing whitespace.

    my $reexpand = 1;           # Assume it will be a regular assignment.
    if ($assignment_type eq ':') {
      $reexpand = 0;            # := assignment.
    }
    elsif ($assignment_type eq '+') { # Append?
      $reexpand = $self->{VAR_REEXPAND}{$var_name}; # Keep same type as before.
    }
    $reexpand or $var_value = $self->expand_text($var_value, $makefile_line);
                                # Expand immediately if we're supposed to.

    $targets =~ s/\%/*/g;       # Convert % wildcard to normal filename wildcard.
    Glob::wildcard_action map(unquote($_), split_on_whitespace($targets)),
    sub {                       # This subroutine is called for every file
                                # that matches the wildcard.
      my $tinfo = $_[0];
      if ($assignment_type eq '+') { # Append?
        my $old_val = $tinfo->{TARGET_SPECIFIC_VARS}{$var_name};
                                # Append to previous target-specific value,
                                # if there is one.
        defined($old_val) or $old_val = $ {$self->{PACKAGE} . "::$var_name"};
        $tinfo->{TARGET_SPECIFIC_VARS}{$var_name} = "$old_val $var_value";
      }
      else {
        $tinfo->{TARGET_SPECIFIC_VARS}{$var_name} = $var_value;
      }

      $reexpand and
	$tinfo->{TARGET_SPECIFIC_REEXPAND}{$var_name} = 1;
      
    };
  }
  else {
#
# Not a target-specific assignment:
#
    $var_name =~ /[\s:\#]/ and return undef; # More than one word on the LHS
				# implies it's not an assignment.

    $var_name eq "MAKE" && $main::warn_level and
      main::print_error("warning: MAKE redefined at $makefile_line, recursive make won't work as expected");

    my $mpackage = $self->{PACKAGE}; # Get the package name conveniently.

    if ($assignment_type eq '+') { # Append?
       $ {$mpackage . "::$var_name"} .= ' ' . 
	(exists($self->{VAR_REEXPAND}{$var_name}) ? $var_value : # Was it a regular =?
	 $self->expand_text($var_value, $makefile_line));
				# Expand the RHS if it was set with := 
				# previously.
    }
    elsif ($assignment_type eq ':') { # Immediate evaluation?
      $ {$mpackage . "::$var_name"} = $self->expand_text($var_value, $makefile_line);
      delete $self->{VAR_REEXPAND}{$var_name}; # Don't expand this text again.
    }
    elsif ($assignment_type eq '!') { # Run through shell to evaluate?
      $ {$mpackage . "::$var_name"} = Makesubs::f_shell($self->expand_text($var_value, $makefile_line), $self, $makefile_line);
    } elsif ($assignment_type eq '?') { # Assign only if not defined?
      if (!defined($ {$mpackage . "::$var_name"}) &&
	  !defined($self->{COMMAND_LINE_VARS}{$var_name}) &&
	  !defined($self->{ENVIRONMENT}{$var_name})) {
	$ {$mpackage . "::$var_name"} = $var_value;
	$self->{VAR_REEXPAND}{$var_name} = 1; # Reexpand when invoked.
      }
    } else {			# Ordinary, vanilla assignment?
      $ {$mpackage . "::$var_name"} = $var_value;
      $self->{VAR_REEXPAND}{$var_name} = 1; # Remember to expand this 
				# variable's contents when it's invoked.
    }
  }

  return $self;			# Return a true value.
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

  local $main::implicitly_load_makefiles = ($self->{RECURSIVE_MAKE} ? 0 :
					    $main::implicitly_load_makefiles);
				# Turn off implicit makefile loading if there
				# is an invocation of recursive make in this
				# file.  (This is not passed to the wildcard
				# action routine.)

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
	 $last_line_was_blank) ||
	($whitespace_len >= 8 &&
	 $whitespace_len <= $target_whitespace_len)) {
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
      my $foreach_val = $1;        # Make a copy of $1.  $1 gets wiped out and
                                # so it isn't valid to pass it to
                                # expand_text.
      $foreach = $self->expand_text($foreach_val, $makefile_line);
      pop @after_colon;
    }
    elsif ($after_colon[-1] =~ /^\s*signature\s+(\w+)/) { # Specify signature class?
      $signature and die "$makefile_line: multiple :signature clauses\n";
      my $signature_val = $1;
      $signature = $self->expand_text($signature_val, $makefile_line);
      defined($ {"Signature::${signature}::$signature"}) or
	die "$makefile_line: invalid signature class $signature\n";
      $signature = $ {"Signature::${signature}::$signature"};
      pop @after_colon;
    }
    elsif ($after_colon[-1] =~ /^\s*scanner\s+(.*)$/) { # Specify scanner class?
      $scanner and die "$makefile_line: multiple :scanner clauses\n";
      my $scanner_val = $1;
      $scanner = $self->expand_text($scanner_val, $makefile_line);
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
      die "$makefile_line: target has % wildcard but no % dependencies.
  This is currently not supported.\n";

    unless ($foreach) {		# No foreach explicitly specified?
      $foreach = $deps[0];	# Add one, making wildcard from first dep.
      if ($main::percent_subdirs ||
	  $ {$self->{PACKAGE} . "::percent_subdirs"}) { # % searches subdirs?
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
      local $main::implicitly_load_makefiles = ($self->{RECURSIVE_MAKE} ? 0 :
						$main::implicitly_load_makefiles);
				# Turn off implicit makefile loading if there
				# is an invocation of recursive make in this
				# file.  (This is not passed to the wildcard

      my $pattern_level = $was_wildcard_flag ?
	($finfo->{PATTERN_LEVEL} || 0) + 1 : 0;
				# Count the number of successive pattern rules
				# applied to produce this file.
      return if $pattern_level > 3; # Don't keep on applying pattern rules
				# to their own output.

      my $rule = new Rule($target_string, $after_colon[0], $action, $self, $makefile_line);
				# Make the rule.
      local $Makesubs::rule = $rule; # Put it so $(FOREACH) can properly expand.
      $self->{DEFAULT_SIGNATURE_METHOD} and
	$rule->set_signature_method_default($self->{DEFAULT_SIGNATURE_METHOD});
				# Get the signature method from the signature
				# statement.
      $signature and $rule->set_signature_method($signature);
				# Override that with the method from the
				# :signature clause, if any.
      $scanner and $rule->{ACTION_SCANNER} = $scanner;
      $rule->{FOREACH} = $finfo; # Remember what to expand $(FOREACH) as.
      $rule->{PATTERN_LEVEL} = $pattern_level if $was_wildcard_flag;
				# Mark it as a pattern rule if it was actually
				# done with a wildcard.

      my @targets = split_on_whitespace($self->expand_text($target_string, $makefile_line));
				# Get the targets for this rule.

      foreach (@targets) {
	my $tinfo = main::find_makepp_info(unquote($_), $self->{CWD}); # Access the target object.
	$tinfo->{PATTERN_LEVEL} = $pattern_level if $was_wildcard_flag;
				# Remember the pattern level, so we can prevent
				# infinite loops on patterns.  This must be
				# set before we call set_rule or we'll get
				# infinite recursion.
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
      
	$self->{DEFAULT_SIGNATURE_METHOD} and
	  $rule->set_signature_method_default($self->{DEFAULT_SIGNATURE_METHOD});
				# Get the signature method from the signature
				# statement.
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
    else {
#
# We're just adding a dependency to this target, like this:
#   target : additional-dependency
#
      if (@targets == 1) {
	if ($targets[0] =~ /^\s*\.PHONY\s*$/) {
				# Mark other targets as phony?
          my @dependencies = map(main::find_makepp_info(unquote($_), $self->{CWD}),
                                 split_on_whitespace($self->expand_text($after_colon[0], $makefile_line)));

	  foreach (@dependencies) {
	    $_->{IS_PHONY} = 1; # Mark as phony.
	  }	
	  return;
	}	
	if ($targets[0] =~ /^\s*\.SUFFIXES\s*$/) {
				# Control the default rules?
	  if ($after_colon[0] !~ /\S/) { # Turn off all suffixes?
	    $ {$self->{PACKAGE} . "::makepp_no_builtin"} = 1;
				# Suppress loading of all builtin rules.
	  }
	  return;
	}
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

  local $Makefile::makefile_contents;
  {
    local $/ = undef;		# Read in the whole file with one slurp.
    local *MAKEFILE_FH;		# Make a local file handle.
    open(MAKEFILE_FH, $Makefile::makefile_name) || 
      die "can't read makefile $Makefile::makefile_name--$!\n";
    $Makefile::makefile_contents = <MAKEFILE_FH>; # Read the whole makefile.
    $Makefile::makefile_contents =~ s/\r//g;
				# Strip out those annoying CR characters
				# which get put in sometimes on windows.
    close MAKEFILE_FH;		# Done with the makefile.
  }

  $Makefile::makefile_contents =~ /\$[\(\{]MAKE[\}\)]/ and
    $self->{RECURSIVE_MAKE} = 1;
				# If there's a recursive invocation of make,
				# remember this so we can turn off implicit
				# makefile loading.  We have to know this
				# before we process any rules or anything
				# else from the makefile.

  local $Makefile::makefile_lineno = 0;	# We're on the first line.

  local $Makefile::hold_line;	# Nothing in the hold area yet.

  local $Makefile::last_conditional_start;
				# Don't mess up error messages from parent
				# makefile.
  my $mpackage = $self->{PACKAGE}; # Access the package.

 makefile_line:
  while (defined($_ = read_makefile_line_stripped())) { # Read a line at a time.
    next if /^\s*$/;		# Skip blank lines.

    my $makefile_line = $Makefile::makefile_name . ":$Makefile::makefile_lineno";
				# The line name to use for error messages.

    my $equals = index_ignoring_quotes($_, '=');
				# Search for the equals of an assignment.
				# We use index_ignoring_quotes to skip over
				# equals signs that happen to be in quotes or
				# inside other make expressions.
    if ($equals >= 0) {
      parse_assignment($self, substr($_, 0, $equals),
		       substr($_, $equals+1), $makefile_line)
	and next;		# If it's a real assignment, then we're done.
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
      $rtn =~ s/\./_dot_/g;
      if (defined(*{$self->{PACKAGE} . "::s_$rtn"}{CODE})) { # Function from makefile?
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

  ++$Makefile::makefile_lineno;	# Keep the line counter accurate.
  length($Makefile::makefile_contents) == 0 and return undef;
				# End of file.
  $Makefile::makefile_contents =~ s/^(.*\n?)//;
				# Strip off the next line.  (Using pos() and
				# /\G/gc doesn't work, apparently because the
				# position gets lost when local() is executed.)
  return $1;			# Return the next line.
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

  defined($line) or return undef; # No point checking at end of file.

#
# Handle GNU make's conditionals:
#

  if ($line =~ s/^\s*if(eq|neq|def|ndef)\b//) {
				# Looks like an if statement?
    $Makefile::last_conditional_start = $Makefile::makefile_lineno;
				# Remember what line this was on so we can
				# give better error messages.
    my $cond = $1;		# Remember what the condition was.
    $line = $Makefile::makefile->expand_text($line, $Makefile::makefile_name . ":$Makefile::makefile_lineno");
				# Expand away all the variables.
    $line =~ s/^\s+//;		# Strip leading whitespace.
    $line =~ s/\s+$//;		# Strip trailing whitespace.

    my $truthval;
    if ($cond =~ /def$/) {	# See whether something is defined?
      my $var = $Makefile::makefile->{COMMAND_LINE_VARS}{$line} ||
        $ {$Makefile::makefile->{PACKAGE} . "::$line"} ||
          $Makefile::makefile->{ENVIRONMENT}{$line};
				# See if it was defined on the command line,
				# in the makefile, or in the environment.
      $truthval = defined($var) && $var ne '';
                                # GNU make regards variables set equal to the
                                # null string as undefined.
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
    my $line = &read_makefile_line; # Read another line.
    !defined($line) and
      die "$Makefile::makefile_name:$Makefile::last_conditional_start: end of makefile inside conditional\n";
    while ($line =~ s/\\\s*$/ /) {
      my $nextline = &read_makefile_line; # Handle continuations, because
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
