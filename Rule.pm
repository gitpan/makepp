# $Id: Rule.pm,v 1.2 2003/06/19 02:57:47 grholt Exp $
use strict qw(vars subs);

package Rule;

use MakeEvent qw(when_done wait_for);
use FileInfo;
use FileInfo_makepp;
use TextSubs;
use Signature::exact_match;

=head1  NAME

Rule -- Stores information about a build rule

=head1 USAGE

  my $rule = new Rule "targets", "dependencies",
    "command"[, $makefile, $makefile_line];

=head1 DESCRIPTION

A rule object contains all the information necessary to make a specific file.

$makefile is a pointer to the structure returned by load_makefile.
$makefile_line is an ASCII string that is used if an error message occurs
while expanding variables in this rule.

This does not actually place the rule into the FileInfo hierarchy.

=cut
sub new {
  my ($class, $targets, $dependencies, $command, $makefile, $source_line) = @_;
				# Name the arguments.

  return bless { TARGET_STRING => $targets,
		 DEPENDENCY_STRING => $dependencies,
		 COMMAND_STRING => $command,
		 MAKEFILE => $makefile,
		 LOAD_IDX => $makefile->{LOAD_IDX},
		 RULE_SOURCE => $source_line }, $class;
				# Make the rule object.
}

#
# Return the directory that this rule should be executed in.
#
sub build_cwd {
  return $_[0]->{MAKEFILE}{CWD};
}

#
# Return the makefile that made this rule:
#
sub makefile {
  return $_[0]->{MAKEFILE};
}

#
# This subroutine is called to find all the targets and dependencies of the
# rule.  It does so by repeatedly expanding the dependency and target string,
# and repeatedly scanning the command, until there are no further changes.
#
# Usage:
#   ($all_targets, $all_dependencies, $action_string) =
#     $rule->find_all_targets_dependencies();
#
# Where all_targets is a reference to a list of target object info structs,
# all_dependencies is a reference to a list of dependency object info structs,
# and $action_string is the final build command after expanding all make
# variables.
#
sub find_all_targets_dependencies {
  my $self = $_[0];

  my $build_cwd = $self->build_cwd; # Access the default directory.

  my %all_targets;		# A hash of all targets, so we can tell quickly
				# whether we already know about a given target.
  local $Makesubs::rule = $self; # Set this up so that subroutines can find the
				# rule that is currently being expanded.

  local $self->{ALL_TARGETS} = \%all_targets;
				# Store this so it can be found easily but
				# also goes away automatically.

  my %all_dependencies;		# A hash of all dependencies, so we can tell
				# whether we already know about a given
				# dependency.
  local $self->{ALL_DEPENDENCIES} = \%all_dependencies;

  my @explicit_dependencies;	# The ones that were listed in the dependency
				# list or explicitly somewhere else in the
				# makefile.
  local $self->{EXPLICIT_DEPENDENCIES} = \@explicit_dependencies;

  my @explicit_targets;		# The ones that were listed and not inferred
				# from the command.

  local $self->{EXPLICIT_TARGETS} = \@explicit_targets;

  my @extra_dependencies;
  local $self->{EXTRA_DEPENDENCIES} = \@extra_dependencies;

  my $makefile = $self->makefile;

#
# Get the full list of explicit targets:
#
  my $target_string =
    $makefile->expand_text($self->{TARGET_STRING}, $self->{RULE_SOURCE});


  foreach (split_on_whitespace($target_string)) {
    my $tinfo = main::find_makepp_info(unquote($_), $build_cwd);
    push @explicit_targets, $tinfo;
    $self->add_target($tinfo);
  }

#
# Get the full list of explicit dependencies:
#
  my $dependency_string =
    $makefile->expand_text($self->{DEPENDENCY_STRING}, $self->{RULE_SOURCE});
				# Get the list of files.

  foreach (split_on_whitespace($dependency_string)) {
    my $fname = unquote($_);	# Handle quoting.
    if (/[\[\*\?]/) {		# Is it a wildcard?
      push @explicit_dependencies, Glob::zglob_fileinfo($fname, $build_cwd);
    } else {
      push @explicit_dependencies, main::find_makepp_info($fname, $build_cwd);
    }
  }

  push @explicit_dependencies, @extra_dependencies;
				# Extra dependencies go at the end of the
				# list.

  foreach (@explicit_dependencies) {
    $self->add_dependency($_);	# Make sure we know about each dependency.
  }

#
# Now expand the command string.  This must be done last because it
# can depend on both the explicit targets and the explicit dependencies.
#
  my $command_string;
  if (@explicit_targets && $explicit_targets[0]->{TARGET_SPECIFIC_VARS})
  {
    local $Makefile::target_specific = $explicit_targets[0]->{TARGET_SPECIFIC_VARS};
                                # Temporarily set up target-specific variables,
                                # if there actually are any.
    local $Makefile::target_specific_reexpand = $explicit_targets[0]->{TARGET_SPECIFIC_REEXPAND};
                                # Remember the difference between = and :=
                                # variables for target-specific vars too.
    $command_string = $makefile->expand_text($self->{COMMAND_STRING},
                                             $self->{RULE_SOURCE});
				# Get the text of the command.
  }                             # Turn off the target-specific variables.
  else {
    $command_string = $makefile->expand_text($self->{COMMAND_STRING},
                                             $self->{RULE_SOURCE});
				# Get the text of the command.
  }

  $command_string =~ s/^\s+//;  # Strip out leading and trailing whitespace
  $command_string =~ s/\s+$//;  # so we don't trigger unnecessary rebuilds
                                # quite as often.

  $self->scan_action($command_string);
				# Look for any additional dependencies (or
				# targets) that we didn't know about.

#
# For some reason, the #@!$@#% linux kernel makefiles have a file that
# depends on itself.  This looks like a simple mistake, but I want this
# to work properly on the linux kernel, so we explicitly remove dependencies
# that are equal to the target.
#
  my @bogus_dependencies;
  foreach (values %all_targets) {
    if ($all_dependencies{$_}) {
      delete $all_dependencies{$_}; # Remove it from the dependency list.
      my $warn_flag = 0;
      for (my $idx = 0; $idx < @explicit_dependencies; ++$idx) {
	if ($explicit_dependencies[$idx] == $_) { # Was it an explicit dependency?
	  splice(@explicit_dependencies, $idx, 1); # Remove it.
	  $warn_flag++ or
	    main::print_error("warning: " . $_->name . " depends on itself; circular dependency removed");
	}
      }
      
    }
  }

#
# Make a list of the targets and the dependencies, first listing the explicitly
# specified ones, and then listing the implicit ones later.  It's confusing
# (and breaks some makefiles) if dependencies are built in a different order
# than they are specified.
#
  foreach (@explicit_targets) { delete $all_targets{$_}; }
  foreach (@explicit_dependencies) { delete $all_dependencies{$_}; }

  return ([ @explicit_targets, values %all_targets ],
	  [ @explicit_dependencies, values %all_dependencies ],
	  $command_string);
}

=head2 $rule->execute($command_string)

   $handle = $rule->execute($command_string);

Executes the given command string, and returns a handle for the executing
process (which may not have finished by the time we return).  The command
string may contain several shell commands separated by newlines.

This is part of the Rule class so the default execute routine can be 
overridden.  You can replace the execute routine with any complicated
operation you want, provided it returns the appropriate kind of handles.

=cut

my $tmp_seed = '00000';		# A unique string to append to the temp file
				# name so it's different every time we use it.

sub execute {
  my ($self, $actions) = @_;	# Name the arguments.

  if ($actions =~ /\brecursive_makepp\b/) { # Do we need to listen for
    main::setup_recursive_make_socket(); # recursive make?
  }

  my $build_cwd = $self->build_cwd;
#
# If we're not building in parallel, then we don't change the standard output
# of the commands.  In this case, if there's a directory change, we print
# it out before executing the commands.
#
  if (!$main::parallel_make) {
    print_build_cwd($build_cwd); # Make sure we notify user if we change
				# directories.
    return wait_for new MakeEvent::Process sub {
      execute_command($build_cwd, $actions,
		      $self->{MAKEFILE}{EXPORTS}, $self->{MAKEFILE}{ENVIRONMENT}); # Execute the command.
    };
				# Wait for the process, because we don't want
				# to get ahead and start scanning files we
				# can't build yet.  That could mix up output
				# between make and the other process.
  }
  
#
# We're executing in parallel.  We can't print the directory name initially,
# and we have to redirect STDOUT, because we don't want to mix output from
# different commands.
#
# At present, we direct the output to a file, and then print it when we're
# done.
#
  else {
    my $tmpfile = "/tmp/makepp.$$." . $tmp_seed++;
				# Make the name of a temporary file.
    my $proc_handle = new MakeEvent::Process sub {
#
# Child process.  Redirect our output to the temporary file and then start
# the build.
#
#      close STDIN;
# (Turns out we don't want to close STDIN, because then duping STDOUT causes
# fd 0 to be opened, rather than 2, so STDERR is actually file handle 0.  This
# caused a weird bug where error messages were not displayed on parallel
# builds.)
      close STDOUT;
      close STDERR;
      open(STDOUT, "> $tmpfile") ||
	die "$main::progname: can't redirect standard output to $tmpfile--$!\n";
      open(STDERR, ">&STDOUT") || die "can't dup STDOUT\n";
				# Make stderr go to the same place.
      execute_command($build_cwd, $actions,
		      $self->{MAKEFILE}{EXPORTS}, $self->{MAKEFILE}{ENVIRONMENT});
				# Execute the action(s).
    };

    when_done $proc_handle, sub {
#
# Gets executed in the parent process when the child has finished.
#
      local *JOB_OUTPUT;	# Make a local file handle.
      if (open(JOB_OUTPUT, "$tmpfile")) { # Is there anything?
	print_build_cwd($build_cwd); # Display any directory change.
	print <JOB_OUTPUT>;	# Dump it all to STDOUT.
	close JOB_OUTPUT;
	unlink "$tmpfile"; # Get rid of it.
      }
      return 0;			# No error.
    }, ERROR => sub {
#
# Process exited with an error.  Again, print out the program's output, but
# then exit with non-zero status.
#
      local *JOB_OUTPUT;	# Make a local file handle.
      if (open(JOB_OUTPUT, "$tmpfile")) { # Is there anything?
	print_build_cwd($build_cwd); # Display any directory change.
	print <JOB_OUTPUT>;	# Dump it all to STDOUT.
	close JOB_OUTPUT;
	unlink "$tmpfile"; # Get rid of it.
      }
      $main::error_found = $_[0] # Remember the error status.  This will
	unless $main::keep_going; # cause us to stop compilation as soon as
				# possible.
      return $_[0];		# Propagate the error status.
    };
  }
}

#
# The following internal subroutine executes each action.  Arguments:
# a) The directory that we're supposed to execute this from.
# b) The lines of actions to execute.
# c) A hash of variables that must be exported to the environment, and their
#    values.
# d) The environment to use (not including the exports).
#
# At this point, STDOUT and STDERR should be set up to wherever the command
# is supposed to output to.
#
sub execute_command {
  my ($build_cwd, $actions, $exports, $environment) = @_; # Name the arguments.

  chdir $build_cwd;		# Move to the correct directory.
  %ENV = %$environment;		# Set the environment.
#
# Handle any exported variables. 
#
  if ($exports) {		# Any exported variables?
    my ($var, $val);
    while (($var, $val) = each %$exports) {
      $ENV{$var} = $val;
    }	
  }	
  $main::recursive_make_socket_name and	# Pass info about recursive make.
    $ENV{'MAKEPP_SOCKET'} = $main::recursive_make_socket_name;

#
# Now execute each action.  We exec the action if it is the last in the series
# and we don't need to ignore the return code; otherwise, we call system()
# instead.  This means we leave an extra process lying around in those cases,
# but it's too tricky to avoid it in such comparatively rare situations.
#
  my @actions = split(/\n/, $actions); # Get the commands to execute.
  
  for (my $action_idx = 0; $action_idx < @actions; ++$action_idx) {
    my $action = $actions[$action_idx];

#
# Parse the @ and - in front of the command.
#
    my $silent_flag = $main::silent_execution;
    my $error_abort = 1;
    $action =~ s/^\@// and $silent_flag = 1;
    $action =~ s/^-// and $error_abort = 0;
    $action =~ s/^\@// and $silent_flag = 1;

    $action =~ s/^noecho\s+// and $silent_flag = 1;
    $action =~ s/^ignore_error\s+// and $error_abort = 0;
    $action =~ s/^noecho\s+// and $silent_flag = 1;

    $| = 1;			# Flush immediately.  Otherwise, with perl
				# 5.005, the action is never printed for
				# some reason.
    print "$action\n" unless $silent_flag;
    
    if (!$main::can_fork) {	# Can't fork or exec on windows.
      system(TextSubs::format_exec_args($action));
      $? and return ($?/256) || 1; # Quit if an error.
      next;			# Process the next action.
    }

    if ($error_abort && $action_idx == @actions-1) { # Is this the last action?
      $SIG{__WARN__} = sub {};	# Suppress annyong warning message here if the
				# exec fails.
      exec(TextSubs::format_exec_args($action));
				# Then execute it and don't return.
				# This discards the extra make process (and
				# probably saves lots of memory).
      die "exec failed--$!\n"; # Should never get here.
    }
    else {			# We have more to do after the process
				# finishes, so we need to keep running
				# after the process starts.
#
# We used to do with with system(), but system has two serious drawbacks:
# 1) A nasty message is printed if the first word is not a valid shell
#    command, and the perl process aborts, apparently without setting the
#    correct exit code.
# 2) On linux, system() blocks signals, which means that makepp wouldn't
#    respond to ^C or other things like that.  (See the man page.)
#
      my $pid = fork();
      unless ($pid) {		# Executed in child process:
	$SIG{__WARN__} = sub {}; # Suppress annoying warning message here if 
				# the exec fails.
	exec(TextSubs::format_exec_args($action));
	die "exec failed--$!\n"; # Should never get here.
      }

      $pid == -1 and die "fork failed--$!\n";
      wait;			# Wait for it to finish.
      $error_abort && $? and exit(($?/256) || 1);
				# Quit on an error.  $status/256 is the exit
				# status of the process, but if the system()
				# call failed for some reason, that may be 0.
    }	
  }

  $main::can_fork or return 0;	# If we didn't fork, we'll always get here.

  exit 0;			# If we get here, it means that the last
				# command in the series was executed with
				# ignore_error.
}

#
# This subroutine prints out a message if we've changed directories.  This
# message is important for programs like emacs that look through the compiler
# output--it helps them find the directory that the file containing the errors
# is located in.
#
sub print_build_cwd {
  my $build_cwd = $_[0];
  if (!$Rule::last_build_cwd ||	# No previous cwd?
      $Rule::last_build_cwd != $build_cwd) { # Different from previous?
    print "$main::progname: Leaving directory `" . $Rule::last_build_cwd->absolute_filename . "'\n"
      if $Rule::last_build_cwd;	# Don't let the directory stack fill up.
    print "$main::progname: Entering directory `" . $build_cwd->absolute_filename . "'\n";
    $Rule::last_build_cwd = $build_cwd;
  }
}


=head2 $rule->signature_method

  my $sig_method = $rule->signature_method;

Returns the signature method to use to determine whether this rule needs to
be applied.

The default method is exact_match (see Signature::exact_match for details).
However, if the name of the target is Makefile or Makefile.in, then the
default method is target_newer.  This is to allow legacy makefiles to work.

=cut

sub signature_method {
  my $self = $_[0];

  $self->{SIGNATURE_METHOD} and return $self->{SIGNATURE_METHOD};

  return $main::default_signature_method;
				# Use the default method.
}

sub set_signature_method {
  $_[0]->{SIGNATURE_METHOD} = $_[1];
}

#
# This subroutine sets the signature method if one hasn't already been
# specified.
#
sub set_signature_method_default {
  $_[0]->{SIGNATURE_METHOD} ||= $_[1];
}

=head2 $rule->scan_action

  $rule->scan_action($action_string);

Scans the given actions, looking for additional dependencies (such as include
files) or targets (such as .libs/xyz.lo) that weren't explicitly listed.  Make
variables in the action string have already been expanded.

Calls $rule->add_dependency for new dependencies, and $rule->add_target for
new targets.

=cut
my %scanner_warnings;

sub scan_action {
  my ($self, $action) = @_;	# Name the arguments.

  my $scanner = $self->{ACTION_SCANNER}; # Was one explicitly set?

#
# Try to find a scanner based on the first word of the command:
#
  unless ($scanner) {
#
# First strip off any leading environment variable assignments.
#
    $action =~ s/^[-\@\s]+//;	# Strip out the noecho modifier and any
				# leading whitespace.
				# (The "noecho" and "ignore_errors" modifiers
				# are handled by the scanner_skip_word routine.)
    while ($action =~ s/^\s*\w+=//) { # Is there an environment variable assignment?
      my $ix = index_ignoring_quotes($action, " ");
				# Look for the next whitespace.
      $ix >= 0 or $ix = index_ignoring_quotes($action, "\t");
				# Oops, it must be a tab.
      $ix >= 0 or last;		# Can't find the end--something's wrong.

      $action = substr($action, $ix+1);	# Chop off the environment variable.
    }

    $action =~ /^\s*(\S+)\s*/; # Get and store the first word.
    my $firstword = $1 || '';

    my $scanner_hash = \%{$self->{MAKEFILE}{PACKAGE} . "::scanners"};
    $scanner = $scanner_hash->{$firstword};
				# First try it unmodified.
    unless ($scanner) {		# If that fails, strip out the
				# directory path and try again.
      $firstword =~ s@^.*/@@ and # Is there a directory path?
	$scanner ||= $scanner_hash->{$firstword};
    }
  }

  if ($scanner) {		# Did we get one?
    &$scanner($action, $self);
				# Call the routine.
  }
  else {			# No scanner:
#
# We want to try to avoid the case where because of some quirk in the command,
# we failed to find a scanner.  Scanners are especially important for 
# C/C++ compilation.  See if this rule looks like it's some sort of a 
# compile:
#
    my $tinfo = $self->{EXPLICIT_TARGETS}[0];
    if (ref($tinfo) eq 'FileInfo' && # Target is a file?
	$tinfo->{NAME} =~ /\.l?o$/) { # And it's an object file?
      my $deps = $self->{EXPLICIT_DEPENDENCIES};
      if (@$deps &&		# There is a dependency?
	  ref($deps->[0]) eq 'FileInfo' && # It's a file?
	  $deps->[0]{NAME} =~ /\.(?:c|cxx|c\+\+|cc|cpp)$/i) {
				# Looks like C source code?
	if ($main::warn_level) {
	  main::print_error("warning: action scanner not found for rule at ",
			    $self->source, "\nalthough it seems to be a compilation command.
This means that makepp will not know about any include files.
To specify a scanner (and to get rid of this annoying message), add a
:scanner modifier line to the rule actions, like this:
    : scanner c_compilation	# Scans for include files.
or
    : scanner none		# Does not scan, but suppresses warning.
")
	    unless $scanner_warnings{$self->source}++;
	}
      }
    }
  }
}


=head2 $rule->source

  $source_string = $rule->source;

Returns a description of where this rule was encounteredq, suitable for error
messages.

=cut
sub source {
  return $_[0]->{RULE_SOURCE};
}

#
# This subroutine is called to add a possibly new dependency to the list of
# all dependencies for the current rule.
#
# Arguments:
#	$rule->add_dependency($object_info);
#
# This should only be called from subroutines which are called by
# find_all_targets_dependencies.  This basically means it should only be
# called by the command scanners.
#
sub add_dependency {
  $_[0]->{ALL_DEPENDENCIES}{$_[1]} ||= $_[1];
				# Store it if we didn't already know about it.
}

#
# This subroutine is called to add a possibly new target to the list of targets
# for the current rule.
#
# Arguments:
#     $rule->add_target($object_info);
#
# This should only be called from subroutines which are called by
# find_all_targets_dependencies.  This basically means it should only be
# called by the command scanners.
#
sub add_target {
  my ($self, $oinfo) = @_;

  return if $self->{ALL_TARGETS}{$oinfo}; # Quit if we already knew about this.

#
# This target may have dependencies which were explicitly listed in some other
# part of the makefile, as used to be common for specifying include files.
# For example, we might have a rule like:
#
#   %.o: %.c
#     compilation commnad
#
#   xyz.o : abc.h def.h ghi.h
#
  if ($oinfo->{ADDITIONAL_DEPENDENCIES}) { # Any additional explicit dependencies?
    foreach (@{$oinfo->{ADDITIONAL_DEPENDENCIES}}) {
      my ($dep_str, $makefile, $makefile_line) = @$_;
				# Name the components of the stored info.
      foreach my $depname (split_on_whitespace($makefile->expand_text($dep_str, $makefile_line))) {
	my @dep_infos;
	if ($depname =~ /[\[\*\?]/) {		# Is it a wildcard?
	  @dep_infos = Glob::zglob_fileinfo(unquote($depname), $makefile->{CWD});
				# Get which files this is referring to.
	} else {
	  @dep_infos = (main::find_makepp_info(unquote($depname), $makefile->{CWD}));
	}

	foreach my $dep (@dep_infos) { $self->add_dependency($dep); }
				# Add them to the dependency list.
	push @{$self->{EXTRA_DEPENDENCIES}}, @dep_infos;
      }
    }
  }

  $self->{ALL_TARGETS}{$oinfo} = $oinfo;
}

###############################################################################
#
# This is a replacement for the Rule when there is no actual rule specified
# in the file.  Most of the subroutines in this package are dummies to
# replace the functionality in the Rule class.
#
package DefaultRule;

use TextSubs;

#
# Establish a default rule for a new file:
#
sub new {
  my ($class, $target) = @_;
				# Name the arguments.

  return bless { TARGET => $target }, $class;
				# Make the rule object.  Declare it local
				# so it's accessible to any subroutines that
				# Makefile::expand_text calls.
}

sub build_cwd {
  return $FileInfo::CWD_INFO;
}

sub find_all_targets_dependencies {
  my $self = shift @_;
  my $target = $self->{TARGET};	# Get the target object info.
  if ($target->{ADDITIONAL_DEPENDENCIES}) {
				# Are there dependencies for this file even
				# though there's no rule?
				# We have to expand the dependency list now.
    my @addl_deps;
    foreach (@{$target->{ADDITIONAL_DEPENDENCIES}}) {
      my ($dep_str, $makefile, $makefile_line) = @$_;
				# Name the components of the stored info.
      foreach my $depname (split_on_whitespace($makefile->expand_text($dep_str, $makefile_line))) {
	if ($depname =~ /[\[\*\?]/) {		# Is it a wildcard?
	  push(@addl_deps,
	       Glob::zglob_fileinfo(unquote($depname), $makefile->{CWD}));
				# Get which file(s) this is referring to.
	} else {
	  push @addl_deps, main::find_makepp_info(unquote($depname),
						  $makefile->{CWD});
	}
      }
    }
    
    return ([$target], \@addl_deps, "");
  }

  return ([ $target ], [], "");
}

#
# If we ever get to this subroutine, it means there's no way to build this
# file.
#
sub execute {
  my $self = $_[0];

  return MakeEvent::when_done sub { 
    my $target = $self->{TARGET};
    if ($target->{ADDITIONAL_DEPENDENCIES}) {
				# If it has additional dependencies, then
				# there was a rule, or at least we have to
				# pretend there was.
      $target->may_have_changed; # It's possible that some other command
				# made the target while we weren't looking.
				# Some makefiles are written that way.
      $target->file_exists and return 0;
				# If the file doesn't exist yet, we may
				# have to link it from a repository.
      if ($target->{ALTERNATE_VERSIONS}) {
	$target->move_or_link_target($target->{ALTERNATE_VERSIONS}[0], 0);
				# Link it in from the repository.
      }
      return 0;			# Return success even if the file doesn't exist.
				# This is to handle things like
				# FORCE:
				# which are really just dummy phony targets.
    }
    main::print_error("No rule to make ", $target->name);
    return -1;			# Return nonzero to indicate error.
  };
}

#
# The signature method that we use causese the file to be remade if it doesn't
# exist, and leaves it alone if it does.
#
sub signature_method {
  return $Signature::default_rule::default_rule;
}

sub source { return "default rule" };

package Signature::default_rule;
#
# This package implements signature checking for files for which there is no
# rule.  If there are no dependencies, then we "rebuild" only if the file
# does not exist.  If there are dependencies specified (even though there's no
# rule), then we say the file needs "rebuilding" if any of the signatures of
# the dependencies have changed.  ("Rebuilding" simply means invalidating
# the cached signature.)  This is necessary because of code
# like this which is actually pretty common in makefiles:
#
#  y.tab.c: y.tab.h
#  y.tab.h: parse.y
#      yacc -d $<
#
# This simply specifies that the yacc command produces both y.tab.c and
# y.tab.h in the same invocation.  (This is a lousy way to specify it, but
# it's the only way with the traditional make.)
#

@Signature::default_rule::ISA = qw(Signature::exact_match);
				# Inherit the check_move_or_link function.

$Signature::default_rule::default_rule = bless {}; # Make the singleton object.

sub signature {
  return $_[1]->signature;
}

sub build_check {
  shift @_;			# Get rid of dummy argument.
  my ($tinfo, $command_string, $cwd, $sorted_dep_str, $dep_sig_str) = @_;

  if ($sorted_dep_str eq "") {	# No dependencies?
    return $tinfo->signature ? undef : 1; # Return 1 if the target doesn't
				# exist (that means we need to build it,
				# which means fetching from a repository).
  }	

  my $last_dep_sigs = $tinfo->build_info_string("DEP_SIGS") || '';

  if ($last_dep_sigs ne $dep_sig_str) {
				# Any files changed, or maybe we have different
				# files now?
    if ($main::log_level) {	# Do we have to figure out what is different?
      my $last_dep_str = $tinfo->build_info_string("SORTED_DEPS") || '';
      my (%cur_deps, %last_deps);
      @cur_deps{split(/\01/, $sorted_dep_str)} = split(/\01/, $dep_sig_str);
      @last_deps{split(/\01/, $last_dep_str)} = split(/\01/, $last_dep_sigs);

      foreach (keys %cur_deps, keys %last_deps) {
	next if (($cur_deps{$_} || '') eq ($last_deps{$_} || ''));
	
	main::print_log("Rebuild ", $tinfo->name, " because $_ ",
			($cur_deps{$_} ? 
			 ($last_deps{$_} ? "changed" :
			  "was not previously a dependency") :
			 "is no longer a dependency"));
      }
    }
    return 1;
  }

  return undef;		# No build necessary.
  
}

1;
