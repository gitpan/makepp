# use strict qw(vars subs);

package MakeEvent;

require Exporter;
@MakeEvent::ISA = qw(Exporter);

@MakeEvent::EXPORT_OK = qw(wait_for when_done read_wait);

=head1 NAME

MakeEvent -- event loop for makepp

=head1 USAGE

  use MakeEvent;

  $handle = new MakeEvent::Process STDOUT => ">> /tmp/junk", STDIN => undef,
    "shell command to execute";
  $handle = new MakeEvent::Process sub { ... };

  $handle = when_done $handle1, $handle2, ..., sub {
  # Code that gets executed when the previous handles have finished.
  };

  $status = $handle->status;

  $status = wait_for $handle1, $handle2, ...;

  read_wait FILEHANDLE, sub { ... };
				# Called when there is data to be read on
				# the given file handle.

=head1 DESCRIPTION

MakeEvent provides a way of multi-threading perl code without actually using
any of the thread extensions.  It relies on perl closures instead.  So it's a
little harder to write event-driven code than it would be if your code were
threaded, but not much.

It also supports waiting for input availability on an arbitrary set of
channels using the IO::Select module.  Currently, it does not support waiting
to write or waiting for exceptions.

=cut

#
# Some global variables:
#
$MakeEvent::max_proc = 1;	# Set this to be the maximum number of
				# subprocesses to run simultaneously.
				# By default, we can only run one, which
				# means that when we try to start up a second
				# one, we wait for the first.
$MakeEvent::n_external_processes = 0; # Number of processes currently running.

$MakeEvent::fork_level = 0;	# How many times we've forked.

$MakeEvent::exit_on_error = 0;	# Exit as soon as possible if an error occured,
				# instead of propagating the error up the
				# chain.
#
# Some internal variables:
#
my %read_wait;			# A hash of subroutines to call when a file
				# handle is readable, indexed by the file
				# handle (or whatever you passed to 
				# read_wait()).

my $child_exited = 0;		# 1 if we've received a SIGCHLD.

my $read_vec;			# Vector of file handles that we are listening
				# to.
my @read_handles;		# The file handles or FileHandles or globs
				# that we're waiting for, indexed by the 
				# fileno (same as index into $read_vec).
my @read_subs;			# The read subroutines associated with each
				# of the handles in @read_handles (also 
				# indexed by fileno).

=head2 MakeEvent::event_loop

  &MakeEvent::event_loop;

This is the main event loop.  It waits for one event, processes it, and
returns.  You probably don't want to call this function directly; most likely,
you want MakeEvent::wait_until.

=cut

sub event_loop {
  unless ($child_exited) {	# Do not call select() if we already have stuff
				# to do.  That can cause a hang.
#
# Check for file handles which can be read.  We used to use IO::Select but
# it's buggy (doesn't even bother to call the select function if no
# handles have been specified--not a friendly interface!).
#
    my $r = $read_vec;		# Make a modifiable copy of the list of file
				# handles to wait for.
#    print "Event loop: waiting for " . join(" ", keys %MakeEvent::Process::running_processes) . "\n";
    my $n_handles = select($r, undef, undef, 5);
				# Supply a 1 s timeout, so we do not wait
				# forever if the signal happened to come
				# between when we tested select_finished_subs
				# and when we called select.
    if ($n_handles > 0) {	# Data available on any handles?
				# Scan backwards to find out which handles it
				# might have been on, since we are more likely
				# to be waiting on later file handles.
      for (my $fileno = @read_handles; $fileno >= 0; --$fileno) {
	if (vec($r, $fileno, 1)) { # This bit returned on?
	  my $read_sub = $read_subs[$fileno]; # Get the subroutine.
	  my $fh = $read_handles[$fileno];
	  vec($read_vec, $fileno, 1) = 0; # Do not wait for it again (unless it
	  $read_subs[$fileno] = undef; # is requeued).
	  $read_handles[$fileno] = undef;
	  defined $read_sub and &$read_sub($fh); # Call the subroutine.
	}
      }
    }
  }

#
# Check for other kinds of interruptions:
#
  if ($child_exited) {		# Need to wait() on child processes?
    $child_exited = 0;		# Reset the flag.
    &MakeEvent::Process::process_reaper;
  }
}

=head2 read_wait

  read_wait FILE_HANDLE, sub { ... };

Queue a subroutie to be activated whenever there is data on the given file
handle (or IO::Handle object, or anything that can be supplied as an argument
to IO::Select::new.

This is a one-shot queue.  You must call read_wait again if you want the
subroutine to be called again.

=cut

sub read_wait {
  my ($fh, $subr) = @_;		# Name the arguments.

  my $fileno = fileno($fh) || $fh->fileno;
  $read_vec ||= '';		# Avoid usage of undefined variable errors.
  defined $fileno or die "internal error";
  vec($read_vec, $fileno, 1) = 1; # Wait on this file handle.
  $read_handles[$fileno] = $fh;
  $read_subs[$fileno] = $subr;
}


#
# This is an internal subroutine which is called whenever a process exits,
# or whenever a subroutine that was waiting is called.  It handles any
# errors, and activates whoever was waiting for the subroutine.
#
# Arguments:
# a) The handle.
# b) The status.
#
sub process_finished {
  my ($handle, $status) = @_;	# Name the arguments.
  
#  print "*** Error $status\n";
#
# If there was an error and we're supposed to exit as soon as possible, don't
# even bother to activate waiters--just exit immediately.
#
  if ($status && $MakeEvent::exit_on_error) {
    main::print_error("error, stopping now"); # Print a suitable message.
#    if ($MakeEvent::n_external_processes > 0) {	# Did we run anything?
#      main::print_error("waiting for other jobs to complete");
#      while ($MakeEvent::n_external_processes > 0) {
#	my $pid = wait;		# Wait for all the other processes to finish,
#				# but don't start up any new ones.
#	next unless WIFEXITED($?); # Make sure it really has exited.
#	--$MakeEvent::n_external_processes;
#      }
#    }
    exit 1;			# Exit with error status.
  }

  $handle->{STATUS} ||= $status; # Store the new status value.  Note that we
				# use ||= rather than =.
				# It's possible that the status value could
				# have been set by a previous error in one of
				# the things we were waiting for.  (See the
				# code below for error status.)  In this
				# case, we want to set the status to error
				# if any of the things it was waiting for
				# had errors.

  if ($handle->{STATUS} && $handle->{ERROR_HANDLER}) {
				# Is there an error and an error handler in
				# this handle?
    $handle->{ARGS} = [ delete $handle->{STATUS} ]; # Pass the status code to 
				# the error handler, and get rid of it so
				# there's a place to put the return code from
				# the error handler.
    $handle->{CODE} = delete $handle->{ERROR_HANDLER};
				# Replace the subroutine with the error
				# handler instead.
    $handle->start;		# Start off the error handler.  (Note that
				# this will always call
				# MakeEvent::WaitingSubroutine::start, because
				# there is no way to set an error handler for
				# MakeEvent::Processes.)
    return;
  }	

  my $waiters = $handle->{WAITING_FOR};

  foreach (keys %$handle) {
    delete $handle->{$_} unless $_ eq 'STATUS';
				# Get rid of everything but the status, in
				# order to save memory.
  }
  return unless $waiters;	# Don't do anything if no one was waiting
				# for this.

  if ($handle->{STATUS}) {	# Was there some error?
#
# If there was an error, don't activate routines which were waiting;
# activate the error routines, if any.
#
    foreach my $waiter (@$waiters) {
      $waiter->{STATUS} ||= $handle->{STATUS}; # Remember the error.
      if (--$waiter->{WAIT_COUNT} == 0) { # Were we the last thing waiting?
	if (exists $waiter->{ERROR_HANDLER}) { # Activate the error handler
				# instead of the subroutine.
	  $waiter->{ARGS} = [ $waiter->{STATUS} ];
				# Set the status value.
	  delete $waiter->{STATUS}; # The error handler may return no error.

	  $waiter->{CODE} = $waiter->{ERROR_HANDLER};
	  $waiter->start;	# Invoke the error handler instead of the 
				# subroutine.
	}
	else {			# Just pass the error to this routine's caller,
	  process_finished($waiter, $handle->{STATUS}); # without invoking the routine.
	}
      }
    }
  }
  else {
#
# No error.  Activate whoever was waiting for this waiter.
#    
    foreach my $waiter (@$waiters) {
      --$waiter->{WAIT_COUNT} == 0 and # Last thing this one was waiting for?
	$waiter->start;		# Start it up.
    }
  }
}

=head2 when_done

  $handle = when_done $handle1, $handle2, ...,  sub { ... };
  $handle = when_done $handle1, $handle2, ..., sub { ... }, sub { ... };
  $handle = when_done $handle1, $handle2, ..., sub { ... }, ERROR => sub { ... };
  $handle = when_done [$handle1, $handle2], sub { ... };
  $handle = when_done $handle, [sub { ... }, sub { ... }];

Calls the specified subroutine when the processes have finished or the other
subroutines have been called.  The argument list is flattened, so you can
specify array references and when_done will insert the contents of the array
at that point in the argument list.

You can specify more than one subroutine to
call.  In this case, the subroutines are called in sequence.  The subroutines
may return any of the following values:

=over 4

=item 0

0 indicates success, as with unix processes.

=item a non-zero value

A non-zero value indicates failure, and causes the error handler of any 
subroutine waiting to be called.

=item a list of handles

Doesn't activate anyone waiting for this subroutine until each handle in
the list of handles has finished.

=item 'ERROR', then a subroutine reference

The subroutine is called if an error status was returned by any of the
handles.  On entry to the subroutine, $_[0] is the error status code.  The
subroutine should return a status code just like the usual when_done
subroutines.

=back

Each subroutine should return these values.  If you specify more than one
subroutine, and the first returns a list of handles, the second is not called
until all of those handles are done.

You can also specify an error handler, which is called if activity on any
of the given handles returns an error.  (It is not called if the subroutines
themselves return an error; the error handler of whoever is waiting on them
is called.)  The error handler should return the same type of status code as
the main routine.

Instead of specifying the handles, you may also specify status codes.  (This
means that instead of keeping the handle object around, you can just store the
status code when the handle has finished.)

=cut

sub when_done {
  my @handles;
  my @subrs;
  my $error_handler;
  my $status;			# True if we were passed an error status
				# code as one of the arguments.

#
# Parse the arguments:
#
  while (@_) {
    my $arg = shift @_;		# Get the next argument.
    next if !defined($arg);	# Skip undef values--undef means success with
				# no waiting.
    if (ref($arg) eq 'MakeEvent::WaitingSubroutine' ||
	ref($arg) eq 'MakeEvent::Process') { # Is this a handle?
      if (exists $arg->{STATUS}) { # Did this handle already finish?
	$status ||= $arg->{STATUS};
      } else {			# No, we have to wait for it.
	push @handles, $arg;
      }
    }
    elsif (ref($arg) eq 'CODE') { # Is this a subroutine?
      push @subrs, $arg;	# Store it.
    }      
    elsif (ref($arg) eq 'ARRAY') { # Is this a list of subroutines?
      unshift @_, @$arg;	# Flatten the list.
    }
    elsif ($arg eq 'ERROR') {	# Indicates the error subroutine?
      $error_handler = shift @_; # Grab the error handler.
    }
    else {			# Must be an error status code.
      $status ||= $arg;
    }	
  }

#
# Now queue up the subroutines:
#
  my ($first_subr_handle, $subr, $subr_handle);
  while ($subr = shift @subrs) { # Another handle to go?
    $subr_handle = new MakeEvent::WaitingSubroutine $subr;
				# Make the data structure.
    foreach (@handles) {	# Indicate that it is waiting for each one of
      push @{$_->{WAITING_FOR}}, $subr_handle; # these handles.
    }
    $subr_handle->{WAIT_COUNT} = @handles; # So we know when they are done.

    @handles = ($subr_handle);	# Make successive subroutines wait for the
				# first one.

    $first_subr_handle ||= $subr_handle; # Remember the first handle.
  }

  $status and $first_subr_handle->{STATUS} = $status;
				# Store the proper error status.
  $error_handler and $subr_handle->{ERROR_HANDLER} = $error_handler;
				# Put the error handler in the handle for the
				# last subroutine.
  unless ($first_subr_handle->{WAIT_COUNT}) { # Are we waiting for anything now?
    if ($status) {		# Did we already find an error?
      process_finished($first_subr_handle, $status); # Don't even call the
				# routine.
    } else {
      $first_subr_handle->start; # No.  Start it immediately.
    }
  }

  return $subr_handle;		# Return the handle for the last subroutine so
				# if someone waits on it, all the others have
				# also finished.
}

=head2 wait_for

  $status = wait_for $handle1, $handle2, ...;

Waits for the processes or subroutines associated with the specified handles
to finish, and returns the status.

=cut

sub wait_for {
  my $done_flag = 0;
  my $status = 0;		# Assume no error.

  my $handle = when_done @_, sub {
#    print "Wait for: done\n";
    $done_flag = 1;		# Indicate that the subroutine was called.
    0;				# No error.
  }, ERROR => sub {		# Called if there was an error.
#    print "Wait for: done with error $_[0]\n";
    $done_flag = 1;
    $status = $_[0];		# Store the status value.
    return 0;
  };

  while (!$done_flag) {		# Wait until our subroutine is called.
    event_loop();
#    print "Wait for: done_flag = $done_flag, status = " . ($handle->{STATUS} || '') . ", wait count = " . ($handle->{WAIT_COUNT} || '') . "\n";
  }

  return $status;
}

###############################################################################
#
# Structure used to keep track of processes that we've started up:
#

package MakeEvent::Process;

use TextSubs;

my @pending_processes;		# Where we store the processes that we want
				# to run but can't because too many others
				# are already running.
my %running_processes;		# A hash of processes that we start, indexed
				# by the PID.

#
# Start a new process, and return a structure which can be used to control it.
#
sub new {
  my $class = shift @_;		# Get the name of the class to bless this into.
  my $subr = pop @_;		# Get the command to execute.

  my $proc = bless { CODE => $subr, PARAMS => [ @_ ] }, $class;
				# Store the information.
  push(@pending_processes, $proc); # Queue it up.
  
#
# See if we can start this job (and maybe another one) immediately:
#
  while (@pending_processes > 0 &&
	 $MakeEvent::n_external_processes < $MakeEvent::max_proc) {
    (shift @pending_processes)->start; # Start it now.
  }

#
# If we can't start it immediately, wait until we can.  We don't want to get
# too far ahead of the build.
#
  while (@pending_processes > 0) {
				# Too many processes already waiting?
#    print "Waiting: " . scalar(@pending_processes) . " already pending, max_proc = $MakeEvent::max_proc, n_external = $MakeEvent::n_external_processes\n";
    &MakeEvent::event_loop;	# Just pause for a moment, so we don't get
				# too far ahead.
  }

  return $proc;
}

#
# Increment or decrement the maximum number of processes that can run.
# Arguments:
# a) The number to add to the maximum number of processes.
#
sub adjust_max_processes {
  $MakeEvent::max_proc += $_[0]; # Adjust the number of processes.

  while (@pending_processes &&	# Were some processes waiting?
	 $MakeEvent::n_external_processes < $MakeEvent::max_proc) {
				# And we can now run them?
    (shift @pending_processes)->start; # Start up a process now.
  }
}

#
# Return the status code:
#
sub status {
  exists $_[0]->{STATUS} or
    die "MakeEvent::ProcessHandle::status called on an unfinished process";
  return $_[0]->{STATUS};
}

#
# This subroutine actually does the forking and starts the process.
#
sub start {
  my $self = shift @_;

  my $pid;
  $SIG{'CHLD'} = sub { ++$child_exited; }; # Call the reaper subroutine in the
				# mainline code.

  if ($pid = fork()) {		# In the parent process?
    $running_processes{$pid} = $self; # Store this for later.
    ++$MakeEvent::n_external_processes; # Keep track of how many things are running.
    return;
  }

#
# In the child process:
#
  $SIG{CHLD} = 'DEFAULT';	# Put the signal handler back the way it used
				# to be.  (Note: setting this to IGNORE rather
				# than DEFAULT causes a lot of hard-to-explain
				# errors; evidently automatic reaping of
				# processes interferes with system().)

  ++$MakeEvent::fork_level;
#
# Process the parameters.  These are instructions on how to set up
# STDIN, STDOUT, and STDERR.
#
  for (my $par_idx = 0; $par_idx < @{$self->{PARAMS}}; $par_idx += 2) {
    my $fh = $self->{PARAMS}[$par_idx]; # Get which file handle this is.
    close $fh;			# Close down whatever it used to be.
    if (defined($self->{PARAMS}[$par_idx+1])) {
      unless (open($fh, $self->{PARAMS}[$par_idx+1])) { 
	my $errorcode = "$!";
	open(TTY, ">/dev/tty");
	print TTY "could not open $fh as " . $self->{PARAMS}[$par_idx+1] . "--$errorcode\n";
	exit 1;
      }
    }
  }

  my $cmd = $self->{CODE};	# Get the thing to execute.
  if (!ref($cmd)) {		# Is this a string to execute as shell cmd?
    exec(format_exec_args($cmd));
    die "exec failed--$!\n";
  }

  exit &$cmd();			# Call the subroutine.
}

#
# This subroutine is responsible for reaping dead children.  (Unix is
# pretty morbid.)  It grabs the status value for each child, and starts
# up anything which was waiting for them.
#
# This is not called as a signal handler.  The signal handler for SIGCHLD
# makes the event loop call us by setting a flag.
# Note that signals can be coalesced (i.e., if two children died, there might
# be only one SIGCHLD).  I don't know what bozo designed the system that way, 
# but we still have to live with it 30 years later.
#
# Most of this code is adapted from the perl cookbook.
#
use POSIX qw(:signal_h :errno_h :sys_wait_h);

sub process_reaper {
  my $pid;
  while (($pid = waitpid(-1, &WNOHANG)) > 0) {
#
# Note again because of another bozo design decision in unix, waitpid may
# actually return the PID of a process which was stopped, not exited.  So we
# have to check for this.
#
    next unless WIFEXITED($?); # Make sure it really has exited.

    my $proc = delete $running_processes{$pid};
				# Get the structure defining the process.
    $proc or
      die "internal error: wait() returned a process not started by new MakeEvent::Process";
    --$MakeEvent::n_external_processes;

    my $status;
    if ($? > 255) {		# Non-zero exit status?
      $status = $? >> 8;	# Use that as the status.
    }
    elsif (($? & 127) != 0) {	# Exited with a signal?
      $status = "signal " . ($? & 127);
    }
    else {			# No error.  (I don't know if it's possible
				# for the process to dump core then exit with
				# a status code of 0 and no signal.)
      $status = 0;
    }

    MakeEvent::process_finished($proc, $status);

#
# If there were other processes waiting to be run, start one of them now that
# the job slot is free.
#
    while (@pending_processes &&
	   (!$MakeEvent::max_proc ||
	    $MakeEvent::n_external_processes < $MakeEvent::max_proc)) {
      (shift @pending_processes)->start;
    }
  }
}


###############################################################################
#
# Structure used to keep track of subroutines that are waiting to be called.
# This structure also keeps track of ancilliary variables like the log
# file indentation level so when the subroutine is called, the indentation
# level is correct.
#

package MakeEvent::WaitingSubroutine;

sub new {
  my ($classname, $perl_subr, @args) = @_;

  my $self = bless { CODE => $perl_subr,
		     ARGS => \@args,
		     INDENT => $main::indent_level,
		   }, $classname;
				# Make the structure for the process.
  return $self;
}

sub status {
  exists $_[0]->{STATUS} or
    die "MakeEvent::WaitingSubroutine::status called before the subroutine was called";
  return $_[0]->{STATUS};
}

#
# Start the process running.  This is pretty simple--we just execute the code.
# The return value can be one of several things:
# o 0 (for success).  In this case, we activate whoever is waiting for this
#    subroutine to finish.
# o A non-zero value, which is an error code.  We find an error subroutine
#   and call it with this value.
# o One or more handles.  In this case, we only activate whoever is waiting for
#   this subroutine when all of the other handles have also finished.
#
sub start {
  my $this_subr = $_[0];	# Get a reference to the process.
  
  local $main::indent_level = $this_subr->{INDENT};
				# Set the indentation level properly.

  my (@ret_vals) = &{$this_subr->{CODE}}(@{$this_subr->{ARGS}});

#
# Look at the return value and figure out what to do:
#
  my $status;

  foreach (@ret_vals) {
    if ($_) {			# Not 0 or undef?
      if (ref($_) eq 'MakeEvent::WaitingSubroutine' ||
	  ref($_) eq 'MakeEvent::Process') {
				# Something else to wait for?
	if (exists $_->{STATUS}) { # Did that thing already finish?
	   next if $status ||= $_->{STATUS}; # Quit if there was some error.
	}	
	else {			# Hasn't finished yet, we need to wait for
				# it.
	  $this_subr->{CODE} = sub { 0; };
				# Convert this subroutine into a dummy which
				# isn't harmful to call again.
	  push @{$_->{WAITING_FOR}}, $this_subr;
				# Mark this handle as waiting for the specified
				# other handle.
	  ++$this_subr->{WAIT_COUNT}; # Remember that we're waiting for one
				# more thing.
	}	
      }
      else {
	$status = $_;		# Must be a non-zero status.
      }	
    }
  }

  return if $this_subr->{WAIT_COUNT}; # Quit now if we're waiting for something
				# else.
  MakeEvent::process_finished($this_subr, $status);
				# Activate anyone who's waiting for this
				# process.
}

1;

=head1 AUTHOR

Gary Holt (holt@LNC.usc.edu)

=cut

