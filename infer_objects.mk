###############################################################################
#
# Code for inferring C++ object file dependencies given a list of .h files.
#

#
# Usage:
#    target : $(infer_objs seed-list, list of possible objs)
#
sub f_infer_objects {
  my ($text, $makefile, $makefile_line) = @_; # Name the arguments.
  my ($seed_objs, $candidate_list) = split(/,\s*/, $text);
				# Get the arguments.

  $candidate_list or die "infer_objects called without a candidate list\n";
  $rule or die "infer_objects called outside of a rule\n";

  my $build_cwd = $rule->build_cwd;

#
# Build up a list of all the possibilities:
#
  my %candidate_objs;
  foreach my $candidate_obj (map(Glob::zglob_fileinfo_atleastone($_, $build_cwd),
				 split(' ', $candidate_list))) {
				# Get a list of all the possible objs.
    my $objname = $candidate_obj->{NAME};
    $objname =~ s/\.[^\.]+$//;	# Strip off the extension.
    if ($candidate_objs{$objname}) { # Already something by this name?
      ref($candidate_objs{$objname}) eq 'ARRAY' or
	$candidate_objs{$objname} = [ $candidate_objs{$objname} ];
				# Make into an array as appropriate.
      push @{$candidate_objs{$objname}}, $candidate_obj;
    }
    else {			# Just one obj?
      $candidate_objs{$objname} = $candidate_obj;
    }
  }	
#
# Now look at the list of all the include files.  This is a little tricky
# because we don't know the include files until we've actually built the
# dependencies.
#
  my %source_names;		# These are the names of include files for
				# which are look for the corresponding objects.

  my @build_handles;		# Where we put the handles for building objects.
  my @deps = map { Glob::zglob_fileinfo($_, $build_cwd) } split(' ', $seed_objs);
				# Start with the seed files
				# themselves.
  $main::log_level and 
    main::print_log("infer_objects called with seed objects ",
		    join(" ", map { $_->name } @deps));
  
  foreach (@deps) {
    my $name = $_->{NAME};
    $name =~ s/\.[^\.]+$//;	# Strip off the extension.
    $source_names{$name}++;	# Indicate that we already have this as a
				# source file.
  }	


  my $dep_idx = 0;

  for (;;) {
    while ($dep_idx < @deps) {	# Look at each dependency currently available.
      my $o_info = $deps[$dep_idx]; # Access the FileInfo for this object.
      my $bh = main::build($o_info); # Start building it.
      my $handle = when_done $bh, # Build this dependency.
      sub {			# Called when the build is finished:
	$bh->status and return $bh->status;
				# Skip if an error occured.
	my @this_sources = split("\01", $o_info->build_info_string("SORTED_DEPS") || '');
				# Get the list of source files that went into
				# it.
	foreach (@this_sources) {
	  my $name = $_;	# Make a copy of the file.
	  $name =~ s@.*/@@;	# Strip off the path.
	    $name =~ s/\.[^\.]+$//; # Strip off the extension.
	  unless ($source_names{$name}++) { # Did we already know about that source?
	    if (ref($candidate_objs{$name}) eq 'FileInfo') { # Found a file?
	      $main::log_level and
		main::print_log(0, "infer_objects: adding ",
				$candidate_objs{$name}->name,
				" to dependency list because of $_");
	      push @deps, $candidate_objs{$name}; # Scan for its dependencies.
	    }
	    elsif (ref($candidate_objs{$name}) eq 'ARRAY') { # More than 1 match?
	      main::print_error($rule->source, " in infer_objects: more than one possible object for include file $_:\n  ",
				join("\n  ", map { $_->absolute_filename } @{$candidate_objs{$name}}),
				"\n");
	    }
	  }
	}	
      };

      defined($handle) and push @build_handles, $handle;
				# Is this something we need to wait for?
      ++$dep_idx;
    }

    last unless @build_handles;	# Quit if nothing to wait for.
    my $status = wait_for @build_handles; # Wait for them all to build, and 
				# try again.
    @build_handles = ();	# We're done with those handles.
    $status and last;		# Quit if there was an error.
  }	

#
# At this point, we have built all the dependencies, and we also have a
# complete list of all the objects.
#
  return join(" ", map { $_->relative_filename($build_cwd) } @deps);
}
