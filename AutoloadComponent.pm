=head1 NAME

AutoloadComponent -- autoload components instead of individual functions

=head1 USAGE

  use AutoloadComponent;

=head1 DESCRIPTION

AutoloadComponent is a replacement for the standard AutoLoader package with
the following differences:


=over 4

=item *

Instead of splitting functions out automatically from a monolithic source
file, functions are split out manually (and edited in separate pieces, instead
of as one large file).  Each file must begin with the appropriate package
declaration.

=item *

More than one function may be defined per file, and data associated with
the function may also be defined with the file, as well as any cleanup
END {} blocks.  (This is the main reason why I wrote AutoloadComponent.)

=item *

Function stubs are provided by reading the directory containing the
files to autoload.  The names of the files must correspond to the
function to be loaded.  If there is more than one function per file, it
may be necessary to make several soft links to the same file.

=back

=cut

package AutoloadComponent;

#
# This is the subroutine that does the main work of loading the files.
#
AUTOLOAD {

  {				# Braces used to preserve $1, etc.
				# (Stolen from the AutoLoader module.)

    my $sub = $AUTOLOAD;	# Name of the function.
    $sub =~ s@::@/@g;		# Handle any package qualifiers.
    require "autoload/$sub.pl";	# Try to load the file.
    goto &$AUTOLOAD;
  }
}

#
# This subroutine is called when this module is imported into a package.
# It reads the autoload directory, and sets up stubs for any functions
# which can be autoloaded.
#
sub import {
  my $callpkg = caller;		# Get the package we're supposed to autoload
				# the symbols for.

  *{"${callpkg}::AUTOLOAD"} = \&AUTOLOAD; # Export the autoload subroutine.
#
# Now find the stubs for all the functions that we can autoload.  We do this
# by locating the directory containing the files, and then reading the list
# of files.
#  
  my $dirname = $callpkg;
  $dirname =~ s@::@/@g;		# Get a relative path to the directory
				# containing the files.

  foreach my $dir (@INC) {
    if (-d "$dir/autoload/$dirname") { # Found the directory?
      local *FDIR;		# Make a local directory handle.
      opendir(FDIR, "$dir/autoload/$dirname") || 
	die "autoload: can't read directory $dir/autoload/$dirname--$!\n";
      my $func_names = '';
      foreach (grep(/\.pl$/, readdir(FDIR))) { # Get the list of functions.
	my $func_name = $_;
	$func_name =~ s/\.pl$//; # Strip off the extension.
	$func_names .= "sub $func_name;\n";
      }
      eval "package $callpkg; $func_names;"; # Declare all the subroutines.
      closedir FDIR;
    }
  }	
}

1;
