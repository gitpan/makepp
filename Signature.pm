# $Id: Signature.pm,v 1.1.1.1 2002/01/10 07:04:31 grholt Exp $
package Signature;

=head1 NAME

Signature -- Interface definition for various signature classes

=head1 USAGE

  use Signature;		# Really!  That's all.

=head1 DESCRIPTION

Makepp is quite flexible in the algorithm it usese for deciding whether
a target is out of date with respect to its dependencies.  Most of this
flexibility is due to various different implementations of the Signature
class.

Each rule can have a different signature class associated with it,
if necessary.  In the makefile, the signature class is specified by
using the :signature modifier, like this:

   %.o : %.c
	   : signature special_build
	   $(CC) $(CFLAGS) -c $(FIRST_DEPENDENCY) -o $(TARGET)

This causes the signature class C<Signature::special_build> to be used for
this particular rule.  

Only one object from each different signature class is actually created; the
object has no data, and its only purpose is to contain a blessed reference to
the package that actually implements the functions.  Each rule contains a
reference to the Signature object that is appropriate for it.  The object is
found by the name of the Signature class.  For example, the above rule uses
the object referenced by C<$Signature::special_build::special_build>.  (The
purpose of this naming scheme is to make it impossible to inherit accidently a
singleton object, which would cause the wrong Signature class to be used.)


=head2 signature

   $signature = $sigobj->signature($objinfo);

This function returns a signature for the given object (usually a
FileInfo class, but possibly some other kind of object).  A signature is
simply an ASCII string that will change if the object is modified.

$sigobj is the dummy Signature class object.

$objinfo is the a reference to B<makepp>'s internal description of that object
and how it is to be built.  See L<makepp/"Extending makepp"> for details.

The default signature function simply calls $objinfo->signature, i.e.,
it uses the default signature function for objects of that class.

=cut

sub signature {
  return $_[1]->signature;
}

=head2 build_check

  $rebuild_code = $sigobj->build_check($target,
				       $command_string,
				       $cwd,
				       $sorted_dependencies,
				       $depndency_signatures);

Returns undef if the given target does not need rebuilding.  May return any
true value (at the moment) if the object needs rebuilding.

$target is the target we are trying to build.  The function will probably have
to look at the build information for the target (using
$target->build_info_string) to find out how the target was built last time.

$command_string is the shell command(s) that will be executed if the file
needs rebuilding.

$cwd is the default directory relative to the parent directory of the
object that contained the file.

$sorted_dependencies is a string that contains the relative filenames of all
dependencies, sorted in alphabetical order, and separated by "\01" characters.
(Yes, I know it's a weird format, but it's much faster for comparison for the
Signature class which is used most of the time.)  If you need to split this
apart into filenames, just use
    @sorted_dependencies = split(/\01/, $sorted_dependencies);

$dependency_signatures is a string that contains the signature for each one of
those dependencies, int the same order, and separated by "\01" characters.
Again, if you want to split into an array to make this more convenient to use,
do
    @dependency_signatures = split(/\01/, $dependency_signatures);

=cut

sub build_check {
  die "Implement me";
}

=head2 check_move_or_link_target

 $substitute_target = $rule->check_move_or_link_target($target_info,
						       $command_string,
						       $cwd,
						       $sorted_dependencies,
						       $dependency_signatures);

This function is alled when the target doesn't exist or is out of date.  We
check to make sure there aren't other versions of the target around somewhere
in a repository or variant build cache which are not out of date.  If such an
object is found, then we return that object's information.

Arguments are the same as to build_check().

=cut

#
# This function is called when the target doesn't exist or is out of date.
# We check to make sure there aren't other versions of the target around
# somewhere in a repository or variant build cache which are not out of date.
#
# $target_copy = $sigobj->check_move_or_link_target($target_info,
#					   $command_string,
#					   $sorted_dependency_string,
#					   $dependency_signature_string);
#
# where $target_info is the object info for the target,
# $command_string is the shell command to use to build the target,
# $sorted_dependency_string is a list of all the relative filenames of the
#      dependencies, sorted alphabetically, and joined by \01
# $dependency_signature_string is a list of all the signatures of each
#      dependency in the same order as $sorted_dependency_string, separated
#      by \01
#
sub check_move_or_link_target {
  my ($self, $tinfo, $command_string, $cwd, $sorted_dep_str,
      $dep_sig_str) = @_;

  my $other_versions = $tinfo->{ALTERNATE_VERSIONS};

  return undef unless $other_versions;
				# Skip if we don't know about any other
				# versions of this file.

  foreach (@$other_versions) {
    if (!$self->build_check($_, $command_string, $cwd,
			    $sorted_dep_str, $dep_sig_str)) {
      return $_;		# Found one that is valid.
    }
  }

  return undef;
}

1;
