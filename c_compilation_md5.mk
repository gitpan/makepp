#
# This include file forces makepp to use MD5 signatures, ignoring whitespace
# and comments, for computing file signatures for C/C++ compilations.
#
perl_begin

use Signature::c_compilation_md5; # Load the subroutines.

#
# This is a replacement scanner which sets the default rule signature method
# for C compilation to compute MD5 checksums ignoring comments and whitespace.
#
sub scanner_c_compilation_md5 {
  $_[1]->set_signature_method_default($Signature::c_compilation_md5::c_compilation_md5);
  &scanner_c_compilation;	# Now do the regular scanning.
}

#
# Replace all entries in the scanner table that reference the
# C compilation scanner:
#
foreach (keys %scanners) {
  $scanners{$_} == \&scanner_c_compilation and
    $scanners{$_} = \&scanner_c_compilation_md5;
}

perl_end
