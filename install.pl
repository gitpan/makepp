#!/usr/bin/perl -w
    eval 'exec perl -S $0 ${1+"$@"}'
        if 0; # $running_under_some_shell;
#
# This script asks the user the necessary questions for installing
# makepp.
#

use Config;
use File::Copy;

#
# First make sure this version of perl is recent enough:
#
eval { require 5.005; };
if ($@) {			# Not recent enough?
  print "I need perl version 5.005 or newer.  If you have it installed 
somewhere already, run this installation procedure with that perl binary, e.g.,

	perl5.005 install.pl

If you don't have it installed, get it from www.perl.com and install it.
";
  exit 1;
}

$perlbin = $Config{'perlpath'};

print "Using perl in $perlbin.\n";

#
# Now figure out where everything goes:
#
$prefix = "/usr/local";

$bindir = shift(@_) ||
  read_with_prompt("
Makepp needs to know where you want to install it and its data files.
makepp is written in perl, but there is no particular reason to install
any part of it in the perl hierarchy; you can treat it as you would a
compiled binary which is completely independent of perl.

Where should the makepp executable be installed [$prefix/bin]? ") ||
  "$prefix/bin";

$bindir =~ m@(.*)/bin@ and $prefix = $1;
				# See if a prefix was specified.

$datadir = shift @_ || read_with_prompt("
Makepp has a number of library files that it needs to install somewhere.  Some
of these are perl modules, but they can't be used by other perl programs, so
there's no point in installing them in the perl modules hierarchy; they are
simply architecture-independent data that needs to be stored somewhere.

Where should the library files be installed [$prefix/share/makepp]? ") ||
  "$prefix/share/makepp";

$htmldir = shift @_ || read_with_prompt("
Where should the HTML documentation be installed?
Enter \"none\" if you do not want any documentation installed.
HTML documentation directory [$prefix/share/makepp/html]: ") ||
  "$prefix/share/makepp/html";

substitute_file("makepp", $bindir, 0755);
substitute_file("recursive_makepp", $datadir, 0644);

make_dir("$datadir/Signature");
foreach $module (qw(FileInfo FileInfo_makepp MakeEvent Glob Makefile Makesubs Rule
		    Signature TextSubs
		    Signature/exact_match Signature/target_newer
		    Signature/c_compilation_md5 Signature/md5)) {
  copy("$module.pm", "$datadir/$module.pm");
  chmod 0644, "$datadir/$module.pm";
}

foreach $include (qw(c_compilation_md5 infer_objects 
		     makepp_builtin_rules makepp_default_makefile)) {
  copy("$include.mk", "$datadir/$include.mk");
  chmod 0644, "$datadir/$include.mk";
}

#
# Now install the HTML pages.
#
if ($htmldir ne 'none') {
  make_dir($htmldir);
  foreach $file (<doc/*.html>) {
    ($outfile = $file) =~ s@^doc/@@; # Strip off the doc prefix.
    copy($file, "$htmldir/$outfile");
    chmod 0644, "$htmldir/$outfile";
  }
}

#
# Figure out whether we need to do anything about Digest::MD5.
#
eval "use Digest::MD5";		# See if it's already installed.
if ($@) {
  print "\nIt looks like you don't have the perl module Digest::MD5 installed.
Makepp likes to have Digest::MD5 installed because it is a better technique
for checking whether files have changed than just looking at the dates.
Makepp will work without it, however.
   If you would like this feature, you'll have to install this perl module.
If you installed perl from a binary distribution (e.g., from a linux package),
you can probably get a precompiled version of this module from the same place.
Otherwise, a version of this module is included with makepp.  To install it,
do the following:
	tar xf Digest-MD5-2.11.tar
	cd Digest-MD5-2.11
	perl Makefile.PL
	make
	make test
	make install
There is no need to reinstall makepp after installing Digest::MD5.

";
}

print "makepp successfully installed.\n";

#
# This subroutine makes a copy of an input file, substituting all occurences
# of @xyz@ with the perl variable $xyz.  It also fixes up the header line
# "#!/usr/bin/perl" if it sees one.
#
# Arguments:
# a) The input file.
# b) The output directory.
# c) The protection to give the file when it's installed.
#
sub substitute_file {
  my ($infile, $outdir, $prot) = @_;

  open(INFILE, $infile) || die "$0: can't read file $infile--$!\n";
  make_dir($outdir);

  open(OUTFILE, "> $outdir/$infile") || die "$0: can't write to $outdir/$infile--$!\n";

  while (defined($_ = <INFILE>)) {
    s@^\#!\s*(\S+?)/perl@\#!$perlbin@;    # Handle #!/usr/bin/perl.
    s/\@(\w+)\@/$ {$1}/g;	# Substitute anything containg @xyz@.

    print OUTFILE $_;
  }
  close(OUTFILE);

  chmod $prot, "$outdir/$infile";
}
 
#
# Make sure a given directory exists.  Makes it and its parents if necessary.
# Arguments: the name of the directory.
#
sub make_dir {
  my $dirname = '';
  foreach (split(/\//, $_[0])) {
    $dirname .= "/" . $_;	# Make the new directory name.
    -d $dirname or
      mkdir($dirname, 0755);
  }
}

sub read_with_prompt {
  local $| = 1;			# Enable autoflush on STDOUT.

  print @_;			# Print the prompt.
  $_ = <STDIN>;			# Read a line.
  chomp $_;
#
# Expand environment variables and home directories.
#
  s/\$(\w+)/$ENV{$1}/g;		# Expand environment variables.
  if (s/^~(\w*)//) {		# Is there a ~ expansion to do?
    if ($1 eq '') {
      $_ = "$ENV{HOME}$_";
    } else {
      my ($name, $passwd, $uid, $gid, $quota, $comment, $gcos, $dir, $shell) = getpwnam($1);
				# Expand from the passwd file.
      if ($dir) {		# Found it?
	$_ = "$dir$_";
      } else {
	$_ = "~$1";		# Not found.  Just put the ~ back.
      }
    }
  }
  return $_;
}
