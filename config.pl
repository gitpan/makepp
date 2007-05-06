#!/usr/bin/perl -w
# $Id: config.pl,v 1.20 2005/11/15 19:27:13 pfeiffer Exp $
#
# Configure this package.
#
use Config;

#
# First make sure this version of perl is recent enough:
#
eval { require 5.006 };
if ($@) {			# Not recent enough?
  die "I need perl version 5.6 or newer.  If you have it installed somewhere
already, run this installation procedure with that perl binary, e.g.,

	perl5.8.6 install.pl

If you don't have a recent version of perl installed (what kind of system are
you on?), get the latest from www.perl.com and install it.
";
}

if ($] == 5.006) {
  print "**************** You're running perl 5.6.0.  *************************

perl 5.6.0 has a bug which can cause makepp to behave in a bizarre fashion.
This bug is not encountered in every makefile, so you might be ok.  But it
would be safer to upgrade to another version of perl (e.g., 5.8.0 does
not seem to have the problem).

Press return to continue:
";
  $_ = <STDIN>;
}

if ($] == 5.006001) {
  print "**************** You're running perl 5.6.1.  *************************

perl 5.6.1 fails on some architectures and works just fine on others.
If you encounter weird problems with makepp, or if the tests fail,
consider upgrading your version of perl.

Press return to continue:
";
  $_ = <STDIN>;
}

#
# Parse the arguments:
#
$prefix = "/usr/local";
$findbin = "none";

while (@ARGV) {
  $_ = shift @ARGV;             # Get the next argument.
  if (/^--help/ || /^-h/) {
    die "Usage: configure [option]

where options are:
   -h, --help    This help message.
   --prefix=/path/to/installation
                 Specify location where you want to install everything.
   --bindir=/path/to/installation/bin
                 Where the binaries go.  Makepp's binaries are just perl
                 scripts so they are architecture independent.
   --htmldir=/path/to/installation/share/html
                 Where the HTML documentation goes.  Specify 'none' if you
                 do not want the documentation installed.  (You can always
                 read it online at http://makepp.sourceforge.net.)
   --mandir=/path/to/man
                 Where the manual pages should reside.  Specify 'none' if you
                 do not want the documentation installed.
   --datadir=/path/to/installation/share/makepp
                 Where to install makepp's library files.
   --findbin=relative/path/to/datadir/from/bindir
                 Where to find libraries relative to executables. Specify
                 'none' (the default) to find them in datadir.
";
  }
  elsif (/^--?prefix(?:=(.*))?/) {
    $prefix = $1 || shift @ARGV;
  }
  elsif (/^--?bindir(?:=(.*))?/) {
    $bindir = $1 || shift @ARGV;
  }
  elsif (/^--?datadir(?:=(.*))?/) {
    $datadir = $1 || shift @ARGV;
  }
  elsif (/^--?mandir(?:=(.*))?/) {
    $mandir = $1 || shift @ARGV;
  }
  elsif (/^--?htmldir(?:=(.*))?/) {
    $htmldir = $1 || shift @ARGV;
  }
  elsif (/^--?findbin(?:=(.*))?/) {
    $findbin = $1 || shift @ARGV;
  }
}

$bindir ||= "$prefix/bin";
$datadir ||= "$prefix/share/makepp";
$htmldir ||= "$prefix/share/makepp/html";
$mandir ||= "$prefix/man";

foreach ($bindir, $datadir, $htmldir) {
  s@~/@$ENV{'HOME'}/@;
}

#
# Load the current version:
#
open(VERSION, "VERSION") || die "$0: file VERSION is missing\n";
$VERSION = <VERSION>;
chomp $VERSION;
close VERSION;

#
# Write out a makefile for this.  This makefile ought to work with any version
# of bozo make, so it has to be extremely generic.
#
open(MAKEFILE, "> Makefile") or die "$0: can't write Makefile--$!\n";
print MAKEFILE "PERL = $Config{'perlpath'}
BINDIR = $bindir
DATADIR = $datadir
FINDBIN = $findbin
MANDIR = $mandir
HTMLDIR = $htmldir
VERSION = $VERSION
";

print MAKEFILE q[

all: test

test: .test_done

.test_done: *.pm Signature/*.pm Scanner/*.pm CommandParser/*.pm ActionParser/*.pm makepp makepp_tests/*.test makepp_tests/run_tests.pl
	cd makepp_tests && PERL=$(PERL) $(PERL) run_tests.pl
	touch $@

distribution: makepp-$(VERSION).tar.gz

makepp-$(VERSION).tar.gz: README INSTALL LICENSE VERSION makepp.lsm ChangeLog \
	makepp recursive_makepp makeppclean \
	Signature/*.pm Scanner/*.pm \
	BuildCheck/*.pm CommandParser/*.pm ActionParser/*.pm *.mk *.pm \
	pod/*.pod pod/makepp_compatibility.html \
	makepp_tests/*.test makepp_tests/run_tests.pl \
	config.pl configure install.pl makepp_build_cache_control
	rm -rf makepp-$(VERSION)
	./configure         # Reset Makefile.
	mkdir makepp-$(VERSION)
	mkdir makepp-$(VERSION)/pod makepp-$(VERSION)/makepp_tests \
	   makepp-$(VERSION)/Signature makepp-$(VERSION)/Scanner \
	   makepp-$(VERSION)/CommandParser makepp-$(VERSION)/ActionParser
	for file in $^; do cp $$file makepp-$(VERSION)/$$file; done
	GZIP=-9 tar --create --gzip --file $@ makepp-$(VERSION)
	cd makepp-$(VERSION) && make test    # Make sure it all runs.
	rm -rf makepp-$(VERSION)

install: all
	$(PERL) install.pl $(BINDIR) $(DATADIR) $(MANDIR) $(HTMLDIR) $(FINDBIN)

.PHONY: all distribution install test
];
