PERL = /usr/bin/perl
BINDIR = /usr/local/bin
DATADIR = /usr/local/share/makepp
MANDIR = /usr/local/man
HTMLDIR = /usr/local/share/makepp/html
VERSION = 1.19


all: test

test:
	cd makepp_tests && PERL=$(PERL) $(PERL) run_tests.pl

distribution: makepp-$(VERSION).tar.gz

makepp-$(VERSION).tar.gz: README INSTALL LICENSE VERSION makepp.lsm ChangeLog \
	makepp recursive_makepp Signature/*.pm *.mk *.pm \
	pod/*.pod pod/pod2html makepp_tests/*.test makepp_tests/run_tests.pl \
	Makefile config.pl configure install.pl
	rm -rf makepp-$(VERSION)
	./configure         # Reset Makefile.	  
	mkdir makepp-$(VERSION)
	mkdir makepp-$(VERSION)/pod makepp-$(VERSION)/makepp_tests \
	   makepp-$(VERSION)/Signature
	for file in $^; do cp $$file makepp-$(VERSION)/$$file; done
	GZIP=-9 tar --create --gzip --file $@ makepp-$(VERSION)
	cd makepp-$(VERSION) && make test    # Make sure it all runs.
	rm -rf makepp-$(VERSION)

install: all
	$(PERL) install.pl $(BINDIR) $(DATADIR) $(MANDIR) $(HTMLDIR)

.PHONY: all distribution install test
