VERSION	= 1.05

all: test

test:
	cd makepp_tests && perl run_tests.pl

distribution: makepp-$(VERSION).tar.gz

makepp-$(VERSION).tar.gz: README INSTALL LICENSE Makefile makepp.lsm ChangeLog \
	makepp recursive_makepp *.tar Signature/*.pm *.mk *.pm *.pl \
	doc/*.html makepp_tests/*.test makepp_tests/run_tests.pl
	cd makepp_tests && perl run_tests.pl
	mkdir makepp-$(VERSION)
	mkdir makepp-$(VERSION)/doc makepp-$(VERSION)/makepp_tests \
	   makepp-$(VERSION)/Signature
	for file in $^; do cp $$file makepp-$(VERSION)/$$file; done
	GZIP=-9 tar --create --gzip --file $@ makepp-$(VERSION)
	rm -rf makepp-$(VERSION)

install: all
	perl install.pl

.PHONY: all distribution install test
