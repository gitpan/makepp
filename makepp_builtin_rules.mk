#
# These are the rules that are built into makepp, that it can apply without
# anything in the makefile.  Don't put too much junk in here!  One problem
# with GNU make that I ran into was that its default rules included stuff for
# dealing with very rare suffixes that I happened to use in a different
# and incompatible way.
#
# This makefile is read in after reading all of each makefile.
# Currently, it only contains rules; all default variable values are actually
# stored as functions that take no arguments.  (See Makesubs.pm.)
# However, if you want, you can put variable settings in this file.
# If you put in any variable assignments, you must use the ?= assignment
# syntax like this:
#	variable ?= value
# This means that variable assignments in this file will not override
# variables assigned in the makefile.
#

#
# Link command:
#
$(basename $(foreach)) : $(infer_objects $(foreach), *.o) : foreach *.o
	$(infer_linker $(inputs)) $(inputs) $(infer_libraries $(inputs)) $(LOADLIBES) $(LDLIBS) $(LIBS) -o $(output)
	noecho if [ "$(target)" = "test" ]; then \
	  echo "Warning: on unix, to run a program called 'test', you must type"; \
	  echo "  ./test"; \
	  echo "rather than just 'test'."; \
	fi

#
# C++ compilation:
#
$(basename $(foreach)).o : $(foreach) : foreach *.cxx *.c++ *.cc *.cpp *.C
	$(COMPILE.cc) $(input) -o $(output)

#
# C compilation:
#
%.o: %.c
	$(COMPILE.c) $(input) -o $(output)

#
# Fortran compilation:
#
%.o: %.f
	$(COMPILE.f) $(input) -o $(output)

#
# The rules for yacc and lex are marked :signature target_newer because we
# don't want to reexecute them if the file already exists.  Some systems don't
# have yacc or lex installed, and some makefiles (e.g., the KDE makefiles)
# don't have a proper yacc/lex rule but run yacc/lex as part of the action of
# a phony target.
#

#
# Yacc:
#
%.c: %.y
	: signature target_newer
	$(YACC.y) $(input)
	mv -f y.tab.c $(output)

#
# Lex:
#
%.c: %.l
	: signature target_newer
	@$(RM) $(output)
	$(LEX.l) $(input) > $(output)
