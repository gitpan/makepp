# Please do NOT use this as an example of how to write a makefile.  This is
# NOT A typical makefile.
#
# These are the rules that are built into makepp, that it can apply without
# anything in the makefile.  Don't put too much junk in here!  One problem
# with GNU make that I ran into was that its default rules included stuff for
# dealing with very rare suffixes that I happened to use in a different
# and incompatible way.
#
# This file is read in after reading all of each makefile.
#
# It's not a good idea to put definitions of standard variables like CC, etc.,
# in here, because then (a) the $(origin ) function doesn't work; (b) the 
# values won't be visible except in rules, because this file is loaded after
# everything else in the makefile is processed.  Standard variables are 
# implemented as functions that have no arguments (see Makesubs.pm).
#

#
# Rules.  Special code in makepp makes it so these rules never override any 
# kind of rule that is contained in a user makeppfile.
#

#
# Link command:
#
$(basename $(foreach)) : $(infer_objects $(foreach), *.o) : foreach *.o
	$(infer_linker $(inputs)) $(inputs) $(LDLIBS) $(LDFLAGS) $(LIBS) -o $(output)
	noecho if [ "$(target)" = "test" ]; then \
	  echo "Warning: on unix, to run a program called 'test', you usually must type"; \
	  echo "  ./test"; \
	  echo "rather than just 'test'."; \
	fi

#
# C++ compilation:
#
ifneq $(percent_subdirs)
$(basename $(foreach)).o : $(foreach) : foreach **/*.cxx **/*.c++ **/*.cc **/*.cpp **/*.C
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c $(input) -o $(output)
else
$(basename $(foreach)).o : $(foreach) : foreach *.cxx *.c++ *.cc *.cpp *.C
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c $(input) -o $(output)
endif

#
# C compilation:
#
%.o: %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(input) -o $(output)

#
# Fortran compilation:
#
%.o: %.f
	$(FC) $(FFLAGS) $(CPPFLAGS) -c $(input) -o $(output)

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
	$(YACC) $(YFLAGS) $(input)
	mv -f y.tab.c $(output)

#
# Lex:
#
%.c: %.l
	: signature target_newer
	$(LEX) $(LFLAGS) -t $(input)
	mv lex.yy.c $(output)
