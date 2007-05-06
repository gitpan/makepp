# $Id: makepp_builtin_rules.mk,v 1.21 2006/10/09 18:29:11 pfeiffer Exp $
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
# everything else in the makefile is processed.	 Standard variables are
# implemented as functions that have no arguments (see Makesubs.pm).
#

#
# Rules.  Special code in makepp makes it so these rules never override any
# kind of rule that is contained in a user makeppfile.
#
_CPP_SUFFIXES := cxx c++ cc cpp
_OBJ_SUFFIX = .o

ifperl $FileInfo::case_sensitive_filenames
  #
  # Uppercase C as a suffix is indistinguishable from a lowercase C on a case-insensitive
  # file system.
  #
  _CPP_SUFFIXES += C
endif
ifperl ::is_windows()

  ifneq $(filter %cl %bcc32, $(CC) $(CXX))
  or ifneq $(filter %fl, $(FC))
    _OBJ_SUFFIX = .obj
  endif

  ifndef makepp_no_builtin_linker
  or ifeq $(makepp_no_builtin_linker) 0
    _EXE_SUFFIX = .exe
    #
    # We want "makepp xyz" to make xyz.exe if this is Windows.
    # This is a hack hack hack to make a phony target xyz that depends on
    # xyz.exe.  We must mark xyz as a phony target *after* we have associated
    # a rule with the target, or else the rule will not work because makepp
    # specifically rejects builtin rules for phony targets (to prevent disasters).
    # (See code in set_rule().)  So we evaluate $(phony ) only after the
    # rule has been set.  This kind of shenanigan is never necessary in normal
    # makefiles because there are no special restrictions about rules from anywhere
    # except this file.
    #
    $(basename $(foreach)): $(foreach) : foreach *.exe
	@makeperl { '$(phony $(output))' }
  endif
endif

ifndef makepp_no_builtin_linker
or ifeq $(makepp_no_builtin_linker) 0
  #
  # Link commands
  # We could split this up into keywords/builtins common to all Shells
  # and a "may have to type" list for others like "alias".
  #
  $(basename $(foreach))$(_EXE_SUFFIX): $(infer_objects $(foreach), *$(_OBJ_SUFFIX))
	:foreach $(wildcard $( bg break case cd chdir continue do done echo elif else \
		esac eval exec exit export fg fi for getopts hash if jobs kill login \
		newgrp pwd read readonly return set shift stop suspend test then \
		times trap type ulimit umask unset until wait while)$(_OBJ_SUFFIX))
	$(infer_linker $(inputs)) $(inputs) $(LDLIBS) $(LDFLAGS) $(LIBS) -o $(output)
	noecho makeperl {{
	  print "Warning: on unix, to run a program called '$(basename $(foreach))', you usually must type\n";
	  print "  ./$(basename $(foreach))\n";
	  print "rather than just '$(basename $(foreach))'.\n";
	}}

  $(basename $(foreach))$(_EXE_SUFFIX): $(infer_objects $(foreach), *$(_OBJ_SUFFIX))
	:foreach *$(_OBJ_SUFFIX)
	$(infer_linker $(inputs)) $(inputs) $(LDLIBS) $(LDFLAGS) $(LIBS) -o $(output)
endif

#
# C++ compilation:
#
ifneq $(percent_subdirs)
$(basename $(foreach))$(_OBJ_SUFFIX) : $(foreach) : foreach **/*.$(_CPP_SUFFIXES)
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c $(input) -o $(output)
else
$(basename $(foreach))$(_OBJ_SUFFIX) : $(foreach) : foreach *.$(_CPP_SUFFIXES)
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c $(input) -o $(output)
endif

#
# C compilation:
#
%$(_OBJ_SUFFIX): %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(input) -o $(output)

#
# Fortran compilation:
#
%$(_OBJ_SUFFIX): %.f
	$(FC) $(FFLAGS) $(CPPFLAGS) -c $(input) -o $(output)

#
# The rules for yacc and lex are marked :build_check target_newer because we
# don't want to reexecute them if the file already exists.  Some systems don't
# have yacc or lex installed, and some makefiles (e.g., the KDE makefiles)
# don't have a proper yacc/lex rule but run yacc/lex as part of the action of
# a phony target.
#

#
# Yacc:
#
%.c: %.y
	: build_check target_newer
	$(YACC) $(YFLAGS) $(input)
	&mv y.tab.c $(output)

#
# Lex:
#
%.c: %.l
	: build_check target_newer
	$(LEX) $(LFLAGS) -t $(input)
	&mv lex.yy.c $(output)
