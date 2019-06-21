# File: Makefile (unix version)
# (C)opyright 1987 InfoTaskforce.

OBJECTS =	file.o funcs.o infocom.o init.o input.o\
		interp.o io.o jump.o object.o options.o page.o\
		print.o property.o support.o variable.o term.o

# Compile options are described below:
#
# ALLOCALL : As originally coded, when the program allocated
#           memory it always made sure that there was always
#           25k left free. This is not a requirement in most
#           system. If this isn't a requirement on your system
#           then use this option.
# DEBUG    : Compile a debug version of the interpreter. Compare
#           output produced with this option against a working
#           debug version when you suspect problems in the
#           virtual machine [ this commonly produces error 21 ].
# TERMCAP  : Compile a version that uses termcap features for
#           terminal output. You may also have to include a
#           termcap library as well.
# TIMESEED : Seed the random number generator with time (). Use
#           this option if time () is available on you system.
#
CFLAGS=		-O -DALLOCALL -DTIMESEED -DTERMCAP # -DDEBUG

# If TERMCAP option defined include the termcap library here
# -ltermcap not required for Sys V.
LDFLAGS=	-lcurses -ltermcap

infocom: $(OBJECTS)
	cc -o infocom $(OBJECTS) $(LDFLAGS)

$(OBJECTS): infocom.h
