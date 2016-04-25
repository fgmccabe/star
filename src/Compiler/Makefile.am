#
# Makefile.am to create the Cafe compiler
#

SUBDIRS = Headers

bin_PROGRAMS = ccafe

ccafe_SOURCES = main.c options.c location.c\
	grammar.y token.l\
	sexp.c meta.c rewrite.c\
	dict.c\
	package.c\
	errors.c cfun.c escapes.c arith.c types.c constructor.c pattern.c\
	compile.c method.c gc.c\
	moves.c	convert.c\
	expression.c action.c theta.c case.c condition.c catch.c program.c\
	memo.c io.c

AM_YFLAGS=-d -v -t

# -I@top_srcdir@/lightning -I@top_srcdir@/lightning/$(cpu) -I@top_srcdir@/src/Jit/Headers 

ccafe_CPPFLAGS = -I@top_srcdir@/src/Compiler/Headers -I@top_srcdir@/src/Lib/Headers -I@top_srcdir@/src/Asm/Headers -I@top_srcdir@/src/Util/Headers -I@top_srcdir@/src/Memory/Headers -I@top_srcdir@/src/Headers '-DCAFEDIR="@prefix@"'
ccafe_CFLAGS = -std=c99
ccafe_LDADD = @top_builddir@/src/Util/libutil.la @top_builddir@/src/Asm/libassem.la @top_builddir@/src/Lib/libescape.la @top_builddir@/src/Jit/libjit.la @top_builddir@/src/Memory/libmemory.la 

clean-local:
	rm -f grammar.c grammar.h grammar.output token.c

