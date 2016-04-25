#
# Makefile.am for the assembler
#

# SUBDIRS = Headers

bin_PROGRAMS = asm

lib_LTLIBRARIES = libassem.la

AM_YFLAGS=-d -v -t -p ss
AM_LFLAGS=--prefix=ss
LEX_OUTPUT_ROOT = lex.ss

libassem_la_SOURCES = assem.c display.c parser.y tok.l signature.c encode.c asmOptions.c errors.c driver.c 
libassem_la_CPPFLAGS = -I@top_srcdir@/src/Asm/Headers -I@top_srcdir@/src/Memory/Headers -I@top_srcdir@/src/Lib/Headers -I@top_srcdir@/src/Util/Headers -I@top_srcdir@/src/Headers  '-DCAFEDIR="@prefix@"'
libassem_la_CFLAGS = -std=c99 -static

asm_SOURCES = main.c 

asm_CPPFLAGS = -I@top_srcdir@/src/Asm/Headers -I@top_srcdir@/src/Memory/Headers -I@top_srcdir@/src/Lib/Headers -I@top_srcdir@/src/Util/Headers -I@top_srcdir@/src/Headers '-DCAFEDIR="@prefix@"'
asm_CFLAGS = -std=c99
asm_LDADD = @top_builddir@/src/Memory/libmemory.la @top_builddir@/src/Util/libutil.la @top_builddir@/src/Lib/libescape.la @top_builddir@/src/Asm/libassem.la 

clean-local:
	rm -f parser.c parser.h parser.output tok.c
