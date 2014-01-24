#
# Makefile.am for the jit assembler
#

lib_LTLIBRARIES = libjit.la

libjit_la_SOURCES = jit.c x86.c
libjit_la_CPPFLAGS = -I@top_srcdir@/src/Asm/Headers -I@top_srcdir@/src/Lib/Headers -I@top_srcdir@/src/Util/Headers -I@top_srcdir@/src/Jit/Headers -I@top_srcdir@/src/Headers  '-DCAFEDIR="@prefix@"'
libjit_la_CFLAGS = -std=c99
