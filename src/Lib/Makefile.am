#
# Makefile for the library of functions
#

SUBDIRS = Headers

lib_LTLIBRARIES = libescape.la

libescape_la_SOURCES = escapes.c system.c show.c
libescape_la_CPPFLAGS = -I@top_srcdir@/src/Assem/Headers -I@top_srcdir@/src/Lib/Headers -I@top_srcdir@/src/Util/Headers -I@top_srcdir@/src/Headers '-DCAFEDIR="@prefix@"'
libescape_la_CFLAGS = -std=c99
