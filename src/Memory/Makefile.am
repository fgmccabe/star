#
# Makefile.am for memory management library
#

lib_LTLIBRARIES = libmemory.la

libmemory_la_SOURCES = \
  pool.c base.c class.c

libmemory_la_CPPFLAGS = -I@top_srcdir@/src/Memory/Headers -I@top_srcdir@/src/Headers  '-DCAFEDIR="@prefix@"'
libmemory_la_CFLAGS = -std=c99

