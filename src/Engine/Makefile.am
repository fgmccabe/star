#
# Make the Cafe engine
#

SUBDIRS = Headers

bin_PROGRAMS = cafe

cafe_SOURCES = main.c engine.c decode.c eval.c boot.c engineOptions.c heap.c debug.c

cafe_CPPFLAGS = -I@top_srcdir@/src/Engine/Headers -I@top_srcdir@/src/Memory/Headers -I@top_srcdir@/src/Lib/Headers -I@top_srcdir@/src/Util/Headers -I@top_srcdir@/src/Headers '-DCAFEDIR="@prefix@"'
cafe_CFLAGS = -std=c99
cafe_LDADD = @top_builddir@/src/Util/libutil.la @top_builddir@/src/Memory/libmemory.la @top_builddir@/src/Asm/libassem.la @top_builddir@/src/Lib/libescape.la 
