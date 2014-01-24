#
# Makefile.am for utilities library
#

SUBDIRS = UnicodeStuff

lib_LTLIBRARIES = libutil.la

libutil_la_SOURCES = lists.c utils.c stack.c pp.c bitmap.c resources.c\
	meta.c object.c io.c unichar.c unicode.c bytestr.c\
	unitable.c hosts.c hash.c pool.c setenv.c memmove.c memcmp.c\
	utf.c formout.c formin.c file.c files.c sock.c uri.c parseuri.c\
	strio.c pipe.c managed.c timer.c options.c sort.c

libutil_la_CPPFLAGS = -I@top_srcdir@/src/Util/Headers -I@top_srcdir@/src/Headers  '-DCAFEDIR="@prefix@"'
libutil_la_CFLAGS = -std=c99

noinst_PROGRAMS = parseUni

parseUni_SOURCES = parseUni.c
parseUni_CFLAGS = -std=c99
parseUni_CPPFLAGS = -I@top_srcdir@/src/Util/Headers -I@top_srcdir@/src/Headers '-DCAFEDIR="@prefix@"'

unitable.c: UnicodeStuff/UnicodeData.txt parseUni.c
	make parseUni
	./parseUni ${srcdir}/UnicodeStuff/UnicodeData.txt ${srcdir}/unitable.c

AM_YFLAGS=-d -v -t

clean-local:
	rm -f catgrammar.c token.c


