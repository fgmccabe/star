AC_INIT([cafe],[0.0],[fmccabe@gmail.com])
AC_CONFIG_AUX_DIR([build-aux])

dnl Check for system type
AC_CANONICAL_TARGET

AC_MSG_NOTICE([cpu is $cpu, target is $target_cpu])

AC_CANONICAL_HOST

# Minimum autoconf version
AC_PREREQ(2.54)

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([src/Compiler/main.c])
AM_INIT_AUTOMAKE([-Wall -Werror])

AC_CONFIG_MACRO_DIR([macros])

dnl Add macros to aclocal include list
ACLOCAL="aclocal -I macros"

dnl We prefer to install in /opt
AC_PREFIX_DEFAULT(/opt/cafe)

dnl Checks for programs.
AM_PROG_AR
AC_ISC_POSIX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LIBTOOL
AC_PROG_LEX
AC_PROG_YACC
AC_PROG_CXX
AM_PROG_CC_C_O

AC_MSG_CHECKING([getopt accepts '+' prefix])
AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <unistd.h>
extern int optind;
int main(int a, char *v[])
{
	int argc = 3;
	char *argv[4] = { "self", "arg", "-h", NULL };

	/* Returns success (zero) if -h option is detected (ie. getopt permutes) */

	if (getopt(argc, argv, "+h") == -1) {
		optind = 0; 
		if (getopt(argc, argv, "h") == 'h')
			return 0;
	}

	return 1;
}
]])],[AC_DEFINE(GNU_GETOPT_NOPERMUTE, "+",Define "+" prefix to make getopt posixly compliant (non-permuting),
   "" if getopt does not support + prefix, or is non-permuting by default)
AC_MSG_RESULT("yes")],[AC_DEFINE(GNU_GETOPT_NOPERMUTE, "",Define "+" prefix to make getopt posixly compliant (non-permuting),
   "" if getopt does not support + prefix, or is non-permuting by default)
AC_MSG_RESULT("no")],[AC_DEFINE(GNU_GETOPT_NOPERMUTE, "",Define "+" prefix to make getopt posixly compliant (non-permuting),
   "" if getopt does not support + prefix, or is non-permuting by default)
AC_MSG_RESULT("no")
])

dnl enable/disable ANSI colours in output
AC_ARG_ENABLE(colours,
[  --enable-colours        Turn on ANSI colours in output [default=yes]],
[case "${enableval}" in
   no)
	AC_DEFINE(NOCOLOURS,,[Define NOCOLOURS to prevent ANSI output])
	;;
 esac],
dnl colours enabled by default.
[])

dnl Checks for library functions.
AC_PROG_GCC_TRADITIONAL
AC_FUNC_MEMCMP
AC_TYPE_SIGNAL
AC_FUNC_STRFTIME
AC_CHECK_FUNCS(gethostname gettimeofday select socket strerror getdtablesize getrlimit timegm)
AC_CHECK_LIB(m,log10)
AC_CHECK_LIB(socket,accept)
AC_CHECK_LIB(nsl,inet_ntoa)
AC_CHECK_LIB(pthread,pthread_create)
dnl AC_CHECK_FUNCS(pthread_sigmask)
AC_REPLACE_FUNCS(memmove)
AC_REPLACE_FUNCS(memcmp)
AC_REPLACE_FUNCS(setenv)

dnl Set up debugging flag
AC_ARG_ENABLE(debug,
[  --enable-debug          Turn on debugging code],
[case "${enableval}" in
  yes)
	CFLAGS='-g -Wall'
	AC_DEFINE(ALLTRACE,,[Enable tracing debugging code to be compiled])
	;;
  semi)
        CFLAGS='-g -Wall -O3'
       	AC_DEFINE(NDEBUG,,[Turn off assertion testing when enabled])
        ;;
  profile)
	CFLAGS='-g -pg -Wall'
	AC_DEFINE(ALLTRACE)
	;;

  no)  	CFLAGS='-O3 -Wall -DNDEBUG'
       	AC_DEFINE(NDEBUG)
       	AC_DEFINE(NOCOLOURS)
      	;;
  *)    CFLAGS='${enableval}'
        AC_DEFINE(ALLTRACE)
	;;
esac],[CFLAGS='-O3 -Wall'
AC_DEFINE(NDEBUG)
AC_DEFINE(NOCOLOURS)])

dnl Checks for header files.
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS(fcntl.h limits.h sys/time.h syslog.h unistd.h)

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_SIZE_T
AC_HEADER_TIME
AC_STRUCT_TM

dnl AC_CONFIG_LINKS([src/Jit/jit.c:src/Jit/Arch/$target_cpu/jit.c])
AC_CONFIG_LINKS([src/Jit/jit.c:src/Jit/Arch/i386/jit.c])
AC_MSG_NOTICE([Jit is for $target_cpu])

AC_SUBST(target_cpu)
AC_SUBST(cpu)

dnl LIGHTNING_CONFIGURE_IF_NOT_FOUND

dnl LIGHTNING_CONFIGURE_LINKS(
dnl  [AC_DEFINE_UNQUOTED(LIGHTNING_TARGET, [AS_TR_CPP([$LIGHTNING_TARGET])],
dnl		      [Used to pick the appropriate disassembler, for debugging])],
dnl  [AC_MSG_ERROR([cpu $target_cpu not supported])])

dnl AM_CONDITIONAL(LIGHTNING_MAIN, :)

dnl check for sizes of words
AC_CACHE_VAL(_cv_word16,
[
AC_CHECK_SIZEOF(long,4)
AC_CHECK_SIZEOF(int,4)
AC_CHECK_SIZEOF(short int,2)

if test $ac_cv_sizeof_int = 2; then
  _cv_word16="int"
elif test $ac_cv_sizeof_short_int = 2; then
  _cv_word16="short int"
elif test $ac_cv_sizeof_long = 2; then
  _cv_word16="long"
else
  AC_MSG_ERROR([cant determine a 16 bit int type])
fi
])
AC_DEFINE_UNQUOTED(int16,$_cv_word16,[Define with a type that is 16 bits long])
AC_DEFINE_UNQUOTED(uint16,unsigned $_cv_word16,[Define with a type that is 16 bits long])

AC_CACHE_VAL(_cv_word32,
[
AC_CHECK_SIZEOF(long,4)
AC_CHECK_SIZEOF(int,4)
AC_CHECK_SIZEOF(short int,2)

if test $ac_cv_sizeof_int = 4; then
  _cv_word32="int"
elif test $ac_cv_sizeof_short_int = 4; then
  _cv_word32="short int"
elif test $ac_cv_sizeof_long = 4; then
  _cv_word32="long"
else
  AC_MSG_ERROR([cant determine a 32 bit int type])
fi
])
AC_DEFINE_UNQUOTED(int32,$_cv_word32,[Define with a type that is 32 bits long])
AC_DEFINE_UNQUOTED(uint32,unsigned $_cv_word32,[Define with a type that is 32 bits long])

AC_MSG_CHECKING([32 bit int type])
AC_MSG_RESULT($_cv_word32)
 
AC_CACHE_VAL(_cv_word64,
[
AC_CHECK_SIZEOF(long long,8)
AC_CHECK_SIZEOF(long,4)

if test $ac_cv_sizeof_long_long = 8; then
  _cv_word64="long long"
elif test $ac_cv_sizeof_long = 8; then
  _cv_word64="long"
else
  AC_MSG_ERROR([cant determine a 64 bit int type])
fi
])
AC_DEFINE_UNQUOTED(int64,$_cv_word64,[Define with a type that is 64 bits long])
AC_DEFINE_UNQUOTED(uint64,unsigned $_cv_word64,[Define with a type that is 64 bits long])

AC_MSG_CHECKING([64 bit int type])
AC_MSG_RESULT($_cv_word64)

AC_DEFINE_UNQUOTED(POINTER_SIZE,sizeof(void *),[Define size of a pointer])

AC_MSG_CHECKING([if compiler allows zero array size])
save_cflags="$CFLAGS"
CFLAGS="$CFLAGS -Werror"
AC_TRY_COMPILE([],
[
struct {
  int one;
  char somemore[];
} foo;

foo.one = 1;
],
AC_DEFINE(ZEROARRAYSIZE,, [How to specify a zero array size])
AC_MSG_RESULT(no),
AC_DEFINE(ZEROARRAYSIZE, 0, [How to specify a zero array size])
AC_MSG_RESULT(yes))
CFLAGS="$save_cflags"

AC_CONFIG_FILES([
  Makefile
  src/Makefile
  src/Headers/Makefile
  src/Memory/Makefile
  src/Util/Makefile
  src/Util/UnicodeStuff/Makefile
  src/Lib/Makefile
  src/Lib/Headers/Makefile
  src/Asm/Makefile
  src/Engine/Headers/Makefile
  src/Engine/Makefile
  src/Compiler/Headers/Makefile
  src/Compiler/Makefile
  src/Tests/Makefile
])
AC_OUTPUT
