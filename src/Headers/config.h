/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */
#ifndef CONFIG_H_
#define CONFIG_H_

/* Enable tracing insDebugging code to be compiled */
#define ALLTRACE /**/

/* Define to 1 if you have the `getdtablesize' function. */
#define HAVE_GETDTABLESIZE 1

/* Define to 1 if you have the `getrlimit' function. */
#define HAVE_GETRLIMIT 1

/* Name of package */
#define PACKAGE "star"

/* Version number of package */
#define VERSION "0.0"

/* How to specify a zero array size */
#define ZEROARRAYSIZE /**/

// Define a type that is one bit long
typedef  enum {zero=0u, one=1u} uint1;

/* Define a type that is 8bits long */
typedef unsigned char uint8;

typedef signed char int8;



/* Define with a type that is 16 bits long */
typedef short int int16;

/* Define with a type that is 32 bits long */
typedef int int32;

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

/* Define with a type that is 16 bits long */
typedef unsigned short uint16;

/* Define with a type that is 32 bits long */
typedef unsigned int uint32;

/* Define with a type that is 64 bits long */
typedef long long int64;

typedef unsigned long long uint64;

#define VERIFY_OBJECT

#define STARDIR "/opt/star/star-repo/"

#endif
