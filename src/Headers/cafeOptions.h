#ifndef _CAFE_OPTIONS_H_
#define _CAFE_OPTIONS_H_

#include "ooio.h"

#ifdef ALLTRACE
#define TRACECODEGEN
#endif

#ifdef TRACECODEGEN
extern logical debugCodeGen;
#endif

extern logical compileOnly;
extern logical parseOnly;

#ifndef MAX_SYMB_LEN
#define MAX_SYMB_LEN 1024
#endif

#define EXIT_SUCCEED 0    /* Normal exit */
#define EXIT_FAIL 1        /* Failing exit */

#endif
