#ifndef _CAFE_OPTIONS_H_
#define _CAFE_OPTIONS_H_

#include <ooio.h>
#include "config.h"

#ifdef ALLTRACE
#define TRACECODEGEN
#endif

#ifdef TRACECODEGEN
extern logical debugCodeGen;
#endif

extern logical compileOnly;
extern logical parseOnly;

#endif
