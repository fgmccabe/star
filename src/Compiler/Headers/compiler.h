/*
 * General header for compiler
 */

#ifndef _COMPILER_H_
#define _COMPILER_H_

#include "config.h"
#include "location.h"
#include <assert.h>

#include "cafeOptions.h"
#include "utils.h"

extern char copyRight[];

#ifndef Null
#define Null ((void*)0)
#endif

retCode reportError(locationPo loc,char *fmt,...);
retCode reportWarning(locationPo loc,char *fmt,...);
logical isErrorFree();
void reportErrorCount();

extern void setCafeHome(char *home);

#endif
