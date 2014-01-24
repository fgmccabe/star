#ifndef _ERRORS_H_
#define _ERRORS_H_

#include "config.h"
#include <ooio.h>
#include <stdarg.h>

retCode reportError(int line, char *msg,...);
retCode reportWarning(int line, char *msg,...);

logical isErrorFree();
void reportErrorCount();

#endif
