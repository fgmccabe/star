/*
 * Error reporting module
 */
#include "config.h"
#include "compiler.h"

#include "file.h"
#include "formioP.h"
#include <stdarg.h>

static int errorCount = 0;
static int warningCount = 0;

retCode reportError(locationPo loc,char *fmt,...)
{
  va_list args;
  va_start(args,fmt);
  outMsg(logFile,"error: %L\n",loc);
  __voutMsg(logFile,(unsigned char*)fmt,args);
  outStr(logFile,"\n");
  va_end(args);
  errorCount++;
  return Error;
}

retCode reportWarning(locationPo loc,char *fmt,...)
{
  va_list args;
  va_start(args,fmt);
  outMsg(logFile,"warning: %L\n",loc);
  __voutMsg(logFile,(unsigned char*)fmt,args);
  outStr(logFile,"\n");
  va_end(args);
  warningCount++;
  return Ok;
}

logical isErrorFree()
{
  return errorCount==0;
}

void reportErrorCount()
{
  outMsg(logFile,"%d errors, %d warnings\n",errorCount,warningCount);
}
