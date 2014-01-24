/*
 * Error reporting module
 */

#include "asm.h"
#include "formioP.h"
#include "errors.h"

static int errorCount = 0;
static int warningCount = 0;

retCode reportError(int line, char *msg,...)
{
  va_list args;
  va_start(args,msg);
  outMsg(logFile,"error: %d\n",line);
  __voutMsg(logFile,(unsigned char*)msg,args);
  outStr(logFile,"\n");
  va_end(args);
  errorCount++;
  return Error;
}

retCode reportWarning(int line, char *msg,...)
{
  va_list args;
  va_start(args,msg);
  outMsg(logFile,"warning: %d\n",line);
  __voutMsg(logFile,(unsigned char*)msg,args);
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
  if(errorCount!=0 || warningCount!=0)
    outMsg(logFile,"%d errors, %d warnings\n",errorCount,warningCount);
}
