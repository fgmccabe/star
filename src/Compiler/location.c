/*
 * The implementation of the location object
 */

#include "config.h"
#include "compiler.h"
#include "Headers/locationP.h"

#include "hash.h"
#include "pool.h"
#include <ooio.h>
#include <formioP.h> // We are going to install an extension to outMsg

static poolPo locationPool = Null;
static retCode displayLocation(ioPo f,void *p,long width,long prec,logical alt);

void initLocation()
{
  locationPool = newPool(sizeof(LocationRec),1024);
  installMsgProc('L',displayLocation);	/* extend outMsg to cope with locations */
}

locationPo newLocation(char *name,int firstLine,int lastLine)
{
  if(locationPool == Null)
    initLocation();

  locationPo loc = (locationPo)allocPool(locationPool);

  loc->fileName = uniIntern(name);
  loc->firstLine = firstLine;
  loc->lastLine = lastLine;
  return loc;
}

locationPo newLoc(char *name,int lineNumber,int start,int end)
{
  locationPo loc = (locationPo)allocPool(locationPool);

  loc->fileName = uniIntern(name);
  loc->firstLine = lineNumber;
  loc->start = start;
  loc->end = end;
  return loc;
}

locationPo mergeLocs(locationPo l1,locationPo l2)
{
  return newLoc(l1->fileName,l1->firstLine,l1->start,l2->end);
}

static retCode displayLocation(ioPo f,void *p,long width,long prec,logical alt)
{
  locationPo loc = (locationPo)p;
  if(loc!=Null){
    if(loc->firstLine!=loc->lastLine)
      return outMsg(f,"%U:%d-%d",loc->fileName,loc->firstLine,loc->lastLine);
    else
      return outMsg(f,"%U:%ld",loc->fileName,loc->firstLine);
  }
  else
    return outMsg(f,"nowhere");
}

retCode dL(locationPo loc)
{
  outMsg(logFile,"%L\n",loc);
  flushOut();
  return Ok;
}
