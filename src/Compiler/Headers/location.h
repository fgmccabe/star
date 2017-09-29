/*
 * Location entity. Used in error reporting and otherwise locating a source
 */
#ifndef _LOCATION_H_
#define _LOCATION_H_

#include "config.h"
#include "unicode.h"

// The public part of the location class interface

typedef struct _location_object_ *locationPo;

extern locationPo newLocation(char *name,int firstLine,int lastLine);
extern locationPo newLoc(char *name,int lineNumber,int start,int end);
extern locationPo mergeLocs(locationPo l1,locationPo l2);
#endif
