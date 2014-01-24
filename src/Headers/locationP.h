/*
 * Private header file for the location object
 */

#ifndef _LOCATION_P_H_
#define _LOCATION_P_H_

#include "location.h"

typedef struct _location_object_ {
  uniChar *fileName;			/* The file that this loc refers to */
  int firstLine;			/* The line number within the file */
  int lastLine;				/* Last line number  */
  int start;				/* starting character position */
  int end;				/* ending character position */
} LocationRec;

extern void initLocation();
#endif
