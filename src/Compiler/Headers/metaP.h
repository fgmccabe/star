#ifndef _META_P_H_
#define _META_P_H_

#include "meta.h"

// Keywords in the meta-language
extern char *kwImport;

extern char *kwType,*kwArrow;

extern char *kwFunction, *kwProcedure, *kwPattern, *kwLet;

extern char *kwIs, *kwVar;

extern char *kwAssign, *kwColon;

extern char *kwLabel, *kwLeave, *kwGoto;

extern char *kwContinue, *kwSwitch;

extern char *kwPlus, *kwMinus, *kwTimes, *kwDivide, *kwRemainder;
extern char *kwShiftLeft, *kwShiftRight;

void initMeta();

/*#include "heapP.h"

typedef struct _constructor_spec_ {
  long conIx;
  long fill;
  scavengerPo scavenger;
  evac evacuator;
  char [] name;
} ConstructorSpecifier;

typedef struct _constructor_ {
  conSpecPo specifier;
} Constructor;
*/

#endif
