#ifndef _META_P_H_
#define _META_P_H_

#include "meta.h"

// Keywords in the meta-language
extern uniChar *kwImport;

extern uniChar *kwType,*kwArrow;

extern uniChar *kwFunction, *kwProcedure, *kwPattern, *kwLet;

extern uniChar *kwIs, *kwVar;

extern uniChar *kwAssign, *kwColon;

extern uniChar *kwLabel, *kwLeave, *kwGoto;

extern uniChar *kwContinue, *kwSwitch;

extern uniChar *kwPlus, *kwMinus, *kwTimes, *kwDivide, *kwRemainder;
extern uniChar *kwShiftLeft, *kwShiftRight;

void initMeta();

/*#include "heapP.h"

typedef struct _constructor_spec_ {
  long conIx;
  long fill;
  scavengerPo scavenger;
  evac evacuator;
  uniChar [] name;
} ConstructorSpecifier;

typedef struct _constructor_ {
  conSpecPo specifier;
} Constructor;
*/

#endif
