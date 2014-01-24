/*
 * Private definitions for dictionary
 */
#ifndef _DICTP_H_
#define _DICTP_H_

#include "dict.h" // Public interface for dictionary
#include "hash.h"

typedef struct _dict_ {
  hashPo vars;
  hashPo constructors;
  hashPo types;
  long nextLocal;
  long localsSize;
  long freeSize;
  int floats;
  dictPo parent;
  dictStatePo undoState;
} DictRecord;

typedef enum {
  ConstructorDef, VarDef
} VarDefType;

typedef struct _dict_state_ {
  uniChar *name;
  dictPo dict;
  VarDefType entryType;
  dictStatePo prev;
} DictUndoRec;
  
#endif
