/*
 * Public interface for dictionary
 */
#ifndef _DICT_H_
#define _DICT_H_

#include "location.h"
#include "sexp.h"
#include "assem.h"

typedef struct _dict_ *dictPo;
typedef struct _varinfo_ *varInfoPo;
typedef struct _dict_state_ *dictStatePo;

// The read-write model for variables
typedef enum { readOnly, readWrite} rwMode;

// The different places data can be found
typedef enum {basedVar, registr, literal, label, fixed } varSource;

// The kind of value
typedef enum {ptrTerm, rawChar, rawInt, rawLong, rawFloat } sourceKind;

extern char *sourceName(varSource where);
extern char *kindName(sourceKind kind);
extern char *accessName(rwMode access);

typedef enum { argReg, lclReg, freeReg} RegisterClass;

typedef struct {
  RegisterClass regCl;
  short regNo;
} Register;

typedef struct _varinfo_ {
  locationPo loc;
  char *name;
  sxPo type;
  rwMode access;
  Register base;
  sourceKind kind;
  varSource where;
  union{
    Register reg;			/* If the value is in a register */
    int off;				/* or is based off of the base */
    lPo lit;				/* or has a known address */
    integer ix;				/* or is a literal integer */
    double d;				/* or is a literal float */
    char *str;			/* or is a literal string */
    void *bx;				/* or is an arbitrary value */
  } l;
  logical inited;
} VarInfoRec;

typedef struct _con_def_ {
  char *name;
  lPo lbl;
  lxPo args;
  sxPo type;
  int conIx;
  int maxIx;
  long conSize;
} ConstructorRecord, *conDefPo;

typedef struct _type_def_ {
  char *name;
  sxPo typeSpec;
  hashPo fields;
} TypeDefRecord, *typeDefPo;

typedef struct _field_spec_ {
  char *name;			/* name of the field */
  sxPo type;
} FieldSpecRecord, *fieldPo;

extern void initDict();
extern dictPo rootDict;

extern varInfoPo search(char *name, dictPo dict);

extern varInfoPo varReference(char *name,dictPo dict);

extern varInfoPo reserve(locationPo loc,char *name, sxPo type, rwMode access,
			 logical isLocal, sourceKind kind,dictPo dict);

extern void declareVar(varInfoPo var,dictPo dict);

extern varInfoPo declare(locationPo loc,char *name, sxPo type,
			 rwMode access, logical isLocal, sourceKind kind,
			 int offset,
			 dictPo dict);

extern void declareLit(locationPo loc,char *name,char *str,
		       sxPo type,sourceKind kind,dictPo dict);

extern void declareInfo(varInfoPo info,char *str,dictPo dict);

typedef retCode (*dictProc)(char *name,varInfoPo var,void *cl);

extern retCode processDict(dictPo dict,dictProc proc,void *cl);

extern void dDict(dictPo dict);

extern char* vrInfName(varInfoPo var);
extern varSource vrInfSource(varInfoPo var);
extern sourceKind vrInfKind(varInfoPo var);
extern logical isVrLocal(varInfoPo var);
extern logical isVrFree(varInfoPo var);
extern int vrInfOffset(varInfoPo var);
extern sxPo vrInfType(varInfoPo var);
extern locationPo vrLoc(varInfoPo var);

extern logical isConstructor(char *name,dictPo dict);
extern conDefPo findConstructor(char *name,dictPo dict);
extern logical isEnumerated(char *name,dictPo dict);
extern lPo findEnumerated(char *name,dictPo dict);

extern typeDefPo findType(char *name,dictPo dict);
extern fieldPo findFieldSpec(char *name,typeDefPo desc);

extern typeDefPo declareType(char *name,sxPo type,dictPo dict);
extern fieldPo declareField(char *name,sxPo fieldType,typeDefPo type);

extern conDefPo declareConstructor(char *name,int conIx,int maxIx,
				   long conSize,
				   lxPo args,
				   sxPo type,
				   lPo con,dictPo dict);
extern lxPo conDefArgs(conDefPo con);

extern dictPo newDict();
extern dictPo funDict(long localOffset,dictPo outer);
extern dictPo forkDict(dictPo outer);
extern retCode mergeDict(dictPo dict,dictPo source);
extern dictPo parentDict(dictPo dict);

extern dictStatePo dictState(dictPo dict);
extern void resetDict(dictPo dict,dictStatePo mark);

extern void setLocalsOffset(dictPo dict,long offset);
extern void popDict(dictPo dict);

extern long localsSize(dictPo dict);
extern long localDepth(dictPo dict);
extern long freeSize(dictPo dict);

extern char *ArithOverFlow;
extern char *ArithZeroDivide;
extern char *TrueName;
extern char *FalseName;
extern char *ValofName;
extern char *CatchName;
extern char *TailName;
extern char *ColonName;
extern char *QuoteName;
extern char *EvalName;
extern char *MainName;
extern char *NilName;
extern char *VoidName;

extern char *PkgDictVarName;

extern char *EqualName;
extern char *NotEqualName;
extern char *LessEqualName;
extern char *LessName;
extern char *GreaterEqualName;
extern char *GreaterName;

extern char *ANONYMOUS;

extern char *Nothing;


#endif
