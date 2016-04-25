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
typedef enum {general, rawChar, rawInt, rawLong, rawFloat } sourceKind;

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
  uniChar *name;
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
    uniChar *str;			/* or is a literal string */
    void *bx;				/* or is an arbitrary value */
  } l;
  logical inited;
} VarInfoRec;

typedef struct _con_def_ {
  uniChar *name;
  lPo lbl;
  lxPo args;
  sxPo type;
  int conIx;
  int maxIx;
  long conSize;
} ConstructorRecord, *conDefPo;

typedef struct _type_def_ {
  uniChar *name;
  sxPo typeSpec;
  hashPo fields;
} TypeDefRecord, *typeDefPo;

typedef struct _field_spec_ {
  uniChar *name;			/* name of the field */
  sxPo type;
} FieldSpecRecord, *fieldPo;

extern void initDict();
extern dictPo rootDict;

extern varInfoPo search(uniChar *name, dictPo dict);

extern varInfoPo varReference(uniChar *name,dictPo dict);

extern varInfoPo reserve(locationPo loc,uniChar *name, sxPo type, rwMode access,
			 logical isLocal, sourceKind kind,dictPo dict);

extern void declareVar(varInfoPo var,dictPo dict);

extern varInfoPo declare(locationPo loc,uniChar *name, sxPo type,
			 rwMode access, logical isLocal, sourceKind kind,
			 int offset,
			 dictPo dict);

extern void declareLit(locationPo loc,uniChar *name,uniChar *str,
		       sxPo type,sourceKind kind,dictPo dict);

extern void declareInfo(varInfoPo info,uniChar *str,dictPo dict);

typedef retCode (*dictProc)(uniChar *name,varInfoPo var,void *cl);

extern retCode processDict(dictPo dict,dictProc proc,void *cl);

extern void dDict(dictPo dict);

extern uniChar* vrInfName(varInfoPo var);
extern varSource vrInfSource(varInfoPo var);
extern sourceKind vrInfKind(varInfoPo var);
extern logical isVrLocal(varInfoPo var);
extern logical isVrFree(varInfoPo var);
extern int vrInfOffset(varInfoPo var);
extern sxPo vrInfType(varInfoPo var);
extern locationPo vrLoc(varInfoPo var);

extern logical isConstructor(uniChar *name,dictPo dict);
extern conDefPo findConstructor(uniChar *name,dictPo dict);
extern logical isEnumerated(uniChar *name,dictPo dict);
extern lPo findEnumerated(uniChar *name,dictPo dict);

extern typeDefPo findType(uniChar *name,dictPo dict);
extern fieldPo findFieldSpec(uniChar *name,typeDefPo desc);

extern typeDefPo declareType(uniChar *name,sxPo type,dictPo dict);
extern fieldPo declareField(uniChar *name,sxPo fieldType,typeDefPo type);

extern conDefPo declareConstructor(uniChar *name,int conIx,int maxIx,
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

extern uniChar *ArithOverFlow;
extern uniChar *ArithZeroDivide;
extern uniChar *TrueName;
extern uniChar *FalseName;
extern uniChar *ValofName;
extern uniChar *CatchName;
extern uniChar *TailName;
extern uniChar *ColonName;
extern uniChar *QuoteName;
extern uniChar *EvalName;
extern uniChar *MainName;
extern uniChar *NilName;
extern uniChar *VoidName;

extern uniChar *PkgDictVarName;

extern uniChar *EqualName;
extern uniChar *NotEqualName;
extern uniChar *LessEqualName;
extern uniChar *LessName;
extern uniChar *GreaterEqualName;
extern uniChar *GreaterName;

extern uniChar *ANONYMOUS;

extern uniChar *Nothing;


#endif
