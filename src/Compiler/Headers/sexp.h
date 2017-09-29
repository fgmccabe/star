/*
 * s-exp definitions.
 *
 * The s-expression interface has a C interface, but the values constructed are
 * also first-class cafe values. This takes a little juggling to get right.
 */
#ifndef _SEXP_H_
#define _SEXP_H_

#include "location.h"

/*
 * The sexp type has definition:
 *
 * type $Exp is
 *   $Id($loc string)
 *   $Char($loc char)
 *   $Str($loc string)
 *   $Int($loc integer)
 *   $Long($loc long)
 *   $Float($loc float)
 *   $Apply($loc $Exp $List($Exp)))
 *
 * 
 * With the $loc type being defined as
 *
 * type($loc
 *    $Loc(string integer integer integer))
 */

typedef struct _sexp_object_ *sxPo;
typedef struct _sequence_object_ *lxPo;

extern lxPo nil;

extern sxPo mId(locationPo loc,char *name);
extern sxPo mChar(locationPo loc,char ch);
extern sxPo mStr(locationPo loc,char *str);
extern sxPo mInt(locationPo loc,int i);
extern sxPo mLong(locationPo loc,integer i);
extern sxPo mFloat(locationPo loc,double d);
extern sxPo mApply(locationPo loc,sxPo op,lxPo args);

extern lxPo mCons(sxPo head,lxPo tail);
extern lxPo mEmptySeq();
extern sxPo sxHead(lxPo lx);
extern lxPo sxTail(lxPo lx);

extern logical sxIsIden(sxPo sx);
extern logical sxIsChar(sxPo sx);
extern logical sxIsStr(sxPo sx);
extern logical sxIsInt(sxPo sx);
extern logical sxIsLong(sxPo sx);
extern logical sxIsFloat(sxPo sx);
extern logical sxIsApply(sxPo sx);

extern locationPo sxLoc(sxPo sexp);

extern char sxChar(sxPo sx);
extern integer sxInt(sxPo sx);
extern integer sxLong(sxPo sx);
extern double sxFloat(sxPo sx);
extern char* sxIden(sxPo sx);
extern char* sxText(sxPo sx);

extern sxPo sxTerm(locationPo loc,char *op,...);

extern sxPo sxOp(sxPo sx);
extern lxPo sxArgs(sxPo sx);

extern sxPo setOp(sxPo sx,sxPo op);
extern sxPo setArgs(sxPo sx,lxPo args);
extern sxPo sxArg(sxPo sx,int ix);
extern sxPo sxTerm(locationPo loc,char *op,...);

extern lxPo setTail(lxPo lx,lxPo tail);
extern logical sxIsList(lxPo sx,int len);
extern sxPo sxEl(lxPo sq,int ix);
extern int sxLength(lxPo sx);

extern void initSexpressions();
extern retCode testSexpr(char *name);

extern logical sxIsUnary(sxPo sx,char *name);
extern logical sxIsBinary(sxPo sx,char *name);
extern logical sxIsTernary(sxPo sx,char *name);

extern sxPo sxUnary(locationPo loc,char *name,sxPo arg);
extern sxPo sxBinary(locationPo loc,char *name,sxPo left,sxPo right);
extern sxPo sxTernary(locationPo loc,char *name,sxPo left,sxPo mdl,sxPo right);

#endif
