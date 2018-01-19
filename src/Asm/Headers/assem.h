#ifndef _ASSEM_H_
#define _ASSEM_H_

#include "config.h"
#include <ooio.h>

/*
 * Header for the Cafe assembler
 */

#include "signature.h"

typedef struct _assem_package_ *pkgPo;
typedef struct _assem_method_ *mtdPo;
typedef struct _label_ *lPo;
typedef struct _assem_instruction_ *assemInsPo;
typedef struct _local_data_ *localVarPo;

extern void initAssem();
extern pkgPo newPkg(char *name, char *version);
extern void dumpPkgCode(pkgPo pkg);

extern void addImport(pkgPo pkg, char *name, char *version, logical isPublic);

extern mtdPo defineMethod(pkgPo pkg, char *name, integer arity, char *sig);
extern char *methodSignature(mtdPo mtd);
extern int32 findMethod(mtdPo mtd, char *name, int arity);

extern void endFunction(mtdPo mtd);

extern lPo newLbl(mtdPo code,char *name);
extern lPo currLbl(mtdPo code,char *name);
extern void defineLbl(mtdPo code, lPo lbl);
extern logical labelDefined(lPo lbl);
extern char *lblName(lPo lbl);

extern logical isJumpIns(assemInsPo ins,lPo tgt);
extern assemInsPo endIns(mtdPo code);
extern void removeIns(mtdPo code,assemInsPo ins);

extern int32 newIntegerConstant(mtdPo mtd,int64 ix);
extern int32 newFloatConstant(mtdPo mtd,double dx);
extern int32 newStringConstant(mtdPo mtd,char *str);
extern int32 newStrctConstant(mtdPo mtd,char *str,integer ar);
extern int32 newPrgConstant(mtdPo mtd,char *str,integer ar);
extern int32 newEscapeConstant(mtdPo mtd,char *str);

extern retCode getStringConstant(mtdPo mtd,char **name);

extern void defineFrame(mtdPo mtd,char *str);
extern void defineLocal(mtdPo mtd, char *name, char *sig, lPo from, lPo to);
extern int32 findLocal(mtdPo mtd, const char *name);

// Define the instruction functions themselves
#undef instruction

#define opnOp(X)
#define opi32(X) ,int32 i##X
#define oplcl(X) ,int32 i##X
#define oparg(X) ,int32 i##X
#define oplit(X) ,int32 i##X
#define opoff(X) ,lPo l##X
#define opEs(X) ,int32 f##X

#define instruction(Op,A1,Cmt) \
extern retCode A##Op(mtdPo mtd op##A1(1));

#include "instructions.h"

#undef instruction
#undef opnOp
#undef opi32
#undef oplcl
#undef oparg
#undef oplit
#undef opoff
#undef opEs

#endif
