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

extern void initAssem();
extern pkgPo newPkg(uniChar *name);
extern void dumpPkgCode(pkgPo pkg);

extern int32 findMethod(mtdPo mtd,uniChar *name);
extern mtdPo defineMethod(pkgPo pkg,uniChar *name,uniChar *sig,uniChar *freeSig);
extern uniChar *methodSignature(mtdPo mtd);
extern uniChar *freeSignature(mtdPo mtd);

extern void endFunction(mtdPo mtd);

extern lPo newLbl(mtdPo code,uniChar *name);
extern lPo currLbl(mtdPo code,uniChar *name);
extern void defineLbl(mtdPo code, lPo lbl);
extern logical labelDefined(lPo lbl);

extern logical isJumpIns(assemInsPo ins,lPo tgt);
extern assemInsPo endIns(mtdPo code);
extern void removeIns(mtdPo code,assemInsPo ins);

extern int32 newIntegerConstant(mtdPo mtd,int64 ix);
extern int32 newFloatConstant(mtdPo mtd,double dx);
extern int32 newStringConstant(mtdPo mtd,uniChar *str);
extern int32 newEscapeConstant(mtdPo mtd,uniChar *str);

extern void defineFrame(mtdPo mtd,uniChar *str);
extern void defineLocal(mtdPo mtd,uniChar *name,uniChar *sig,int32 off,
			lPo from,lPo to);

// Define the instruction functions themselves
#undef instruction

#define optos(X)
#define opnOp(X)
#define opi32(X) ,int32 i##X

#define oplcl(X) ,int32 i##X
#define openv(X) ,int32 i##X
#define oparg(X) ,int32 i##X
#define oplit(X) ,int32 i##X
#define opoff(X) ,lPo l##X
#define opEs(X) ,int32 f##X

#define instruction(Op,A1,A2,Cmt) \
extern assemInsPo A##Op(mtdPo code op##A1(1));

#include "instructions.h"

#undef instruction
#undef optos
#undef opnOp
#undef opi32
#undef oplcl
#undef oparg
#undef openv
#undef oplit
#undef opoff
#undef opEs

/*
 * Closure constants
 */

#define FIELD_OFFSET (-4*POINTER_SIZE)
#define GC_EVAC_OFFSET (-POINTER_SIZE)
#define GC_SCAV_OFFSET (-2*POINTER_SIZE)
#define GC_SCAN_OFFSET (-3*POINTER_SIZE)
#define CASE_OFFSET (-4*POINTER_SIZE)
#define CATCH_OFFSET (-4*POINTER_SIZE)
#define FORWARD_OFFSET (POINTER_SIZE)

#define FREE_OFFSET (POINTER_SIZE)
#define ARG_OFFSET (3*POINTER_SIZE)
#define ENV_ARG_OFFSET (2*POINTER_SIZE)
#define FIRST_ARG_OFFSET ARG_OFFSET
#define RTN_OFFSET POINTER_SIZE

#endif
