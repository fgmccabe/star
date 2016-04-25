#ifndef _ASSEM_P_H_
#define _ASSEM_P_H_

#include <ooio.h>
#include "assem.h"
#include "opcodes.h"
#include "codeP.h"
#include "list.h"

typedef struct _fixup_ *fixupPo;

typedef struct _local_data_ *localVarPo;

typedef struct _label_ {
  uniChar *lbl;
  void *address;			/* Used for absolute addresses */
  assemInsPo pc;			/* Which instruction */
} LabelRec;

typedef struct _pool_constant_ *constPo;
typedef logical (*constCmp)(constPo,uniChar *sig,void *con);
typedef retCode (*constDump)(ioPo,constPo);

typedef struct _pool_constant_ {
  union{
    double dx;				/* float value */
    mtdPo mtd;				/* a method */
    integer ix;				/* integer value */
    uniChar *txt;			/* String */
  } value;				/* What is this constant? */
  uniChar *sig;				/* Type signature */
  constCmp same;			/* Used to test for equality */
  constDump show;			/* Used to display constant */
  constDump encode;			/* Used to encode */
} PoolConstantRecord;

typedef struct _assem_method_ {
  uniChar *name;			/* The name of this code block */
  hashPo labels;			/* All the labels in this code */
  assemInsPo first;			/* Instructions */
  assemInsPo last;			/* Last instruction */
  listPo constants;			/* list of constant records */
  pkgPo owner;				/* Owning package */
  int32 sig;				/* Signature of this method */
  int32 freeSig;			/* Signature of free variables */
  listPo locals;			/* Local variables */
  listPo frames;      /* Active frames in the method */
} AssemMethod;

typedef struct _local_data_ {
  int32 name;				/* name of variable */
  int32 sig;				/* signature of variable */
  int32 off;
  lPo from;				/* start label */
  lPo to;				/* end label */
} LocalVarRecord;
  
typedef struct _assem_instruction_ {
  int pc;				/* program counter */
  OpCode op;				/* The opcode of the instruction */
  int64 i;				/* most operands are integer */
  lPo lbl;				/* they may be a label somewhere */
  assemInsPo next;			/* Next instruction in sequence */
} AssemInstruction;

typedef struct _assem_package_ {
  uniChar *name;		 /* name of the package */
  listPo methods;		 /* All the functions defined in this package */
} AssemPackage;

extern retCode encodePkg(ioPo out,pkgPo pkg);
extern mtdPo getPkgMethod(pkgPo pkg,uniChar *name);
extern mtdPo createMethod(pkgPo pkg,uniChar *name,uniChar *sig);

extern retCode dumpIns(ioPo f,mtdPo mtd,assemInsPo ins);
extern int32 codeSize(mtdPo mtd);
extern int32 poolCount(mtdPo mtd);
extern int32 frameCount(mtdPo mtd);
extern constPo poolConstant(mtdPo mtd,int32 ix);

static inline int32 localCount(mtdPo mtd)
{
  return listCount(mtd->locals);
}

#endif
