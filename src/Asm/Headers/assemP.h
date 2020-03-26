#ifndef _ASSEM_P_H_
#define _ASSEM_P_H_

#include <ooio.h>
#include "assem.h"
#include "opcodes.h"
#include "objectP.h"
#include "list.h"
#include "pkgP.h"

typedef struct _fixup_ *fixupPo;

typedef struct _label_ {
  char *lbl;
  void *address;      /* Used for absolute addresses */
  assemInsPo pc;      /* Which instruction */
} LabelRec;

typedef struct _assem_struct_ {
  const char *name;       /* The name of this structure */
  integer arity;
} StrctLbl, *strctPo;

typedef struct _import_struct_ {
  PackageRec pkg;
  logical isPublic;
} ImportRec, *importPo;

extern integer strctHash(strctPo st);

extern comparison strctComp(strctPo st1, strctPo st2);

/* Set up a class structure for pool constants */
typedef struct _const_record_ *constPo;

typedef logical (*constCmp)(constPo, char *sig, void *con);
typedef retCode (*constDump)(ioPo, constPo);

typedef union {
  double dx;        /* float value */
  mtdPo mtd;        /* a method */
  StrctLbl strct;      /* A term structure name */
  integer ix;        /* integer value */
  char *txt;      /* String */
} ConValue;

typedef struct {
  ConValue value;   /* What is this constant? */
  char *sig;        /* Type signature */
  constCmp same;      /* Used to test for equality */
  constDump show;      /* Used to display constant */
  constDump encode;      /* Used to encode */
} ConstObjectRec;

typedef struct _const_record_ {
  ObjectRec object;                     /* object level of the structure */
  ConstObjectRec con;                   // Constant part of object
} ConstRecord;

typedef struct {
} ConstClassPart;

typedef struct _const_class_ {
  ObjectClassRec objectPart;
  ConstClassPart constPart;
} ConstClassRec;

extern ConstClassRec ConstClass;
extern classPo constClass;

extern constPo newConstant(char *sig, constCmp same, constDump show, constDump encode, ConValue *value);

#ifdef VERIFY_OBJECT
#define O_CONST(c) ((constPo)(checkCast((c),constClass)))
#else
#define O_CONST(c) ((constPo)(c))
#endif

/* Set up a class structure for line number references */

typedef struct {
  int32 locRef;
  lPo lbl;           /* Where in the code */
} LineObjectRec;

typedef struct _line_record_ {
  ObjectRec object;                     /* object level of the structure */
  LineObjectRec line;                   // Line part of object
} LineRecord;

typedef struct {
} LineClassPart;

typedef struct _line_class_ {
  ObjectClassRec objectPart;
  LineClassPart linePart;
} LineClassRec;

extern LineClassRec LineClass;
extern classPo lineClass;

#ifdef VERIFY_OBJECT
#define O_LINE(c) ((linePo)(checkCast((c),lineClass)))
#else
#define O_LINE(c) ((linePo)(c))
#endif

typedef struct _assem_method_ {
  StrctLbl name;      /* The name of this method */
  integer lclCount;
  hashPo labels;      /* All the labels in this code */
  assemInsPo first;      /* Instructions */
  assemInsPo last;      /* Last instruction */
  arrayPo constants;      /* list of constant records */
  int32 sig;        /* Signature of this method */
  int32 lclSig;         /* Signature of the local vars */
  hashPo locals;      /* Local variables */
  hashPo frames;      /* Active frames in the method */
  arrayPo lines;       // Line number table
} AssemMethod;

typedef struct _local_data_ {
  char *name;        /* name of variable */
  int32 sig;        /* signature of variable */
  int32 off;
  lPo from;        /* start label */
  lPo to;        /* end label */
} LocalVarRecord;

typedef struct _assem_instruction_ {
  int pc;        /* program counter */
  OpCode op;        /* The opcode of the instruction */
  int64 i;        /* most operands are integer */
  lPo lbl;        /* they may be a label somewhere */
  char *txt;            // There may be a reference to a literal string (escape)
  assemInsPo next;      /* Next instruction in sequence */
} AssemInstruction;

typedef struct _assem_package_ {
  PackageRec pkg;
  char *signature;    // The package signature
  hashPo imports;     /* All the imports in this package */
  hashPo methods;     /* All the functions defined in this package */
} AssemPackage;

extern retCode encodePkg(ioPo out, pkPo pkg);
extern mtdPo getPkgMethod(pkPo pkg, const char *name, integer arity);
extern mtdPo createMethod(pkPo pkg, char *name, char *sig);

extern retCode dumpIns(ioPo f, mtdPo mtd, assemInsPo ins);
extern int32 insCount(mtdPo mtd);
extern int32 codeCount(mtdPo mtd);
extern int64 poolCount(mtdPo mtd);
extern int32 frameCount(mtdPo mtd);
extern constPo poolConstant(mtdPo mtd, int64 ix);

static inline int32 localCount(mtdPo mtd) {
  return (int32) hashSize(mtd->locals);
}

#endif
