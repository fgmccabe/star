#ifndef _CODE_P_H_
#define _CODE_P_H_

/*
 * Private header for the code format structure
 */

#include "code.h"
#include "termP.h"
#include "heapP.h"
#include "jit.h"

#include <assert.h>

typedef struct instruction_ {
  OpCode op;
  int32 fst;            // First integer operand
  union {
    int32 alt;          // but some may also have a second operand
    insPo exit;
    blockPo block;      // sub-block
  } snd;                // second operand
} Instruction;

typedef struct block_ {
  integer insCount;     // How many instructions in the block
  Instruction ins[ZEROARRAYSIZE];
} InstructionBlock;

blockPo allocateCodeBlock(integer insCount);

typedef struct method_ {
  clssPo clss;          // == specialClass
  jitCode jit;          /* Pointer to jit'ed code */
  integer entryCount;

  integer arity;        /* How many arguments in method */
  integer lclcnt;       // How many locals in the environment
  integer stackDelta;   // How much space to allocate for the stack
  normalPo pool;        /* A pool tuple of constants */
  normalPo locals;      /* A tuple of sorted locals */
  normalPo lines;       // A tuple of line information
  blockPo block;        // a block of instructions
} MethodRec;

extern clssPo methodClass;

#define MtdCellCount CellCount(sizeof(MethodRec))

// These are needed during GC
extern methodPo haltMethod;
methodPo underflowMethod;
methodPo newFiberMethod;
methodPo newTaskMethod;
methodPo spawnMethod;

static inline logical isMethod(termPo m) {
  return hasClass(m, methodClass);
}

static inline insPo entryPoint(methodPo mtd) {
  assert(mtd != Null);
  return mtd->block->ins;
}

static inline blockPo entryBlock(methodPo mtd){
  return mtd->block;
}

static inline int64 argCount(methodPo mtd) {
  assert(mtd != Null);
  return mtd->arity;
}

static inline void incEntryCount(methodPo mtd) {
  mtd->entryCount++;
}

static inline logical hasJit(methodPo mtd) {
  assert(mtd != Null);
  return mtd->jit != Null;
}

static inline jitCode codeJit(methodPo mtd) {
  assert(mtd != Null && mtd->jit != Null);
  return mtd->jit;
}

retCode setJitCode(methodPo mtd, jitCode code);

labelPo mtdLabel(methodPo mtd);

extern retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt);
extern logical validPC(methodPo mtd, insPo pc);

extern logical pcInBlock(blockPo block, insPo pc);

methodPo defineMtd(heapPo H, blockPo block, integer lclCount, integer stackDelta, labelPo lbl, normalPo pool,
                   normalPo locals, normalPo lines);

methodPo declareMethod(const char *name, integer arity, insPo ins, integer insCount);

void showMtdCounts(ioPo out);

blockPo allocateBlock(integer insCount, blockPo parent, integer offset);

void freeBlock(blockPo block);
#endif
