//
// Created by Francis McCabe on 5/22/25.
//

#ifndef EVALP_H
#define EVALP_H

#include <arithP.h>
#include "engineP.h"
#include "debugP.h"
#include "cellP.h"
#include "closureP.h"
#include "singleP.h"


#ifdef TRACESTACK
#define checkAlloc(Count) STMT_WRAP({  \
  if (reserveSpace(H, Count) != Ok) {  \
    saveRegisters();                   \
    retCode ret = gcCollect(H, Count); \
    if (ret != Ok)                     \
      return ret;                      \
    if (traceStack > noTracing){       \
      verifyStack(P->stk, H);             \
      verifyHeap(H);                   \
    }                                  \
    restoreRegisters();                \
    check(reserveSpace(H,Count)==Ok,"could not reserve space");\
  }                                    \
})
#else
#define checkAlloc(Count) STMT_WRAP({  \
  if (reserveSpace(H, Count) != Ok) {  \
    saveRegisters();                   \
    retCode ret = gcCollect(H, Count); \
    if (ret != Ok)                     \
      return ret;                      \
    restoreRegisters();                \
    check(reserveSpace(H,Count)==Ok,"could not reserve space");\
  }                                    \
})
#endif

#define pop() (*SP++)
#define top() (*SP)
#define push(T) STMT_WRAP({*--SP=(termPo)(T);})
#define local(lcl) (ARGS[-(lcl)])
#define arg(aix) (ARGS[aix])
#define stackRoom(amnt) (SP - (amnt) > ((ptrPo)(FP+1)))
#define saveRegisters() STMT_WRAP({ \
  STK->prog = PROG;                 \
  STK->pc = PC;                     \
  STK->sp = SP;                     \
  STK->args = ARGS;                 \
  STK->fp = FP;                     \
  })
#define restoreRegisters() STMT_WRAP({ \
  STK = P->stk;                        \
  FP = STK->fp;                        \
  PROG = STK->prog;                    \
  ARGS = STK->args;                    \
  PC = STK->pc;                        \
  SP = STK->sp;                        \
  })

#define bail() STMT_WRAP({\
  saveRegisters();\
  stackTrace(P, logFile, STK,displayDepth,showLocalVars,100);\
  return Error;\
  })

#define stackGrow(Amnt, SaveArity) STMT_WRAP({\
  saveRegisters();\
  P->stk = glueOnStack(H, STK, maximum(stackHwm(STK),(STK->sze * 3) / 2 + (Amnt)),SaveArity); \
  restoreRegisters();\
  if (!stackRoom(Amnt)) {\
    logMsg(logFile, "cannot extend stack sufficiently");\
    bail();\
  }\
  })

#define breakOut(EX) STMT_WRAP({                     \
  PC += PC->alt + 1;                                 \
  SP = &local(lclCount(PROG) + PC->fst - 1); \
  PC += PC->alt + 1;                                 \
  push(EX);                                          \
  })

#define breakBlock() STMT_WRAP({                     \
  PC += PC->alt + 1;                                 \
  PC += PC->alt + 1;                                 \
})

#endif //EVALP_H
