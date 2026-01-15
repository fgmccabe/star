//
// Created by Francis McCabe on 1/15/26.
//

#ifndef STAR_SSAEMIT_H
#define STAR_SSAEMIT_H

#include "escape.h"
#include "ssa.h"

typedef struct ssa_buffer_ *ssaBufferPo;

retCode emitHalt(ssaBufferPo b, varDescPo v);
retCode emitAbort(ssaBufferPo b, termPo l, varDescPo v);

retCode emitCall(ssaBufferPo bf, int32 sym, varDescPo vs);
retCode emitOCall(ssaBufferPo bf, int32 arity, varDescPo d, varDescPo ss);
retCode emitEscape(ssaBufferPo bf, escapePo es, varDescPo vs);
retCode emitXCall(ssaBufferPo bf, int32 sym, int32 lvl, varDescPo vs);
retCode emitXOCall(ssaBufferPo bf, int32 arity, int32 lvl, varDescPo d, varDescPo ss);
retCode emitXEscape(ssaBufferPo bf, escapePo es, int32 lvl, varDescPo vs);
retCode emitTCall(ssaBufferPo bf, int32 sym, varDescPo vs);
retCode emitTOCall(ssaBufferPo bf, int32 arity, varDescPo d, varDescPo ss);
retCode emitEntry(ssaBufferPo bf, int32 cnt);

retCode emitRet(ssaBufferPo bf, varDescPo v);
retCode emitXRet(ssaBufferPo bf, varDescPo v);

retCode emitBlock(ssaBufferPo bf, int32 cnt, ssaInsPo bLk);
retCode emitValof(ssaBufferPo bf, int32 cnt, ssaInsPo bLk, varDescPo v);
retCode emitBreak(ssaBufferPo bf, int32 lvl);
retCode emitResult(ssaBufferPo bf, int32 lvl, varDescPo v);
retCode emitLoop(ssaBufferPo bf, int32 lvl);

retCode emitFiber(ssaBufferPo bf, varDescPo v);
retCode emitSuspend(ssaBufferPo bf, varDescPo d, varDescPo s);
retCode emitResume(ssaBufferPo bf, varDescPo d, varDescPo s);
retCode emitRetire(ssaBufferPo bf, varDescPo d, varDescPo s);
retCode emitUnderflow(ssaBufferPo bf);

retCode emitMvV(ssaBufferPo bf,varDescPo v);
retCode emitMvC(ssaBufferPo bf, varDescPo v, termPo l);
retCode emitMv(ssaBufferPo bf, varDescPo d, varDescPo s);

retCode emitMvG(ssaBufferPo bf, varDescPo v, int32 glb);
retCode emitStG(ssaBufferPo bf, int32 glb, varDescPo v);

retCode emitSav(ssaBufferPo bf, varDescPo v);
retCode emitLdSav(ssaBufferPo bf, varDescPo d, int32 lvl, varDescPo s);
retCode emitTstSav(ssaBufferPo bf, varDescPo d, varDescPo st);
retCode emitStSav(ssaBufferPo bf, varDescPo d, varDescPo s);

retCode emitCell(ssaBufferPo bf, varDescPo v);
retCode emitGet(ssaBufferPo bf, varDescPo d, varDescPo s);
retCode emitAssign(ssaBufferPo bf, varDescPo d, varDescPo s);

retCode emitCLbl(ssaBufferPo bf, int32 sym, int32 lvl, varDescPo v);
retCode emitCInt(ssaBufferPo bf, termPo l, int32 lvl, varDescPo v);
retCode emitCChar(ssaBufferPo bf, termPo l, int32 lvl, varDescPo v);
retCode emitCFlt(ssaBufferPo bf, termPo l, int32 lvl, varDescPo v);
retCode emitCLit(ssaBufferPo bf, termPo l, int32 lvl, varDescPo v);
retCode emitNth(ssaBufferPo bf, varDescPo d, int32 off, varDescPo s);
retCode emitStNth(ssaBufferPo bf, varDescPo d, int32 off, varDescPo s);

retCode emitIf(ssaBufferPo bf, int32 lvl,varDescPo v);
retCode emitIfNot(ssaBufferPo bf, int32 lvl, varDescPo v);

retCode emitICase(ssaBufferPo bf, varDescPo v, int32 lvls);
retCode emitCase(ssaBufferPo bf, varDescPo v, int32 lvls);
retCode emitIxCase(ssaBufferPo bf, varDescPo v, int32 lvls);

retCode emitIAdd(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitISub(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitIMul(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitIDiv(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitIMod(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitIAbs(ssaBufferPo bf, varDescPo d, varDescPo s);

retCode emitIEq(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitILt(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitIGe(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);

retCode emitCEq(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitCLt(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitCGe(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);

retCode emitBAnd(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitBOr(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitBXor(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitBLsl(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitBLsr(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitBAsr(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitBNot(ssaBufferPo bf, varDescPo d, varDescPo s);

retCode emitFAdd(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitFSub(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitFMul(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitFDiv(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitFMod(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitFAbs(ssaBufferPo bf, varDescPo d, varDescPo s);

retCode emitFEq(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitFLt(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);
retCode emitFGe(ssaBufferPo bf, varDescPo d, varDescPo s1, varDescPo s2);

retCode emitAlloc(ssaBufferPo bf, varDescPo v, int32 sym,varDescPo vs);
retCode emitClosure(ssaBufferPo bf, varDescPo d, int32 sym, varDescPo s);

retCode emitLine(ssaBufferPo bf, termPo l);
retCode emitBind(ssaBufferPo bf, termPo l, varDescPo v);
retCode emitdBug(ssaBufferPo bf, termPo l);

#endif //STAR_SSAEMIT_H