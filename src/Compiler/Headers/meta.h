/*
 * Higher-level access to s-exp structures.
 */
#ifndef _META_H_
#define _META_H_

#include "sexp.h"
#include "type.h"
#include <ooio.h>

extern logical isSxPackage(sxPo term);

extern sxPo sxPackage(locationPo loc,sxPo name,lxPo definitions);

extern sxPo sxPackageName(sxPo term);

extern lxPo sxPackageContent(sxPo term);

extern sxPo sxIdent(locationPo loc,uniChar *name,sxPo type);

extern logical sxIsIdent(sxPo sx);

extern logical sxIsIdentifier(sxPo sx,uniChar *name);

extern sxPo sxImport(locationPo loc,uniChar *pkg);

extern logical sxIsImport(sxPo sx);

extern uniChar *sxImportPkg(sxPo sx);

extern sxPo sxFunction(locationPo loc,uniChar *name,sxPo type,lxPo args,sxPo sx);

extern logical sxIsFunction(sxPo sx);

extern uniChar *sxFunName(sxPo sx);

extern sxPo sxFunType(sxPo sx);

extern sxPo sxFunExp(sxPo sx);

extern lxPo sxFunArgs(sxPo sx);

extern sxPo sxMemo(locationPo loc,uniChar *name,sxPo type,sxPo exp);

extern logical sxIsMemo(sxPo sx);

extern uniChar *sxMemoName(sxPo sx);

extern sxPo sxMemoType(sxPo sx);

extern sxPo sxMemoExp(sxPo sx);

extern sxPo sxProcedure(locationPo loc,uniChar *name,lxPo args,sxPo sx);

extern logical sxIsProcedure(sxPo sx);

extern uniChar *sxProcName(sxPo sx);

extern lxPo sxProcArgs(sxPo sx);

extern sxPo sxProcType(sxPo sx);

extern sxPo sxProcBody(sxPo sx);

extern sxPo sxThunk(locationPo loc,uniChar *name,sxPo type,sxPo sx);

extern logical sxIsThunk(sxPo sx);

extern uniChar *sxThunkName(sxPo sx);

extern sxPo sxThunkType(sxPo sx);

extern sxPo sxThunkExp(sxPo sx);

extern sxPo sxPattern(locationPo loc,uniChar *name,lxPo args,sxPo type,
		      sxPo ptn,sxPo cond);

extern uniChar *sxPatternName(sxPo sx);

extern logical sxIsIsDeclaration(sxPo sx);

extern sxPo sxIsDeclaration(locationPo loc,sxPo lval,sxPo rval);

extern sxPo sxDeclLval(sxPo sx);

extern uniChar* sxLvalName(sxPo sx);

extern sxPo sxLvalType(sxPo sx);

extern sxPo sxDeclValue(sxPo sx);

extern sxPo sxVarDeclaration(locationPo loc,sxPo lval,sxPo rval);

extern logical sxIsVarDeclaration(sxPo sx);

extern uniChar *sxDeclaredVarName(sxPo sx);

extern sxPo sxResourceDeclaration(locationPo loc,sxPo lval,sxPo rval);

extern logical sxIsResourceDeclaration(sxPo sx);

extern uniChar *sxDeclaredResourceName(sxPo sx);

extern sxPo sxAssignment(locationPo loc,sxPo lval,sxPo rval);

extern logical sxIsAssignment(sxPo sx);

extern sxPo sxAsgnLVal(sxPo sx);

extern sxPo sxAsgnRVal(sxPo sx);

extern logical sxIsCast(sxPo sx);

extern sxPo sxTypedExp(locationPo loc,sxPo exp,sxPo type);

extern sxPo sxCastExp(sxPo sx);

extern sxPo sxCastType(sxPo sx);

extern sxPo sxOr(locationPo loc,sxPo lhs,sxPo rhs);

extern sxPo sxAnd(locationPo loc,sxPo lhs,sxPo rhs);

extern sxPo sxNot(locationPo loc,sxPo rhs);

extern sxPo sxIsTrue(locationPo loc,sxPo exp);

extern sxPo sxMatches(locationPo loc,sxPo exp,sxPo ptn);

extern sxPo sxApply(locationPo loc,uniChar *name,lxPo args);

extern logical sxIsApply(sxPo sx);

extern uniChar *sxApplyOp(sxPo sx);

extern lxPo sxApplyArgs(sxPo sx);

extern sxPo sxSequence(locationPo loc,lxPo els);

extern lxPo sxList(locationPo loc,...);

extern sxPo sxContinue(locationPo loc,uniChar *lbl,lxPo args);

extern logical sxIsContinue(sxPo sx);

extern uniChar* sxContinueOp(sxPo sx);

extern lxPo sxContinueArgs(sxPo sx);

extern logical sxIsArithExp(sxPo sx);

extern uniChar* sxArithOp(sxPo sx);

extern sxPo sxArithLhs(sxPo sx);

extern sxPo sxArithRhs(sxPo sx);

extern sxPo sxUnary(locationPo loc,uniChar *name,sxPo rhs);

extern logical sxIsUnry(sxPo sx);

extern logical sxIsUnary(sxPo sx,uniChar *op);

extern sxPo sxUnaryArg(sxPo sx);

extern sxPo sxBinary(locationPo loc,uniChar *name,sxPo lhs,sxPo rhs);

extern sxPo sxLhs(sxPo sx);

extern sxPo sxRhs(sxPo sx);

extern sxPo sxTernary(locationPo loc,uniChar *name,sxPo lhs,sxPo mdl,sxPo rhs);

extern logical sxIsTern(sxPo sx);

extern logical sxIsTernary(sxPo sx,uniChar *name);

extern sxPo sxTernaryLhs(sxPo sx);

extern sxPo sxTernaryMdl(sxPo sx);

extern sxPo sxTernaryRhs(sxPo sx);

extern sxPo sxCall(locationPo loc,uniChar *name,lxPo args);

extern logical sxIsCall(sxPo call);

extern uniChar *sxCallOp(sxPo sx);

extern lxPo sxCallArgs(sxPo sx);

extern sxPo sxConstructor(locationPo loc,uniChar *name,lxPo args);

extern logical sxIsConstructor(sxPo call);

extern uniChar *sxConstructorOp(sxPo sx);

extern lxPo sxConstructorArgs(sxPo sx);

extern sxPo sxSwitch(locationPo loc,sxPo sel,lxPo cases);

extern logical sxIsSwitch(sxPo sx);

extern sxPo sxSwitchSel(sxPo sx);

extern lxPo sxSwitchCases(sxPo sx);

extern logical sxIsCaseRule(sxPo sx);

extern sxPo sxCaseRule(locationPo loc,sxPo ptn, sxPo body);

extern sxPo sxCasePtn(sxPo sx);

extern sxPo sxCaseBody(sxPo sx);

extern logical sxIsDefaultRule(sxPo rule);

extern sxPo sxDefaultRule(locationPo loc,sxPo rhs);

extern sxPo sxDefltBody(sxPo rule);

extern sxPo sxConditional(locationPo loc,sxPo tst,sxPo th, sxPo el);

extern sxPo sxLet(locationPo loc,lxPo defs,sxPo bound);

extern logical sxIsLet(sxPo sx);

extern lxPo sxLetDefs(sxPo sx);

extern sxPo sxLetBound(sxPo sx);

extern logical sxIsValof(sxPo sx);

extern sxPo sxValof(locationPo loc,sxPo sx);

extern sxPo sxValofAction(sxPo sx);

extern sxPo sxValis(locationPo loc,sxPo sx);

extern logical sxIsValis(sxPo sx);

extern sxPo sxValisExp(sxPo sx);

extern sxPo sxAssert(locationPo loc,sxPo sx);

extern logical sxIsAssert(sxPo sx);

extern sxPo sxAssertCond(sxPo sx);

extern sxPo sxBlock(locationPo loc,lxPo els);

extern logical sxIsBlock(sxPo sx);

extern lxPo sxBlockContent(sxPo sx);

extern logical sxIsCondition(sxPo sx);

extern uniChar *sxConditionOp(sxPo sx);

extern logical sxIsConditional(sxPo sx);

extern sxPo sxConditionalTest(sxPo sx);

extern sxPo sxConditionalThen(sxPo sx);

extern sxPo sxConditionalElse(sxPo sx);

extern sxPo sxLabeledAction(locationPo loc,uniChar *lbl,sxPo sx);

extern logical sxIsLabeled(sxPo sx);

extern uniChar *sxLabeledLabel(sxPo sx);

extern sxPo sxLabeledBody(sxPo sx);

extern sxPo sxLeaveAction(locationPo loc,uniChar *lbl);

extern logical sxIsLeaveAction(sxPo sx);

extern uniChar *sxLeaveLabel(sxPo sx);

extern sxPo sxGotoAction(locationPo loc,uniChar *lbl);

extern logical sxIsGotoAction(sxPo sx);

extern uniChar * sxGotoLabel(sxPo sx);

extern sxPo sxSync(locationPo loc,sxPo var,sxPo act);

extern logical sxIsSync(sxPo sx);

extern sxPo sxSyncVar(sxPo sx);

extern sxPo sxSyncAct(sxPo sx);

extern logical sxIsWhile(sxPo sx);

extern sxPo sxWhileAction(locationPo loc,sxPo test,sxPo body);

extern sxPo sxWhileTest(sxPo sx);

extern sxPo sxWhileBody(sxPo sx);

extern sxPo sxLoopAction(locationPo loc,sxPo sx);

extern logical sxIsLoopAction(sxPo sx);

extern sxPo sxLoopBody(sxPo sx);

extern sxPo sxCatch(locationPo loc,sxPo body,lxPo clauses);

extern logical sxIsCatch(sxPo sx);

extern sxPo sxCatchBody(sxPo sx);

extern lxPo sxCatchClauses(sxPo sx);

extern sxPo sxThrowAction(locationPo loc,sxPo sx);

extern logical sxIsThrowAction(sxPo sx);

extern sxPo sxThrowExp(sxPo sx);

extern sxPo sxNothing(locationPo loc);

extern logical isNothing(sxPo sx);

extern sxPo sxConSpec(locationPo loc,uniChar *name,lxPo args);

extern logical sxIsBin(sxPo sx);

extern logical sxIsBinary(sxPo sx,uniChar *op);

extern sxPo sxLhs(sxPo sx);

extern sxPo sxRhs(sxPo sx);

extern sxPo parseContent(uniChar *path);

#endif
