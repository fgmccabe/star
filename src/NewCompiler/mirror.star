star.compiler.mirror{
  -- Implement a mirror interface
  import star.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.misc.
  import star.compiler.location.
  import star.compiler.macro.infra.
  import star.compiler.wff.
  import star.meta.

  -- generate a mirror implementation from a type definition

  generateMirror:(ast,macroContext) => macroState.
--  generateMirror(A) where (Lc,Q,C,Tp,B) ?= isAlgebraicTypeStmt(A) => .

  reflectType:(ast)=>ast.
  reflectType(A) where (Lc,"void") ?= isName(A) => enum(Lc,"voidTp").
  reflectType(A) where (Lc,Nm) ?= isName => mkCon(Lc,"nominal",[A]).
  reflectType(A) where (Lc,Els) ?= isTuple(A) =>
    mkCon(Lc,"tupleTp",typeElems(Lc,Els,reflectType)).
  reflectType(A) where (Lc,L,R) ?= isFuncType(A) && (ALc,As) ?= isTuple(L) =>
    mkCon(Lc,"funTp",[typeElems(Lc,As),reflectType(R),enum(Lc,"voidTp")]).
  reflectType(A) where (Lc,L,R,T) ?= isThrwFunctionType(A) && (ALc,As) ?= isTuple(L) =>
    mkCon(Lc,"funTp",[typeElems(Lc,As),reflectType(R),reflectType(T)]).
  reflectType(A) where (Lc,L,T) ?= isPrcType(A) && (ALc,As) ?= isTuple(L) =>
    mkCon(Lc,"prcTp",[typeElems(Lc,As,reflectType),(Th?=T ?? reflectType(Th)||enum(Lc,"voidTp"))]).
  reflectType(A) where (Lc,Els) ?= isBrTuple(A) =>
    mkCon(Lc,"faceTp",[typeElems(Lc,Els,typeField)]).
  reflectType(A) where (Lc,Op,Arg) ?= isSquareTerm(A) && (NLc,Nm)?=isName(Op) =>
    reflectTypeExp(Lc,mkCon(Lc,"tpFun",[.str(NLc,Nm),.int(NLc,argCount(Arg))]),Arg).
    

  typeElems(Lc,[],_) => enum(Lc,"nil").
  typeElems(Lc,[T,..Ts],TF) => mkCon(Lc,"cons",[TF(T),typeElems(Ts)]).

  typeField(A) where (Lc,L,R) ?= isTypeDeclaration(A) && (NLc,Nm) ?= isName(L) =>
    rndTuple(Lc,.str(NLc,Nm),reflectType(R)).
}
