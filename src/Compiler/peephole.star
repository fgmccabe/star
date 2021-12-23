star.compiler.peephole{
  import star.
  import star.pkg.

  import star.compiler.assem.
  import star.compiler.errors.
  import star.compiler.misc.

  import star.compiler.location.
  import star.compiler.terms.

  implementation equality[assemLbl] => @<
    al(L1) == al(L2) => L1==L2.
  @>

  implementation hash[assemLbl] => @<
    hash(al(L)) => hash(L).
  @>

  public peepOptimize:(cons[assemOp])=>cons[assemOp].
  peepOptimize(Ins) => cleanLbls(peep(Ins,findTgts(Ins,[]))).

  -- Low-level optimizations.
  peep:(cons[assemOp],map[assemLbl,cons[assemOp]])=>cons[assemOp].
  peep([],_) => [].
  peep([iCase(Cx),..Code],Map) => [iCase(Cx),..copyPeep(Cx,Code,Map)].
  peep([iJmp(Lb),iLbl(Lb),..Code],Map) => peep([iLbl(Lb),..Code],Map).
  peep([iJmp(Lb),..Code],Map) where [iJmp(XLb),.._]^=Map[Lb] && ~XLb==Lb => peep([iJmp(XLb),..Code],Map).
  peep([iCall(Lb),iFrame(_),.iRet,..Code],Map) => [iTail(Lb),..dropTillLbls(Code,Map)].
  peep([iOCall(Lb),iFrame(_),.iRet,..Code],Map) => [iOTail(Lb),..dropTillLbls(Code,Map)].
  peep([iRst(_),iRst(D),..Code],Map) => peep([iRst(D),..Code],Map).
  peep([iRst(_),iLbl(Lb),iRst(D),..Code],Map) => peep([iLbl(Lb),iRst(D),..Code],Map).
  peep([iBlock(Tpe,Inner),..Code],Map) => [iBlock(Tpe,peep(Inner,Map)),..peep(Code,Map)].
  peep([.iRet,..Code],Map) => [.iRet,..dropTillLbls(Code,Map)].
  peep([Ins,..Code],Map) => [Ins,..peep(Code,Map)].

  dropTillLbls([],_) => [].
  dropTillLbls([iLbl(Lb),..Code],Map) => peep([iLbl(Lb),..Code],Map).
  dropTillLbls([_,..Code],Map) => dropTillLbls(Code,Map).
  

  copyPeep(0,Code,Map) => peep(Code,Map).
  copyPeep(Cx,[iJmp(Lb),..Code],Map) => [iJmp(Lb),..copyPeep(Cx-1,Code,Map)].

  findTgts:(cons[assemOp],map[assemLbl,cons[assemOp]]) => map[assemLbl,cons[assemOp]].
  findTgts([],M) => M.
  findTgts([iLbl(Lb),..Ins],Tgts) => findTgts(Ins,Tgts[Lb->Ins]).
  findTgts([I,..Ins],Tgts) => findTgts(Ins,Tgts).

  findJumps:(cons[assemOp],set[assemLbl]) => set[assemLbl].
  findJumps([],Lbls) => Lbls.
  findJumps([iJmp(Lb),..Ins],Lbls) => findJumps(Ins,Lbls\+Lb).
  findJumps([iCLbl(Lb),..Ins],Lbls) => findJumps(Ins,Lbls\+Lb).
  findJumps([iICmp(Lb),..Ins],Lbls) => findJumps(Ins,Lbls\+Lb).
  findJumps([iFCmp(Lb),..Ins],Lbls) => findJumps(Ins,Lbls\+Lb).
  findJumps([iCmp(Lb),..Ins],Lbls) => findJumps(Ins,Lbls\+Lb).
  findJumps([iCV(Lb),..Ins],Lbls) => findJumps(Ins,Lbls\+Lb).
  findJumps([iThrow(Lb),..Ins],Lbls) => findJumps(Ins,Lbls\+Lb).
  findJumps([iUnwind(Lb),..Ins],Lbls) => findJumps(Ins,Lbls\+Lb).
  findJumps([iUnpack(_,Lb),..Ins],Lbls) => findJumps(Ins,Lbls\+Lb).
  findJumps([iBlock(_,Inner),..Ins],Lbls) => findJumps(Inner,findJumps(Ins,Lbls)).
  findJumps([_,..Ins],Lbls) => findJumps(Ins,Lbls).

  isJump(iJmp(_))=>.true.
  isJump(_) default => .false.

  deleteUnusedLbls:(boolean,cons[assemOp],set[assemLbl]) => cons[assemOp].
  deleteUnusedLbls(_,[],_) => [].
  deleteUnusedLbls(J,[iLbl(Lb),..Ins],Lbls) =>
    (Lb.<.Lbls ?
	[iLbl(Lb),..deleteUnusedLbls(.false,Ins,Lbls)] ||
	dropUntilLbl(J,Ins,Lbls)).
  deleteUnusedLbls(_,[I,..Ins],Lbls) => [I,..deleteUnusedLbls(isJump(I),Ins,Lbls)].

  -- We only actually drop if prior instruction is an unconditional jump
  dropUntilLbl(_,[],_) => [].
  dropUntilLbl(J,[iLbl(L),..Ins],Lbls) =>
    deleteUnusedLbls(J,[iLbl(L),..Ins],Lbls).
  dropUntilLbl(.true,[_,..Ins],Lbls) => dropUntilLbl(.true,Ins,Lbls).
  dropUntilLbl(.false,Ins,Lbls) => deleteUnusedLbls(.false,Ins,Lbls).

  cleanLbls:(cons[assemOp])=>cons[assemOp].
  cleanLbls(Ins) => deleteUnusedLbls(.false,Ins,
    findJumps(Ins,[])).
}
