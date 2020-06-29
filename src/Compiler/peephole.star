star.compiler.peephole{
  import star.
  import star.pkg.

  import star.compiler.assem.
  import star.compiler.errors.
  import star.compiler.misc.

  import star.compiler.location.
  import star.compiler.terms.

  implementation equality[assemLbl] => {.
    al(L1) == al(L2) => L1==L2.
  .}

  implementation hash[assemLbl] => {.
    hash(al(L)) => hash(L).
  .}

  public peepOptimize:(cons[assemOp])=>cons[assemOp].
--  peepOptimize(Ins) => cleanLbls(trace("drop unused code ",peep(Ins,findTgts(Ins,[])))).
  peepOptimize(Ins) => peep(Ins,findTgts(Ins,[])).

  -- Low-level optimizations.
  peep:(cons[assemOp],map[assemLbl,cons[assemOp]])=>cons[assemOp].
  peep([],_) => [].
  peep([iCase(Cx),..Code],Map) => [iCase(Cx),..copyPeep(Cx,Code,Map)].
  peep([iJmp(Lb),iLbl(Lb),..Code],Map) => peep([iLbl(Lb),..Code],Map).
--  peep([iJmp(Lb),..Code],Map) where [iJmp(XLb),.._]^=Map[Lb] && ~XLb==Lb => peep([iJmp(XLb),..Code],Map).
  peep([iCall(Lb),iFrame(_),.iRet,..Code],Map) => [iTail(Lb),..peep(Code,Map)].
  peep([iOCall(Lb),iFrame(_),.iRet,..Code],Map) => [iOTail(Lb),..peep(Code,Map)].
  peep([iRst(_),iRst(D),..Code],Map) => peep([iRst(D),..Code],Map).
  peep([Ins,..Code],Map) => [Ins,..peep(Code,Map)].

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
  findJumps([iBf(Lb),..Ins],Lbls) => findJumps(Ins,Lbls\+Lb).
  findJumps([iBt(Lb),..Ins],Lbls) => findJumps(Ins,Lbls\+Lb).
  findJumps([iCV(Lb),..Ins],Lbls) => findJumps(Ins,Lbls\+Lb).
  findJumps([iThrow(Lb),..Ins],Lbls) => findJumps(Ins,Lbls\+Lb).
  findJumps([iUnwind(Lb),..Ins],Lbls) => findJumps(Ins,Lbls\+Lb).
  findJumps([_,..Ins],Lbls) => findJumps(Ins,Lbls).

  deleteUnusedLbls:(cons[assemOp],set[assemLbl]) => cons[assemOp].
  deleteUnusedLbls([],_) => [].
  deleteUnusedLbls([iLbl(Lb),..Ins],Lbls) =>
    (Lb.<.Lbls ?
	[iLbl(Lb),..deleteUnusedLbls(Ins,Lbls)] ||
	deleteUnusedLbls(trace("$(Lb) dropped ",dropUntilLbl(trace("drop $(Lb) ",Ins))),Lbls)).
  deleteUnusedLbls([I,..Ins],Lbls) => [I,..deleteUnusedLbls(Ins,Lbls)].

  dropUntilLbl([]) => [].
  dropUntilLbl([iLbl(L),..Ins]) => [iLbl(L),..Ins].
  dropUntilLbl([_,..Ins]) => dropUntilLbl(Ins).

  cleanLbls:(cons[assemOp])=>cons[assemOp].
  cleanLbls(Ins) => deleteUnusedLbls(Ins,findJumps(Ins,[])).
}
