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
  peepOptimize(Ins) => peep(Ins).

  peep:(cons[assemOp])=>cons[assemOp].
  peep([]) => [].
  peep([iCase(Cx),..Code]) => [iCase(Cx),..copyPeep(Cx,Code)].
  peep([iJmp(Lb),iLbl(Lb),..Code]) => peep([iLbl(Lb),..Code]).
  peep([iCall(Lb),iFrame(_),.iRet,..Code]) => [iTail(Lb),..peep(Code)].
  peep([iOCall(Lb),iFrame(_),.iRet,..Code]) => [iOTail(Lb),..peep(Code)].
  peep([iRst(_),iRst(D),..Code]) => peep([iRst(D),..Code]).
  peep([Ins,..Code]) => [Ins,..peep(Code)].

  copyPeep(0,Code) => peep(Code).
  copyPeep(Cx,[iJmp(Lb),..Code]) => [iJmp(Lb),..copyPeep(Cx-1,Code)].

  findAllLbls:(cons[assemOp],set[assemLbl]) => set[assemLbl].
  findAllLbls([],Lbls) => Lbls.
  findAllLbls([iJmp(Lb),..Ins],Lbls) => findAllLbls(Ins,Lbls\+Lb).
  findAllLbls([iCLbl(Lb),..Ins],Lbls) => findAllLbls(Ins,Lbls\+Lb).
  findAllLbls([iICmp(Lb),..Ins],Lbls) => findAllLbls(Ins,Lbls\+Lb).
  findAllLbls([iFCmp(Lb),..Ins],Lbls) => findAllLbls(Ins,Lbls\+Lb).
  findAllLbls([iCmp(Lb),..Ins],Lbls) => findAllLbls(Ins,Lbls\+Lb).
  findAllLbls([iBf(Lb),..Ins],Lbls) => findAllLbls(Ins,Lbls\+Lb).
  findAllLbls([iBt(Lb),..Ins],Lbls) => findAllLbls(Ins,Lbls\+Lb).
  findAllLbls([iThrow(Lb),..Ins],Lbls) => findAllLbls(Ins,Lbls\+Lb).
  findAllLbls([iUnwind(Lb),..Ins],Lbls) => findAllLbls(Ins,Lbls\+Lb).
  findAllLbls([_,..Ins],Lbls) => findAllLbls(Ins,Lbls).

  deleteUnusedLbls:(cons[assemOp],set[assemLbl]) => cons[assemOp].
  deleteUnusedLbls([],_) => [].
  deleteUnusedLbls([iLbl(Lb),..Ins],Lbls) =>
    (Lb.<.Lbls ?
	[iLbl(Lb),..deleteUnusedLbls(Ins,Lbls)] ||
	deleteUnusedLbls(Ins,Lbls)).
  deleteUnusedLbls([I,..Ins],Lbls) => [I,..deleteUnusedLbls(Ins,Lbls)].

  cleanLbls:(cons[assemOp])=>cons[assemOp].
  cleanLbls(Ins) => deleteUnusedLbls(Ins,findAllLbls(Ins,[])).
}
