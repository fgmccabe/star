star.compiler.peephole{
  import star.
  import star.pkg.
  import star.multi.

  import star.compiler.assem.
  import star.compiler.errors.
  import star.compiler.meta.
  import star.compiler.misc.

  import star.compiler.location.
  import star.compiler.data.

  public peepOptimize:(codeSegment)=>codeSegment.
  peepOptimize(.func(Lbl,Pol,Tp,LcMap,Ins)) => valof{
    Ins0 = peepCode(Ins,[]);
    (LcMp1,Ins1) = findUnusedVars(LcMap,Ins0);
    valis .func(Lbl,Pol,Tp,LcMp1,adjustEntry(peepCode(Ins1,[]),size(LcMp1)))
  }
  peepOptimize(Df) default => Df.

  peepCode(Ins,Lbls) => peep(dropUnreachable(Ins),Lbls).

  findUnusedVars([],Ins) => ([],Ins).
  findUnusedVars([(Nm,Spec),..LcMp],Ins) => valof{
    if varRead(Nm,Ins) then {
      (Lm1,I1) = findUnusedVars(LcMp,Ins);
      valis ([(Nm,Spec),..Lm1],I1)
    } else
    valis findUnusedVars(LcMp,dropVar(Nm,Ins))
  }

  varRead(Vr,[]) => .false.
  varRead(Vr,[I,..Is]) => vrRead(Vr,I) || varRead(Vr,Is).

  vrRead(Vr,.iBlock(_,Is)) => varRead(Vr,Is).
  vrRead(Vr,.iLdL(V)) => V==Vr.
  vrRead(Vr,.iLbl(_,I)) => vrRead(Vr,I).
  vrRead(_,_) default => .false.

  dropVar(Vr,[]) => [].
  dropVar(Vr,[.iTL(Vr),..Is]) => dropVar(Vr,Is).
  dropVar(Vr,[.iStL(Vr),..Is]) => [.iDrop,..dropVar(Vr,Is)].
  dropVar(Vr,[.iStV(Vr),..Is]) => dropVar(Vr,Is).
  dropVar(Vr,[.iBlock(Tp,Bs),..Is]) => [.iBlock(Tp,dropVar(Vr,Bs)),..dropVar(Vr,Is)].
  dropVar(Vr,[.iLbl(Lb,I),..Is]) => valof{
    IRx = dropVar(Vr,[I]);
    if isEmpty(IRx) then
      valis dropVar(Vr,Is)
    else if [Ix].=IRx then
      valis [.iLbl(Lb,Ix),..dropVar(Vr,Is)]
    else{
      reportTrap("problem in dropVar $(I)");
      valis []
    }
  }
  dropVar(Vr,[I,..Is]) => [I,..dropVar(Vr,Is)].

  dropUnreachable([]) => [].
  dropUnreachable([.iBreak(Lvl),.._]) => [.iBreak(Lvl)].
  dropUnreachable([.iResult(Lbl),.._]) => [.iResult(Lbl)].
  dropUnreachable([.iLoop(Lvl),.._]) => [.iLoop(Lvl)].
  dropUnreachable([.iRet,.._]) => [.iRet].
  dropUnreachable([.iXRet,.._]) => [.iXRet].
  dropUnreachable([.iTCall(Lb),.._]) => [.iTCall(Lb)].
  dropUnreachable([.iTOCall(Ar),.._]) => [.iTOCall(Ar)].
  dropUnreachable([.iAbort,.._]) => [.iAbort].
  dropUnreachable([.iHalt(Ix),.._]) => [.iHalt(Ix)].
  dropUnreachable([.iRetire,.._]) => [.iRetire].
  dropUnreachable([.iCase(Mx),..Is]) => [.iCase(Mx),..copyN(Mx,Is)].
  dropUnreachable([.iICase(Mx),..Is]) => [.iICase(Mx),..copyN(Mx,Is)].
  dropUnreachable([.iUnpack(Mx),..Is]) => [.iUnpack(Mx),..copyN(Mx,Is)].
  dropUnreachable([I,..Is]) => [I,..dropUnreachable(Is)].

  copyN(0,_) => [].
  copyN(K,[I,..Is]) => [I,..copyN(K-1,Is)].
    
  -- Low-level optimizations.
  peep:(multi[assemOp],cons[(string,multi[assemOp])])=>multi[assemOp].
  peep([],_) => [].
  peep([.iLine(Lc),.iLine(_),..Ins],Lbls) => peep([.iLine(Lc),..Ins],Lbls).
  peep([.iStL(Off),.iLdL(Off),.iRet,.._],_) => [.iRet].
  peep([.iStL(Off),.iLdL(Off),.iXRet,.._],_) => [.iXRet].
  peep([.iStL(Off),.iLdL(Off),..Ins],Lbls) => peep([.iTL(Off),..Ins],Lbls).
  peep([.iRot(0),..Ins],Lbls) => peep(Ins,Lbls).
  peep([.iLdL(_),.iDrop,..Ins],Lbls) => peep(Ins,Lbls).
  peep([.iLdL(_),.iNth(_),.iDrop,..Ins],Lbls) => peep(Ins,Lbls).
  peep([.iLdA(_),.iNth(_),.iDrop,..Ins],Lbls) => peep(Ins,Lbls).
  peep([.iLdA(_),.iDrop,..Ins],Lbls) => peep(Ins,Lbls).
  peep([.iNth(_),.iDrop,..Ins],Lbls) => peep([.iDrop,..Ins],Lbls).
  peep([.iBlock(L,Is),..Ins],Lbls) => [.iBlock(L,peepCode(Is,Lbls)),..peep(Ins,Lbls)].
  peep([.iLbl(Lb,.iBlock(Lvl,Is)),..Ins],Lbls) => valof{
    Is0 = peepCode(Is,[(Lb,Is),..Lbls]);
    if lblReferenced(Lb,Is0) then
      valis [.iLbl(Lb,.iBlock(Lvl,Is0)),..peep(Ins,Lbls)]
    else
    valis peepCode(Is0++Ins,Lbls)
  }
  peep([.iIf(Lb),..Ins],Lbls) =>
    [.iIf(resolveLbl(Lb,Lbls)),..peep(Ins,Lbls)].
  peep([.iIfNot(Lb),..Ins],Lbls) =>
    [.iIfNot(resolveLbl(Lb,Lbls)),..peep(Ins,Lbls)].
  peep([.iCLbl(Tgt,Lb),..Ins],Lbls) =>
    [.iCLbl(Tgt,resolveLbl(Lb,Lbls)),..peep(Ins,Lbls)].
  peep([.iCInt(Tgt,Lb),..Ins],Lbls) =>
    [.iCInt(Tgt,resolveLbl(Lb,Lbls)),..peep(Ins,Lbls)].
  peep([.iCChar(Tgt,Lb),..Ins],Lbls) =>
    [.iCChar(Tgt,resolveLbl(Lb,Lbls)),..peep(Ins,Lbls)].
  peep([.iCFlt(Tgt,Lb),..Ins],Lbls) =>
    [.iCFlt(Tgt,resolveLbl(Lb,Lbls)),..peep(Ins,Lbls)].
  peep([.iCLit(Tgt,Lb),..Ins],Lbls) =>
    [.iCLit(Tgt,resolveLbl(Lb,Lbls)),..peep(Ins,Lbls)].
  peep([.iCmp(Lb),..Ins],Lbls) =>
    [.iCmp(resolveLbl(Lb,Lbls)),..peep(Ins,Lbls)].
  peep([.iICmp(Lb),..Ins],Lbls) =>
    [.iICmp(resolveLbl(Lb,Lbls)),..peep(Ins,Lbls)].
  peep([.iFCmp(Lb),..Ins],Lbls) =>
    [.iFCmp(resolveLbl(Lb,Lbls)),..peep(Ins,Lbls)].
  peep([.iCCmp(Lb),..Ins],Lbls) =>
    [.iCCmp(resolveLbl(Lb,Lbls)),..peep(Ins,Lbls)].
  peep([.iBreak(Lb),.._],Lbls) =>
    [.iBreak(resolveLbl(Lb,Lbls))].
  peep([.iResult(Lb),.._],Lbls) => [.iResult(resolveLbl(Lb,Lbls))].
  peep([.iLdSav(Lb),..Ins],Lbls) =>
    [.iLdSav(resolveLbl(Lb,Lbls)),..peep(Ins,Lbls)].
  peep([.iLoop(Lb),.._],_Lbls) => [.iLoop(Lb)].
  peep([.iRetire,.._],_Lbls) => [.iRetire].
  peep([.iICase(Mx),..Ins],Lbls) => [.iICase(Mx),..copyN(Mx,Ins)].
  peep([.iCase(Mx),..Ins],Lbls) => [.iCase(Mx),..copyN(Mx,Ins)].
  peep([.iUnpack(Mx),..Ins],Lbls) => [.iUnpack(Mx),..copyN(Mx,Ins)].
  peep([I,..Ins],Lbls) => [I,..peep(Ins,Lbls)].

  lblReferenced(_,[]) => .false.
  lblReferenced(Lb,[.iBreak(Lb),.._]) => .true.
  lblReferenced(Lb,[.iLoop(Lb),.._]) => .true.
  lblReferenced(Lb,[.iResult(Lb),.._]) => .true.
  lblReferenced(Lb,[.iXCall(_,Lb),.._]) => .true.
  lblReferenced(Lb,[.iXOCall(_,Lb),.._]) => .true.
  lblReferenced(Lb,[.iXEscape(_,Lb),.._]) => .true.
  lblReferenced(Lb,[.iIf(Lb),.._]) => .true.
  lblReferenced(Lb,[.iIfNot(Lb),.._]) => .true.
  lblReferenced(Lb,[.iCmp(Lb),.._]) => .true.
  lblReferenced(Lb,[.iCCmp(Lb),.._]) => .true.
  lblReferenced(Lb,[.iICmp(Lb),.._]) => .true.
  lblReferenced(Lb,[.iFCmp(Lb),.._]) => .true.
  lblReferenced(Lb,[.iCInt(_,Lb),.._]) => .true.
  lblReferenced(Lb,[.iCChar(_,Lb),.._]) => .true.
  lblReferenced(Lb,[.iCFlt(_,Lb),.._]) => .true.
  lblReferenced(Lb,[.iCLit(_,Lb),.._]) => .true.
  lblReferenced(Lb,[.iCLbl(_,Lb),.._]) => .true.
  lblReferenced(Lb,[.iIDiv(Lb),.._]) => .true.
  lblReferenced(Lb,[.iIMod(Lb),.._]) => .true.
  lblReferenced(Lb,[.iFDiv(Lb),.._]) => .true.
  lblReferenced(Lb,[.iFMod(Lb),.._]) => .true.
  lblReferenced(Lb,[.iLbl(_,I),..Ins]) =>
    lblReferenced(Lb,[I]) || lblReferenced(Lb,Ins).
  lblReferenced(Lb,[.iBlock(_,I),..Ins]) =>
    lblReferenced(Lb,I) || lblReferenced(Lb,Ins).
  lblReferenced(Lb,[.iLdSav(Lb),.._]) => .true.
  lblReferenced(Lb,[_,..Ins]) => lblReferenced(Lb,Ins).

  resolveLbl(Lb,[]) default => Lb.
  resolveLbl(Lb,[(Lb,Is),..Lbls]) where [.iBreak(Lbx),.._].=Is =>
    resolveLbl(Lbx,Lbls).
  resolveLbl(Lb,[(Lb,_),.._]) => Lb.
  resolveLbl(Lb,[_,..Lbls]) => resolveLbl(Lb,Lbls).

  adjustEntry([.iEntry(_),..Ins],Cnt) => [.iEntry(Cnt),..Ins].
  adjustEntry([Op,..Ins],Cnt) => [Op,..adjustEntry(Ins,Cnt)].
}
