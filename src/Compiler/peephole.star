star.compiler.peephole{
  import star.
  import star.pkg.
  import star.multi.

  import star.compiler.ssa.
  import star.compiler.errors.
  import star.compiler.meta.
  import star.compiler.misc.

  import star.compiler.location.
  import star.compiler.data.

  public peepOptimize:(codeSegment)=>codeSegment.
  peepOptimize(.func(Lbl,Pol,Tp,LcMap,Ins)) => valof{
    (LcMp1,Ins1) = findUnusedVars(LcMap,peepCode(Ins,[]));
    (LcMp2,Ins2) = findUnusedVars(LcMp1,peepCode(Ins1,[]));
    valis .func(Lbl,Pol,Tp,LcMp2,adjustEntry(Ins2,LcMp2//fst))
  }
  peepOptimize(Df) default => Df.

  peepCode(Ins,Lbls) => peep(dropUnreachable(Ins),Lbls).

  findUnusedVars([],Ins) => ([],Ins).
  findUnusedVars([(Nm,Spec),..LcMp],Ins) => valof{
    if varRead(Nm,Ins) then {
      (Lm1,I1) = findUnusedVars(LcMp,Ins);
      valis ([(Nm,Spec),..Lm1],I1)
    } else {
      valis findUnusedVars(LcMp,dropVar(Nm,Ins))
    }
  }

  varRead(Vr,[]) => .false.
  varRead(Vr,[I,..Is]) => vrRead(Vr,I) || varRead(Vr,Is).

  vrRead(Vr,.iEntry(As,_)) => Vr .<. As.
  vrRead(Vr,.iCall(_,As)) => Vr .<. As.
  vrRead(Vr,.iTCall(_,As)) => Vr .<. As.
  vrRead(Vr,.iEscape(_,As)) => Vr .<. As.
  vrRead(Vr,.iOCall(O,As)) => O==Vr || Vr .<. As.
  vrRead(Vr,.iTOCall(O,As)) => O==Vr || Vr .<. As.
  vrRead(Vr,.iHalt(V)) => V==Vr.
  vrRead(Vr,.iAbort(_,V)) => V==Vr.
  vrRead(Vr,.iRet(V)) => V==Vr.
  vrRead(Vr,.iXRet(V)) => V==Vr.

  vrRead(Vr,.iRSX(_,V)) => V==Vr. -- special case, because of the jump
  
  vrRead(Vr,.iBlock(As,Is)) => Vr.<.As || varRead(Vr,Is).
  vrRead(Vr,.iResult(_,As)) => Vr .<. As.
  vrRead(Vr,.iFiber(_,V)) => V==Vr.
  vrRead(Vr,.iSuspend(T,E)) => T==Vr || E==Vr.
  vrRead(Vr,.iResume(T,E)) => T==Vr || E==Vr.
  vrRead(Vr,.iRetire(T,E)) => T==Vr || E==Vr.

  vrRead(Vr,.iBind(_,V)) => V==Vr.
  vrRead(Vr,.iMv(_,V)) => V==Vr.
  vrRead(Vr,.iSG(_,V)) => V==Vr.
  vrRead(Vr,.iLdSav(_,_,V)) => V==Vr.
  vrRead(Vr,.iTstSav(_,V)) => V==Vr.
  vrRead(Vr,.iStSav(_,S,V)) => V==Vr || S==Vr.
  vrRead(Vr,.iCell(_,V)) => V==Vr.
  vrRead(Vr,.iGet(_,V)) => V==Vr.
  vrRead(Vr,.iAssign(T,V)) => V==Vr || T==Vr.

  vrRead(Vr,.iCLbl(_,_,V)) => V==Vr.
  vrRead(Vr,.iCChar(_,_,V)) => V==Vr.
  vrRead(Vr,.iCInt(_,_,V)) => V==Vr.
  vrRead(Vr,.iCFlt(_,_,V)) => V==Vr.
  vrRead(Vr,.iCLit(_,_,V)) => V==Vr.

  vrRead(Vr,.iNth(_,_,V)) => V==Vr.
  vrRead(Vr,.iStNth(T,_,V)) => V==Vr || T==Vr.

  vrRead(Vr,.iICase(V,_)) => V==Vr.
  vrRead(Vr,.iCase(V,_)) => V==Vr.
  vrRead(Vr,.iIxCase(V,_)) => V==Vr.

  vrRead(Vr,.iIAbs(_,L)) => L==Vr.
  vrRead(Vr,.iIAdd(_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iISub(_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iIMul(_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iIDiv(_,_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iIMod(_,_,L,R)) => L==Vr || R==Vr.

  vrRead(Vr,.iFAbs(_,L)) => L==Vr.
  vrRead(Vr,.iFAdd(_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iFSub(_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iFMul(_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iFDiv(_,_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iFMod(_,_,L,R)) => L==Vr || R==Vr.

  vrRead(Vr,.iIEq(_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iILt(_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iIGe(_,L,R)) => L==Vr || R==Vr.

  vrRead(Vr,.iFEq(_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iFLt(_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iFGe(_,L,R)) => L==Vr || R==Vr.

  vrRead(Vr,.iBNot(_,L)) => L==Vr.
  vrRead(Vr,.iBAnd(_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iBOr(_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iBXor(_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iBLsl(_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iBAsr(_,L,R)) => L==Vr || R==Vr.
  vrRead(Vr,.iBLsr(_,L,R)) => L==Vr || R==Vr.

  vrRead(Vr,.iAlloc(_,_,As)) => Vr .<. As.
  vrRead(Vr,.iClosure(_,_,F)) => F==Vr.
  
  vrRead(Vr,.iLbl(_,I)) => vrRead(Vr,I).
  vrRead(_,_) default => .false.

  vrWrite(Vr,.iRSP(V)) => V==Vr.
  vrWrite(Vr,.iRSX(_,V)) => V==Vr.
  vrWrite(Vr,.iBlock(Vrs,_)) => Vr .<. Vrs.
  vrWrite(Vr,.iFiber(V,_)) => V==Vr.

  vrWrite(Vr,.iMv(V,_)) => V==Vr.
  vrWrite(Vr,.iMC(V,_)) => V==Vr.

  vrWrite(Vr,.iSav(V)) => V==Vr.

  vrWrite(Vr,.iLdSav(V,_,_)) => V==Vr.
  vrWrite(Vr,.iTstSav(V,_)) => V==Vr.
  vrWrite(Vr,.iStSav(V,_,_)) => V==Vr.

  vrWrite(Vr,.iCell(V,_)) => V==Vr.
  vrWrite(Vr,.iGet(V,_)) => V==Vr.

  vrWrite(Vr,.iNth(V,_,_)) => V==Vr.

  vrWrite(Vr,.iIAdd(V,_,_)) => V==Vr.
  vrWrite(Vr,.iISub(V,_,_)) => V==Vr.
  vrWrite(Vr,.iIMul(V,_,_)) => V==Vr.
  vrWrite(Vr,.iIDiv(_,V,_,_)) => V==Vr.
  vrWrite(Vr,.iIMod(_,V,_,_)) => V==Vr.
  vrWrite(Vr,.iIAbs(V,_)) => V==Vr.

  vrWrite(Vr,.iFAdd(V,_,_)) => V==Vr.
  vrWrite(Vr,.iFSub(V,_,_)) => V==Vr.
  vrWrite(Vr,.iFMul(V,_,_)) => V==Vr.
  vrWrite(Vr,.iFDiv(_,V,_,_)) => V==Vr.
  vrWrite(Vr,.iFMod(_,V,_,_)) => V==Vr.
  vrWrite(Vr,.iFAbs(V,_)) => V==Vr.

  vrWrite(Vr,.iIEq(V,_,_)) => V==Vr.
  vrWrite(Vr,.iILt(V,_,_)) => V==Vr.
  vrWrite(Vr,.iIGe(V,_,_)) => V==Vr.

  vrWrite(Vr,.iFEq(V,_,_)) => V==Vr.
  vrWrite(Vr,.iFLt(V,_,_)) => V==Vr.
  vrWrite(Vr,.iFGe(V,_,_)) => V==Vr.

  vrWrite(Vr,.iCEq(V,_,_)) => V==Vr.
  vrWrite(Vr,.iCLt(V,_,_)) => V==Vr.
  vrWrite(Vr,.iCGe(V,_,_)) => V==Vr.

  vrWrite(Vr,.iBAnd(V,_,_)) => V==Vr.
  vrWrite(Vr,.iBOr(V,_,_)) => V==Vr.
  vrWrite(Vr,.iBXor(V,_,_)) => V==Vr.
  vrWrite(Vr,.iBLsl(V,_,_)) => V==Vr.
  vrWrite(Vr,.iBAsr(V,_,_)) => V==Vr.
  vrWrite(Vr,.iBLsr(V,_,_)) => V==Vr.
  vrWrite(Vr,.iBNot(V,_)) => V==Vr.

  vrWrite(Vr,.iAlloc(_,V,_)) => V==Vr.
  vrWrite(Vr,.iClosure(_,V,_)) => V==Vr.

  vrWrite(_,_) default => .false.

  dropVar(Vr,[]) => [].
  dropVar(Vr,[.iBlock(Vs,BI),..Is]) => [.iBlock(Vs,dropVar(Vr,BI)),..dropVar(Vr,Is)].
  dropVar(Vr,[.iLbl(Lb,I),..Is]) => valof{
    Ix = dropVar(Vr,[I]);
    if isEmpty(Ix) then
      valis dropVar(Vr,Is)
    else if [Ir] .= Ix then
      valis [.iLbl(Lb,Ir),..dropVar(Vr,Is)]
    else{
      reportTrap("problem in dropVar $(I)");
      valis []
    }
  }
  dropVar(Vr,[I,..Is]) where vrWrite(Vr,I) => dropVar(Vr,Is).
  dropVar(Vr,[I,..Is]) => [I,..dropVar(Vr,Is)].

  dropUnreachable([]) => [].
  dropUnreachable([.iBreak(Lvl),.._]) => [.iBreak(Lvl)].
  dropUnreachable([.iResult(Lbl,Vrs),.._]) => [.iResult(Lbl,Vrs)].
  dropUnreachable([.iCont(Lvl),.._]) => [.iCont(Lvl)].
  dropUnreachable([.iRtn,.._]) => [.iRtn].
  dropUnreachable([.iRet(V),.._]) => [.iRet(V)].
  dropUnreachable([.iXRet(V),.._]) => [.iXRet(V)].
  dropUnreachable([.iTCall(Lb,As),.._]) => [.iTCall(Lb,As)].
  dropUnreachable([.iTOCall(O,As),.._]) => [.iTOCall(O,As)].
  dropUnreachable([.iAbort(Lc,Vr),.._]) => [.iAbort(Lc,Vr)].
  dropUnreachable([.iHalt(Vr),.._]) => [.iHalt(Vr)].
  dropUnreachable([.iRetire(T,E),.._]) => [.iRetire(T,E)].
  dropUnreachable([.iCase(Mx,Cs),.._]) => [.iCase(Mx,Cs)].
  dropUnreachable([.iICase(Mx,Cs),.._]) => [.iICase(Mx,Cs)].
  dropUnreachable([.iIxCase(Mx,Cs),.._]) => [.iIxCase(Mx,Cs)].
  dropUnreachable([I,..Is]) => [I,..dropUnreachable(Is)].

  -- Low-level optimizations.
  peep:(multi[insOp],cons[(string,multi[insOp])])=>multi[insOp].
  peep([],_) => [].
  peep([.iLine(Lc),.iLine(_),..Ins],Lbls) => peep([.iLine(Lc),..Ins],Lbls).
  peep([.iMv(D,S),.iRet(D),.._],_) => [.iRet(S)].
  peep([.iMv(D,S),.iXRet(D),.._],_) => [.iXRet(S)].
  peep([.iBlock(L,Is),..Ins],Lbls) => [.iBlock(L,peepCode(Is,Lbls)),..peep(Ins,Lbls)].
  peep([.iLbl(Lb,.iBlock(Vs,Is)),..Ins],Lbls) => valof{
    Is0 = peepCode(Is,[(Lb,Is),..Lbls]);
    if lblReferenced(Lb,Is0) then
      valis [.iLbl(Lb,.iBlock(Vs,Is0)),..peep(Ins,Lbls)]
    else{
      valis peepCode(Is0++Ins,Lbls)
    }
  }
  peep([.iCLbl(Tgt,Lb,Vr),..Ins],Lbls) =>
    [.iCLbl(Tgt,resolveLbl(Lb,Lbls),Vr),..peep(Ins,Lbls)].
  peep([.iCInt(Tgt,Lb,Vr),..Ins],Lbls) =>
    [.iCInt(Tgt,resolveLbl(Lb,Lbls),Vr),..peep(Ins,Lbls)].
  peep([.iCChar(Tgt,Lb,Vr),..Ins],Lbls) =>
    [.iCChar(Tgt,resolveLbl(Lb,Lbls),Vr),..peep(Ins,Lbls)].
  peep([.iCFlt(Tgt,Lb,Vr),..Ins],Lbls) =>
    [.iCFlt(Tgt,resolveLbl(Lb,Lbls),Vr),..peep(Ins,Lbls)].
  peep([.iCLit(Tgt,Lb,Vr),..Ins],Lbls) =>
    [.iCLit(Tgt,resolveLbl(Lb,Lbls),Vr),..peep(Ins,Lbls)].
  peep([.iBreak(Lb),.._],Lbls) =>
    [.iBreak(resolveLbl(Lb,Lbls))].
  peep([.iResult(Lb,Vs),.._],Lbls) => [.iResult(resolveLbl(Lb,Lbls),Vs)].
  peep([.iLdSav(D,Lb,S),..Ins],Lbls) =>
    [.iLdSav(D,resolveLbl(Lb,Lbls),S),..peep(Ins,Lbls)].
  peep([.iMv(I,S),.iMv(T,I),..Ins],Lbls) => peep([.iMv(I,S),.iMv(T,S),..Ins],Lbls).
  peep([.iCont(Lb),.._],_Lbls) => [.iCont(Lb)].
  peep([.iRtn,.._],_Lbls) => [.iRtn].
  peep([.iRet(Vr),.._],_Lbls) => [.iRet(Vr)].
  peep([.iXRet(Vr),.._],_Lbls) => [.iXRet(Vr)].
  peep([.iRetire(T,E),.._],_Lbls) => [.iRetire(T,E)].
  peep([.iICase(Mx,Cs),.._],Lbls) => [.iICase(Mx,Cs)].
  peep([.iCase(Mx,Cs),.._],Lbls) => [.iCase(Mx,Cs)].
  peep([.iIxCase(Mx,Cs),.._],Lbls) => [.iIxCase(Mx,Cs)].
  peep([.iAbort(Lc,Vr),.._],Lbls) => [.iAbort(Lc,Vr)].
  peep([.iCall(Lb,As),.iRSP(Rslt),.iRet(Rslt),.._],Lbls) => [.iTCall(Lb,As)].
  peep([.iOCall(Lb,As),.iRSP(Rslt),.iRet(Rslt),.._],Lbls) => [.iTOCall(Lb,As)].
  peep([I,..Ins],Lbls) => [I,..peep(Ins,Lbls)].

  lblReferenced(_,[]) => .false.
  lblReferenced(Lb,[.iBreak(Lb),.._]) => .true.
  lblReferenced(Lb,[.iCont(Lb),.._]) => .true.
  lblReferenced(Lb,[.iResult(Lb,_),.._]) => .true.
  lblReferenced(Lb,[.iRSX(Lb,_),.._]) => .true.
  lblReferenced(Lb,[.iCInt(_,Lb,_),.._]) => .true.
  lblReferenced(Lb,[.iCChar(_,Lb,_),.._]) => .true.
  lblReferenced(Lb,[.iCFlt(_,Lb,_),.._]) => .true.
  lblReferenced(Lb,[.iCLit(_,Lb,_),.._]) => .true.
  lblReferenced(Lb,[.iCLbl(_,Lb,_),.._]) => .true.
  lblReferenced(Lb,[.iIDiv(Lb,_,_,_),.._]) => .true.
  lblReferenced(Lb,[.iIMod(Lb,_,_,_),.._]) => .true.
  lblReferenced(Lb,[.iFDiv(Lb,_,_,_),.._]) => .true.
  lblReferenced(Lb,[.iFMod(Lb,_,_,_),.._]) => .true.
  lblReferenced(Lb,[.iLbl(_,I),..Ins]) =>
    lblReferenced(Lb,[I]) || lblReferenced(Lb,Ins).
  lblReferenced(Lb,[.iBlock(_,I),..Ins]) =>
    lblReferenced(Lb,I) || lblReferenced(Lb,Ins).
  lblReferenced(Lb,[.iCase(_,I),..Ins]) =>
    lblReferenced(Lb,I) || lblReferenced(Lb,Ins).
  lblReferenced(Lb,[.iICase(_,I),..Ins]) =>
    lblReferenced(Lb,I) || lblReferenced(Lb,Ins).
  lblReferenced(Lb,[.iIxCase(_,I),..Ins]) =>
    lblReferenced(Lb,I) || lblReferenced(Lb,Ins).
  lblReferenced(Lb,[.iLdSav(_,Lb,_),.._]) => .true.
  lblReferenced(Lb,[_,..Ins]) => lblReferenced(Lb,Ins).

  resolveLbl(Lb,[]) default => Lb.
  resolveLbl(Lb,[(Lb,Is),..Lbls]) where [.iBreak(Lbx),.._].=Is =>
    resolveLbl(Lbx,Lbls).
  resolveLbl(Lb,[(Lb,_),.._]) => Lb.
  resolveLbl(Lb,[_,..Lbls]) => resolveLbl(Lb,Lbls).

  adjustEntry([.iEntry(As,Ls),..Ins],Lsx) => [.iEntry(As,Lsx),..Ins].
  adjustEntry([Op,..Ins],Cnt) => [Op,..adjustEntry(Ins,Cnt)].
}
