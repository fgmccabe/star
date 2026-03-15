/* Automatically generated, do not edit */

:- module(ssa,[assemSSA/2, dispSSA/1, ssaHash/1]).
:- use_module(misc).
:- use_module(lterms).
:- use_module(types).
:- use_module(encode).
:- use_module(display).
:- use_module(escapes).
:- use_module(errors).

assemSSA(func(Nm,Pol,Sig,LSpecs,Ins),MTpl) :-
    findLit([],Nm,_,Ls0),
    findLit(Ls0,strg(Sig),SgIx,Ls1),
    (is_member(iEntry(Ags,Lcs),Ins) ->
     declareArgs(Ags,LSpecs,[],AgMap,[],AgDescs),
     declareLocals(Lcs,LSpecs,AgMap,LcMap,AgDescs,VrDescs);
     reportFatal("no valid iEntry found")),
    createLocals(VrDescs,LcsTpl),
    assemBlock(Ins,none,[],Ls1,Lts,LcMap,Code),
    mkLitTpl(Lts,LtTpl),
    encPolicy(Pol,HP),
    length(Lcs,Lx),
    mkCons("code",[Nm,HP,intgr(SgIx),intgr(Lx),LtTpl,Code,LcsTpl],MTpl).
assemSSA(struct(Lbl,Sig,Ix),Tpl) :-
    mkCons("struct",[Lbl,Sig,intgr(Ix)],Tpl).
assemSSA(tipe(Tp,Rl,Map),Tpl) :-
    encMap(Map,MapEls),
    tpName(Tp,TpNm),
    encType(Rl,RlSig),
    mkTpl(MapEls,MapTpl),
    mkCons("type",[strg(TpNm),strg(RlSig),MapTpl],Tpl).

encPolicy(hard,T) :-
  mkTpl([],T).
encPolicy(soft,T) :-
  mkTpl([strg("soft")],T).

encMap([],[]).
encMap([(Lbl,Ix)|Map],[E|MM]) :-
  mkTpl([Lbl,intgr(Ix)],E),
  encMap(Map,MM).

countLocal(Nm,Lcs,Lcs,Hwm,Hwm) :-
  is_member(Nm,Lcs),!.
countLocal(Nm,Lcs,Lx,Hwm,H1) :-
  add_mem(Nm,Lcs,Lx),
  length(Lx,Ln),
  (Ln>Hwm ->
    H1 = Ln;
    H1 = Hwm).

assemBlock(Ins,Lb,Lbs,Lt,Lts,LsMap,Code) :-
    mnem(Ins,[Lb|Lbs],Lt,Lts,LsMap,Cde,[]),
    mkTpl(Cde,Code).

mnem([],_,Lt,Lt,_LsMap,Cdx,Cdx).
mnem([iLbl(Lb,Inner)|Ins],Lbs,Lt,Lts,LsMap,Code,Cdx) :-
      baseOffset(Lbs,Base),
      mnem([Inner],[(Lb,Base)|Lbs],Lt,Lt0,LsMap,Code,Cd0),
      mnem(Ins,Lbs,Lt0,Lts,LsMap,Cd0,Cdx).
mnem([iHalt(V0)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(0), Off0|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iAbort(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(1),intgr(L0), Off1|Cd],Cdx) :-
  findLit(Lt0,V0,L0,Lt1),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iCall(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(2),intgr(L0), LL1|Cd],Cdx) :-
  findLit(Lt0,V0,L0,Lt1),
  findLocals(V1,LsMap,LL1),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iOCall(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(3), Off0, LL1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocals(V1,LsMap,LL1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iEscape(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(4), strg(V0), LL1|Cd],Cdx) :-
  findLocals(V1,LsMap,LL1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iTCall(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(5),intgr(L0), LL1|Cd],Cdx) :-
  findLit(Lt0,V0,L0,Lt1),
  findLocals(V1,LsMap,LL1),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iTOCall(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(6), Off0, LL1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocals(V1,LsMap,LL1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iRSP(V0)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(7), Off0|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iRSX(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(8), intgr(L0), Off1|Cd],Cdx) :-
  findLevel(Lbls,V0,L0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iEntry(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(9), intgr(LL0), intgr(LL1)|Cd],Cdx) :-
  length(V0,LL0),
  length(V1,LL1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iRtn|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(10)|Cd],Cdx) :-
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iRet(V0)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(11), Off0|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iXRet(V0)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(12), Off0|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iBlock(V0)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(13), B0|Cd],Cdx) :-
  assemBlock(V0,none,Lbls,Lt0,Lt1,LsMap,B0),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iValof(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(14), Off0, B1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  assemBlock(V1,none,Lbls,Lt0,Lt1,LsMap,B1),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iBreak(V0)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(15), intgr(L0)|Cd],Cdx) :-
  findLevel(Lbls,V0,L0),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iResult(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(16), intgr(L0), Off1|Cd],Cdx) :-
  findLevel(Lbls,V0,L0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iLoop(V0)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(17), intgr(L0)|Cd],Cdx) :-
  findLevel(Lbls,V0,L0),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iIf(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(18), intgr(L0), Off1|Cd],Cdx) :-
  findLevel(Lbls,V0,L0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iIfNot(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(19), intgr(L0), Off1|Cd],Cdx) :-
  findLevel(Lbls,V0,L0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iICase(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(20), Off0, B1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  assemBlock(V1,none,Lbls,Lt0,Lt1,LsMap,B1),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iCase(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(21), Off0, B1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  assemBlock(V1,none,Lbls,Lt0,Lt1,LsMap,B1),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iIxCase(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(22), Off0, B1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  assemBlock(V1,none,Lbls,Lt0,Lt1,LsMap,B1),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iCLbl(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(23),intgr(L0), intgr(L1), Off2|Cd],Cdx) :-
  findLit(Lt0,V0,L0,Lt1),
  findLevel(Lbls,V1,L1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iCInt(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(24),intgr(L0), intgr(L1), Off2|Cd],Cdx) :-
  findLit(Lt0,V0,L0,Lt1),
  findLevel(Lbls,V1,L1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iCChar(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(25),intgr(L0), intgr(L1), Off2|Cd],Cdx) :-
  findLit(Lt0,V0,L0,Lt1),
  findLevel(Lbls,V1,L1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iCFlt(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(26),intgr(L0), intgr(L1), Off2|Cd],Cdx) :-
  findLit(Lt0,V0,L0,Lt1),
  findLevel(Lbls,V1,L1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iCLit(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(27),intgr(L0), intgr(L1), Off2|Cd],Cdx) :-
  findLit(Lt0,V0,L0,Lt1),
  findLevel(Lbls,V1,L1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iMC(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(28), Off0,intgr(L1)|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLit(Lt0,V1,L1,Lt1),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iMv(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(29), Off0, Off1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iLG(V0)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(30), strg(V0)|Cd],Cdx) :-
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iSG(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(31), strg(V0), Off1|Cd],Cdx) :-
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iSav(V0)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(32), Off0|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iLdSav(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(33), Off0, intgr(L1), Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLevel(Lbls,V1,L1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iTstSav(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(34), Off0, Off1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iStSav(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(35), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iCell(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(36), Off0, Off1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iGet(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(37), Off0, Off1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iAssign(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(38), Off0, Off1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iNth(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(39), Off0, intgr(V1), Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iStNth(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(40), Off0, intgr(V1), Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iIAdd(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(41), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iISub(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(42), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iIMul(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(43), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iIDiv(V0, V1, V2, V3)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(44), intgr(L0), Off1, Off2, Off3|Cd],Cdx) :-
  findLevel(Lbls,V0,L0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  findLocal(V3,LsMap,Off3),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iIMod(V0, V1, V2, V3)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(45), intgr(L0), Off1, Off2, Off3|Cd],Cdx) :-
  findLevel(Lbls,V0,L0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  findLocal(V3,LsMap,Off3),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iIAbs(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(46), Off0, Off1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iIEq(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(47), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iILt(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(48), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iIGe(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(49), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iCEq(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(50), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iCLt(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(51), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iCGe(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(52), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iBAnd(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(53), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iBOr(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(54), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iBXor(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(55), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iBLsl(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(56), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iBLsr(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(57), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iBAsr(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(58), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iBNot(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(59), Off0, Off1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iFAdd(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(60), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iFSub(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(61), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iFMul(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(62), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iFDiv(V0, V1, V2, V3)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(63), intgr(L0), Off1, Off2, Off3|Cd],Cdx) :-
  findLevel(Lbls,V0,L0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  findLocal(V3,LsMap,Off3),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iFMod(V0, V1, V2, V3)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(64), intgr(L0), Off1, Off2, Off3|Cd],Cdx) :-
  findLevel(Lbls,V0,L0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  findLocal(V3,LsMap,Off3),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iFAbs(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(65), Off0, Off1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iFEq(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(66), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iFLt(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(67), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iFGe(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(68), Off0, Off1, Off2|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iAlloc(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(69),intgr(L0), Off1, LL2|Cd],Cdx) :-
  findLit(Lt0,V0,L0,Lt1),
  findLocal(V1,LsMap,Off1),
  findLocals(V2,LsMap,LL2),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iClosure(V0, V1, V2)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(70),intgr(L0), Off1, Off2|Cd],Cdx) :-
  findLit(Lt0,V0,L0,Lt1),
  findLocal(V1,LsMap,Off1),
  findLocal(V2,LsMap,Off2),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iBump(V0)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(71), Off0|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iDrop(V0)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(72), Off0|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iFiber(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(73), Off0, Off1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iSuspend(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(74), Off0, Off1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iResume(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(75), Off0, Off1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iRetire(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(76), Off0, Off1|Cd],Cdx) :-
  findLocal(V0,LsMap,Off0),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iUnderflow|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(77)|Cd],Cdx) :-
  mnem(Ins,Lbls,Lt0,Ltx,LsMap,Cd,Cdx).
mnem([iLine(V0)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(78),intgr(L0)|Cd],Cdx) :-
  findLit(Lt0,V0,L0,Lt1),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iBind(V0, V1)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(79),intgr(L0), Off1|Cd],Cdx) :-
  findLit(Lt0,V0,L0,Lt1),
  findLocal(V1,LsMap,Off1),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).
mnem([iDBug(V0)|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(80),intgr(L0)|Cd],Cdx) :-
  findLit(Lt0,V0,L0,Lt1),
  mnem(Ins,Lbls,Lt1,Ltx,LsMap,Cd,Cdx).


baseOffset([(_,Base)|_],Base).
baseOffset([none|Lbs],Base) :-
  baseOffset(Lbs,Base).
baseOffset([],0).

findLevel(Lbls,Tgt,Lvl) :-
  findLevel(Tgt,Lbls,0,Lvl),!.

findLevel(Tgt,[(Tgt,_)|_],Lvl,Lvl) :-!.
findLevel(Tgt,[none|Ends],L,Lo) :-
      L1 is L+1,
      findLevel(Tgt,Ends,L1,Lo).
findLevel(Tgt,[_|Ends],L,Lo) :-
  findLevel(Tgt,Ends,L,Lo).

findLocal(Nm,LsMap,intgr(Off)) :-
      is_member((Nm,Off),LsMap),!.
findLocal(Nm,_,_) :-
      reportFatal("internal: Cannot assemble variable reference %s",[Nm]).

findLocals(Nms,LsMap,Tpl) :-
  fndLcls(Nms,LsMap,Offs),!,
  mkTpl(Offs,Tpl).

fndLcls([],_Map,[]).
fndLcls([Nm|Ns],Map,[Off|Ofs]) :-
  findLocal(Nm,Map,Off),
  fndLcls(Ns,Map,Ofs).

declareArgs(Lst,Specs,Map,Mapx,Els,Lclx) :-
  declareArgs(Lst,Specs,0,Map,Mapx,Els,Lclx).

declareArgs([],_,_,VrMap,VrMap,Lcs,Lcs) :-!.
declareArgs([Nm|As],Specs,Off,AgMap,AgMapx,Lcls,Lclx) :-
  is_member((Nm,Spec),Specs),!,
  Off1 is Off+1,
  declareArgs(As,Specs,Off1,[(Nm,Off)|AgMap],AgMapx,[(strg(Nm),intgr(Off),Spec)|Lcls],Lclx).
declareArgs([],_,_,VrMap,VrMap,Lcs,Lcs) :-!.
declareArgs([Nm|As],Specs,Off,AgMap,AgMapx,Lcls,Lclx) :-
  Off1 is Off+1,
  declareArgs(As,Specs,Off1,[(Nm,Off)|AgMap],AgMapx,Lcls,Lclx).

declareLocals(Lst,Specs,Map,Mapx,Lcls,Lclx) :-
  declareLocals(Lst,Specs,-1,Map,Mapx,Lcls,Lclx).

declareLocals([],_,_,Map,Map,Lcls,Lcls) :-!.
declareLocals([Nm|Lcls],Specs,Off,Map,Mapx,Lx,Lclx) :-
  is_member((Nm,Spec),Specs),!,
  Off1 is Off-1,
  declareLocals(Lcls,Specs,Off1,[(Nm,Off)|Map],Mapx,[(strg(Nm),intgr(Off),Spec)|Lx],Lclx).
declareLocals([Nm|Lcls],Specs,Off,Map,Mapx,Lx,Lclx) :-
  Off1 is Off-1,
  declareLocals(Lcls,Specs,Off1,[(Nm,Off)|Map],Mapx,Lx,Lclx).

createLocals(Entries,Tpl) :-
  sortEntries(Entries,Sorted),
  makeEntries(Sorted,Els),!,
  mkTpl(Els,Tpl).

sortEntries(Entries,Sorted) :-
  sort(Entries,ssa:compareEntry,Sorted).

compareEntry((_,intgr(O1),_),(_,intgr(O2),_)) :- O1<O2.

makeEntries([],[]).
makeEntries([(Nm,Off,Spec)|Es],[E|Entries]) :-
  mkTpl([Nm,Off,Spec],E),
  makeEntries(Es,Entries).

findLit(Lits,V,LtNo,Lits) :- is_member((V,LtNo),Lits),!.
findLit(Lits,V,LtNo,[(V,LtNo)|Lits]) :- length(Lits,LtNo).

mkLitTpl(Lits,Tpl) :-
    reverse(Lits,RLit),
    project0(RLit,Els),
    mkTpl(Els,Tpl).

mkInsTpl(Is,Tpl) :-
    map(Is,ssa:mkIns,Ins),
    mkTpl(Ins,Tpl).

mkIns(O,intgr(O)) :- number(O),!.
mkIns(S,strg(S)) :- string(S),!.
mkIns(C,Tpl) :- length(C,_),
  mkInsTpl(C,Tpl).

dispSSA(Prog) :-
  showCode(Prog,O),
  validSS(O),
  displayln(O).

showCode(func(Nm,H,Sig,Lcls,Ins),sq([HH,ss(" "),NN,ss(":"),ss(Sig),nl(0),LL,iv(nl(0),II)])) :-
  ssLocals(Lcls,LL),
  ssTrm(Nm,0,NN),
  showMnems(Ins,[0],II),
  ssPolicy(H,HH),!.
showCode(struct(Lbl,Sig,Ix),sq([ss("symb "),LL,ss(":"),TT,ss(" @ "),ix(Ix)])) :-
  ssTrm(Lbl,0,LL),
  ssType(Sig,false,0,TT).
showCode(tipe(Tp,_Rl,Map),sq([ss("type "),TT,ss(" = "),XX])) :-
  ssConsMap(Map,XX),
  ssType(Tp,false,0,TT).

ssArgs(Ags,sq([iv(nl(2),[ss("Args:")|LL]),nl(0)])) :- \+Ags=[], ssLcs(Ags,LL),!.
ssArgs([],ss("")).

ssLocals(Lcs,sq([iv(nl(2),[ss("Locals:")|LL]),nl(0)])) :- \+Lcs=[], ssLcs(Lcs,LL),!.
ssLocals([],ss("")).

ssLcs([],[]).
ssLcs([(Nm,Spec)|Lcs],[sq([ss(Nm),ss(":"),SS])|LL]) :-
  ssTrm(Spec,0,SS),
  ssLcs(Lcs,LL).

ssPolicy(soft,ss("soft")).
ssPolicy(hard,ss("hard")).

dispIns(Ins) :-
  showBlock(Ins,[],O),
  displayln(O).

showBlock(Ins,Prefix,iv(nl(K),II)) :-
  pcSpace(Prefix,K),
  showMnems(Ins,[0|Prefix],II).

showMnems([],_,[ss("end")]).
showMnems([iLbl(Lb,L)|Ins],Pc,[sq([ss(Lb),ss(":"),LL])|II]) :-
  showMnem(L,Pc,LL),
  bumpPc(Pc,Pc1),
  showMnems(Ins,Pc1,II).
showMnems([M|Ins],Pc,[MM|II]) :-
  showMnem(M,Pc,MM),
  bumpPc(Pc,Pc1),
  showMnems(Ins,Pc1,II).

showMnem(iHalt(V0),Pc,sq([PcDx,ss(": "),ss("Halt"),ss(" "),ss(V0)])) :- !,
  showPc(Pc,PcDx).
showMnem(iAbort(V0, V1),Pc,sq([PcDx,ss(": "),ss("Abort"),ss(" "),SS0,ss(" "),ss(V1)])) :- !,
  ssTrm(V0,0,SS0),
  showPc(Pc,PcDx).
showMnem(iCall(V0, V1),Pc,sq([PcDx,ss(": "),ss("Call"),ss(" "),SS0,ss(" "),VV1])) :- !,
  ssTrm(V0,0,SS0),
  showCallArgs(V1,VV1),
  showPc(Pc,PcDx).
showMnem(iOCall(V0, V1),Pc,sq([PcDx,ss(": "),ss("OCall"),ss(" "),ss(V0),ss(" "),VV1])) :- !,
  showCallArgs(V1,VV1),
  showPc(Pc,PcDx).
showMnem(iEscape(V0, V1),Pc,sq([PcDx,ss(": "),ss("Escape"),ss(" "),ss(V0),ss(" "),VV1])) :- !,
  showCallArgs(V1,VV1),
  showPc(Pc,PcDx).
showMnem(iTCall(V0, V1),Pc,sq([PcDx,ss(": "),ss("TCall"),ss(" "),SS0,ss(" "),VV1])) :- !,
  ssTrm(V0,0,SS0),
  showCallArgs(V1,VV1),
  showPc(Pc,PcDx).
showMnem(iTOCall(V0, V1),Pc,sq([PcDx,ss(": "),ss("TOCall"),ss(" "),ss(V0),ss(" "),VV1])) :- !,
  showCallArgs(V1,VV1),
  showPc(Pc,PcDx).
showMnem(iRSP(V0),Pc,sq([PcDx,ss(": "),ss("RSP"),ss(" "),ss(V0)])) :- !,
  showPc(Pc,PcDx).
showMnem(iRSX(V0, V1),Pc,sq([PcDx,ss(": "),ss("RSX"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iEntry(V0, V1),Pc,sq([PcDx,ss(": "),ss("Entry"),ss(" "),VV0,ss(" "),VV1])) :- !,
  showCallArgs(V0,VV0),
  showCallArgs(V1,VV1),
  showPc(Pc,PcDx).
showMnem(iRtn,Pc,sq([PcDx,ss(": "),ss("Rtn")])) :- !,
  showPc(Pc,PcDx).
showMnem(iRet(V0),Pc,sq([PcDx,ss(": "),ss("Ret"),ss(" "),ss(V0)])) :- !,
  showPc(Pc,PcDx).
showMnem(iXRet(V0),Pc,sq([PcDx,ss(": "),ss("XRet"),ss(" "),ss(V0)])) :- !,
  showPc(Pc,PcDx).
showMnem(iBlock(V0),Pc,sq([PcDx,ss(": "),ss("Block"),ss(" "),sq([nl(Dp),iv(nl(Dp),SS0)])])) :- !,
  blockPc(Pc,SPc),
  pcSpace(SPc,Dp),
  showMnems(V0,SPc,SS0),
  showPc(Pc,PcDx).
showMnem(iValof(V0, V1),Pc,sq([PcDx,ss(": "),ss("Valof"),ss(" "),ss(V0),ss(" "),sq([nl(Dp),iv(nl(Dp),SS1)])])) :- !,
  blockPc(Pc,SPc),
  pcSpace(SPc,Dp),
  showMnems(V1,SPc,SS1),
  showPc(Pc,PcDx).
showMnem(iBreak(V0),Pc,sq([PcDx,ss(": "),ss("Break"),ss(" "),ss(V0)])) :- !,
  showPc(Pc,PcDx).
showMnem(iResult(V0, V1),Pc,sq([PcDx,ss(": "),ss("Result"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iLoop(V0),Pc,sq([PcDx,ss(": "),ss("Loop"),ss(" "),ss(V0)])) :- !,
  showPc(Pc,PcDx).
showMnem(iIf(V0, V1),Pc,sq([PcDx,ss(": "),ss("If"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iIfNot(V0, V1),Pc,sq([PcDx,ss(": "),ss("IfNot"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iICase(V0, V1),Pc,sq([PcDx,ss(": "),ss("ICase"),ss(" "),ss(V0),ss(" "),sq([nl(Dp),iv(nl(Dp),SS1)])])) :- !,
  blockPc(Pc,SPc),
  pcSpace(SPc,Dp),
  showMnems(V1,SPc,SS1),
  showPc(Pc,PcDx).
showMnem(iCase(V0, V1),Pc,sq([PcDx,ss(": "),ss("Case"),ss(" "),ss(V0),ss(" "),sq([nl(Dp),iv(nl(Dp),SS1)])])) :- !,
  blockPc(Pc,SPc),
  pcSpace(SPc,Dp),
  showMnems(V1,SPc,SS1),
  showPc(Pc,PcDx).
showMnem(iIxCase(V0, V1),Pc,sq([PcDx,ss(": "),ss("IxCase"),ss(" "),ss(V0),ss(" "),sq([nl(Dp),iv(nl(Dp),SS1)])])) :- !,
  blockPc(Pc,SPc),
  pcSpace(SPc,Dp),
  showMnems(V1,SPc,SS1),
  showPc(Pc,PcDx).
showMnem(iCLbl(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("CLbl"),ss(" "),SS0,ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  ssTrm(V0,0,SS0),
  showPc(Pc,PcDx).
showMnem(iCInt(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("CInt"),ss(" "),SS0,ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  ssTrm(V0,0,SS0),
  showPc(Pc,PcDx).
showMnem(iCChar(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("CChar"),ss(" "),SS0,ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  ssTrm(V0,0,SS0),
  showPc(Pc,PcDx).
showMnem(iCFlt(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("CFlt"),ss(" "),SS0,ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  ssTrm(V0,0,SS0),
  showPc(Pc,PcDx).
showMnem(iCLit(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("CLit"),ss(" "),SS0,ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  ssTrm(V0,0,SS0),
  showPc(Pc,PcDx).
showMnem(iMC(V0, V1),Pc,sq([PcDx,ss(": "),ss("MC"),ss(" "),ss(V0),ss(" "),SS1])) :- !,
  ssTrm(V1,0,SS1),
  showPc(Pc,PcDx).
showMnem(iMv(V0, V1),Pc,sq([PcDx,ss(": "),ss("Mv"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iLG(V0),Pc,sq([PcDx,ss(": "),ss("LG"),ss(" "),ss(V0)])) :- !,
  showPc(Pc,PcDx).
showMnem(iSG(V0, V1),Pc,sq([PcDx,ss(": "),ss("SG"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iSav(V0),Pc,sq([PcDx,ss(": "),ss("Sav"),ss(" "),ss(V0)])) :- !,
  showPc(Pc,PcDx).
showMnem(iLdSav(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("LdSav"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iTstSav(V0, V1),Pc,sq([PcDx,ss(": "),ss("TstSav"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iStSav(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("StSav"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iCell(V0, V1),Pc,sq([PcDx,ss(": "),ss("Cell"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iGet(V0, V1),Pc,sq([PcDx,ss(": "),ss("Get"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iAssign(V0, V1),Pc,sq([PcDx,ss(": "),ss("Assign"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iNth(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("Nth"),ss(" "),ss(V0),ss(" "),ix(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iStNth(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("StNth"),ss(" "),ss(V0),ss(" "),ix(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iIAdd(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("IAdd"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iISub(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("ISub"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iIMul(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("IMul"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iIDiv(V0, V1, V2, V3),Pc,sq([PcDx,ss(": "),ss("IDiv"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2),ss(" "),ss(V3)])) :- !,
  showPc(Pc,PcDx).
showMnem(iIMod(V0, V1, V2, V3),Pc,sq([PcDx,ss(": "),ss("IMod"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2),ss(" "),ss(V3)])) :- !,
  showPc(Pc,PcDx).
showMnem(iIAbs(V0, V1),Pc,sq([PcDx,ss(": "),ss("IAbs"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iIEq(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("IEq"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iILt(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("ILt"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iIGe(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("IGe"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iCEq(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("CEq"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iCLt(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("CLt"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iCGe(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("CGe"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iBAnd(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("BAnd"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iBOr(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("BOr"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iBXor(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("BXor"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iBLsl(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("BLsl"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iBLsr(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("BLsr"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iBAsr(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("BAsr"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iBNot(V0, V1),Pc,sq([PcDx,ss(": "),ss("BNot"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iFAdd(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("FAdd"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iFSub(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("FSub"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iFMul(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("FMul"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iFDiv(V0, V1, V2, V3),Pc,sq([PcDx,ss(": "),ss("FDiv"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2),ss(" "),ss(V3)])) :- !,
  showPc(Pc,PcDx).
showMnem(iFMod(V0, V1, V2, V3),Pc,sq([PcDx,ss(": "),ss("FMod"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2),ss(" "),ss(V3)])) :- !,
  showPc(Pc,PcDx).
showMnem(iFAbs(V0, V1),Pc,sq([PcDx,ss(": "),ss("FAbs"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iFEq(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("FEq"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iFLt(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("FLt"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iFGe(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("FGe"),ss(" "),ss(V0),ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  showPc(Pc,PcDx).
showMnem(iAlloc(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("Alloc"),ss(" "),SS0,ss(" "),ss(V1),ss(" "),VV2])) :- !,
  ssTrm(V0,0,SS0),
  showCallArgs(V2,VV2),
  showPc(Pc,PcDx).
showMnem(iClosure(V0, V1, V2),Pc,sq([PcDx,ss(": "),ss("Closure"),ss(" "),SS0,ss(" "),ss(V1),ss(" "),ss(V2)])) :- !,
  ssTrm(V0,0,SS0),
  showPc(Pc,PcDx).
showMnem(iBump(V0),Pc,sq([PcDx,ss(": "),ss("Bump"),ss(" "),ss(V0)])) :- !,
  showPc(Pc,PcDx).
showMnem(iDrop(V0),Pc,sq([PcDx,ss(": "),ss("Drop"),ss(" "),ss(V0)])) :- !,
  showPc(Pc,PcDx).
showMnem(iFiber(V0, V1),Pc,sq([PcDx,ss(": "),ss("Fiber"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iSuspend(V0, V1),Pc,sq([PcDx,ss(": "),ss("Suspend"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iResume(V0, V1),Pc,sq([PcDx,ss(": "),ss("Resume"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iRetire(V0, V1),Pc,sq([PcDx,ss(": "),ss("Retire"),ss(" "),ss(V0),ss(" "),ss(V1)])) :- !,
  showPc(Pc,PcDx).
showMnem(iUnderflow,Pc,sq([PcDx,ss(": "),ss("Underflow")])) :- !,
  showPc(Pc,PcDx).
showMnem(iLine(V0),Pc,sq([PcDx,ss(": "),ss("Line"),ss(" "),SS0])) :- !,
  ssTrm(V0,0,SS0),
  showPc(Pc,PcDx).
showMnem(iBind(V0, V1),Pc,sq([PcDx,ss(": "),ss("Bind"),ss(" "),SS0,ss(" "),ss(V1)])) :- !,
  ssTrm(V0,0,SS0),
  showPc(Pc,PcDx).
showMnem(iDBug(V0),Pc,sq([PcDx,ss(": "),ss("dBug"),ss(" "),SS0])) :- !,
  ssTrm(V0,0,SS0),
  showPc(Pc,PcDx).

showCallArgs(Lcs,sq([ss("("),sq(LL),ss(")")])) :-
  shLs(Lcs,ss(""),LL).

shLs([],_,[]) :-!.
shLs([L|Ls],Sep,[Sep,ss(L)|LLs]) :-
  shLs(Ls,ss(", "),LLs).

ssaHash(672621438903973766).

bumpPc([Pc|Rest],[Pc1|Rest]) :- Pc1 is Pc+1.

blockPc(Pc,[0|Pc]).

showPc(Pc,iv(ss("."),Pcs)) :-
  reverse(Pc,RPc),
  map(RPc,ssa:shPc,Pcs).

shPc(I,ix(I)).

pcSpace(Pc,Dp) :-
  length(Pc,D),
  Dp is D*2.

