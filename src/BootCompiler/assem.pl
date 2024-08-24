/* Automatically generated, do not edit */

:- module(assemble,[assem/2, dispIns/1]).
:- use_module(misc).
:- use_module(lterms).
:- use_module(types).
:- use_module(encode).
:- use_module(display).
:- use_module(escapes).

assem(func(Nm,H,Sig,Lx,Ins),MTpl) :-
    findLit([],Nm,_,Ls0),
    assemBlock(Ins,labels{},Ls0,Lts,[],Lcs,[],Lines,0,_,[],Cde,[]),
    mkInsTpl(Cde,Code),
    mkLitTpl(Lts,LtTpl),
    mkTpl(Lcs,LcsTpl),
    sortLines(Lines,SLines),
    mkTpl(SLines,LnsTpl),
    encPolicy(H,HP),
    mkCons("func",[Nm,HP,strg(Sig),intgr(Lx),Code,LtTpl,LcsTpl,LnsTpl],MTpl).
assem(struct(Lbl,Sig,Ix),Tpl) :-
    mkCons("cons",[Lbl,Sig,intgr(Ix)],Tpl).
assem(tipe(Tp,Rl,Map),Tpl) :-
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

assemBlock(Ins,Lbs,Lt,Lts,Lc,Lcx,Ln,Lnx,Pc,Pcx,Ends,Code,Cdx) :-
    genLblTbl(Ins,Pc,Pc1,Lbs,Lbs0),!,
    mnem(Ins,Lbs0,Lt,Lts,Lc,Lcx,Ln,Lnx,Pc,Pcx,[Pc1|Ends],Code,Cdx).

mnem([],_,Lt,Lt,Lc,Lc,Lns,Lns,Pc,Pc,_,Cdx,Cdx).
mnem([iLbl(_)|Ins],Lbs,Lt,Lts,Lc,Lcx,Ln,Lnx,Pc,Pcx,Ends,Code,Cdx) :- mnem(Ins,Lbs,Lt,Lts,Lc,Lcx,Ln,Lnx,Pc,Pcx,Ends,Code,Cdx).
mnem([iLocal(Nm,Frm,End,Off)|Ins],Lbs,Lt,Lts,Lc,Lcx,Ln,Lnx,Pc,Pcx,Ends,Code,Cdx) :-
    findLbl(Frm,Lbs,F),
    findLbl(End,Lbs,T),
    mkTpl([strg(Nm),intgr(F),intgr(T),intgr(Off)],Entry),
    (is_member(Entry,Lc)->Lc0=Lc;Lc0=[Entry|Lc]),
    mnem(Ins,Lbs,Lt,Lts,Lc0,Lcx,Ln,Lnx,Pc,Pcx,Ends,Code,Cdx).
mnem([iLine(Loc)|Ins],Lbs,Lt,Lts,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,Code,Cdx) :-
    mkTpl([Loc,intgr(Pc)],LneEntry),
    (is_member(LneEntry,Lns) -> Lns1 = Lns; Lns1=[LneEntry|Lns]),
    mnem(Ins,Lbs,Lt,Lts,Lc,Lcx,Lns1,Lnx,Pc,Pcx,Ends,Code,Cdx).
mnem([iHalt(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[0,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iNop|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[1|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iAbort|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[2|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[3,LtNo|M],Cdx) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iOCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[4,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iEscape(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[5,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iTCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[6,LtNo|M],Cdx) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iTOCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[7,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iLocals(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[8,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iRet|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[9|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iJmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[10,Off|M],Cdx) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iDrop|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[11|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iDup|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[12|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iRot(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[13,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iRst(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[14,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFiber|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[15|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iSpawn|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[16|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iSuspend|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[17|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iResume|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[18|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iRetire|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[19|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iUnderflow|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[20|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iTEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[21|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iTry(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[22,Off|M],Cdx) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iEndTry|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[23|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iThrow|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[24|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iReset|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[25|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iShift|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[26|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iInvoke|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[27|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iLdV|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[28|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iLdC(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[29,LtNo|M],Cdx) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iLdA(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[30,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iLdL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[31,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iStL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[32,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iStV(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[33,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iTL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[34,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iStA(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[35,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iLdG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[36,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iStG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[37,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iTG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[38,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iThunk|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[39|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iLdTh(W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[40,Off|M],Cdx) :- Pc1 is Pc+3,
      findLbl(W,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iStTh|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[41|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iTTh|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[42|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iCell|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[43|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iGet|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[44|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iAssign|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[45|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iCLbl(V,W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[46,LtNo,Off|M],Cdx) :- Pc1 is Pc+5,
      findLit(Lt,V,LtNo,Lt1),
      findLbl(W,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[47,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iStNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[48,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIf(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[49,Off|M],Cdx) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIfNot(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[50,Off|M],Cdx) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iCase(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[51,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIndxJmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[52,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iUnpack(V,W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[53,LtNo,Off|M],Cdx) :- Pc1 is Pc+5,
      findLit(Lt,V,LtNo,Lt1),
      findLbl(W,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIAdd|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[54|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iISub|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[55|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIMul|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[56|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIDiv|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[57|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIMod|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[58|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIAbs|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[59|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[60|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iILt|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[61|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIGe|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[62|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iICmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[63,Off|M],Cdx) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iCEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[64|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iCLt|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[65|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iCGe|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[66|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iCCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[67,Off|M],Cdx) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iBAnd|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[68|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iBOr|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[69|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iBXor|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[70|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iBLsl|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[71|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iBLsr|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[72|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iBAsr|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[73|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iBNot|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[74|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFAdd|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[75|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFSub|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[76|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFMul|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[77|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFDiv|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[78|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFMod|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[79|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFAbs|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[80|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[81|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFLt|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[82|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFGe|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[83|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[84,Off|M],Cdx) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iAlloc(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[85,LtNo|M],Cdx) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iClosure(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[86,LtNo|M],Cdx) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[87,Off|M],Cdx) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFrame(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[88,LtNo|M],Cdx) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iDBug|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[89|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).


findLevel(0,[Tgt|_],Tgt) :-!.
findLevel(L,[_|Ends],Tgt) :-
  L1 is L-1,
  findLevel(L1,Ends,Tgt).

findLbl(L,Lbs,Tgt) :-
  makeKey(L,Ky),
  get_dict(Ky,Lbs,Tgt),!.

defineLbl(Lbl,Pc,Lbls,Lblx) :-
  makeKey(Lbl,Key),
  put_dict(Key,Lbls,Pc,Lblx).

genLblTbl([],Pc,Pc,Lbls,Lbls).
genLblTbl([iLbl(Lbl)|Ins],Pc,Pcx,Lbls,Lbx) :-
  defineLbl(Lbl,Pc,Lbls,Lbli),
  genLblTbl(Ins,Pc,Pcx,Lbli,Lbx).
genLblTbl([iLocal(_,_,_,_)|Ins],Pc,Pcx,Lbls,Lbx) :- genLblTbl(Ins,Pc,Pcx,Lbls,Lbx).
genLblTbl([iLine(_)|Ins],Pc,Pcx,Lbls,Lbx) :- genLblTbl(Ins,Pc,Pcx,Lbls,Lbx).
genLblTbl([iHalt(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iNop|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iAbort|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iCall(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iOCall(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iEscape(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iTCall(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iTOCall(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iLocals(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iRet|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iJmp(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iDrop|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iDup|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iRot(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iRst(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iFiber|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iSpawn|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iSuspend|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iResume|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iRetire|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iUnderflow|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iTEq|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iTry(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iEndTry|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iThrow|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iReset|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iShift|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iInvoke|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iLdV|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iLdC(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iLdA(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iLdL(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iStL(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iStV(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iTL(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iStA(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iLdG(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iStG(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iTG(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iThunk|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iLdTh(_B)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iStTh|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iTTh|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iCell|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iGet|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iAssign|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iCLbl(_A,_B)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+5,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iNth(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iStNth(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iIf(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iIfNot(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iCase(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iIndxJmp(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iUnpack(_A,_B)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+5,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iIAdd|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iISub|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iIMul|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iIDiv|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iIMod|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iIAbs|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iIEq|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iILt|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iIGe|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iICmp(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iCEq|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iCLt|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iCGe|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iCCmp(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iBAnd|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iBOr|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iBXor|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iBLsl|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iBLsr|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iBAsr|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iBNot|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iFAdd|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iFSub|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iFMul|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iFDiv|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iFMod|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iFAbs|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iFEq|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iFLt|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iFGe|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iFCmp(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iAlloc(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iClosure(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iCmp(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iFrame(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iDBug|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).


pcGap(Pc,Tgt,Off) :- Off is Tgt-Pc.

findLit(Lits,V,LtNo,Lits) :- is_member((V,LtNo),Lits),!.
findLit(Lits,V,LtNo,[(V,LtNo)|Lits]) :- length(Lits,LtNo).

mkLitTpl(Lits,Tpl) :-
    reverse(Lits,RLit),
    project0(RLit,Els),
    mkTpl(Els,Tpl).

mkInsTpl(Is,Tpl) :-
    map(Is,assemble:mkIns,Ins),
    mkTpl(Ins,Tpl).

mkIns(O,intgr(O)) :- number(O).
mkIns(S,strg(S)) :- string(S).
mkIns(C,Tpl) :- length(C,_),
  mkInsTpl(C,Args),
  mkTpl(Args,Tpl).

sortLines(Lns,Sorted) :-
 sort(Lns,assemble:compLine,Sorted).

compLine(ctpl(_,[_,intgr(Pc1)]),ctpl(_,[_,intgr(Pc2)])) :- Pc1<Pc2.

dispIns(Prog) :-
  showIns(Prog,O),
  displayln(O).

showIns(func(Nm,H,Sig,_Lx,Ins),sq([HH,ss(" "),NN,ss(":"),ss(Sig),nl(0),iv(nl(0),II)])) :-
  ssTrm(Nm,0,NN),
  showMnem(Ins,0,_,[],II),
  ssPolicy(H,HH),!.

ssPolicy(soft,ss("soft")).
ssPolicy(hard,ss("hard")).

showMnem([],Pc,Pc,_,[]).
showMnem([iLbl(Lb)|Ins],Pc,PcX,Lbs,[sq([ss(Lb),ss(":")])|II]) :-
  showMnem(Ins,Pc,PcX,[(Lb,Pc)|Lbs],II).
showMnem([iLocal(Nm,Frm,End,Off)|Ins],Pc,PcX,Lbs,[sq([ss(Nm),ss("::"),ss(Frm),ss("-"),ss(End),ss(":"),ix(Off)])|II]) :-
  showMnem(Ins,Pc,PcX,Lbs,II).
showMnem([iLine(Loc)|Ins],Pc,PcX,Lbs,[sq([ss("Line "),LL])|II]) :-
  ssTrm(Loc,0,LL),
  showMnem(Ins,Pc,PcX,Lbs,II).
showMnem([iHalt(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Halt"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iNop|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Nop")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iAbort|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Abort")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iCall(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Call"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iOCall(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("OCall"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iEscape(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Escape"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),!,
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iTCall(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("TCall"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iTOCall(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("TOCall"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iLocals(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Locals"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iRet|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Ret")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iJmp(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Jmp"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iDrop|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Drop")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iDup|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Dup")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iRot(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Rot"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iRst(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Rst"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iFiber|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Fiber")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iSpawn|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Spawn")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iSuspend|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Suspend")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iResume|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Resume")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iRetire|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Retire")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iUnderflow|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Underflow")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iTEq|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("TEq")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iTry(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Try"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iEndTry|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("EndTry")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iThrow|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Throw")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iReset|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Reset")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iShift|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Shift")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iInvoke|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Invoke")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iLdV|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("LdV")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iLdC(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("LdC"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iLdA(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("LdA"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iLdL(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("LdL"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iStL(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("StL"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iStV(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("StV"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iTL(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("TL"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iStA(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("StA"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iLdG(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("LdG"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iStG(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("StG"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iTG(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("TG"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iThunk|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Thunk")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iLdTh(V)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("LdTh"), ss(","), VV])|II]) :- !,
  Pc0 is Pc+1,
  VV=ss(V),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iStTh|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("StTh")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iTTh|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("TTh")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iCell|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Cell")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iGet|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Get")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iAssign|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Assign")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iCLbl(U,V)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("CLbl"), ss(" "), UU, ss(","), VV])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  VV=ss(V),
  Pc2 is Pc1+2,
  showMnem(Ins,Pc2,PcX,Lbls,II).
showMnem([iNth(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Nth"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iStNth(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("StNth"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iIf(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("If"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iIfNot(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("IfNot"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iCase(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Case"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iIndxJmp(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("IndxJmp"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iUnpack(U,V)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Unpack"), ss(" "), UU, ss(","), VV])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  VV=ss(V),
  Pc2 is Pc1+2,
  showMnem(Ins,Pc2,PcX,Lbls,II).
showMnem([iIAdd|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("IAdd")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iISub|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("ISub")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iIMul|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("IMul")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iIDiv|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("IDiv")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iIMod|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("IMod")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iIAbs|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("IAbs")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iIEq|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("IEq")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iILt|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("ILt")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iIGe|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("IGe")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iICmp(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("ICmp"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iCEq|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("CEq")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iCLt|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("CLt")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iCGe|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("CGe")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iCCmp(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("CCmp"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iBAnd|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("BAnd")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iBOr|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("BOr")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iBXor|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("BXor")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iBLsl|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("BLsl")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iBLsr|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("BLsr")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iBAsr|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("BAsr")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iBNot|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("BNot")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iFAdd|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("FAdd")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iFSub|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("FSub")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iFMul|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("FMul")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iFDiv|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("FDiv")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iFMod|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("FMod")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iFAbs|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("FAbs")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iFEq|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("FEq")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iFLt|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("FLt")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iFGe|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("FGe")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).
showMnem([iFCmp(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("FCmp"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iAlloc(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Alloc"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iClosure(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Closure"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iCmp(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Cmp"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iFrame(U)|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("Frame"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,PcX,Lbls,II).
showMnem([iDBug|Ins],Pc,PcX,Lbls,[sq([ix(Pc),ss(":"),ss("dBug")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,PcX,Lbls,II).

