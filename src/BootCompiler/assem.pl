/* Automatically generated, do not edit */

:- module(assemble,[assem/2, dispIns/1]).
:- use_module(misc).
:- use_module(lterms).
:- use_module(encode).
:- use_module(display).

assem(method(Nm,Sig,Lx,Ins),MTpl) :-
    findLit([],Nm,_,Ls0),
    assemBlock(Ins,labels{},Ls0,Lts,[],Lcs,[],Lines,0,_,[],Cde,[]),
    mkInsTpl(Cde,Code),
    mkLitTpl(Lts,LtTpl),
    mkTpl(Lcs,LcsTpl),
    sortLines(Lines,SLines),
    mkTpl(SLines,LnsTpl),
    mkCons("method",[Nm,strg(Sig),intgr(Lx),Code,LtTpl,LcsTpl,LnsTpl],MTpl).
assem(struct(Lbl,Ix,Sig),Tpl) :-
    mkCons("cons",[Lbl,Sig,intgr(Ix)],Tpl).
assem(tipe(Tp,Rl,Map),Tpl) :-
    encMap(Map,MapEls),
    encType(Tp,TpSig),
    encType(Rl,RlSig),
    mkTpl(MapEls,MapTpl),
    mkCons("type",[strg(TpSig),strg(RlSig),MapTpl],Tpl).

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
    mnem([iDLine(Loc)|Ins],Lbs,Lt,Lts,Lc,Lcx,Lns1,Lnx,Pc,Pcx,Ends,Code,Cdx).
mnem([iHalt(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[0,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[1,LtNo|M],Cdx) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iOCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[2,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iEscape(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[3,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iTail(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[4,LtNo|M],Cdx) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iOTail(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[5,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iRet|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[6|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iJmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[7,Off|M],Cdx) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iDrop|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[8|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iDup|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[9|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iRst(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[10,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iPrompt|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[11|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iCut|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[12|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iRestore|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[13|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iLdV|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[14|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iLdG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[15,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iLdC(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[16,LtNo|M],Cdx) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iLdA(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[17,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iLdL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[18,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iStL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[19,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iStV(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[20,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iTL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[21,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iStA(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[22,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iStG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[23,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iTG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[24,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iCLbl(V,W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[25,LtNo,Off|M],Cdx) :- Pc1 is Pc+5,
      findLit(Lt,V,LtNo,Lt1),
      findLbl(W,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iCmpVd(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[26,Off|M],Cdx) :- Pc1 is Pc+3,
      findLevel(V,Ends,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[27,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iStNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[28,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iGet(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[29,LtNo|M],Cdx) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iSet(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[30,LtNo|M],Cdx) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIf(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[31,Off|M],Cdx) :- Pc1 is Pc+3,
      findLevel(V,Ends,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIfNot(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[32,Off|M],Cdx) :- Pc1 is Pc+3,
      findLevel(V,Ends,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iCase(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[33,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIndxJmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[34,V|M],Cdx) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iUnpack(V,W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[35,LtNo,Off|M],Cdx) :- Pc1 is Pc+5,
      findLit(Lt,V,LtNo,Lt1),
      findLbl(W,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIAdd|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[36|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iISub|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[37|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIMul|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[38|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIDiv|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[39|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIMod|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[40|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIAbs|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[41|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[42|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iILt|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[43|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iIGe|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[44|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iICmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[45,Off|M],Cdx) :- Pc1 is Pc+3,
      findLevel(V,Ends,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iBAnd|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[46|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iBOr|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[47|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iBXor|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[48|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iBLsl|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[49|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iBLsr|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[50|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iBAsr|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[51|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iBNot|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[52|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFAdd|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[53|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFSub|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[54|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFMul|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[55|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFDiv|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[56|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFMod|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[57|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFAbs|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[58|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[59|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFLt|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[60|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFGe|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[61|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[62,Off|M],Cdx) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iAlloc(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[63,LtNo|M],Cdx) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iAlTpl(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[64,LtNo|M],Cdx) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[65,Off|M],Cdx) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iComp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[66,Off|M],Cdx) :- Pc1 is Pc+3,
      findLevel(V,Ends,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iFrame(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[67,LtNo|M],Cdx) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iThrow(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[68,Off|M],Cdx) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iUnwind(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[69,Off|M],Cdx) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iDLine(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[70,LtNo|M],Cdx) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iDBug|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[71|M],Cdx) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).
mnem([iDBreak|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[72|M],Cdx) :- Pc1 is Pc+1,
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
genLblTbl([iLine(Lc)|Ins],Pc,Pcx,Lbls,Lbx) :- genLblTbl([iDLine(Lc)|Ins],Pc,Pcx,Lbls,Lbx).
genLblTbl([iHalt(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iCall(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iOCall(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iEscape(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iTail(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iOTail(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iRet|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iJmp(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iDrop|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iDup|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iRst(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iPrompt|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iCut|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iRestore|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iLdV|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iLdG(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iLdC(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iLdA(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iLdL(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iStL(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iStV(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iTL(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iStA(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iStG(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iTG(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iCLbl(_A,_B)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+5,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iCmpVd(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iNth(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iStNth(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iGet(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iSet(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
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
genLblTbl([iAlTpl(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iCmp(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iComp(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iFrame(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iThrow(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iUnwind(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iDLine(_A)|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iDBug|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).
genLblTbl([iDBreak|Ins],Pc,Pcx,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).


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

sortLines(Lns,Sorted) :-
 sort(Lns,assemble:compLine,Sorted).

compLine(ctpl(_,[_,intgr(Pc1)]),ctpl(_,[_,intgr(Pc2)])) :- Pc1<Pc2.

dispIns(Prog) :-
  showIns(Prog,O),
  displayln(O).

showIns(method(Nm,Sig,Lx,Ins),sq([NN,ss(":"),ss(Sig),nl(0),iv(nl(0),[sq([ss("locals "),ix(Lx)])|II])])) :-
  ssTrm(Nm,0,NN),
  showMnem(Ins,0,[],II).

showMnem([],_,_,[]).
showMnem([iLbl(Lb)|Ins],Pc,Lbs,[sq([ss(Lb),ss(":")])|II]) :-
  showMnem(Ins,Pc,[(Lb,Pc)|Lbs],II).
showMnem([iLocal(Nm,Frm,End,_Off)|Ins],Pc,Lbs,[sq([ss(Nm),ss("::"),ss(Frm),ss("-"),ss(End)])|II]) :-
  showMnem(Ins,Pc,Lbs,II).
showMnem([iLine(Loc)|Ins],Pc,Lbs,[sq([ss("Line "),LL])|II]) :-
  ssTrm(Loc,0,LL),
  showMnem(Ins,Pc,Lbs,II).
showMnem([iHalt(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Halt"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iCall(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Call"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iOCall(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("OCall"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iEscape(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Escape"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iTail(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Tail"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iOTail(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("OTail"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iRet|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Ret")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iJmp(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Jmp"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iDrop|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Drop")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iDup|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Dup")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iRst(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Rst"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iPrompt|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Prompt")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iCut|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Cut")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iRestore|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Restore")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iLdV|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("LdV")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iLdG(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("LdG"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iLdC(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("LdC"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iLdA(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("LdA"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iLdL(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("LdL"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iStL(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("StL"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iStV(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("StV"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iTL(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("TL"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iStA(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("StA"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iStG(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("StG"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iTG(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("TG"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iCLbl(U,V)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("CLbl"), ss(" "), UU, ss(","), VV])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  VV=ss(V),
  Pc2 is Pc1+2,
  showMnem(Ins,Pc2,Lbls,II).
showMnem([iCmpVd(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("CmpVd"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iNth(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Nth"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iStNth(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("StNth"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iGet(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Get"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iSet(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Set"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iIf(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("If"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iIfNot(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("IfNot"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iCase(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Case"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iIndxJmp(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("IndxJmp"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iUnpack(U,V)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Unpack"), ss(" "), UU, ss(","), VV])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  VV=ss(V),
  Pc2 is Pc1+2,
  showMnem(Ins,Pc2,Lbls,II).
showMnem([iIAdd|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("IAdd")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iISub|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("ISub")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iIMul|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("IMul")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iIDiv|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("IDiv")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iIMod|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("IMod")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iIAbs|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("IAbs")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iIEq|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("IEq")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iILt|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("ILt")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iIGe|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("IGe")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iICmp(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("ICmp"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iBAnd|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("BAnd")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iBOr|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("BOr")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iBXor|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("BXor")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iBLsl|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("BLsl")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iBLsr|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("BLsr")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iBAsr|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("BAsr")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iBNot|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("BNot")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iFAdd|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("FAdd")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iFSub|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("FSub")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iFMul|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("FMul")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iFDiv|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("FDiv")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iFMod|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("FMod")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iFAbs|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("FAbs")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iFEq|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("FEq")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iFLt|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("FLt")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iFGe|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("FGe")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iFCmp(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("FCmp"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iAlloc(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Alloc"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iAlTpl(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("AlTpl"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iCmp(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Cmp"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iComp(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Comp"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ix(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iFrame(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Frame"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iThrow(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Throw"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iUnwind(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("Unwind"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  UU=ss(U),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iDLine(U)|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("dLine"), ss(" "), UU])|II]) :- !,
  Pc0 is Pc+1,
  ssTrm(U,0,UU),
  Pc1 is Pc0+2,
  showMnem(Ins,Pc1,Lbls,II).
showMnem([iDBug|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("dBug")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).
showMnem([iDBreak|Ins],Pc,Lbls,[sq([ix(Pc),ss(":"),ss("dBreak")])|II]) :- !,
  Pc0 is Pc+1,
  showMnem(Ins,Pc0,Lbls,II).

