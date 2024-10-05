/* Automatically generated, do not edit */

:- module(assemble,[assem/2, dispIns/1, opcodeHash/1]).
:- use_module(misc).
:- use_module(lterms).
:- use_module(types).
:- use_module(encode).
:- use_module(display).
:- use_module(escapes).

assem(func(Nm,H,Sig,Lx,Ins),MTpl) :-
    findLit([],Nm,_,Ls0),
    findLit(Ls0,Sig,SgIx,Ls1),
    assemBlock(Ins,[],Ls1,Lts,[],Lcs,Cde,[]),
    mkInsTpl(Cde,Code),
    mkLitTpl(Lts,LtTpl),
    mkTpl(Lcs,LcsTpl),
    encPolicy(H,HP),
    mkCons("func",[Nm,HP,intgr(SgIx),intgr(Lx),Code,LtTpl,LcsTpl],MTpl).
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

assemBlock(Ins,Lbs,Lt,Lts,Lc,Lcx,Code,Cdx) :-
    mnem(Ins,[none|Lbs],Lt,Lts,Lc,Lcx,Code,Cdx).

mnem([],_,Lt,Lt,Lc,Lc,Cdx,Cdx).
mnem([iLbl(Lb,Inner)|Ins],Lbs,Lt,Lts,Lc,Lcx,Code,Cdx) :-
  mnem(Inner,[Lb|Lbs],Lt,Lt0,Lc,Lc0,Code,Cd0),
  mnem(Ins,Lbs,Lt0,Lts,Lc0,Lcx,Cd0,Cdx).
mnem([iLocal(Nm,Scope,Off)|Ins],Lbs,Lt,Lts,Lc,Lcx,Code,Cdx) :-
    findLevel(0,Lbs,bp(Scope),Lvl),
    mkTpl([strg(Nm),intgr(Lvl),intgr(Off)],Entry),
    (is_member(Entry,Lc)->Lc0=Lc;Lc0=[Entry|Lc]),
    mnem(Ins,Lbs,Lt,Lts,Lc0,Lcx,Code,Cdx).
mnem([iHalt(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[0,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iNop|Ins],Lbls,Lt,Ltx,Lc,Lcx,[1|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iAbort|Ins],Lbls,Lt,Ltx,Lc,Lcx,[2|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[3,LtNo|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iOCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[4,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iEscape(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[5,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iTCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[6,LtNo|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iTOCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[7,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iLocals(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[8,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iRet|Ins],Lbls,Lt,Ltx,Lc,Lcx,[9|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iBlock(V,W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[10,LtNo,B|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      assemBlock(W,Lbls,Lt1,Lt2,Lc,Lcx,B,[]),
      mnem(Ins,Lbls,Lt2,Ltx,Lc,Lcx,M,Cdx).
mnem([iBreak(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[11,Lvl|M],Cdx) :-
      findLevel(V,Lbls,V,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iDrop|Ins],Lbls,Lt,Ltx,Lc,Lcx,[12|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iDup|Ins],Lbls,Lt,Ltx,Lc,Lcx,[13|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iRot(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[14,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iRst(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[15,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFiber|Ins],Lbls,Lt,Ltx,Lc,Lcx,[16|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iSpawn|Ins],Lbls,Lt,Ltx,Lc,Lcx,[17|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iSuspend|Ins],Lbls,Lt,Ltx,Lc,Lcx,[18|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iResume|Ins],Lbls,Lt,Ltx,Lc,Lcx,[19|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iRetire|Ins],Lbls,Lt,Ltx,Lc,Lcx,[20|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iUnderflow|Ins],Lbls,Lt,Ltx,Lc,Lcx,[21|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iTEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,[22|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iTry(V,W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[23,LtNo,B|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      assemBlock(W,Lbls,Lt1,Lt2,Lc,Lcx,B,[]),
      mnem(Ins,Lbls,Lt2,Ltx,Lc,Lcx,M,Cdx).
mnem([iEndTry(W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[24,Lvl|M],Cdx) :-
      findLevel(Lbls,W,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iThrow|Ins],Lbls,Lt,Ltx,Lc,Lcx,[25|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iReset|Ins],Lbls,Lt,Ltx,Lc,Lcx,[26|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iShift|Ins],Lbls,Lt,Ltx,Lc,Lcx,[27|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iInvoke|Ins],Lbls,Lt,Ltx,Lc,Lcx,[28|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iLdV|Ins],Lbls,Lt,Ltx,Lc,Lcx,[29|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iLdC(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[30,LtNo|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iLdA(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[31,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iLdL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[32,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iStL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[33,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iStV(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[34,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iTL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[35,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iStA(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[36,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iLdG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[37,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iStG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[38,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iTG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[39,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iThunk|Ins],Lbls,Lt,Ltx,Lc,Lcx,[40|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iLdTh|Ins],Lbls,Lt,Ltx,Lc,Lcx,[41|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iStTh|Ins],Lbls,Lt,Ltx,Lc,Lcx,[42|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iTTh|Ins],Lbls,Lt,Ltx,Lc,Lcx,[43|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCell|Ins],Lbls,Lt,Ltx,Lc,Lcx,[44|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iGet|Ins],Lbls,Lt,Ltx,Lc,Lcx,[45|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iAssign|Ins],Lbls,Lt,Ltx,Lc,Lcx,[46|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCLbl(V,W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[47,LtNo,Lvl|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      findLevel(Lbls,W,0,Lvl),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[48,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iStNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[49,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIf(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[50,Lvl|M],Cdx) :-
      findLevel(V,Lbls,V,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIfNot(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[51,Lvl|M],Cdx) :-
      findLevel(V,Lbls,V,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCase(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[52,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIndxJmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[53,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iUnpack(V,W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[54,LtNo,Lvl|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      findLevel(Lbls,W,0,Lvl),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iIAdd|Ins],Lbls,Lt,Ltx,Lc,Lcx,[55|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iISub|Ins],Lbls,Lt,Ltx,Lc,Lcx,[56|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIMul|Ins],Lbls,Lt,Ltx,Lc,Lcx,[57|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIDiv|Ins],Lbls,Lt,Ltx,Lc,Lcx,[58|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIMod|Ins],Lbls,Lt,Ltx,Lc,Lcx,[59|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIAbs|Ins],Lbls,Lt,Ltx,Lc,Lcx,[60|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,[61|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iILt|Ins],Lbls,Lt,Ltx,Lc,Lcx,[62|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIGe|Ins],Lbls,Lt,Ltx,Lc,Lcx,[63|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iICmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[64,Lvl|M],Cdx) :-
      findLevel(V,Lbls,V,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,[65|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCLt|Ins],Lbls,Lt,Ltx,Lc,Lcx,[66|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCGe|Ins],Lbls,Lt,Ltx,Lc,Lcx,[67|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[68,Lvl|M],Cdx) :-
      findLevel(V,Lbls,V,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iBAnd|Ins],Lbls,Lt,Ltx,Lc,Lcx,[69|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iBOr|Ins],Lbls,Lt,Ltx,Lc,Lcx,[70|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iBXor|Ins],Lbls,Lt,Ltx,Lc,Lcx,[71|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iBLsl|Ins],Lbls,Lt,Ltx,Lc,Lcx,[72|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iBLsr|Ins],Lbls,Lt,Ltx,Lc,Lcx,[73|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iBAsr|Ins],Lbls,Lt,Ltx,Lc,Lcx,[74|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iBNot|Ins],Lbls,Lt,Ltx,Lc,Lcx,[75|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFAdd|Ins],Lbls,Lt,Ltx,Lc,Lcx,[76|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFSub|Ins],Lbls,Lt,Ltx,Lc,Lcx,[77|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFMul|Ins],Lbls,Lt,Ltx,Lc,Lcx,[78|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFDiv|Ins],Lbls,Lt,Ltx,Lc,Lcx,[79|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFMod|Ins],Lbls,Lt,Ltx,Lc,Lcx,[80|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFAbs|Ins],Lbls,Lt,Ltx,Lc,Lcx,[81|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,[82|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFLt|Ins],Lbls,Lt,Ltx,Lc,Lcx,[83|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFGe|Ins],Lbls,Lt,Ltx,Lc,Lcx,[84|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[85,Lvl|M],Cdx) :-
      findLevel(V,Lbls,V,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iAlloc(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[86,LtNo|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iClosure(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[87,LtNo|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[88,Lvl|M],Cdx) :-
      findLevel(V,Lbls,V,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFrame(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[89,LtNo|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iDBug|Ins],Lbls,Lt,Ltx,Lc,Lcx,[90|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iLine(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[91,LtNo|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iLocal(V,W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[92,LtNo,W|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).


findLevel(Lvl,[Tgt|_],bo(Tgt),Lvl) :-!.
findLevel(Lvl,[Tgt|_],bp(Tgt),MLvl) :-!, MLvl is -Lvl.
findLevel(L,[none|Ends],Tgt,Lo) :-
  L1 is L+1,
  findLevel(L1,Ends,Tgt,Lo).
findLevel(L,[_|Ends],Tgt,Lo) :-
  findLevel(L,Ends,Tgt,Lo).

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

dispIns(Prog) :-
  showMnem(Prog,O),
  displayln(O).

showIns(func(Nm,H,Sig,_Lx,Ins),sq([HH,ss(" "),NN,ss(":"),ss(Sig),nl(0),iv(nl(0),II)])) :-
  ssTrm(Nm,0,NN),
  showMnem(Ins,[0],_,[],II),
  ssPolicy(H,HH),!.

ssPolicy(soft,ss("soft")).
ssPolicy(hard,ss("hard")).

showBlock(Ins,Prefix,iv(nl(K),II)) :-
  K is length(Prefix)*2,
  showMnem(Ins,[0|Prefix],II).

showMnem([],_,[ss("end")]).
showMnem([iLbl(Lb)|Ins],Pc,[sq([ss(Lb),ss(":")])|II]) :-
  showMnem(Ins,Pc,II).
showMnem([iLocal(Nm,Scope,Off)|Ins],Pc,[sq([ss(Nm),ss("::"),ss(Scope),ss(":"),ix(Off)])|II]) :-
  showMnem(Ins,Pc,II).
showMnem([iHalt(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("Halt"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iNop|Ins],Pc,[sq([PcDx,ss(":"),ss("Nop")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iAbort|Ins],Pc,[sq([PcDx,ss(":"),ss("Abort")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iCall(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("Call"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  ssTrm(U,0,UU),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iOCall(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("OCall"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iEscape(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("Escape"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ss(U),!,
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iTCall(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("TCall"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  ssTrm(U,0,UU),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iTOCall(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("TOCall"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iLocals(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("Locals"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iRet|Ins],Pc,[sq([PcDx,ss(":"),ss("Ret")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iBlock(U,V)|Ins],Pc,[sq([PcDx,ss(":"),ss("Block"), ss(" "), UU, ss(","), VV])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  ssTrm(U,0,UU),
  bumpPc(Pc0,1,Pc1),
  blockPc(Pc,SPc),
  showMnem(V, SPc, Ms),
  VV = iv(ss("\n"), Ms),
  bumpPc(Pc1,1,Pc2),
  showMnem(Ins,Pc2,II).
showMnem([iBreak(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("Break"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iDrop|Ins],Pc,[sq([PcDx,ss(":"),ss("Drop")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iDup|Ins],Pc,[sq([PcDx,ss(":"),ss("Dup")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iRot(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("Rot"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iRst(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("Rst"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iFiber|Ins],Pc,[sq([PcDx,ss(":"),ss("Fiber")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iSpawn|Ins],Pc,[sq([PcDx,ss(":"),ss("Spawn")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iSuspend|Ins],Pc,[sq([PcDx,ss(":"),ss("Suspend")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iResume|Ins],Pc,[sq([PcDx,ss(":"),ss("Resume")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iRetire|Ins],Pc,[sq([PcDx,ss(":"),ss("Retire")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iUnderflow|Ins],Pc,[sq([PcDx,ss(":"),ss("Underflow")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iTEq|Ins],Pc,[sq([PcDx,ss(":"),ss("TEq")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iTry(U,V)|Ins],Pc,[sq([PcDx,ss(":"),ss("Try"), ss(" "), UU, ss(","), VV])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  ssTrm(U,0,UU),
  bumpPc(Pc0,1,Pc1),
  blockPc(Pc,SPc),
  showMnem(V, SPc, Ms),
  VV = iv(ss("\n"), Ms),
  bumpPc(Pc1,1,Pc2),
  showMnem(Ins,Pc2,II).
showMnem([iEndTry(V)|Ins],Pc,[sq([PcDx,ss(":"),ss("EndTry"), ss(","), VV])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  VV=ix(V),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iThrow|Ins],Pc,[sq([PcDx,ss(":"),ss("Throw")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iReset|Ins],Pc,[sq([PcDx,ss(":"),ss("Reset")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iShift|Ins],Pc,[sq([PcDx,ss(":"),ss("Shift")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iInvoke|Ins],Pc,[sq([PcDx,ss(":"),ss("Invoke")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iLdV|Ins],Pc,[sq([PcDx,ss(":"),ss("LdV")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iLdC(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("LdC"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  ssTrm(U,0,UU),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iLdA(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("LdA"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iLdL(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("LdL"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ss(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iStL(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("StL"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ss(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iStV(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("StV"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ss(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iTL(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("TL"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ss(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iStA(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("StA"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iLdG(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("LdG"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ss(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iStG(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("StG"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ss(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iTG(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("TG"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ss(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iThunk|Ins],Pc,[sq([PcDx,ss(":"),ss("Thunk")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iLdTh|Ins],Pc,[sq([PcDx,ss(":"),ss("LdTh")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iStTh|Ins],Pc,[sq([PcDx,ss(":"),ss("StTh")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iTTh|Ins],Pc,[sq([PcDx,ss(":"),ss("TTh")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iCell|Ins],Pc,[sq([PcDx,ss(":"),ss("Cell")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iGet|Ins],Pc,[sq([PcDx,ss(":"),ss("Get")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iAssign|Ins],Pc,[sq([PcDx,ss(":"),ss("Assign")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iCLbl(U,V)|Ins],Pc,[sq([PcDx,ss(":"),ss("CLbl"), ss(" "), UU, ss(","), VV])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  ssTrm(U,0,UU),
  bumpPc(Pc0,1,Pc1),
  VV=ix(V),
  bumpPc(Pc1,1,Pc2),
  showMnem(Ins,Pc2,II).
showMnem([iNth(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("Nth"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iStNth(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("StNth"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iIf(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("If"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iIfNot(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("IfNot"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iCase(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("Case"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iIndxJmp(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("IndxJmp"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iUnpack(U,V)|Ins],Pc,[sq([PcDx,ss(":"),ss("Unpack"), ss(" "), UU, ss(","), VV])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  ssTrm(U,0,UU),
  bumpPc(Pc0,1,Pc1),
  VV=ix(V),
  bumpPc(Pc1,1,Pc2),
  showMnem(Ins,Pc2,II).
showMnem([iIAdd|Ins],Pc,[sq([PcDx,ss(":"),ss("IAdd")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iISub|Ins],Pc,[sq([PcDx,ss(":"),ss("ISub")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iIMul|Ins],Pc,[sq([PcDx,ss(":"),ss("IMul")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iIDiv|Ins],Pc,[sq([PcDx,ss(":"),ss("IDiv")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iIMod|Ins],Pc,[sq([PcDx,ss(":"),ss("IMod")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iIAbs|Ins],Pc,[sq([PcDx,ss(":"),ss("IAbs")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iIEq|Ins],Pc,[sq([PcDx,ss(":"),ss("IEq")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iILt|Ins],Pc,[sq([PcDx,ss(":"),ss("ILt")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iIGe|Ins],Pc,[sq([PcDx,ss(":"),ss("IGe")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iICmp(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("ICmp"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iCEq|Ins],Pc,[sq([PcDx,ss(":"),ss("CEq")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iCLt|Ins],Pc,[sq([PcDx,ss(":"),ss("CLt")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iCGe|Ins],Pc,[sq([PcDx,ss(":"),ss("CGe")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iCCmp(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("CCmp"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iBAnd|Ins],Pc,[sq([PcDx,ss(":"),ss("BAnd")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iBOr|Ins],Pc,[sq([PcDx,ss(":"),ss("BOr")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iBXor|Ins],Pc,[sq([PcDx,ss(":"),ss("BXor")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iBLsl|Ins],Pc,[sq([PcDx,ss(":"),ss("BLsl")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iBLsr|Ins],Pc,[sq([PcDx,ss(":"),ss("BLsr")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iBAsr|Ins],Pc,[sq([PcDx,ss(":"),ss("BAsr")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iBNot|Ins],Pc,[sq([PcDx,ss(":"),ss("BNot")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iFAdd|Ins],Pc,[sq([PcDx,ss(":"),ss("FAdd")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iFSub|Ins],Pc,[sq([PcDx,ss(":"),ss("FSub")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iFMul|Ins],Pc,[sq([PcDx,ss(":"),ss("FMul")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iFDiv|Ins],Pc,[sq([PcDx,ss(":"),ss("FDiv")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iFMod|Ins],Pc,[sq([PcDx,ss(":"),ss("FMod")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iFAbs|Ins],Pc,[sq([PcDx,ss(":"),ss("FAbs")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iFEq|Ins],Pc,[sq([PcDx,ss(":"),ss("FEq")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iFLt|Ins],Pc,[sq([PcDx,ss(":"),ss("FLt")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iFGe|Ins],Pc,[sq([PcDx,ss(":"),ss("FGe")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iFCmp(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("FCmp"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iAlloc(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("Alloc"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  ssTrm(U,0,UU),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iClosure(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("Closure"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  ssTrm(U,0,UU),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iCmp(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("Cmp"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  UU=ix(U),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iFrame(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("Frame"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  ssTrm(U,0,UU),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iDBug|Ins],Pc,[sq([PcDx,ss(":"),ss("dBug")])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  showMnem(Ins,Pc0,II).
showMnem([iLine(U)|Ins],Pc,[sq([PcDx,ss(":"),ss("Line"), ss(" "), UU])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  ssTrm(U,0,UU),
  bumpPc(Pc0,1,Pc1),
  showMnem(Ins,Pc1,II).
showMnem([iLocal(U,V)|Ins],Pc,[sq([PcDx,ss(":"),ss("Local"), ss(" "), UU, ss(","), VV])|II]) :- !,
  showPc(Pc,PcDx),
  bumpPc(Pc,1,Pc0),
  ssTrm(U,0,UU),
  bumpPc(Pc0,1,Pc1),
  VV=ss(V),
  bumpPc(Pc1,1,Pc2),
  showMnem(Ins,Pc2,II).


opcodeHash(1626975299147032880).

bumpPc([Pc|Rest],Dl,[Pc1|Rest]) :- Pc1 is Pc+Dl.

showPc(Pc,iv(ss(":"),Pcs)) :-
  map(Pc,assemble:shPc,Pcs).

shPc(I,ix(I)).

