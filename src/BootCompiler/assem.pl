/* Automatically generated, do not edit */

:- module(assemble,[assem/2, dispCode/1, opcodeHash/1]).
:- use_module(misc).
:- use_module(lterms).
:- use_module(types).
:- use_module(encode).
:- use_module(display).
:- use_module(escapes).

assem(func(Nm,H,Sig,Lx,Ins),MTpl) :-
    findLit([],Nm,_,Ls0),
    findLit(Ls0,strg(Sig),SgIx,Ls1),
    assemBlock(Ins,[],Ls1,Lts,[],Lcs,Cde,[]),
    mkInsTpl(Cde,Code),
    mkLitTpl(Lts,LtTpl),
    mkTpl(Lcs,LcsTpl),
    encPolicy(H,HP),
    hwm(Ins,0,0,HWM),
    mkCons("func",[Nm,HP,intgr(SgIx),intgr(HWM),intgr(Lx),Code,LtTpl,LcsTpl],MTpl).
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

hwm([iHalt(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iNop|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iAbort|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iCall(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iOCall(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iEscape(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iTCall(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iTOCall(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iEntry|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iRet|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iBlock(_,W)|Ins],CH0,H0,Hwm) :-
  hwm(W,CH0,H0,H1),
  hwm(Ins,CH0,H1,Hwm).
hwm([iBreak(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iLoop(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iDrop|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iDup|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iRot(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iRst(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iPick(_,_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iFiber|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iSpawn|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iSuspend|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iResume|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iRetire|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iUnderflow|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iTEq|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iTry(_,W)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(W,CH1,H1,H2),
  hwm(Ins,CH1,H2,Hwm).
hwm([iEndTry(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iThrow|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iReset|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iShift|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iInvoke|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iLdV|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iLdC(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iLdA(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iLdL(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iStL(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iStV(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iTL(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iStA(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iLdG(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iStG(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iTG(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iThunk|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iLdTh|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iStTh|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iTTh|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iCell|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iGet|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iAssign|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iCLbl(_,_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iNth(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iStNth(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iIf(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iIfNot(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iCase(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iIndxJmp(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iIAdd|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iISub|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iIMul|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iIDiv|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iIMod|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iIAbs|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iIEq|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iILt|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iIGe|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iICmp(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iCEq|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iCLt|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iCGe|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iCCmp(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iBAnd|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iBOr|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iBXor|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iBLsl|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iBLsr|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iBAsr|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iBNot|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iFAdd|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iFSub|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iFMul|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iFDiv|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iFMod|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iFAbs|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iFEq|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iFLt|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iFGe|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iFCmp(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iAlloc(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iClosure(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iCmp(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  hwm(Ins,CH1,H1,Hwm).
hwm([iFrame(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iDBug|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iLine(_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).
hwm([iLocal(_,_)|Ins],CH0,H0,Hwm) :-
  hwm(Ins,CH0,H0,Hwm).


assemBlock(Ins,Lb,Lbs,Lt,Lts,Lc,Lcx,Code,Cdx) :-
    mnem(Ins,[Lb|Lbs],Lt,Lts,Lc,Lcx,Code,Cdx).

mnem([],_,Lt,Lt,Lc,Lc,Cdx,Cdx).
mnem([iLbl(Lb,Inner)|Ins],Lbs,Lt,Lts,Lc,Lcx,Code,Cdx) :-
      baseOffset(Lbs,Base),
      mnem(Inner,[(Lb,Base,[])|Lbs],Lt,Lt0,Lc,Lc0,Code,Cd0),
      mnem(Ins,Lbs,Lt0,Lts,Lc0,Lcx,Cd0,Cdx).
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
mnem([iEntry|Ins],Lbls,Lt,Ltx,Lc,Lcx,[8|M],Cdx) :-
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
mnem([iLoop(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[12,Lvl|M],Cdx) :-
      findLevel(V,Lbls,V,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iDrop|Ins],Lbls,Lt,Ltx,Lc,Lcx,[13|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iDup|Ins],Lbls,Lt,Ltx,Lc,Lcx,[14|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iRot(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[15,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iRst(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[16,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iPick(V,W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[17,V,W|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFiber|Ins],Lbls,Lt,Ltx,Lc,Lcx,[18|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iSpawn|Ins],Lbls,Lt,Ltx,Lc,Lcx,[19|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iSuspend|Ins],Lbls,Lt,Ltx,Lc,Lcx,[20|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iResume|Ins],Lbls,Lt,Ltx,Lc,Lcx,[21|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iRetire|Ins],Lbls,Lt,Ltx,Lc,Lcx,[22|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iUnderflow|Ins],Lbls,Lt,Ltx,Lc,Lcx,[23|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iTEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,[24|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iTry(V,W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[25,LtNo,B|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      assemBlock(W,Lbls,Lt1,Lt2,Lc,Lcx,B,[]),
      mnem(Ins,Lbls,Lt2,Ltx,Lc,Lcx,M,Cdx).
mnem([iEndTry(W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[26,Lvl|M],Cdx) :-
      findLevel(Lbls,W,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iThrow|Ins],Lbls,Lt,Ltx,Lc,Lcx,[27|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iReset|Ins],Lbls,Lt,Ltx,Lc,Lcx,[28|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iShift|Ins],Lbls,Lt,Ltx,Lc,Lcx,[29|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iInvoke|Ins],Lbls,Lt,Ltx,Lc,Lcx,[30|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iLdV|Ins],Lbls,Lt,Ltx,Lc,Lcx,[31|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iLdC(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[32,LtNo|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iLdA(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[33,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iLdL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[34,Off|M],Cdx) :-
      findLocal(V,Lbls,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iStL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[35,Off|M],Cdx) :-
      findLocal(V,Lbls,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iStV(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[36,Off|M],Cdx) :-
      findLocal(V,Lbls,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iTL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[37,Off|M],Cdx) :-
      findLocal(V,Lbls,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iStA(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[38,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iLdG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[39,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iStG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[40,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iTG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[41,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iThunk|Ins],Lbls,Lt,Ltx,Lc,Lcx,[42|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iLdTh|Ins],Lbls,Lt,Ltx,Lc,Lcx,[43|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iStTh|Ins],Lbls,Lt,Ltx,Lc,Lcx,[44|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iTTh|Ins],Lbls,Lt,Ltx,Lc,Lcx,[45|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCell|Ins],Lbls,Lt,Ltx,Lc,Lcx,[46|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iGet|Ins],Lbls,Lt,Ltx,Lc,Lcx,[47|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iAssign|Ins],Lbls,Lt,Ltx,Lc,Lcx,[48|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCLbl(V,W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[49,LtNo,Lvl|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      findLevel(Lbls,W,0,Lvl),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[50,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iStNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[51,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIf(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[52,Lvl|M],Cdx) :-
      findLevel(V,Lbls,V,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIfNot(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[53,Lvl|M],Cdx) :-
      findLevel(V,Lbls,V,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCase(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[54,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIndxJmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[55,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIAdd|Ins],Lbls,Lt,Ltx,Lc,Lcx,[56|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iISub|Ins],Lbls,Lt,Ltx,Lc,Lcx,[57|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIMul|Ins],Lbls,Lt,Ltx,Lc,Lcx,[58|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIDiv|Ins],Lbls,Lt,Ltx,Lc,Lcx,[59|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIMod|Ins],Lbls,Lt,Ltx,Lc,Lcx,[60|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIAbs|Ins],Lbls,Lt,Ltx,Lc,Lcx,[61|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,[62|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iILt|Ins],Lbls,Lt,Ltx,Lc,Lcx,[63|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIGe|Ins],Lbls,Lt,Ltx,Lc,Lcx,[64|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iICmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[65,Lvl|M],Cdx) :-
      findLevel(V,Lbls,V,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,[66|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCLt|Ins],Lbls,Lt,Ltx,Lc,Lcx,[67|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCGe|Ins],Lbls,Lt,Ltx,Lc,Lcx,[68|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[69,Lvl|M],Cdx) :-
      findLevel(V,Lbls,V,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iBAnd|Ins],Lbls,Lt,Ltx,Lc,Lcx,[70|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iBOr|Ins],Lbls,Lt,Ltx,Lc,Lcx,[71|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iBXor|Ins],Lbls,Lt,Ltx,Lc,Lcx,[72|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iBLsl|Ins],Lbls,Lt,Ltx,Lc,Lcx,[73|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iBLsr|Ins],Lbls,Lt,Ltx,Lc,Lcx,[74|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iBAsr|Ins],Lbls,Lt,Ltx,Lc,Lcx,[75|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iBNot|Ins],Lbls,Lt,Ltx,Lc,Lcx,[76|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFAdd|Ins],Lbls,Lt,Ltx,Lc,Lcx,[77|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFSub|Ins],Lbls,Lt,Ltx,Lc,Lcx,[78|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFMul|Ins],Lbls,Lt,Ltx,Lc,Lcx,[79|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFDiv|Ins],Lbls,Lt,Ltx,Lc,Lcx,[80|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFMod|Ins],Lbls,Lt,Ltx,Lc,Lcx,[81|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFAbs|Ins],Lbls,Lt,Ltx,Lc,Lcx,[82|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,[83|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFLt|Ins],Lbls,Lt,Ltx,Lc,Lcx,[84|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFGe|Ins],Lbls,Lt,Ltx,Lc,Lcx,[85|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[86,Lvl|M],Cdx) :-
      findLevel(V,Lbls,V,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iAlloc(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[87,LtNo|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iClosure(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[88,LtNo|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[89,Lvl|M],Cdx) :-
      findLevel(V,Lbls,V,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iFrame(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[90,LtNo|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iDBug|Ins],Lbls,Lt,Ltx,Lc,Lcx,[91|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iLine(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[92,LtNo|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iLocal(V,W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[93,Off,LtNo|M],Cdx) :-
      declareLocal(V,Lbls,Lbl0,Off),
      findLit(Lt,W,LtNo,Lt1),
      mnem(Ins,Lbl0,Lt1,Ltx,Lc,Lcx,M,Cdx).


baseOffset([(_,Base,_)|_],Base).
baseOffset([],0).

findLevel(Lvl,[(Tgt,_,_)|_],Tgt,Lvl) :-!.
findLevel(L,[none|Ends],Tgt,Lo) :-
      L1 is L+1,
      findLevel(L1,Ends,Tgt,Lo).
findLevel(L,[_|Ends],Tgt,Lo) :-
  findLevel(L,Ends,Tgt,Lo).

findLocal(Nm,[(_,_.Lcls)|_],Off) :-
      is_member((Nm,Off),Lcls),!.
findLocal(Nm,[_|Lvls],Off) :-
      findLocal(Nm,Lvls,Off).

declareLocal(Nm,[(Tgt,Off,Lcls)|Lbs],[(Tgt,NxtOff,[(Nm,Off)|Lcls])|Lbs],Off) :-
      NxtOff is Off+1.

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

dispCode(Prog) :-
  showCode(Prog,O),
  displayln(O).

showCode(func(Nm,H,Sig,_Lx,Ins),sq([HH,ss(" "),NN,ss(":"),ss(Sig),nl(0),iv(nl(0),II)])) :-
  ssTrm(Nm,0,NN),
  showMnems(Ins,[0],II),
  ssPolicy(H,HH),!.
showCode(struct(Lbl,Sig,Ix),sq([ss("symb "),LL,ss(":"),TT,ss(" @ "),ix(Ix)])) :-
  ssTrm(Lbl,0,LL),
  ssType(Sig,false,0,TT).
showCode(tipe(Tp,_Rl,Map),sq([ss("type "),TT,ss(" = "),XX])) :-
  ssConsMap(Map,XX),
  ssType(Tp,false,0,TT).

ssPolicy(soft,ss("soft")).
ssPolicy(hard,ss("hard")).

showBlock(Ins,Prefix,iv(nl(K),II)) :-
  K is length(Prefix)*2,
  showMnems(Ins,[0|Prefix],II).

showMnems([],_,[ss("end")]).
showMnems([iLbl(Lb,L)|Ins],Pc,[sq([ss(Lb),ss(":"),nl(Dp),LL])|II]) :-
  pcSpace(Pc,Dp),
  showMnem(L,Pc,LL),
  showMnems(Ins,Pc,II).
showMnems([M|Ins],Pc,[MM|II]) :-
  showMnem(M,Pc,MM),
  bumpPc(Pc,Pc1),
  showMnems(Ins,Pc1,II).

showMnem(iHalt(U),Pc,sq([PcDx,ss(": "),ss("Halt"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iNop,Pc,sq([PcDx,ss(": "),ss("Nop")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iAbort,Pc,sq([PcDx,ss(": "),ss("Abort")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iCall(U),Pc,sq([PcDx,ss(": "),ss("Call"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  true.
showMnem(iOCall(U),Pc,sq([PcDx,ss(": "),ss("OCall"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iEscape(U),Pc,sq([PcDx,ss(": "),ss("Escape"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),!,
  true.
showMnem(iTCall(U),Pc,sq([PcDx,ss(": "),ss("TCall"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  true.
showMnem(iTOCall(U),Pc,sq([PcDx,ss(": "),ss("TOCall"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iEntry,Pc,sq([PcDx,ss(": "),ss("Entry")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iRet,Pc,sq([PcDx,ss(": "),ss("Ret")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iBlock(U,V),Pc,sq([PcDx,ss(": "),ss("Block"), ss(" "), UU, ss(","), VV])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  blockPc(Pc,SPc),
  showMnems(V, SPc, Ms),
  pcSpace(SPc,Dp),
  VV = sq([nl(Dp),iv(nl(Dp), Ms)]),
  true.
showMnem(iBreak(U),Pc,sq([PcDx,ss(": "),ss("Break"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),!,
  true.
showMnem(iLoop(U),Pc,sq([PcDx,ss(": "),ss("Loop"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),!,
  true.
showMnem(iDrop,Pc,sq([PcDx,ss(": "),ss("Drop")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iDup,Pc,sq([PcDx,ss(": "),ss("Dup")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iRot(U),Pc,sq([PcDx,ss(": "),ss("Rot"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iRst(U),Pc,sq([PcDx,ss(": "),ss("Rst"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iPick(U,V),Pc,sq([PcDx,ss(": "),ss("Pick"), ss(" "), UU, ss(","), VV])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  VV=ix(V),
  true.
showMnem(iFiber,Pc,sq([PcDx,ss(": "),ss("Fiber")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iSpawn,Pc,sq([PcDx,ss(": "),ss("Spawn")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iSuspend,Pc,sq([PcDx,ss(": "),ss("Suspend")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iResume,Pc,sq([PcDx,ss(": "),ss("Resume")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iRetire,Pc,sq([PcDx,ss(": "),ss("Retire")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iUnderflow,Pc,sq([PcDx,ss(": "),ss("Underflow")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iTEq,Pc,sq([PcDx,ss(": "),ss("TEq")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iTry(U,V),Pc,sq([PcDx,ss(": "),ss("Try"), ss(" "), UU, ss(","), VV])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  blockPc(Pc,SPc),
  showMnems(V, SPc, Ms),
  pcSpace(SPc,Dp),
  VV = sq([nl(Dp),iv(nl(Dp), Ms)]),
  true.
showMnem(iEndTry(V),Pc,sq([PcDx,ss(": "),ss("EndTry"), ss(","), VV])) :- !,
  showPc(Pc,PcDx),
  VV=ss(V),!,
  true.
showMnem(iThrow,Pc,sq([PcDx,ss(": "),ss("Throw")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iReset,Pc,sq([PcDx,ss(": "),ss("Reset")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iShift,Pc,sq([PcDx,ss(": "),ss("Shift")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iInvoke,Pc,sq([PcDx,ss(": "),ss("Invoke")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iLdV,Pc,sq([PcDx,ss(": "),ss("LdV")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iLdC(U),Pc,sq([PcDx,ss(": "),ss("LdC"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  true.
showMnem(iLdA(U),Pc,sq([PcDx,ss(": "),ss("LdA"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iLdL(U),Pc,sq([PcDx,ss(": "),ss("LdL"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),
  true.
showMnem(iStL(U),Pc,sq([PcDx,ss(": "),ss("StL"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),
  true.
showMnem(iStV(U),Pc,sq([PcDx,ss(": "),ss("StV"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),
  true.
showMnem(iTL(U),Pc,sq([PcDx,ss(": "),ss("TL"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),
  true.
showMnem(iStA(U),Pc,sq([PcDx,ss(": "),ss("StA"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iLdG(U),Pc,sq([PcDx,ss(": "),ss("LdG"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iStG(U),Pc,sq([PcDx,ss(": "),ss("StG"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iTG(U),Pc,sq([PcDx,ss(": "),ss("TG"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iThunk,Pc,sq([PcDx,ss(": "),ss("Thunk")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iLdTh,Pc,sq([PcDx,ss(": "),ss("LdTh")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iStTh,Pc,sq([PcDx,ss(": "),ss("StTh")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iTTh,Pc,sq([PcDx,ss(": "),ss("TTh")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iCell,Pc,sq([PcDx,ss(": "),ss("Cell")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iGet,Pc,sq([PcDx,ss(": "),ss("Get")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iAssign,Pc,sq([PcDx,ss(": "),ss("Assign")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iCLbl(U,V),Pc,sq([PcDx,ss(": "),ss("CLbl"), ss(" "), UU, ss(","), VV])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  VV=ss(V),!,
  true.
showMnem(iNth(U),Pc,sq([PcDx,ss(": "),ss("Nth"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iStNth(U),Pc,sq([PcDx,ss(": "),ss("StNth"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iIf(U),Pc,sq([PcDx,ss(": "),ss("If"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),!,
  true.
showMnem(iIfNot(U),Pc,sq([PcDx,ss(": "),ss("IfNot"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),!,
  true.
showMnem(iCase(U),Pc,sq([PcDx,ss(": "),ss("Case"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iIndxJmp(U),Pc,sq([PcDx,ss(": "),ss("IndxJmp"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iIAdd,Pc,sq([PcDx,ss(": "),ss("IAdd")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iISub,Pc,sq([PcDx,ss(": "),ss("ISub")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iIMul,Pc,sq([PcDx,ss(": "),ss("IMul")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iIDiv,Pc,sq([PcDx,ss(": "),ss("IDiv")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iIMod,Pc,sq([PcDx,ss(": "),ss("IMod")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iIAbs,Pc,sq([PcDx,ss(": "),ss("IAbs")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iIEq,Pc,sq([PcDx,ss(": "),ss("IEq")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iILt,Pc,sq([PcDx,ss(": "),ss("ILt")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iIGe,Pc,sq([PcDx,ss(": "),ss("IGe")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iICmp(U),Pc,sq([PcDx,ss(": "),ss("ICmp"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),!,
  true.
showMnem(iCEq,Pc,sq([PcDx,ss(": "),ss("CEq")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iCLt,Pc,sq([PcDx,ss(": "),ss("CLt")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iCGe,Pc,sq([PcDx,ss(": "),ss("CGe")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iCCmp(U),Pc,sq([PcDx,ss(": "),ss("CCmp"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),!,
  true.
showMnem(iBAnd,Pc,sq([PcDx,ss(": "),ss("BAnd")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iBOr,Pc,sq([PcDx,ss(": "),ss("BOr")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iBXor,Pc,sq([PcDx,ss(": "),ss("BXor")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iBLsl,Pc,sq([PcDx,ss(": "),ss("BLsl")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iBLsr,Pc,sq([PcDx,ss(": "),ss("BLsr")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iBAsr,Pc,sq([PcDx,ss(": "),ss("BAsr")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iBNot,Pc,sq([PcDx,ss(": "),ss("BNot")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iFAdd,Pc,sq([PcDx,ss(": "),ss("FAdd")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iFSub,Pc,sq([PcDx,ss(": "),ss("FSub")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iFMul,Pc,sq([PcDx,ss(": "),ss("FMul")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iFDiv,Pc,sq([PcDx,ss(": "),ss("FDiv")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iFMod,Pc,sq([PcDx,ss(": "),ss("FMod")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iFAbs,Pc,sq([PcDx,ss(": "),ss("FAbs")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iFEq,Pc,sq([PcDx,ss(": "),ss("FEq")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iFLt,Pc,sq([PcDx,ss(": "),ss("FLt")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iFGe,Pc,sq([PcDx,ss(": "),ss("FGe")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iFCmp(U),Pc,sq([PcDx,ss(": "),ss("FCmp"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),!,
  true.
showMnem(iAlloc(U),Pc,sq([PcDx,ss(": "),ss("Alloc"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  true.
showMnem(iClosure(U),Pc,sq([PcDx,ss(": "),ss("Closure"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  true.
showMnem(iCmp(U),Pc,sq([PcDx,ss(": "),ss("Cmp"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),!,
  true.
showMnem(iFrame(U),Pc,sq([PcDx,ss(": "),ss("Frame"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  true.
showMnem(iDBug,Pc,sq([PcDx,ss(": "),ss("dBug")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iLine(U),Pc,sq([PcDx,ss(": "),ss("Line"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  true.
showMnem(iLocal(U,V),Pc,sq([PcDx,ss(": "),ss("Local"), ss(" "), UU, ss(","), VV])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),
  ssTrm(V,0,VV),
  true.


opcodeHash(1960799436663205384).

bumpPc([Pc|Rest],[Pc1|Rest]) :- Pc1 is Pc+1.

blockPc(Pc,[0|Pc]).

showPc(Pc,iv(ss("."),Pcs)) :-
  reverse(Pc,RPc),
  map(RPc,assemble:shPc,Pcs).

shPc(I,ix(I)).

pcSpace(Pc,Dp) :-
  length(Pc,D),
  Dp is D*2.

