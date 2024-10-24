/* Automatically generated, do not edit */

:- module(assemble,[assem/2, dispCode/1, dispIns/1, opcodeHash/1]).
:- use_module(misc).
:- use_module(lterms).
:- use_module(types).
:- use_module(encode).
:- use_module(display).
:- use_module(escapes).

assem(func(Nm,H,Sig,Ins),MTpl) :-
    findLit([],Nm,_,Ls0),
    findLit(Ls0,strg(Sig),SgIx,Ls1),
    assemBlock(Ins,none,[],Ls1,Lts,[],Lcs,Cde,[]),
    mkInsTpl(Cde,Code),
    mkLitTpl(Lts,LtTpl),
    mkTpl(Lcs,LcsTpl),
    encPolicy(H,HP),
    stackHwm(Ins,0,0,HWM),
    localHwm(Ins,[],0,Lx),
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

stackHwm([],_,H,H).
stackHwm([iHalt(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iNop|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iAbort|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iCall(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iOCall(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iEscape(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iTCall(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iTOCall(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iEntry|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iRet|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iBlock(_,W)|Ins],CH0,H0,Hwm) :-
  stackHwm(W,CH0,H0,H1),
  stackHwm(Ins,CH0,H1,Hwm).
stackHwm([iBreak(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iLoop(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iDrop|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iDup|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iRot(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iRst(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iPick(_,_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iFiber|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iSpawn|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iSuspend|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iResume|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iRetire|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iUnderflow|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iTEq|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iTry(_,W)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(W,CH1,H1,H2),
  stackHwm(Ins,CH1,H2,Hwm).
stackHwm([iEndTry(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iThrow|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iReset|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iShift|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iInvoke|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iLdV|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iLdC(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iLdA(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iLdL(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iStL(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iStV(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iTL(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iStA(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iLdG(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iStG(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iTG(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iThunk|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iLdTh|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iStTh|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iTTh|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iCell|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iGet|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iAssign|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iCLbl(_,_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iNth(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iStNth(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iIf(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iIfNot(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iCase(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iIndxJmp(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iIAdd|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iISub|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iIMul|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iIDiv|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iIMod|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iIAbs|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iIEq|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iILt|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iIGe|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iICmp(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iCEq|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iCLt|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iCGe|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iCCmp(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iBAnd|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iBOr|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iBXor|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iBLsl|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iBLsr|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iBAsr|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iBNot|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iFAdd|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iFSub|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iFMul|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iFDiv|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iFMod|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iFAbs|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iFEq|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iFLt|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iFGe|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iFCmp(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iAlloc(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iClosure(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iCmp(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iFrame(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iDBug|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iLine(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iLocal(_,_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).


countLocal(Nm,Lcs,Lcs,Hwm,Hwm) :-
  is_member(Nm,Lcs),!.
countLocal(Nm,Lcs,Lx,Hwm,H1) :-
  add_mem(Nm,Lcs,Lx),
  length(Lx,Ln),
  (Ln>Hwm ->
    H1 = Ln;
    H1 = Hwm).

localHwm([],_,H,H).
localHwm([iHalt(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iNop|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iAbort|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iCall(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iOCall(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iEscape(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iTCall(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iTOCall(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iEntry|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iRet|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iBlock(_,W)|Ins],C0,H0,Hwm) :-
  localHwm(W,C0,H0,H1),
  localHwm(Ins,C0,H1,Hwm).
localHwm([iBreak(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iLoop(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iDrop|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iDup|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iRot(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iRst(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iPick(_,_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iFiber|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iSpawn|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iSuspend|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iResume|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iRetire|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iUnderflow|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iTEq|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iTry(_,W)|Ins],C0,H0,Hwm) :-
  localHwm(W,C0,H0,H1),
  localHwm(Ins,C0,H1,Hwm).
localHwm([iEndTry(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iThrow|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iReset|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iShift|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iInvoke|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iLdV|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iLdC(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iLdA(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iLdL(V)|Ins],C0,H0,Hwm) :-
  countLocal(V,C0,C1,H0,H1),
  localHwm(Ins,C1,H1,Hwm).
localHwm([iStL(V)|Ins],C0,H0,Hwm) :-
  countLocal(V,C0,C1,H0,H1),
  localHwm(Ins,C1,H1,Hwm).
localHwm([iStV(V)|Ins],C0,H0,Hwm) :-
  countLocal(V,C0,C1,H0,H1),
  localHwm(Ins,C1,H1,Hwm).
localHwm([iTL(V)|Ins],C0,H0,Hwm) :-
  countLocal(V,C0,C1,H0,H1),
  localHwm(Ins,C1,H1,Hwm).
localHwm([iStA(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iLdG(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iStG(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iTG(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iThunk|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iLdTh|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iStTh|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iTTh|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iCell|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iGet|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iAssign|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iCLbl(_,_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iNth(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iStNth(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iIf(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iIfNot(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iCase(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iIndxJmp(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iIAdd|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iISub|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iIMul|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iIDiv|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iIMod|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iIAbs|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iIEq|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iILt|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iIGe|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iICmp(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iCEq|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iCLt|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iCGe|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iCCmp(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iBAnd|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iBOr|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iBXor|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iBLsl|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iBLsr|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iBAsr|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iBNot|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iFAdd|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iFSub|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iFMul|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iFDiv|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iFMod|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iFAbs|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iFEq|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iFLt|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iFGe|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iFCmp(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iAlloc(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iClosure(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iCmp(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iFrame(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iDBug|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iLine(_)|Ins],C0,H0,Hwm) :-
  localHwm(Ins,C0,H0,Hwm).
localHwm([iLocal(V,_)|Ins],C0,H0,Hwm) :-
  countLocal(V,C0,C1,H0,H1),
  localHwm(Ins,C1,H1,Hwm).


assemBlock(Ins,Lb,Lbs,Lt,Lts,Lc,Lcx,Code,Cdx) :-
    mnem(Ins,[Lb|Lbs],Lt,Lts,Lc,Lcx,Code,Cdx).

mnem([],_,Lt,Lt,Lc,Lc,Cdx,Cdx).
mnem([iLbl(Lb,Inner)|Ins],Lbs,Lt,Lts,Lc,Lcx,Code,Cdx) :-
      baseOffset(Lbs,Base),
      mnem([Inner],[(Lb,Base,[])|Lbs],Lt,Lt0,Lc,Lc0,Code,Cd0),
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
      assemBlock(W,none,Lbls,Lt1,Lt2,Lc,Lcx,B,[]),
      mnem(Ins,Lbls,Lt2,Ltx,Lc,Lcx,M,Cdx).
mnem([iBreak(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[11,Lvl|M],Cdx) :-
      findLevel(V,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iLoop(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[12,Lvl|M],Cdx) :-
      findLevel(V,Lbls,0,Lvl),
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
      assemBlock(W,none,Lbls,Lt1,Lt2,Lc,Lcx,B,[]),
      mnem(Ins,Lbls,Lt2,Ltx,Lc,Lcx,M,Cdx).
mnem([iEndTry(W)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[26,Lvl|M],Cdx) :-
      findLevel(W,Lbls,0,Lvl),
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
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[50,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iStNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[51,V|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIf(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[52,Lvl|M],Cdx) :-
      findLevel(V,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iIfNot(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[53,Lvl|M],Cdx) :-
      findLevel(V,Lbls,0,Lvl),
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
      findLevel(V,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,[66|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCLt|Ins],Lbls,Lt,Ltx,Lc,Lcx,[67|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCGe|Ins],Lbls,Lt,Ltx,Lc,Lcx,[68|M],Cdx) :-
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iCCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[69,Lvl|M],Cdx) :-
      findLevel(V,Lbls,0,Lvl),
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
      findLevel(V,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,M,Cdx).
mnem([iAlloc(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[87,LtNo|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iClosure(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[88,LtNo|M],Cdx) :-
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,M,Cdx).
mnem([iCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,[89,Lvl|M],Cdx) :-
      findLevel(V,Lbls,0,Lvl),
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
baseOffset([none|Lbs],Base) :-
  baseOffset(Lbs,Base).
baseOffset([],0).

findLevel(Tgt,[(Tgt,_,_)|_],Lvl,Lvl) :-!.
findLevel(Tgt,[none|Ends],L,Lo) :-
      L1 is L+1,
      findLevel(Tgt,Ends,L1,Lo).
findLevel(Tgt,[_|Ends],L,Lo) :-
  findLevel(Tgt,Ends,L,Lo).

findLocal(Nm,[(_,_,Lcls)|_],Off) :-
      is_member((Nm,Off),Lcls),!.
findLocal(Nm,[none|Lvls],Off) :-
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

showCode(func(Nm,H,Sig,Ins),sq([HH,ss(" "),NN,ss(":"),ss(Sig),nl(0),iv(nl(0),II)])) :-
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

dispIns(Ins) :-
  showBlock(Ins,[],O),
  displayln(O).

showBlock(Ins,Prefix,iv(nl(K),II)) :-
  pcSpace(Prefix,K),
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

