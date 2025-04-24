/* Automatically generated, do not edit */

:- module(assemble,[assem/2, dispCode/1, dispIns/1, opcodeHash/1]).
:- use_module(misc).
:- use_module(lterms).
:- use_module(types).
:- use_module(encode).
:- use_module(display).
:- use_module(escapes).
:- use_module(errors).

assem(func(Nm,Pol,Sig,Lcls,Ins),MTpl) :-
    findLit([],Nm,_,Ls0),
    findLit(Ls0,strg(Sig),SgIx,Ls1),
    declareLocals(Lcls,LsMap,LcsTpl),
    assemBlock(Ins,none,[],Ls1,Lts,Lns,[],0,_Pcx,LsMap,Cde,[]),
    mkInsTpl(Cde,Code),
    mkLitTpl(Lts,LtTpl),
    encPolicy(Pol,HP),
    mkTpl(Lns,LnTpl),
    stackHwm(Ins,0,0,HWM),
    length(Lcls,Lx),
    mkCons("func",[Nm,HP,intgr(SgIx),intgr(HWM),intgr(Lx),LtTpl,Code,LcsTpl,LnTpl],MTpl).
assem(struct(Lbl,Sig,Ix),Tpl) :-
    mkCons("struct",[Lbl,Sig,intgr(Ix)],Tpl).
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
stackHwm([iLbl(_,I)|Ins],C,H,Hx) :-
  stackHwm([I|Ins],C,H,Hx).
stackHwm([iLine(_)|Ins],C,H,Hx) :-
  stackHwm(Ins,C,H,Hx).
stackHwm([iHalt(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iNop|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iAbort|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
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
stackHwm([iXCall(_,_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iXOCall(_,_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iXEscape(_,_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iTCall(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iTOCall(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iEntry(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iRet|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iXRet|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iBlock(_,W)|Ins],CH0,H0,Hwm) :-
  stackHwm(W,CH0,H0,H1),
  stackHwm(Ins,CH0,H1,Hwm).
stackHwm([iBreak(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iResult(_,_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
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
stackHwm([iTry(_,W)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0+1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(W,CH1,H1,H2),
  stackHwm(Ins,CH1,H2,Hwm).
stackHwm([iEndTry(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iTryRslt(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iThrow|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
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
stackHwm([iSav|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iLdSav(_)|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iTstSav|Ins],CH0,H0,Hwm) :-
  stackHwm(Ins,CH0,H0,Hwm).
stackHwm([iStSav|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-2,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iTSav|Ins],CH0,H0,Hwm) :-
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
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iCLit(_,_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
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
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
stackHwm([iIndxJmp(_)|Ins],CH0,H0,Hwm) :-
  CH1 is CH0-1,
  (CH1>H0 -> H1 = CH1 ; H1 = H0),
  stackHwm(Ins,CH1,H1,Hwm).
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
  CH1 is CH0-1,
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


countLocal(Nm,Lcs,Lcs,Hwm,Hwm) :-
  is_member(Nm,Lcs),!.
countLocal(Nm,Lcs,Lx,Hwm,H1) :-
  add_mem(Nm,Lcs,Lx),
  length(Lx,Ln),
  (Ln>Hwm ->
    H1 = Ln;
    H1 = Hwm).

localHwm([],Cx,Cx,H,H).
localHwm([iLbl(_,I)|Ins],C,Cx,H,Hx) :-
  localHwm([I|Ins],C,Cx,H,Hx).
localHwm([iLine(_)|Ins],C,Cx,H,Hx) :-
  localHwm(Ins,C,Cx,H,Hx).
localHwm([iHalt(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iNop|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iAbort|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iCall(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iOCall(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iEscape(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iXCall(_,_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iXOCall(_,_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iXEscape(_,_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iTCall(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iTOCall(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iEntry(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iRet|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iXRet|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iBlock(_,W)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(W,C0,C1,H0,H1),
  localHwm(Ins,C1,Cx,H1,Hwm).
localHwm([iBreak(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iResult(_,_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iLoop(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iDrop|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iDup|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iRot(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iRst(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iPick(_,_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iFiber|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iSuspend|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iResume|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iRetire|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iUnderflow|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iTry(_,W)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(W,C0,C1,H0,H1),
  localHwm(Ins,C1,Cx,H1,Hwm).
localHwm([iEndTry(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iTryRslt(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iThrow|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iLdV|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iLdC(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iLdA(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iLdL(V)|Ins],C0,Cx,H0,Hwm) :-
  countLocal(V,C0,C1,H0,H1),
  localHwm(Ins,C1,Cx,H1,Hwm).
localHwm([iStL(V)|Ins],C0,Cx,H0,Hwm) :-
  countLocal(V,C0,C1,H0,H1),
  localHwm(Ins,C1,Cx,H1,Hwm).
localHwm([iStV(V)|Ins],C0,Cx,H0,Hwm) :-
  countLocal(V,C0,C1,H0,H1),
  localHwm(Ins,C1,Cx,H1,Hwm).
localHwm([iTL(V)|Ins],C0,Cx,H0,Hwm) :-
  countLocal(V,C0,C1,H0,H1),
  localHwm(Ins,C1,Cx,H1,Hwm).
localHwm([iLdG(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iStG(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iTG(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iSav|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iLdSav(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iTstSav|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iStSav|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iTSav|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iCell|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iGet|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iAssign|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iCLbl(_,_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iCLit(_,_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iNth(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iStNth(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iIf(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iIfNot(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iCase(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iIndxJmp(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iIAdd|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iISub|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iIMul|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iIDiv|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iIMod|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iIAbs|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iIEq|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iILt|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iIGe|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iICmp(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iCEq|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iCLt|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iCGe|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iCCmp(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iBAnd|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iBOr|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iBXor|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iBLsl|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iBLsr|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iBAsr|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iBNot|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iFAdd|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iFSub|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iFMul|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iFDiv|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iFMod|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iFAbs|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iFEq|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iFLt|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iFGe|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iFCmp(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iAlloc(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iClosure(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iCmp(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iFrame(_)|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).
localHwm([iDBug|Ins],C0,Cx,H0,Hwm) :-
  localHwm(Ins,C0,Cx,H0,Hwm).


assemBlock(Ins,Lb,Lbs,Lt,Lts,Ln,Lnx,Pc,Pcx,LsMap,Code,Cdx) :-
    mnem(Ins,[Lb|Lbs],Lt,Lts,Ln,Lnx,Pc,Pcx,LsMap,Code,Cdx).

mnem([],_,Lt,Lt,Lnx,Lnx,Pcx,Pcx,_LsMap,Cdx,Cdx).
mnem([iLbl(Lb,Inner)|Ins],Lbs,Lt,Lts,Ln,Lnx,Pc,Pcx,LsMap,Code,Cdx) :-
      baseOffset(Lbs,Base),
      mnem([Inner],[(Lb,Base)|Lbs],Lt,Lt0,Ln,Ln1,Pc,Pc1,LsMap,Code,Cd0),
      mnem(Ins,Lbs,Lt0,Lts,Ln1,Lnx,Pc1,Pcx,LsMap,Cd0,Cdx).
mnem([iLine(Lne)|Ins],Lbs,Lt,Lts,[Line|Lns],Lnx,Pc,Pcx,LsMap,Code,Cdx) :-
  mkTpl([intgr(Pc),Lne],Line),
  mnem(Ins,Lbs,Lt,Lts,Lns,Lnx,Pc,Pcx,LsMap,Code,Cdx).
mnem([iHalt(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[0,V|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iNop|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[1|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iAbort|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[2|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iCall(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[3,LtNo|M],Cdx) :-
      Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iOCall(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[4,V|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iEscape(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[5,V|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iXCall(V,W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[6,LtNo,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iXOCall(V,W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[7,V,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iXEscape(V,W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[8,V,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iTCall(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[9,LtNo|M],Cdx) :-
      Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iTOCall(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[10,V|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iEntry(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[11,V|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iRet|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[12|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iXRet|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[13|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iBlock(V,W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[14,LtNo,B|M],Cdx) :-
      Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      assemBlock(W,none,Lbls,Lt1,Lt2,Ln,Ln1,Pc1,Pc2,LsMap,B,[]),
      mnem(Ins,Lbls,Lt2,Ltx,Ln1,Lnx,Pc2,Pcx,LsMap,M,Cdx).
mnem([iBreak(W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[15,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iResult(V,W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[16,V,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iLoop(W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[17,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iDrop|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[18|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iDup|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[19|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iRot(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[20,V|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iRst(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[21,V|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iPick(V,W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[22,V,W|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iFiber|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[23|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iSuspend|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[24|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iResume|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[25|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iRetire|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[26|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iUnderflow|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[27|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iTry(V,W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[28,LtNo,B|M],Cdx) :-
      Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      assemBlock(W,none,Lbls,Lt1,Lt2,Ln,Ln1,Pc1,Pc2,LsMap,B,[]),
      mnem(Ins,Lbls,Lt2,Ltx,Ln1,Lnx,Pc2,Pcx,LsMap,M,Cdx).
mnem([iEndTry(W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[29,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iTryRslt(W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[30,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iThrow|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[31|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iLdV|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[32|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iLdC(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[33,LtNo|M],Cdx) :-
      Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iLdA(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[34,V|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iLdL(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[35,Off|M],Cdx) :-
      Pc1 is Pc+1,
      findLocal(V,LsMap,Off),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iStL(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[36,Off|M],Cdx) :-
      Pc1 is Pc+1,
      findLocal(V,LsMap,Off),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iStV(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[37,Off|M],Cdx) :-
      Pc1 is Pc+1,
      findLocal(V,LsMap,Off),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iTL(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[38,Off|M],Cdx) :-
      Pc1 is Pc+1,
      findLocal(V,LsMap,Off),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iLdG(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[39,V|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iStG(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[40,V|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iTG(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[41,V|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iSav|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[42|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iLdSav(W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[43,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iTstSav|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[44|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iStSav|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[45|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iTSav|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[46|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iCell|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[47|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iGet|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[48|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iAssign|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[49|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iCLbl(V,W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[50,LtNo,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iCLit(V,W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[51,LtNo,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iNth(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[52,V|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iStNth(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[53,V|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iIf(W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[54,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iIfNot(W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[55,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iCase(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[56,V|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iIndxJmp(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[57,V|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iIAdd|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[58|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iISub|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[59|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iIMul|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[60|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iIDiv|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[61|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iIMod|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[62|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iIAbs|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[63|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iIEq|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[64|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iILt|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[65|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iIGe|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[66|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iICmp(W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[67,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iCEq|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[68|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iCLt|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[69|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iCGe|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[70|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iCCmp(W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[71,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iBAnd|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[72|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iBOr|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[73|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iBXor|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[74|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iBLsl|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[75|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iBLsr|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[76|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iBAsr|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[77|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iBNot|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[78|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iFAdd|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[79|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iFSub|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[80|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iFMul|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[81|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iFDiv|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[82|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iFMod|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[83|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iFAbs|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[84|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iFEq|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[85|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iFLt|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[86|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iFGe|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[87|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iFCmp(W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[88,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iAlloc(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[89,LtNo|M],Cdx) :-
      Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iClosure(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[90,LtNo|M],Cdx) :-
      Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iCmp(W)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[91,Lvl|M],Cdx) :-
      Pc1 is Pc+1,
      findLevel(W,Lbls,0,Lvl),
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iFrame(V)|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[92,LtNo|M],Cdx) :-
      Pc1 is Pc+1,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).
mnem([iDBug|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[93|M],Cdx) :-
      Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).


baseOffset([(_,Base)|_],Base).
baseOffset([none|Lbs],Base) :-
  baseOffset(Lbs,Base).
baseOffset([],0).

findLevel(Tgt,[(Tgt,_)|_],Lvl,Lvl) :-!.
findLevel(Tgt,[none|Ends],L,Lo) :-
      L1 is L+1,
      findLevel(Tgt,Ends,L1,Lo).
findLevel(Tgt,[_|Ends],L,Lo) :-
  findLevel(Tgt,Ends,L,Lo).

findLocal(Nm,LsMap,Off) :-
      is_member((Nm,Off),LsMap),!.
findLocal(Nm,_,_) :-
      reportFatal("internal: Cannot assemble variable reference %s",[Nm]).

declareLocals(Lst, Map, Tpl) :-
  declareLocals(Lst,1,Map, Lcls),
  mkTpl(Lcls,Tpl).

declareLocals([],_,[],[]) :-!.
declareLocals([(Nm,Spec)|Lcls],Off,[(Nm,Off)|LsMap],[Entry|Lx]) :-
  mkTpl([strg(Nm),intgr(Off),Spec],Entry),
  Off1 is Off+1,
  declareLocals(Lcls,Off1,LsMap,Lx).

findLit(Lits,V,LtNo,Lits) :- is_member((V,LtNo),Lits),!.
findLit(Lits,V,LtNo,[(V,LtNo)|Lits]) :- length(Lits,LtNo).

mkLitTpl(Lits,Tpl) :-
    reverse(Lits,RLit),
    project0(RLit,Els),
    mkTpl(Els,Tpl).

mkInsTpl(Is,Tpl) :-
    map(Is,assemble:mkIns,Ins),
    mkTpl(Ins,Tpl).

mkIns(O,intgr(O)) :- number(O),!.
mkIns(S,strg(S)) :- string(S),!.
mkIns(C,Tpl) :- length(C,_),
  mkInsTpl(C,Tpl).

dispCode(Prog) :-
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

showMnem(iLine(U),Pc,sq([PcDx,ss(": "),ss("Line"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU).
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
showMnem(iXCall(U,V),Pc,sq([PcDx,ss(": "),ss("XCall"), ss(" "), UU, ss(","), VV])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  VV=ss(V),!,
  true.
showMnem(iXOCall(U,V),Pc,sq([PcDx,ss(": "),ss("XOCall"), ss(" "), UU, ss(","), VV])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  VV=ss(V),!,
  true.
showMnem(iXEscape(U,V),Pc,sq([PcDx,ss(": "),ss("XEscape"), ss(" "), UU, ss(","), VV])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),!,
  VV=ss(V),!,
  true.
showMnem(iTCall(U),Pc,sq([PcDx,ss(": "),ss("TCall"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  true.
showMnem(iTOCall(U),Pc,sq([PcDx,ss(": "),ss("TOCall"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iEntry(U),Pc,sq([PcDx,ss(": "),ss("Entry"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  true.
showMnem(iRet,Pc,sq([PcDx,ss(": "),ss("Ret")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iXRet,Pc,sq([PcDx,ss(": "),ss("XRet")])) :- !,
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
showMnem(iBreak(V),Pc,sq([PcDx,ss(": "),ss("Break"), ss(","), VV])) :- !,
  showPc(Pc,PcDx),
  VV=ss(V),!,
  true.
showMnem(iResult(U,V),Pc,sq([PcDx,ss(": "),ss("Result"), ss(" "), UU, ss(","), VV])) :- !,
  showPc(Pc,PcDx),
  UU=ix(U),
  VV=ss(V),!,
  true.
showMnem(iLoop(V),Pc,sq([PcDx,ss(": "),ss("Loop"), ss(","), VV])) :- !,
  showPc(Pc,PcDx),
  VV=ss(V),!,
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
showMnem(iTry(U,V),Pc,sq([PcDx,ss(": "),ss("Try"), ss(" "), UU, ss(","), VV])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  blockPc(Pc,SPc),
  showMnems(V, SPc, Ms),
  pcSpace(SPc,Dp),
  VV = sq([nl(Dp),iv(nl(Dp), Ms)]),
  true.
showMnem(iEndTry(V),Pc,sq([PcDx,ss(": "),ss("EndTry"), ss(" "), VV])) :- !,
  showPc(Pc,PcDx),
  VV=ss(V),!,
  true.
showMnem(iTryRslt(V),Pc,sq([PcDx,ss(": "),ss("TryRslt"), ss(" "), VV])) :- !,
  showPc(Pc,PcDx),
  VV=ss(V),!,
  true.
showMnem(iThrow,Pc,sq([PcDx,ss(": "),ss("Throw")])) :- !,
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
showMnem(iLdG(U),Pc,sq([PcDx,ss(": "),ss("LdG"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),!,
  true.
showMnem(iStG(U),Pc,sq([PcDx,ss(": "),ss("StG"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),!,
  true.
showMnem(iTG(U),Pc,sq([PcDx,ss(": "),ss("TG"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  UU=ss(U),!,
  true.
showMnem(iSav,Pc,sq([PcDx,ss(": "),ss("Sav")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iLdSav(V),Pc,sq([PcDx,ss(": "),ss("LdSav"), ss(" "), VV])) :- !,
  showPc(Pc,PcDx),
  VV=ss(V),!,
  true.
showMnem(iTstSav,Pc,sq([PcDx,ss(": "),ss("TstSav")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iStSav,Pc,sq([PcDx,ss(": "),ss("StSav")])) :- !,
  showPc(Pc,PcDx),
  true.
showMnem(iTSav,Pc,sq([PcDx,ss(": "),ss("TSav")])) :- !,
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
showMnem(iCLit(U,V),Pc,sq([PcDx,ss(": "),ss("CLit"), ss(" "), UU, ss(","), VV])) :- !,
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
showMnem(iIf(V),Pc,sq([PcDx,ss(": "),ss("If"), ss(" "), VV])) :- !,
  showPc(Pc,PcDx),
  VV=ss(V),!,
  true.
showMnem(iIfNot(V),Pc,sq([PcDx,ss(": "),ss("IfNot"), ss(" "), VV])) :- !,
  showPc(Pc,PcDx),
  VV=ss(V),!,
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
showMnem(iICmp(V),Pc,sq([PcDx,ss(": "),ss("ICmp"), ss(" "), VV])) :- !,
  showPc(Pc,PcDx),
  VV=ss(V),!,
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
showMnem(iCCmp(V),Pc,sq([PcDx,ss(": "),ss("CCmp"), ss(" "), VV])) :- !,
  showPc(Pc,PcDx),
  VV=ss(V),!,
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
showMnem(iFCmp(V),Pc,sq([PcDx,ss(": "),ss("FCmp"), ss(" "), VV])) :- !,
  showPc(Pc,PcDx),
  VV=ss(V),!,
  true.
showMnem(iAlloc(U),Pc,sq([PcDx,ss(": "),ss("Alloc"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  true.
showMnem(iClosure(U),Pc,sq([PcDx,ss(": "),ss("Closure"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  true.
showMnem(iCmp(V),Pc,sq([PcDx,ss(": "),ss("Cmp"), ss(" "), VV])) :- !,
  showPc(Pc,PcDx),
  VV=ss(V),!,
  true.
showMnem(iFrame(U),Pc,sq([PcDx,ss(": "),ss("Frame"), ss(" "), UU])) :- !,
  showPc(Pc,PcDx),
  ssTrm(U,0,UU),
  true.
showMnem(iDBug,Pc,sq([PcDx,ss(": "),ss("dBug")])) :- !,
  showPc(Pc,PcDx),
  true.


opcodeHash(135986043654767512).

bumpPc([Pc|Rest],[Pc1|Rest]) :- Pc1 is Pc+1.

blockPc(Pc,[0|Pc]).

showPc(Pc,iv(ss("."),Pcs)) :-
  reverse(Pc,RPc),
  map(RPc,assemble:shPc,Pcs).

shPc(I,ix(I)).

pcSpace(Pc,Dp) :-
  length(Pc,D),
  Dp is D*2.

