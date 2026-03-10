:- module(ssapeep,[peepOptimize/2]).

:- use_module(misc).
:- use_module(lterms).
:- use_module(ssa).
:- use_module(errors).

peepOptimize(func(Nm,H,Sig,AgMap,LsMap,Ins),func(Nm,H,Sig,AgMap,LsMx,Insx)) :-
  peepCode(Ins,[],PIns),
  findUnusedVars(LsMap,AgMap,PIns,LsMx,Ins0),!,
  peepCode(Ins0,[],Ins1),  % We need to peep twice, cos dropping vars gives us a chance for more
  adjustEntry(Ins1,LsMx,Insx).

peepCode(Ins,Lbls,Code) :-
  dropUnreachable(Ins,Is),!,
  peep(Is,Lbls,Is0),
  length(Is,L1),
  length(Is0,L2),
  (L2<L1*0.9 -> peep(Is0,Lbls,Code) ; Code=Is0).

findUnusedVars([(Vr,_Spec)|Vrs],AgMap,Ins,AVrs,ACde) :-
  \+varRead(Vr,Ins),
  \+is_member((Vr,_),AgMap),
  dropVar(Vr,Ins,I1),!,
  findUnusedVars(Vrs,AgMap,I1,AVrs,ACde).
findUnusedVars([(Vr,Spec)|Vrs],AgMap,Ins,[(Vr,Spec)|AVrs],ACde) :-
  findUnusedVars(Vrs,AgMap,Ins,AVrs,ACde).
findUnusedVars([],_,Ins,[],Ins).

varRead(Vr,[I|_]) :- vrRead(Vr,I),!.
varRead(Vr,[_|Ins]) :- varRead(Vr,Ins).

vrRead(Vr,iCall(_,As)) :-
    is_member(Vr,As).
vrRead(Vr,iTCall(_,As)) :-
    is_member(Vr,As).
vrRead(Vr,iEscape(_,As)) :-
    is_member(Vr,As).
vrRead(Vr,iRSP(_,Vr)).
vrRead(Vr,iOCall(_,_,Vr,_)).
vrRead(Vr,iOCall(_,_,_,As)) :-
    is_member(Vr,As),!.
vrRead(Vr,iTOCall(Vr,_)).
vrRead(Vr,iTOCall(_,As)) :-
    is_member(Vr,As),!.
vrRead(Vr,iHalt(Vr)).
vrRead(Vr,iAbort(_,Vr)).
vrRead(Vr,iRet(Vr)).
vrRead(Vr,iXRet(Vr)).

vrRead(Vr,iBlock(I)) :- varRead(Vr,I).
vrRead(Vr,iValof(_,I)) :- varRead(Vr,I).
vrRead(Vr,iResult(_,Vr)).
vrRead(Vr,iFiber(_,Vr)).
vrRead(Vr,iSuspend(Vr,_)).
vrRead(Vr,iSuspend(_,Vr)).
vrRead(Vr,iResume(Vr,_)).
vrRead(Vr,iResume(_,Vr)).
vrRead(Vr,iRetire(Vr,_)).
vrRead(Vr,iRetire(_,Vr)).

vrRead(Vr,iBind(_,Vr)).
vrRead(Vr,iMv(_,Vr)).
vrRead(Vr,iSG(_,Vr)).

vrRead(Vr,iLdSav(_,_,Vr)).
vrRead(Vr,iTstSav(_,Vr)).
vrRead(Vr,iStSav(_,Vr)).

vrRead(Vr,iCell(_,Vr)).
vrRead(Vr,iGet(_,Vr)).
vrRead(Vr,iAssign(_,Vr,_)).
vrRead(Vr,iAssign(_,_,Vr)).

vrRead(Vr,iCLbl(_,_,Vr)).
vrRead(Vr,iCChar(_,_,Vr)).
vrRead(Vr,iCFlt(_,_,Vr)).
vrRead(Vr,iCLit(_,_,Vr)).
vrRead(Vr,iCInt(_,_,Vr)).

vrRead(Vr,iNth(_,_,Vr)).
vrRead(Vr,iStNth(_,_,_,Vr)).
vrRead(Vr,iStNth(_,_,Vr,_)).

vrRead(Vr,iIf(_,Vr)).
vrRead(Vr,iIfNot(_,Vr)).

vrRead(Vr,iICase(Vr,_)).
vrRead(Vr,iCase(Vr,_)).
vrRead(Vr,iIXCase(Vr,_)).

vrRead(Vr,iIAdd(_,_,Vr)).
vrRead(Vr,iIAdd(_,Vr,_)).
vrRead(Vr,iISub(_,_,Vr)).
vrRead(Vr,iISub(_,Vr,_)).
vrRead(Vr,iIMul(_,_,Vr)).
vrRead(Vr,iIMul(_,Vr,_)).
vrRead(Vr,iIDiv(_,_,_,Vr)).
vrRead(Vr,iIDiv(_,_,Vr,_)).
vrRead(Vr,iIMod(_,_,_,Vr)).
vrRead(Vr,iIMod(_,_,Vr,_)).
vrRead(Vr,iAbs(_,Vr)).

vrRead(Vr,iFAdd(_,_,Vr)).
vrRead(Vr,iFAdd(_,Vr,_)).
vrRead(Vr,iFSub(_,_,Vr)).
vrRead(Vr,iFSub(_,Vr,_)).
vrRead(Vr,iFMul(_,_,Vr)).
vrRead(Vr,iFMul(_,Vr,_)).
vrRead(Vr,iFDiv(_,_,_,Vr)).
vrRead(Vr,iFDiv(_,_,Vr,_)).
vrRead(Vr,iFMod(_,_,_,Vr)).
vrRead(Vr,iFMod(_,_,Vr,_)).
vrRead(Vr,iFbs(_,Vr)).

vrRead(Vr,iIEq(_,Vr,_)).
vrRead(Vr,iIEq(_,_,Vr)).
vrRead(Vr,iILt(_,Vr,_)).
vrRead(Vr,iILt(_,_,Vr)).
vrRead(Vr,iIGe(_,Vr,_)).
vrRead(Vr,iIGe(_,_,Vr)).

vrRead(Vr,iFEq(_,Vr,_)).
vrRead(Vr,iFEq(_,_,Vr)).
vrRead(Vr,iFLt(_,Vr,_)).
vrRead(Vr,iFLt(_,_,Vr)).
vrRead(Vr,iFGe(_,Vr,_)).
vrRead(Vr,iFGe(_,_,Vr)).

vrRead(Vr,iCEq(_,Vr,_)).
vrRead(Vr,iCEq(_,_,Vr)).
vrRead(Vr,iCLt(_,Vr,_)).
vrRead(Vr,iCLt(_,_,Vr)).
vrRead(Vr,iCGe(_,Vr,_)).
vrRead(Vr,iCGe(_,_,Vr)).

vrRead(Vr,iBAnd(_,Vr,_)).
vrRead(Vr,iBAnd(_,_,Vr)).
vrRead(Vr,iBOr(_,Vr,_)).
vrRead(Vr,iBOr(_,_,Vr)).
vrRead(Vr,iBXor(_,Vr,_)).
vrRead(Vr,iBXor(_,_,Vr)).
vrRead(Vr,iBLsl(_,Vr,_)).
vrRead(Vr,iBLsl(_,_,Vr)).
vrRead(Vr,iBAsr(_,Vr,_)).
vrRead(Vr,iBAsr(_,_,Vr)).
vrRead(Vr,iBLsr(_,Vr,_)).
vrRead(Vr,iBLsr(_,_,Vr)).
vrRead(Vr,iBNot(_,Vr)).

vrRead(Vr,iAlloc(_,_,As)) :-
  is_member(Vr,As).
vrRead(Vr,iClosure(_,_,Vr)).
vrRead(Vr,iBump(Vr)).
vrRead(Vr,iDrop(Vr)).

vrRead(Vr,iLbl(_,I)) :- vrRead(Vr,I).

vrWrite(Vr,iRSP(_,Vr)).
vrWrite(Vr,iValof(Vr,_)).
vrWrite(Vr,iFiber(Vr,_)).

vrWrite(Vr,iMv(Vr,_)).
vrWrite(Vr,iMC(Vr,_)).

vrWrite(Vr,iSav(Vr)).

vrWrite(Vr,iLdSav(Vr,_,_)).
vrWrite(Vr,iTstSav(Vr,_)).
vrWrite(Vr,iStSav(Vr,_)).

vrWrite(Vr,iCell(Vr,_)).
vrWrite(Vr,iGet(Vr,_)).
vrWrite(Vr,iAssign(Vr,_,_)).

vrWrite(Vr,iNth(Vr,_,_)).

vrWrite(Vr,iIAdd(Vr,_,_)).
vrWrite(Vr,iISub(Vr,_,_)).
vrWrite(Vr,iIMul(Vr,_,_)).
vrWrite(Vr,iIDiv(_,Vr,_,_)).
vrWrite(Vr,iIMod(_,Vr,_,_)).
vrWrite(Vr,iIAbs(Vr,_)).

vrWrite(Vr,iFAdd(Vr,_,_)).
vrWrite(Vr,iFSub(Vr,_,_)).
vrWrite(Vr,iFMul(Vr,_,_)).
vrWrite(Vr,iFDiv(_,Vr,_,_)).
vrWrite(Vr,iFMod(_,Vr,_,_)).
vrWrite(Vr,iFbs(Vr,_)).

vrWrite(Vr,iIEq(Vr,_,_)).
vrWrite(Vr,iILt(Vr,_,_)).
vrWrite(Vr,iIGe(Vr,_,_)).

vrWrite(Vr,iFEq(Vr,_,_)).
vrWrite(Vr,iFLt(Vr,_,_)).
vrWrite(Vr,iFGe(Vr,_,_)).

vrWrite(Vr,iCEq(Vr,_,_)).
vrWrite(Vr,iCLt(Vr,_,_)).
vrWrite(Vr,iCGe(Vr,_,_)).

vrWrite(Vr,iBAnd(Vr,_,_)).
vrWrite(Vr,iBOr(Vr,_,_)).
vrWrite(Vr,iBXor(Vr,_,_)).
vrWrite(Vr,iBLsl(Vr,_,_)).
vrWrite(Vr,iBAsr(Vr,_,_)).
vrWrite(Vr,iBLsr(Vr,_,_)).
vrWrite(Vr,iBNot(Vr,_)).

vrWrite(Vr,iAlloc(Vr,_,_)).

vrWrite(Vr,iClosure(Vr,_,_)).

dropVar(_,[],[]).
dropVar(Vr,[iBlock(In)|Is],[iBlock(Inx)|Isx]) :-
  dropVar(Vr,In,Inx),
  dropVar(Vr,Is,Isx).
dropVar(Vr,[iValof(Vr,_)|Is],Isx) :-
  dropVar(Vr,Is,Isx).
dropVar(Vr,[iLbl(Lb,I)|Ins],Inx) :-
  dropVar(Vr,[I],IRx),
  (IRx=[] ->
   dropVar(Vr,Ins,Inx);
   IRx=[Ix] ->
   dropVar(Vr,Ins,Insx),
   Inx = [iLbl(Lb,Ix)|Insx];
   reportFatal("problem in dropVar",[])).
dropVar(Vr,[I|Is],Isx) :-
  vrWrite(Vr,I),!,
  dropVar(Vr,Is,Isx).
dropVar(Vr,[I|Is],Isx) :-
  vrRead(Vr,I),!,
  dropVar(Vr,Is,Isx).
dropVar(Vr,[I|Is],[I|Isx]) :-
  dropVar(Vr,Is,Isx).

dropUnreachable([],[]) :-!.
dropUnreachable([iBreak(Lvl)|_],[iBreak(Lvl)]) :-!.
dropUnreachable([iLoop(Lvl)|_],[iLoop(Lvl)]) :-!.
dropUnreachable([iResult(Lbl,Vr)|_],[iResult(Lbl,Vr)]) :-!.
dropUnreachable([iRtn|_],[iRtn]) :-!.
dropUnreachable([iRet(Vr)|_],[iRet(Vr)]) :-!.
dropUnreachable([iXRet(Vr)|_],[iXRet(Vr)]) :-!.

dropUnreachable([iTCall(Lb,As)|_],[iTCall(Lb,As)]) :-!.
dropUnreachable([iTOCall(Lb,As)|_],[iTOCall(Lb,As)]) :-!.
dropUnreachable([iAbort(Lt,Vr)|_],[iAbort(Lt,Vr)]) :-!.
dropUnreachable([iHalt(Vr)|_],[iHalt(Vr)]) :-!.
dropUnreachable([iRetire(Cn,As)|_],[iRetire(Cn,As)]) :-!.
dropUnreachable([iCase(Mx,Cs)|_],[iCase(Mx,Cs)]) :-!.
dropUnreachable([iICase(Mx,Cs)|_],[iICase(Mx,Cs)]) :- !.
dropUnreachable([iIxCase(Mx,Cs)|_],[iIxCase(Mx,Cs)]) :- !.
dropUnreachable([I|Ins],[I|DIns]) :-
  dropUnreachable(Ins,DIns).

peep([],_,[]) :-!.
peep([iLine(Lne),iLine(_)|Ins],Lbls,Inx) :-!,
  peep([iLine(Lne)|Ins],Lbls,Inx).
peep([iLbl(Lb,iBlock(IB))|Is],Lbls, Ins) :-!,
  peepCode(IB,[(Lb,Is)|Lbls],IB0),
  peep(Is,Lbls,Is0),
  (lblReferenced(Lb,IB0) ->
   Ins=[iLbl(Lb,iBlock(IB0))|Is0];
   concat(IB0,Is0,Is1),
   peepCode(Is1,Lbls,Ins)).
peep([iLbl(Lb,iValof(Vr,IB))|Is],Lbls, Ins) :-!,
  peepCode(IB,[(Lb,Is)|Lbls],IB0),
  peep(Is,Lbls,Is0),
  (lblReferenced(Lb,IB0) ->
   Ins=[iLbl(Lb,iValof(Vr,IB0))|Is0];
   concat(IB0,Is0,Is1),
   peepCode(Is1,Lbls,Ins)).
peep([iBlock(IB)|Is],Lbls, [iBlock(IBs)|Ins]) :-!,
  peepCode(IB,Lbls,IBs),
  peep(Is,Lbls, Ins).
peep([iValof(Vr,IB)|Is],Lbls, [iValof(Vr,IBs)|Ins]) :-!,
  peepCode(IB,Lbls,IBs),
  peep(Is,Lbls, Ins).
peep([iIf(Lb,Vr)|In],Lbls,[iIf(LLb,Vr)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iIfNot(Lb,Vr)|In],Lbls,[iIfNot(LLb,Vr)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iCLbl(Tgt,Lb,Vr)|In],Lbls,[iCLbl(Tgt,LLb,Vr)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iCLit(Tgt,Lb,Vr)|In],Lbls,[iCLit(Tgt,LLb,Vr)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iCInt(Tgt,Lb,Vr)|In],Lbls,[iCInt(Tgt,LLb,Vr)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iCChar(Tgt,Lb,Vr)|In],Lbls,[iCChar(Tgt,LLb,Vr)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iCFlt(Tgt,Lb,Vr)|In],Lbls,[iCFlt(Tgt,LLb,Vr)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iBreak(Lb)|_],Lbls,[iBreak(LLb)]) :-
  resolveLblRef(Lb,Lbls,LLb).
peep([iResult(Lb,Vr)|_],Lbls,[iResult(LLb,Vr)]) :-
  resolveLblRef(Lb,Lbls,LLb).
peep([iCall(Lb,Args),iRSP(_,Rslt),iRet(Rslt)|_],_Lbls,[iTCall(Lb,Args)]).
peep([iOCall(Vr,Args),iRSP(_,Rslt),iRet(Rslt)|_],_Lbls,[iTOCall(Vr,Args)]).
peep([iLoop(Lb)|_],_Lbls,[iLoop(Lb)]) :-!.
peep([iRet(L)|_],_Lbls,[iRet(L)]) :-!.
peep([iXRet(L)|_],_Lbls,[iXRet(L)]) :-!.
peep([iRetire(V1,V2)|_],_,[iRetire(V1,V2)]) :-!.
peep([iCase(Vr,Cs)|_],_Lbls,[iCase(Vr,Cs)]).
peep([iICase(Vr,Cs)|_],_Lbls,[iICase(Vr,Cs)]).
peep([iIxCase(Vr,Cs)|_],_Lbls,[iIxCase(Vr,Cs)]).
peep([iAbort(Lt,Vr)|_],_Lbls,[iAbort(Lt,Vr)]).
peep([I|Is],Lbls, [I|Ins]) :- peep(Is,Lbls, Ins).

lblReferenced(Lb,[iBreak(Lb)|_]).
lblReferenced(Lb,[iLoop(Lb)|_]).
lblReferenced(Lb,[iResult(Lb,_)|_]).
lblReferenced(Lb,[iIf(Lb,_)|_]).
lblReferenced(Lb,[iIfNot(Lb,_)|_]).
lblReferenced(Lb,[iCLbl(_,Lb,_)|_]).
lblReferenced(Lb,[iCInt(_,Lb,_)|_]).
lblReferenced(Lb,[iCChar(_,Lb,_)|_]).
lblReferenced(Lb,[iCFlt(_,Lb,_)|_]).
lblReferenced(Lb,[iCLit(_,Lb,_)|_]).
lblReferenced(Lb,[iIDiv(Lb,_,_,_)|_]).
lblReferenced(Lb,[iIMod(Lb,_,_,_)|_]).
lblReferenced(Lb,[iFDiv(Lb,_,_,_)|_]).
lblReferenced(Lb,[iFMod(Lb,_,_,_)|_]).
lblReferenced(Lb,[iRSP(Lb,_)|_]).
lblReferenced(Lb,[iLbl(_,I)|_]) :-
  lblReferenced(Lb,[I]).
lblReferenced(Lb,[iBlock(I)|_]) :-
  lblReferenced(Lb,I).
lblReferenced(Lb,[iValof(_,I)|_]) :-
  lblReferenced(Lb,I).
lblReferenced(Lb,[iLdSav(_,Lb,_)|_]).
lblReferenced(Lb,[iXCall(_,Lb,_,_)|_]).
lblReferenced(Lb,[iXOCall(Lb,_,_,_)|_]).
lblReferenced(Lb,[iXEscape(_,Lb,_,_)|_]).
lblReferenced(Lb,[iCase(_,Ins)|_]) :- lbelReferenced(Lb,Ins).
lblReferenced(Lb,[iICase(_,Ins)|_]) :- lbelReferenced(Lb,Ins).
lblReferenced(Lb,[iIxCase(_,Ins)|_]) :- lbelReferenced(Lb,Ins).
lblReferenced(Lb,[_|Ins]) :- lblReferenced(Lb,Ins).

resolveLblRef(Lb,Lbls,LLb) :-
  is_member((Lb,Cde),Lbls),!,
  (Cde=[iBreak(Lb0)|_] ->
   resolveLblRef(Lb0,Lbls,LLb) ;
   LLb = Lb).

adjustEntry([iEntry(Ar,_)|Ins],LsMx,[iEntry(Ar,Cnt)|Ins]) :-!,
  length(LsMx,Cnt).
adjustEntry([Op|Ins],LsMx,[Op|Insx]) :-
  adjustEntry(Ins,LsMx,Insx).
