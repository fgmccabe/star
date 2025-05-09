:- module(peephole,[peepOptimize/2]).

:- use_module(misc).
:- use_module(types).
:- use_module(lterms).
:- use_module(encode).
:- use_module(assem).
:- use_module(errors).
:- use_module(gensig).
:- use_module(location).

peepOptimize(func(Nm,H,Sig,LsMap,Ins),func(Nm,H,Sig,LsMx,Insx)) :-
  peepCode(Ins,[],PIns),
  findUnusedVars(LsMap,PIns,LsMx,Ins0),!,
  peepCode(Ins0,[],Ins1),  % We need to peep twice, cos droping vars gives us a chance for more
  adjustEntry(Ins1,LsMx,Insx).

peepCode(Ins,Lbls,Code) :-
  dropUnreachable(Ins,Is),!,
  peep(Is,Lbls,Is0),
  length(Is,L1),
  length(Is0,L2),
  (L2<L1*0.9 -> peep(Is0,Lbls,Code) ; Code=Is0).

findUnusedVars([(Vr,_Spec)|Vrs],Ins,AVrs,ACde) :-
  \+varRead(Vr,Ins),
  dropVar(Vr,Ins,I1),!,
  findUnusedVars(Vrs,I1,AVrs,ACde).
findUnusedVars([(Vr,Spec)|Vrs],Ins,[(Vr,Spec)|AVrs],ACde) :-
  findUnusedVars(Vrs,Ins,AVrs,ACde).
findUnusedVars([],Ins,[],Ins).

varRead(Vr,[I|_]) :- vrRead(Vr,I),!.
varRead(Vr,[_|Ins]) :- varRead(Vr,Ins).

vrRead(Vr,iBlock(_,I)) :- varRead(Vr,I).
vrRead(Vr,iLdL(Vr)).
vrRead(Vr,iLdA(Vr)).
vrRead(Vr,iLbl(_,I)) :- vrRead(Vr,I).

dropVar(_,[],[]).
dropVar(Vr,[iTL(Vr)|Ins],Ix) :- dropVar(Vr,Ins,Ix).
dropVar(Vr,[iStL(Vr)|Ins],[iDrop|Ix]) :- dropVar(Vr,Ins,Ix).
dropVar(Vr,[iStV(Vr)|Ins],Ix) :- dropVar(Vr,Ins,Ix).
dropVar(Vr,[iBlock(Sig,In)|Is],[iBlock(Sig,Inx)|Isx]) :-
  dropVar(Vr,In,Inx),
  dropVar(Vr,Is,Isx).
dropVar(Vr,[iLbl(Lb,I)|Ins],Inx) :-
  dropVar(Vr,[I],IRx),
  (IRx=[] ->
   dropVar(Vr,Ins,Inx);
   IRx=[Ix] ->
   dropVar(Vr,Ins,Insx),
   Inx = [iLbl(Lb,Ix)|Insx];
   reportFatal("problem in dropVar",[])).
dropVar(Vr,[I|Ins],[I|Ix]) :- dropVar(Vr,Ins,Ix).

dropUnreachable([],[]) :-!.
dropUnreachable([iBreak(Lvl)|_],[iBreak(Lvl)]) :-!.
dropUnreachable([iLoop(Lvl)|_],[iLoop(Lvl)]) :-!.
dropUnreachable([iResult(Lbl)|_],[iResult(Lbl)]) :-!.
dropUnreachable([iThrow|_],[iThrow]) :-!.
dropUnreachable([iRet|_],[iRet]) :-!.
dropUnreachable([iXRet|_],[iXRet]) :-!.
dropUnreachable([iTCall(Lb)|_],[iTCall(Lb)]) :-!.
dropUnreachable([iTOCall(Lb)|_],[iTOCall(Lb)]) :-!.
dropUnreachable([iAbort|_],[iAbort]) :-!.
dropUnreachable([iHalt(Ix)|_],[iHalt(Ix)]) :-!.
dropUnreachable([iRetire|_],[iRetire]) :-!.
dropUnreachable([iCase(Mx)|I],[iCase(Mx)|Is]) :-
  copyN(Mx,I,[],Is,[]).
dropUnreachable([I|Ins],[I|DIns]) :-
  dropUnreachable(Ins,DIns).

peep([],_,[]) :-!.
peep([iLine(_),iLine(Lne)|Ins],Lbls,Inx) :-!,
  peep([iLine(Lne)|Ins],Lbls,Inx).
peep([iStL(Off),iLdL(Off),iRet|_], _, [iRet]) :-!.
peep([iStL(Off),iLdL(Off)|Is], Lbls, Ins) :-!,
  peep([iTL(Off)|Is],Lbls, Ins).
peep([iLdL(_),iDrop|Is],Lbls,Ins) :- !,
  peep(Is,Lbls,Ins).
peep([iLdL(_),iNth(_),iDrop|Is],Lbls,Ins) :- !,
  peep(Is,Lbls,Ins).
peep([iLdA(_),iDrop|Is],Lbls,Ins) :- !,
  peep(Is,Lbls,Ins).
peep([iLdA(_),iNth(_),iDrop|Is],Lbls,Ins) :- !,
  peep(Is,Lbls,Ins).
peep([iNth(_),iDrop|Is],Lbls,Ins) :-
  peep([iDrop|Is],Lbls,Ins).  
peep([iBlock(Tpe,IB)|Is],Lbls, [iBlock(Tpe,IBs)|Ins]) :-!,
  peepCode(IB,Lbls,IBs),
  peep(Is,Lbls, Ins).
peep([iLbl(Lb,iBlock(Tps,IB))|Is],Lbls, Ins) :-!,
  peepCode(IB,[(Lb,Is)|Lbls],IB0),
  peep(Is,Lbls,Is0),
  (lblReferenced(Lb,IB0) ->
   Ins=[iLbl(Lb,iBlock(Tps,IB0))|Is0];
   concat(IB0,Is0,Is1),
   peepCode(Is1,Lbls,Ins)).
peep([iLbl(Lb,iTry(Tp,IB))|Is],Lbls, Ins) :-!,
  peepCode(IB,[(Lb,Is)|Lbls],IB0),
  peep(Is,Lbls,Is0),
  (lblReferenced(Lb,IB0) ->
   Ins=[iLbl(Lb,iTry(Tp,IB0))|Is0];
   Ins=[iTry(Tp,IB0)|Is0]).
peep([iIf(Lb)|In],Lbls,[iIf(LLb)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iIfNot(Lb)|In],Lbls,[iIfNot(LLb)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iCLbl(Tgt,Lb)|In],Lbls,[iCLbl(Tgt,LLb)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iCLit(Tgt,Lb)|In],Lbls,[iCLit(Tgt,LLb)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iCmp(Lb)|In],Lbls,[iCmp(LLb)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iICmp(Lb)|In],Lbls,[iICmp(LLb)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iFCmp(Lb)|In],Lbls,[iFCmp(LLb)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iCCmp(Lb)|In],Lbls,[iCCmp(LLb)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iCCmp(Lb)|In],Lbls,[iCCmp(LLb)|Inx]) :-
  resolveLblRef(Lb,Lbls,LLb),
  peep(In,Lbls,Inx).
peep([iBreak(Lb)|_],Lbls,[iBreak(LLb)]) :-
  resolveLblRef(Lb,Lbls,LLb).
peep([iResult(Lb)|_],Lbls,[iResult(LLb)]) :-
  resolveLblRef(Lb,Lbls,LLb).
peep([iLoop(Lb)|_],_Lbls,[iLoop(Lb)]) :-!.
peep([iRetire|_],_,[iRetire]) :-!.
peep([iCase(Mx)|In],_Lbls,[iCase(Mx)|Inx]) :-
  copyN(Mx,In,[],Inx,[]).
peep([iIndxJmp(Mx)|In],_Lbls,[iCase(Mx)|Inx]) :-
  copyN(Mx,In,[],Inx,[]).
peep([I|Is],Lbls, [I|Ins]) :- peep(Is,Lbls, Ins).

lblReferenced(Lb,[iBreak(Lb)|_]).
lblReferenced(Lb,[iLoop(Lb)|_]).
lblReferenced(Lb,[iResult(Lb)|_]).
lblReferenced(Lb,[iIf(Lb)|_]).
lblReferenced(Lb,[iIfNot(Lb)|_]).
lblReferenced(Lb,[iCmp(Lb)|_]).
lblReferenced(Lb,[iCCmp(Lb)|_]).
lblReferenced(Lb,[iICmp(Lb)|_]).
lblReferenced(Lb,[iFCmp(Lb)|_]).
lblReferenced(Lb,[iCLbl(_,Lb)|_]).
lblReferenced(Lb,[iCLit(_,Lb)|_]).
lblReferenced(Lb,[iLbl(_,I)|_]) :-
  lblReferenced(Lb,[I]).
lblReferenced(Lb,[iBlock(_,I)|_]) :-
  lblReferenced(Lb,I).
lblReferenced(Lb,[iLdSav(Lb)|_]).
lblReferenced(Lb,[iXCall(_,Lb)|_]).
lblReferenced(Lb,[iXOCall(_,Lb)|_]).
lblReferenced(Lb,[iXEscape(_,Lb)|_]).
lblReferenced(Lb,[_|Ins]) :- lblReferenced(Lb,Ins).

resolveLblRef(Lb,Lbls,LLb) :-
  is_member((Lb,Cde),Lbls),!,
  (Cde=[iBreak(Lb0)|_] ->
   resolveLblRef(Lb0,Lbls,LLb) ;
   LLb = Lb).

copyN(0,I,I,X,X) :-!.
copyN(N,[A|I],Ix,[A|X],Xx) :-
  N1 is N-1,
  copyN(N1,I,Ix,X,Xx).

adjustEntry([iEntry(_)|Ins],LsMx,[iEntry(Cnt)|Ins]) :-!,
  length(LsMx,Cnt).
adjustEntry([Op|Ins],LsMx,[Op|Insx]) :-
  adjustEntry(Ins,LsMx,Insx).
