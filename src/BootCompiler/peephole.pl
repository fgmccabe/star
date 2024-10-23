:- module(peephole,[peepOptimize/2]).

:- use_module(misc).
:- use_module(types).
:- use_module(lterms).
:- use_module(encode).
:- use_module(assem).
:- use_module(errors).
:- use_module(gensig).
:- use_module(location).

peepOptimize(Ins,Code) :-
  dropUnreachable(Ins,Is),
  dispIns(Is),
  peep(Is,Code).

dropUnreachable([],[]) :-!.
dropUnreachable([iBreak(Lvl)|_],[iBreak(Lvl)]) :-!.
dropUnreachable([iLoop(Lvl)|_],[iLoop(Lvl)]) :-!.
dropUnreachable([iEndTry(Lvl)|_],[iEndTry(Lvl)]) :-!.
dropUnreachable([iRet|_],[iRet]) :-!.
dropUnreachable([iTCall(Lb)|_],[iTCall(Lb)]) :-!.
dropUnreachable([iTOCall(Lb)|_],[iTOCall(Lb)]) :-!.
dropUnreachable([iAbort|_],[iAbort]) :-!.
dropUnreachable([iHalt(Ix)|_],[iHalt(Ix)]) :-!.
dropUnreachable([I|Ins],[I|DIns]) :-
  dropUnreachable(Ins,DIns).
dropUnreachable([iCase(Mx)|I],[iCase(Mx)|Is]) :-
  copyN(Mx,I,I0,Is,Is0),
  dropUnreachable(I0,Is0).

peep([],[]) :-!.
peep([iStL(Off),iLdL(Off),iRet|_], [iRet]) :-!.
peep([iStL(Off),iLdL(Off)|Is], Ins) :-
  peep([iTL(Off)|Is],Ins).
peep([iBlock(Tpe,IB)|Is],[iBlock(Tpe,IBs)|Ins]) :-
  peepOptimize(IB,IBs),
  peep(Is,Ins).
peep([iLbl(Lb,iBlock(Tps,IB))|Is],Ins) :-
  peepOptimize(IB,IB0),
  peep(Is,Is0),
  (lblReferenced(Lb,IB0) ->
   Ins=[iLbl(Lb,iBlock(Tps,IB0))|Is0];
   concat(IB0,Is0,Is1),
   dropUnreachable(Is1,Ins)).
peep([I|Is],[I|Ins]) :- peep(Is,Ins).

pullJumps(Ins,InsX) :-
  findTgts(Ins,mp{},Map),
  pullJmps(Ins,Map,InsX).

lblReferenced(Lb,[iBreak(Lb)|_]).
lblReferenced(Lb,[iLoop(Lb)|_]).
lblReferenced(Lb,[iEndTry(Lb)|_]).
lblReferenced(Lb,[iIf(Lb)|_]).
lblReferenced(Lb,[iIfNot(Lb)|_]).
lblReferenced(Lb,[iCmp(Lb)|_]).
lblReferenced(Lb,[iCCmp(Lb)|_]).
lblReferenced(Lb,[iICmp(Lb)|_]).
lblReferenced(Lb,[iFCmp(Lb)|_]).
lblReferenced(Lb,[iCLbl(_,Lb)|_]).
lblReferenced(Lb,[iUnpack(_,Lb)|_]).
lblReferenced(Lb,[iLbl(_,I)|_]) :-
  lblReferenced(Lb,[I]).
lblReferenced(Lb,[_|Ins]) :- lblReferenced(Lb,Ins).

copyN(0,I,I,X,X) :-!.
copyN(N,[A|I],Ix,[A|X],Xx) :-
  N1 is N-1,
  copyN(N1,I,Ix,X,Xx).

