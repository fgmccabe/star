:- module(peephole,[peepOptimize/2]).

:- use_module(misc).
:- use_module(types).
:- use_module(lterms).
:- use_module(encode).
:- use_module(assem).
:- use_module(errors).
:- use_module(gensig).
:- use_module(location).

peepOptimize(Ins,Cde) :-
  findLblUsages(Ins,lbls{},Lbs),
  deleteUnused(false,Ins,Lbs,In0),
  peep(In0,Cde).

findLblUsages([],Lblx,Lblx).
findLblUsages([iJmp(Lbl)|Ins],Lb,Lbx) :-
  addJmp(Lbl,Lb,Lb0),
  findLblUsages(Ins,Lb0,Lbx).
findLblUsages([iCLbl(_,Lbl)|Ins],Lb,Lbx) :-
  addJmp(Lbl,Lb,Lb0),
  findLblUsages(Ins,Lb0,Lbx).
findLblUsages([iIf(Lbl)|Ins],Lb,Lbx) :-
  addJmp(Lbl,Lb,Lb0),
  findLblUsages(Ins,Lb0,Lbx).
findLblUsages([iIfNot(Lbl)|Ins],Lb,Lbx) :-
  addJmp(Lbl,Lb,Lb0),
  findLblUsages(Ins,Lb0,Lbx).
findLblUsages([iUnpack(_,Lbl)|Ins],Lb,Lbx) :-
  addJmp(Lbl,Lb,Lb0),
  findLblUsages(Ins,Lb0,Lbx).
findLblUsages([iFCmp(Lbl)|Ins],Lb,Lbx) :-
  addJmp(Lbl,Lb,Lb0),
  findLblUsages(Ins,Lb0,Lbx).
findLblUsages([iCmp(Lbl)|Ins],Lb,Lbx) :-
  addJmp(Lbl,Lb,Lb0),
  findLblUsages(Ins,Lb0,Lbx).
findLblUsages([iLocal(_,St,En,_)|Ins],Lbs,Lbx) :-
  addJmp(St,Lbs,Lb0),
  addJmp(En,Lb0,Lb1),
  findLblUsages(Ins,Lb1,Lbx).
findLblUsages([_|Ins],Lb,Lbx) :-
  findLblUsages(Ins,Lb,Lbx).

addJmp(Lb,Lbs,Lbx) :-
  makeKey(Lb,Ky),
  put_dict(Ky,Lbs,Lb,Lbx).

isUsedLbl(Lb,Lbs) :-
  makeKey(Lb,Ky),
  get_dict(Ky,Lbs,_),!.

deleteUnused(_,[],_,[]).
deleteUnused(F,[iLbl(Lb)|Ins],Lbs,[iLbl(Lb)|Cde]) :-
  isUsedLbl(Lb,Lbs),!,
  deleteUnused(F,Ins,Lbs,Cde).
deleteUnused(F,[iLbl(Lb)|Ins],Lbs,Cde) :-
  \+isUsedLbl(Lb,Lbs),
  dropUntilLbl(F,Ins,Lbs,Cde).
deleteUnused(F,[I|Ins],Lbs,[I|Cde]) :-
  deleteUnused(F,Ins,Lbs,Cde).

dropUntilLbl(_,[],_,[]).
dropUntilLbl(F,[iLbl(Lb)|Ins],Lbs,Cde) :-
  deleteUnused(F,[iLbl(Lb)|Ins],Lbs,Cde).
dropUntilLbl(true,[_|Ins],Lbs,Cde) :-
  dropUntilLbl(true,Ins,Lbs,Cde).
dropUntilLbl(false,[I|Ins],Lbs,[I|Cde]) :-
  deleteUnused(false,Ins,Lbs,Cde).

peep([],[]).
peep([iLine(Lc),iLine(_)|Ins], Out) :-
  peep([iLine(Lc)|Ins],Out).
peep([I|Ins],[I|Cde]) :-
  peep(Ins,Cde).

