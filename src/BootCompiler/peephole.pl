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
%  dispIns(func(lbl("",0),hard,"",0,In0)),
  findTgts(Ins,mp{},Map),
  peep(In0,Map,Cde),!.

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
findLblUsages([iICmp(Lbl)|Ins],Lb,Lbx) :-
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
deleteUnused(_,[iLbl(Lb)|Ins],Lbs,[iLbl(Lb)|Cde]) :-
  isUsedLbl(Lb,Lbs),!,
  deleteUnused(false,Ins,Lbs,Cde).
deleteUnused(true,[iLbl(Lb)|Ins],Lbs,Cde) :-
  \+isUsedLbl(Lb,Lbs),
  dropUntilLbl(true,Ins,Lbs,Cde).
deleteUnused(false,[iLbl(Lb)|Ins],Lbs,Cde) :-
  \+isUsedLbl(Lb,Lbs),
  deleteUnused(false,Ins,Lbs,Cde).
deleteUnused(F,[iIndxJmp(Ar)|Ins],Lbs,[iIndxJmp(Ar)|Cde]) :-
  copyN(Ins,Ar,Insx,Cde,Rst),!,
  deleteUnused(F,Insx,Lbs,Rst).
deleteUnused(F,[iCase(Ar)|Ins],Lbs,[iCase(Ar)|Cde]) :-
  copyN(Ins,Ar,Insx,Cde,Rst),!,
  deleteUnused(F,Lbs,Insx,Rst).
deleteUnused(_,[I|Ins],Lbs,[I|Cde]) :-
  propagateDrop(I,F1),!,
  deleteUnused(F1,Ins,Lbs,Cde).

copyN([],_,[],Cde,Cde).
copyN(Is,0,Is,Cde,Cde).
copyN([E|Is],Ix,Isx,[E|Cs],Cde) :-
  Ix1 is Ix-1,
  copyN(Is,Ix1,Isx,Cs,Cde).

propagateDrop(I,true) :-
  uncondJump(I),!.
propagateDrop(_,false).

uncondJump(iJmp(_)).
uncondJump(iRet).
uncondJump(iRetire).
uncondJump(iAbort).

dropUntilLbl(_,[],_,[]).
dropUntilLbl(F,[iLbl(Lb)|Ins],Lbs,Cde) :-
  deleteUnused(F,[iLbl(Lb)|Ins],Lbs,Cde).
dropUntilLbl(true,[_|Ins],Lbs,Cde) :-
  dropUntilLbl(true,Ins,Lbs,Cde).
dropUntilLbl(false,[I|Ins],Lbs,[I|Cde]) :-
  deleteUnused(false,Ins,Lbs,Cde).

findTgts([],Mp,Mp).
findTgts([iLbl(Lb)|Ins],Mp,Mpx) :-
  addTgt(Lb,Ins,Mp,Mp0),
  findTgts(Ins,Mp0,Mpx).
findTgts([_|Ins],Mp,Mpx) :-
  findTgts(Ins,Mp,Mpx).

addTgt(Lb,Ins,Mp,Mpx) :-
  makeKey(Lb,Ky),
  put_dict(Ky,Mp,Ins,Mpx).

pickupTgt(Lb,Mp,Ins) :-
  makeKey(Lb,Ky),
  get_dict(Ky,Mp,Ins).
  
peep([],_,[]).
peep([iLine(Lc),iLine(_)|Ins],Map, Out) :-!,
  peep([iLine(Lc)|Ins],Map,Out).
peep(Ins,Map,Out) :-
  accessorPtn(Ins,Int),!,
  peep(Int,Map,Out).
peep([I|Ins],Map,[I|Cde]) :-
  peep(Ins,Map,Cde).

accessorPtn([iUnpack(Lb,Fl)|Ins],[iUnpack(Lb,Fl)|LdDrops]) :-
  dropSeq(Ins,[iStL(Off)|Ins1],LdDrops,[iRet|Inz]),
  dropSeq(Ins1,[iLdL(Off),iRet|Inz],_,_),!.
  
dropSeq([iDrop|Ins],Rest,[iDrop|Iz],Io) :-!,
  dropSeq(Ins,Rest,Iz,Io).
dropSeq(Ins,Ins,Dp,Dp).
