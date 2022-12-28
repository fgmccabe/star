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
  pullJumps(Ins,InsJ),
%  dispIns(func(lbl("",0),hard,"",0,InsJ)),
  findLblUsages(InsJ,lbls{},Lbs),
  deleteUnused(false,InsJ,Lbs,In0),
  findTgts(Ins,mp{},Map),
  peep(In0,Map,Cde),!.

pullJumps(Ins,InsX) :-
  findTgts(Ins,mp{},Map),
  pullJmps(Ins,Map,InsX).

pullJmps([],_,[]).
pullJmps([iJmp(Lbl)|Ins],Map,InsX) :-
  pullJump(Lbl,Map,Ins,InsX).
pullJmps([iIfNot(Lbl)|Ins],Map,[iIfNot(LblX)|InsX]) :-
  pullJump(Lbl,Map,Ins,[iJmp(LblX)|InsX]).
pullJmps([I|Ins],Map,[I|InsX]) :-
  pullJmps(Ins,Map,InsX).

pullJump(Lbl,Map,Ins,InsX) :-
  pickupTgt(Lbl,Map,TgtIns),
  pickupIns(TgtIns,Ins,Map,InsX).
pullJump(Lbl,Map,Ins,[iJmp(Lbl)|InsX]) :-
  pullJmps(Ins,Map,InsX).

pickupIns([iRet|_],Ins,Map,[iRet,iNop,iNop|InsX]) :-!,
  pullJmps(Ins,Map,InsX).
pickupIns([iJmp(L2)|_],Ins,Map,InsX) :-
  pullJmps([iJmp(L2)|Ins],Map,InsX),!.

findLblUsages([],Lblx,Lblx).
findLblUsages([I|Ins],Lbs,Lbx) :-
  addLblUsage(I,peephole:addLbl,Lbs,Lb1),
  findLblUsages(Ins,Lb1,Lbx).

addLblUsage(iJmp(Lbl),H,Lbs,Lbx) :-!,
  call(H,Lbl,Lbs,Lbx).
addLblUsage(iTry(Lbl),H,Lbs,Lbx) :-!,
  call(H,Lbl,Lbs,Lbx).
addLblUsage(iCLbl(_,Lbl),H,Lbs,Lbx) :-!,
  call(H,Lbl,Lbs,Lbx).
addLblUsage(iIf(Lbl),H,Lbs,Lbx) :-!,
  call(H,Lbl,Lbs,Lbx).
addLblUsage(iIfNot(Lbl),H,Lbs,Lbx) :-!,
  call(H,Lbl,Lbs,Lbx).
addLblUsage(iUnpack(_,Lbl),H,Lbs,Lbx) :-!,
  call(H,Lbl,Lbs,Lbx).
addLblUsage(iFCmp(Lbl),H,Lbs,Lbx) :-!,
  call(H,Lbl,Lbs,Lbx).
addLblUsage(iICmp(Lbl),H,Lbs,Lbx) :-!,
  call(H,Lbl,Lbs,Lbx).
addLblUsage(iCmp(Lbl),H,Lbs,Lbx) :-!,
  call(H,Lbl,Lbs,Lbx).
addLblUsage(iCall(_,Lbl),H,Lbs,Lbx) :-!,
  call(H,Lbl,Lbs,Lbx).
addLblUsage(iOCall(_,Lbl),H,Lbs,Lbx) :-!,
  call(H,Lbl,Lbs,Lbx).
addLblUsage(iEscape(_,Lbl),H,Lbs,Lbx) :-!,
  call(H,Lbl,Lbs,Lbx).
addLblUsage(iLdG(_,Lbl),H,Lbs,Lbx) :-!,
  call(H,Lbl,Lbs,Lbx).
addLblUsage(iLocal(_,St,En,_),_H,Lbs,Lbx) :-!,
  softAddLbl(St,Lbs,Lb0),
  softAddLbl(En,Lb0,Lbx).
addLblUsage(_,_,Lbx,Lbx).

addLbl(Lb,Lbs,Lbx) :-
  makeKey(Lb,Ky),
  (get_dict(Ky,Lbs,(L,Cnt)) ->
   Cnt1 is Cnt+1,
   put_dict(Ky,Lbs,(L,Cnt1),Lbx);
   put_dict(Ky,Lbs,(false,1),Lbx)).

softAddLbl(Lb,Lbs,Lbx) :-
  makeKey(Lb,Ky),
  (get_dict(Ky,Lbs,(_,Cnt)) ->
   put_dict(Ky,Lbs,(true,Cnt),Lbx);
   put_dict(Ky,Lbs,(true,0),Lbx)).

dropLbl(Lb,Lbs,Lbx) :-
  makeKey(Lb,Ky),
  (get_dict(Ky,Lbs,(L,Cnt)) ->
   Cnt1 is Cnt-1,
   ((Cnt1>0;L=true) ->
    put_dict(Ky,Lbs,(L,Cnt1),Lbx);
    del_dict(Ky,Lbs,_,Lbx));
   Lbx=Lbs).

isUsedLbl(Lb,Lbs,Lc,Cnt) :-
  makeKey(Lb,Ky),
  get_dict(Ky,Lbs,(Lc,Cnt)),!.

deleteUnused(_,[],_,[]).
deleteUnused(true,[iLbl(Lb)|Ins],Lbs,[iLbl(Lb)|Cde]) :-
  isUsedLbl(Lb,Lbs,true,0),!,
  dropUntilLbl(true,Ins,Lbs,Cde).
deleteUnused(_,[iLbl(Lb)|Ins],Lbs,[iLbl(Lb)|Cde]) :-
  isUsedLbl(Lb,Lbs,_,_),!,
  deleteUnused(false,Ins,Lbs,Cde).
deleteUnused(true,[iLbl(Lb)|Ins],Lbs,Cde) :-
  \+isUsedLbl(Lb,Lbs,_,_),
  dropUntilLbl(true,Ins,Lbs,Cde).
deleteUnused(false,[iLbl(Lb)|Ins],Lbs,Cde) :-
  \+isUsedLbl(Lb,Lbs,_,_),
  deleteUnused(false,Ins,Lbs,Cde).
deleteUnused(F,[iIndxJmp(Ar)|Ins],Lbs,[iIndxJmp(Ar)|Cde]) :-
  copyN(Ins,Ar,Insx,Cde,Rst),!,
  deleteUnused(F,Insx,Lbs,Rst).
deleteUnused(F,[iCase(Ar)|Ins],Lbs,[iCase(Ar)|Cde]) :-
  copyN(Ins,Ar,Insx,Cde,Rst),!,
  deleteUnused(F,Insx,Lbs,Rst).
deleteUnused(_,[I|Ins],Lbs,[I|Cde]) :-
  uncondJump(I),!,
  dropUntilLbl(true,Ins,Lbs,Cde).
deleteUnused(F,[I|Ins],Lbs,[I|Cde]) :-
  deleteUnused(F,Ins,Lbs,Cde).

copyN([],_,[],Cde,Cde).
copyN(Is,0,Is,Cde,Cde).
copyN([E|Is],Ix,Isx,[E|Cs],Cde) :-
  Ix1 is Ix-1,
  copyN(Is,Ix1,Isx,Cs,Cde).

uncondJump(iJmp(_)).
uncondJump(iRet).
uncondJump(iRetX).
uncondJump(iRtG).
uncondJump(iRetire).
uncondJump(iAbort).
uncondJump(iTCall(_)).
uncondJump(iTOCall(_)).

dropUntilLbl(_,[],_,[]).
dropUntilLbl(F,[iLbl(Lb)|Ins],Lbs,Cde) :-
  deleteUnused(F,[iLbl(Lb)|Ins],Lbs,Cde).
dropUntilLbl(true,[I|Ins],Lbs,Cde) :-
  addLblUsage(I,peephole:dropLbl,Lbs,Lb1),
  dropUntilLbl(true,Ins,Lb1,Cde).
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
peep([iStL(O),iLdL(O)|Ins],Map,Cde) :-
  peep([iTL(O)|Ins],Map,Cde).
peep([I|Ins],Map,[I|Cde]) :-
  peep(Ins,Map,Cde).

accessorPtn([iUnpack(Lb,Fl)|Ins],[iUnpack(Lb,Fl)|LdDrops]) :-
  dropSeq(Ins,[iStL(Off)|Ins1],LdDrops,[iRet|Inz]),
  dropSeq(Ins1,[iLdL(Off),iRet|Inz],_,_),!.
  
dropSeq([iDrop|Ins],Rest,[iDrop|Iz],Io) :-!,
  dropSeq(Ins,Rest,Iz,Io).
dropSeq(Ins,Ins,Dp,Dp).
