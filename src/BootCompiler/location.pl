:- module(location,[locOf/2, mergeLoc/3,showLocation/3,lcLine/2,lcColumn/2,lcSize/2,lcOff/2,isLocation/1]).

:- use_module(misc).

locOf(idTok(_,Lc),Lc).
locOf(idQTok(_,Lc),Lc).
locOf(integerTok(_,Lc),Lc).
locOf(floatTok(_,Lc),Lc).
locOf(stringTok(_,Lc),Lc).
locOf(terminal,missing).

mergeLoc(Lc1,Lc2,Lc1) :- var(Lc2),!.
mergeLoc(Lc1,Lc2,Lc2) :- var(Lc1),!.
mergeLoc(loc(Pk,Ln,LnOff,Co1,_),loc(_,_,_,Co2,Len),loc(Pk,Ln,LnOff,Co1,Len1)) :- Len1 is Co2-Co1+Len.

showLocation(loc(Pk,Ln,Col,Pos,Sz),O,E) :-
  appStr(Pk,O,O0),
  appStr("[",O0,O1),
  appInt(Pos,O1,O2),
  appStr(":",O2,O3),
  appInt(Sz,O3,O4),
  appStr("]",O4,O5),
  appInt(Ln,O5,O6),
  appStr(":",O6,O7),
  appInt(Col,O7,E).

lcPk(loc(Pk,_,_,_,_),Pk).
lcLine(loc(_,Ln,_,_,_),Ln).
lcColumn(loc(_,_,Col,_,_),Col).
lcSize(loc(_,_,_,_,Sz),Sz).
lcOff(loc(_,_,_,Off,_),Off).

isLocation(loc(_,_,_,_,_)).
