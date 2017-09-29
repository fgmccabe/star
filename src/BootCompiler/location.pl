:- module(location,[locOf/2, mergeLoc/3,showLocation/3,lcLine/2,lcColumn/2,lcSize/2,lcOff/2,isLocation/1]).

:- use_module(misc).

locOf(idTok(_,Lc),Lc).
locOf(idQTok(_,Lc),Lc).
locOf(integerTok(_,Lc),Lc).
locOf(floatTok(_,Lc),Lc).
locOf(stringTok(_,Lc),Lc).
locOf(terminal,missing).

mergeLoc(loc(Ln,LnOff,Co1,_),loc(_,_,Co2,Len),loc(Ln,LnOff,Co1,Len1)) :- Len1 is Co2-Co1+Len.

showLocation(loc(Ln,Col,_,Sz),O,E) :-
  appInt(Ln,O,O1),
  appStr(":",O1,O2),
  appInt(Col,O2,O3),
  appStr("(",O3,O4),
  appInt(Sz,O4,O5),
  appStr(")",O5,E).

lcLine(loc(Ln,_,_,_),Ln).
lcColumn(loc(_,Col,_,_),Col).
lcSize(loc(_,_,_,Sz),Sz).
lcOff(loc(_,_,Off,_),Off).

isLocation(loc(_,_,_,_)).
