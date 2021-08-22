:- module(location,[locOf/2, mergeLoc/3,
		    showLocation/3,
		    ssLoc/2,
		    lcPk/2,lcLine/2,lcColumn/2,lcSize/2,lcOff/2,isLocation/1,locHash/2,
		   pkgLoc/2]).

:- use_module(misc).

locOf(idTok(_,Lc),Lc).
locOf(idQTok(_,Lc),Lc).
locOf(integerTok(_,Lc),Lc).
locOf(floatTok(_,Lc),Lc).
locOf(stringTok(_,Lc),Lc).
locOf(terminal,missing).

mergeLoc(Lc1,Lc2,Lc1) :- var(Lc2),!.
mergeLoc(Lc1,Lc2,Lc2) :- var(Lc1),!.
mergeLoc(loc(Pk,Ln,Pos,Co1,_),loc(_,_,_,Co2,Len),loc(Pk,Ln,Pos,Co1,Len1)) :- Len1 is Co2-Co1+Len.

showLocation(loc(Pk,Ln,Col,Pos,Sz),O,E) :-
  appStr(Pk,O,O0),
  appStr("[",O0,O1),
  appInt(Ln,O1,O2),
  appStr(":",O2,O3),
  appInt(Col,O3,O4),
  appStr("@",O4,O5),
  appInt(Pos,O5,O6),
  appStr("-",O6,O7),
  appInt(Sz,O7,O8),
  appStr("]",O8,E).
showLocation(missing,O,Ox) :-
  appStr("unknown location",O,Ox).


ssLoc(loc(Pk,Ln,Col,Pos,Sz),
      sq([ss(Pk),ss("["),ix(Ln),ss(":"),ix(Col),ss("@"),
	  ix(Pos),ss("-"),ix(Sz),ss("]")])).
ssLoc(missing,ss("unknown location")).

lcPk(loc(Pk,_,_,_,_),Pk).
lcLine(loc(_,Ln,_,_,_),Ln).
lcColumn(loc(_,_,Col,_,_),Col).
lcSize(loc(_,_,_,_,Sz),Sz).
lcOff(loc(_,_,_,Off,_),Off).

isLocation(loc(_,_,_,_,_)).

pkgLoc(pkg(Pk,_),loc(Pk,0,0,0,0)).

locHash(loc(Pk,_,_,S,L),H) :-
  H0 is S*37+L,
  hashSixtyFour(H0,H1),
  stringHash(H1,Pk,H).
