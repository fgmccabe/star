:- module(do,[genAction/4]).

:- use_module(misc).
:- use_module(canon).
:- use_module(types).

% Implement the monadic transformation of do expressions

genAction(seqDo(_,A,B),ConOp,Cont,Exp) :-
	genAction(B,ConOp,Cont,RR),
	genAction(A,ConOp,RR,Exp).
genAction(returnDo(Lc,A,StTp,ErTp),ConOp,Cont,apply(Lc,Gen,tple(Lc,[A]),Tp)) :-
  (Cont = noDo(_) ; reportError("return: %s must be last action",[A],Lc)),
  typeOfCanon(A,ElTp),!,
  Tp = tpExp(StTp,ElTp),		% monadic type of returned value
  Gen = over(Lc,mtd(Lc,"_return",funType(tupleType([ElTp]),Tp)),
    true,[conTract(ConOp,[StTp],[ErTp])]).
genAction(bindDo(Lc,Ptn,Ex,PtnTp,StTp,ErTp),ConOp,Cont,Exp) :-
  (Cont = noDo(_) ->
   reportError("bind: %s<-%s may not be last action",[Ptn,Ex],Lc);
   true),
  typeOfCanon(Cont,ConTp),
  LTp = funType(tupleType([PtnTp]),ConTp),
  typeOfCanon(Ex,ExTp),
  Lam = lambda(Lc,equation(Lc,tple(Lc,[Ptn]),enm(Lc,"true",type("star.core*boolean")),Cont),LTp),
  Gen = over(Lc,mtd(Lc,"_sequence",funType(tupleType([ExTp,LTp]),ConTp)),
	     true,[conTract(ConOp,[StTp],[ErTp])]),
  Exp = apply(Lc,Gen,tple(Lc,[Ex,Lam]),ConTp).
genAction(varDo(Lc,Ptn,Ex),_ConOp,Cont,Exp) :-
  (Cont = noDo(_) ->
   reportError("bind: %s=%s may not be last action",[Ptn,Ex],Lc);
   true),
  typeOfCanon(Ptn,PtnTp),
  typeOfCanon(Cont,ConTp),
  LTp = funType(tupleType([PtnTp]),ConTp),
  Lam = lambda(Lc,equation(Lc,tple(Lc,[Ptn]),enm(Lc,"true",type("star.core*boolean")),Cont),LTp),
  Exp = apply(Lc,Lam,tple(Lc,[Ex]),ConTp).
genAction(tryCatchDo(Lc,Bdy,Hndlr,StTp,ErTp),ConOp,Cont,Exp) :-
  typeOfCanon(Cont,ConTp),
  typeOfCanon(Hndlr,HType),
  typeOfCanon(Bdy,BType),
  genAction(Bdy,ConOp,noDo(Lc),Body),
  
  H = over(Lc,mtd(Lc,"_handle",funType(tupleType([BType,HType]),ConTp)),
	      true,[conTract(ConOp,[StTp],[ErTp])]),
  HB = apply(Lc,H,tple(Lc,[Body,Hndlr]),ConTp),
  combineActs(Lc,HB,Cont,ConOp,StTp,ErTp,Exp).
genAction(performDo(Lc,Ex,StTp,ErTp),ConOp,Cont,Exp) :-
  combineActs(Lc,Ex,Cont,ConOp,StTp,ErTp,Exp).

combineActs(_,A1,noDo(_),_ConOp,_,_,A1) :-!.
combineActs(Lc,A1,Cont,ConOp,StTp,ErTp,Exp) :-
  typeOfCanon(Cont,ConTp),
  anonVar(Lc,Anon,ATp),
  LTp = funType(tupleType([ATp]),ConTp),
  typeOfCanon(A1,A1Tp),
  Lam = lambda(Lc,equation(Lc,tple(Lc,[Anon]),
			   enm(Lc,"true",type("star.core*boolean")),Cont),LTp),
  Gen = over(Lc,mtd(Lc,"_sequence",funType(tupleType([A1Tp,LTp]),ConTp)),
	     true,[conTract(ConOp,[StTp],[ErTp])]),
  Exp = apply(Lc,Gen,tple(Lc,[A1,Lam]),ConTp).

