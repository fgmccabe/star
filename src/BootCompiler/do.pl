:- module(do,[genAction/4]).

:- use_module(misc).
:- use_module(canon).
:- use_module(types).
:- use_module(errors).

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
genAction(assignDo(Lc,Lhs,Rhs,StTp,ErTp),ConOp,Cont,Exp) :-
  newTypeVar("_",TpV),
  typeOfCanon(Lhs,LTp),
  typeOfCanon(Rhs,RTp),
  combineActs(apply(Lc,v(Lc,"_assign",funType(tupleType([LTp,RTp]),RTp)),
		    tple(Lc,[Lhs,Rhs]),TpV),
	      Cont,ConOp,StTp,ErTp,Exp).
genAction(tryCatchDo(Lc,Bdy,Hndlr,StTp,ElTp,ErTp),ConOp,Cont,Exp) :-
  typeOfCanon(Hndlr,HType),
  ConTp = tpExp(StTp,ElTp),
  genAction(Bdy,ConOp,noDo(Lc),Body),
  typeOfCanon(Body,BType),

  H = over(Lc,mtd(Lc,"_handle",funType(tupleType([BType,HType]),ConTp)),
	      true,[conTract(ConOp,[StTp],[ErTp])]),
  HB = apply(Lc,H,tple(Lc,[Body,Hndlr]),ConTp),
  combineActs(Lc,HB,Cont,ConOp,StTp,ErTp,Exp).
genAction(throwDo(Lc,A,StTp,ErTp),ConOp,Cont,apply(Lc,Gen,tple(Lc,[A]),Tp)) :-
  (Cont = noDo(_) ; reportError("throw %s must be last action",[A],Lc)),
  Tp = tpExp(StTp,ErTp),		% monadic type of thrown value
  Gen = over(Lc,mtd(Lc,"_raise",funType(tupleType([ErTp]),Tp)),
    true,[conTract(ConOp,[StTp],[ErTp])]).
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
