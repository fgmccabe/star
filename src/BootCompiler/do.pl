:- module(do,[genAction/6]).

:- use_module(misc).
:- use_module(canon).
:- use_module(types).
:- use_module(cnc).
:- use_module(errors).

/* Implement the monadic transformation of do expressions */

genAction(Act,ConOp,_StTp,_ErTp,Exp,Path) :-
  locOfCanon(Act,Lc),!,
  genAction(Act,ConOp,noDo(Lc),Exp,Path).

genAction(seqDo(_,A,B),ConOp,Cont,Exp,Path) :-
	genAction(B,ConOp,Cont,RR,Path),
	genAction(A,ConOp,RR,Exp,Path).
genAction(returnDo(Lc,A,StTp,ErTp),ConOp,Cont,Exp,_Path) :-
  (Cont = noDo(_) ; reportError("return: %s must be last action",[A],Lc)),
  genReturn(Lc,A,StTp,ErTp,ConOp,Exp).
genAction(bindDo(Lc,Ptn,Ex,PtnTp,StTp,ErTp),ConOp,Cont,Exp,_) :-
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
/* X = E --> ((X)=>Cont)(E) */
genAction(varDo(Lc,Ptn,Ex),_ConOp,Cont,Exp,_) :-
  (Cont = noDo(_) ->
   reportError("bind: %s=%s may not be last action",[Ptn,Ex],Lc);
   true),
  typeOfCanon(Ptn,PtnTp),
  typeOfCanon(Cont,ConTp),
  LTp = funType(tupleType([PtnTp]),ConTp),
  Lam = lambda(Lc,equation(Lc,tple(Lc,[Ptn]),enm(Lc,"true",type("star.core*boolean")),Cont),LTp),
  Exp = apply(Lc,Lam,tple(Lc,[Ex]),ConTp).

genAction(delayDo(Lc,Actn,StTp,ErTp),ConOp,Cont,Exp,Path) :-
  Unit = tple(Lc,[]),
  genReturn(Lc,Unit,StTp,ErTp,ConOp,RtnUnit),
  genAction(Actn,ConOp,Cont,NAct,Path),
  combineActs(Lc,RtnUnit,NAct,ConOp,StTp,ErTp,Exp).

genAction(assignDo(Lc,Lhs,Rhs,StTp,ErTp),ConOp,Cont,Exp,_) :-
  typeOfCanon(Lhs,LTp),
  typeOfCanon(Rhs,RTp),
  UnitTp = tupleType([]),
  Tp = tpExp(StTp,UnitTp),
  Assgn = apply(Lc,v(Lc,":=",funType(tupleType([LTp,RTp]),Tp)),
		tple(Lc,[Lhs,Rhs]),Tp),
  combineActs(Lc,Assgn,Cont,ConOp,StTp,ErTp,Exp).
genAction(tryCatchDo(Lc,Bdy,Hndlr,StTp,ElTp,ErTp),ConOp,Cont,Exp,Path) :-
  typeOfCanon(Hndlr,HType),
  ConTp = tpExp(StTp,ElTp),
  genAction(Bdy,ConOp,noDo(Lc),Body,Path),
  typeOfCanon(Body,BType),

  H = over(Lc,mtd(Lc,"_handle",funType(tupleType([BType,HType]),ConTp)),
	      true,[conTract(ConOp,[StTp],[ErTp])]),
  HB = apply(Lc,H,tple(Lc,[Body,Hndlr]),ConTp),
  combineActs(Lc,HB,Cont,ConOp,StTp,ErTp,Exp).
genAction(throwDo(Lc,A,StTp,ErTp),ConOp,Cont,apply(Lc,Gen,tple(Lc,[A]),Tp),_) :-
  (Cont = noDo(_) ; reportError("throw %s must be last action",[A],Lc)),
  Tp = tpExp(StTp,ErTp),		% monadic type of thrown value
  Gen = over(Lc,mtd(Lc,"_raise",funType(tupleType([ErTp]),Tp)),
    true,[conTract(ConOp,[StTp],[ErTp])]).
genAction(performDo(Lc,Ex,StTp,ErTp),ConOp,Cont,Exp,_) :-
  genReturn(Lc,Ex,StTp,ErTp,ConOp,Perf),
  combineActs(Lc,Perf,Cont,ConOp,StTp,ErTp,Exp).
genAction(simpleDo(Lc,Ex,StTp,ErTp),ConOp,Cont,Exp,_) :-
  combineActs(Lc,Ex,Cont,ConOp,StTp,ErTp,Exp).
genAction(ifThenDo(Lc,Ts,Th,El,StTp,ElTp),ConOp,Cont,
	  cond(Lc,Ts,Then,Else,tpExp(StTp,ElTp)),Path) :-
  genAction(Th,ConOp,Cont,Then,Path),
  genAction(El,ConOp,Cont,Else,Path).

/* Construct a local iterator function:
   let{
     loop() => do{ if C then { B; loop() } else  <Cont> 
   } in loop()
*/
genAction(whileDo(Lc,Ts,_Lcls,Body,StTp,ErTp),ConOp,Cont,Exp,Path) :-
  packageVarName(Path,"loop",LclName),
  thetaName(Path,"lp",ThPath),
  genstr("loop",Fn),
  genstr("Γ",ThNm),
  UnitTp = tupleType([]),
  LpTp = tpExp(StTp,UnitTp),
  FnTp = funType(tupleType([]),LpTp),
  genAction(seqDo(Lc,
		  Body,
		  simpleDo(Lc,apply(Lc,v(Lc,Fn,FnTp),tple(Lc,[]),LpTp),
			   StTp,ErTp)),
	    ConOp,noDo(Lc),Then,ThPath),
  FF=funDef(Lc,Fn,LclName,FnTp,[],
	    [equation(Lc,tple(Lc,[]),
		      enm(Lc,"true",type("star.core*boolean")),
		      cond(Lc,Ts,Then,Cont,LpTp))]),
  Exp = letExp(Lc,
	      theta(Lc,ThNm,true,[FF],[],[],faceType([],[])),
	       apply(Lc,v(Lc,Fn,FnTp),tple(Lc,[]),LpTp)).

genReturn(Lc,A,StTp,ErTp,ConOp,apply(Lc,Gen,tple(Lc,[A]),Tp)) :-
  typeOfCanon(A,ElTp),!,
  Tp = tpExp(StTp,ElTp),		% monadic type of returned value
  Gen = over(Lc,mtd(Lc,"_return",funType(tupleType([ElTp]),Tp)),
    true,[conTract(ConOp,[StTp],[ErTp])]).

%invokeCont(Cont,St,v(Lc,"_",voidType)).

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

mkDelay(Lc,Act,StTp,ErTp,seqDo(Lc,returnDo(Lc,tple(Lc,[]),StTp,ErTp),Act)).
