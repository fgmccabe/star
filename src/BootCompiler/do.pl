:- module(do,[genAction/6,genPerform/7,genRtn/6,genReturn/6,genIterableGl/7]).

:- use_module(misc).
:- use_module(canon).
:- use_module(types).
:- use_module(cnc).
:- use_module(freevars).
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
genAction(throwDo(Lc,A,StTp,ErTp),ConOp,Cont,Exp,_) :-
  Tp = tpExp(StTp,ErTp),		% monadic type of thrown value
  Gen = over(Lc,mtd(Lc,"_raise",funType(tupleType([ErTp]),Tp)),
	     true,[conTract(ConOp,[StTp],[ErTp])]),
  combineActs(Lc,apply(Lc,Gen,tple(Lc,[A]),Tp),Cont,ConOp,StTp,ErTp,Exp).
genAction(performDo(Lc,Ex,StTp,ErTp),ConOp,Cont,Exp,_) :-
  genReturn(Lc,Ex,StTp,ErTp,ConOp,Perf),
  combineActs(Lc,Perf,Cont,ConOp,StTp,ErTp,Exp).
genAction(simpleDo(Lc,Ex,StTp,ErTp),ConOp,Cont,Exp,_) :-
  combineActs(Lc,Ex,Cont,ConOp,StTp,ErTp,Exp).
genAction(ifThenDo(Lc,Ts,Th,El,StTp,ElTp,ErTp),ConOp,Cont,
	  cond(Lc,Tst,Then,Else,tpExp(StTp,ElTp)),Path) :-
  isIterableGoal(Ts),!,

  genAction(Th,ConOp,Cont,Then,Path),
  genAction(El,ConOp,Cont,Else,Path),

  genIterableGl(Ts,StTp,ErTp,ConOp,tpFun("star.core*option",1),Path,Tst).
genAction(ifThenDo(Lc,Ts,Th,El,StTp,ElTp,_),ConOp,Cont,
	  cond(Lc,Ts,Then,Else,tpExp(StTp,ElTp)),Path) :-
  genAction(Th,ConOp,Cont,Then,Path),
  genAction(El,ConOp,Cont,Else,Path).

/* Construct a local iterator function:
   let{
     loop() => do{ if C then { B; loop() } else  <Cont> 
   } in loop()
*/
genAction(whileDo(Lc,Ts,Body,StTp,ErTp),ConOp,Cont,Exp,Path) :-
  packageVarName(Path,"loop",LclName),
  genNewName(Path,"lp",ThPath),
  genstr("loop",Fn),
  genNewName(Path,"Γ",ThNm),
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


/*
   for C do {A}
  becomes:
  
  <iterator>( do{return ()}, (Lcls,St) => do {A; return St})
*/
genAction(forDo(Lc,Tst,Body,StTp,ErTp),ConOp,Cont,Exp,Path) :-
  Unit = tple(Lc,[]),
  genReturn(Lc,Unit,StTp,ErTp,ConOp,Zed),
  genAction(Body,ConOp,Zed,IterBody,Path),
  genCondition(Tst,Path,
	       do:genRtn(Lc,StTp,ErTp,ConOp),
	       do:genSeq(Lc,StTp,ErTp,ConOp),
	       do:genForBody(Lc,StTp,ErTp,ConOp,IterBody),
%	       do:genRtn(Lc,StTp,ErTp,ConOp),
	       unlifted(Unit),ForLoop),
  combineActs(Lc,ForLoop,Cont,ConOp,StTp,ErTp,Exp).
genAction(noDo(_),_,Cont,Cont,_).


/*
  * 'iterable' conditions become a match on the result of a search
  *
  * becomes:
  *
  * some(PtnV) .= <genCondition>(C,none,...)
*/

genIterableGl(Cond,ExTp,ErTp,ExOp,OptionTp,Path,match(Lc,Ptn,Gl)) :-
  locOfCanon(Cond,Lc),
  goalVars(Cond,Vrs),
  VTpl = tple(Lc,Vrs),
  typeOfCanon(VTpl,ETp),
  OptTp = tpExp(OptionTp,ETp),

  genReturn(Lc,enm(Lc,"none",OptTp),ExTp,ErTp,ExOp,Zed),

  Ptn = apply(Lc,v(Lc,"some",funType(tupleType([ETp]),OptTp)),
		   tple(Lc,[VTpl]),OptTp),
  
  genCondition(Cond,Path,
	       do:genRtn(Lc,ExTp,ErTp,ExOp),
	       checker:genSeq(Lc,ExOp,ExTp,ErTp),
	       do:genVl(Lc,Ptn,ExTp,ErTp,ExOp),
	       lifted(Zed),Seq),
  genPerform(Lc,Seq,OptTp,ExTp,ErTp,ExOp,Gl).
  %reportMsg("iterable goal %s ->\n%s",[Cond,match(Lc,Ptn,Gl)]).

genVl(Lc,Ptn,ExTp,ErTp,ExOp,_,Exp) :-
  genReturn(Lc,Ptn,ExTp,ErTp,ExOp,Exp).

genUse(_Lc,_StTp,_ErTp,_ConOp,Exp,_,Exp).

genRtn(_Lc,_,_,_,lifted(Exp),Exp).
genRtn(Lc,StTp,ErTp,ConOp,unlifted(St),Exp) :-
  genReturn(Lc,St,StTp,ErTp,ConOp,Exp).

genReturn(Lc,A,StTp,ErTp,ConOp,apply(Lc,Gen,tple(Lc,[A]),Tp)) :-
  typeOfCanon(A,ElTp),!,
  Tp = tpExp(StTp,ElTp),		% monadic type of returned value
  Gen = over(Lc,mtd(Lc,"_lift",funType(tupleType([ElTp]),Tp)),
	     true,[conTract(ConOp,[StTp],[ErTp])]).

genPerform(Lc,A,Tp,StTp,ErTp,ConOp,apply(Lc,Perf,tple(Lc,[A]),Tp)) :-
  typeOfCanon(A,MdTp),!,
  Perf = over(Lc,mtd(Lc,"_perform",funType(tupleType([MdTp]),Tp)),
	      true,[conTract(ConOp,[StTp],[ErTp])]).

genForBody(Lc,StTp,ErTp,ConOp,IterBody,St,Exp) :-
  genRtn(Lc,StTp,ErTp,ConOp,St,End),
  combineActs(Lc,IterBody,End,ConOp,StTp,ErTp,Exp).
  %reportMsg("for body-> %s",[Exp]).

genSeq(Lc,ExStTp,ErTp,ExOp,St,Init,Reslt,Exp) :-
  typeOfCanon(St,ATp),
  MdTp = tpExp(ExStTp,ATp),
  LTp = funType(tupleType([ATp]),MdTp),
  Lam = lambda(Lc,equation(Lc,tple(Lc,[St]),
			   enm(Lc,"true",type("star.core*boolean")),Reslt),LTp),
  Gen = over(Lc,mtd(Lc,"_sequence",funType(tupleType([MdTp,LTp]),MdTp)),
	     true,[conTract(ExOp,[ExStTp],[ErTp])]),
  genRtn(Lc,ExStTp,ErTp,ExOp,Init,Initial),
  Exp = apply(Lc,Gen,tple(Lc,[Initial,Lam]),MdTp).

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
