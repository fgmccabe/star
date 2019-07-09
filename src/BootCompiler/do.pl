:- module(do,[genAction/5,genPerform/7,genRtn/6,genReturn/7,genIterableGl/7]).

:- use_module(misc).
:- use_module(canon).
:- use_module(types).
:- use_module(cnc).
:- use_module(freevars).
:- use_module(errors).

/* Implement the monadic transformation of do expressions */

genAction(Act,Contract,_StTp,_ErTp,Exp,Path) :-
  locOfCanon(Act,Lc),!,
  genAction(Act,Contract,noDo(Lc),Exp,Path).

genAction(seqDo(_,A,B),Contract,Cont,Exp,Path) :-
	genAction(B,Contract,Cont,RR,Path),
	genAction(A,Contract,RR,Exp,Path).
genAction(returnDo(Lc,A,ExTp,VlTp,ErTp),Contract,Cont,Exp,_) :-
  (Cont = noDo(_) ; reportError("return: %s must be last action",[A],Lc)),
  genReturn(Lc,A,ExTp,VlTp,ErTp,Contract,Exp).
/* X<-E --> _sequence(E,(X)=>Cont) */
genAction(bindDo(Lc,Ptn,Gen,ExTp),Contract,Cont,Exp,_) :-
  (Cont = noDo(_) ->
   reportError("bind: %s<-%s may not be last action",[Ptn,Gen],Lc);
   true),
  typeOfCanon(Cont,ConTp),
  typeOfCanon(Ptn,PtnTp),
  LTp = funType(tupleType([PtnTp]),ConTp),
  typeOfCanon(Gen,GenTp),
  Lam = lambda(Lc,equation(Lc,tple(Lc,[Ptn]),enm(Lc,"true",type("star.core*boolean")),Cont),LTp),
  Gen = over(Lc,mtd(Lc,"_sequence",funType(tupleType([GenTp,LTp]),ConTp)),
	     true,[conTract(Contract,[ExTp],[])]),
  Exp = apply(Lc,Gen,tple(Lc,[Gen,Lam]),ConTp).
/* X = E --> ((X)=>Cont)(E) */
genAction(varDo(Lc,Ptn,Ex),_Contract,Cont,Exp,_) :-
  (Cont = noDo(_) ->
   reportError("bind: %s=%s may not be last action",[Ptn,Ex],Lc);
   true),
  typeOfCanon(Ptn,PtnTp),
  typeOfCanon(Cont,ConTp),
  LTp = funType(tupleType([PtnTp]),ConTp),
  Lam = lambda(Lc,equation(Lc,tple(Lc,[Ptn]),enm(Lc,"true",type("star.core*boolean")),Cont),LTp),
  Exp = apply(Lc,Lam,tple(Lc,[Ex]),ConTp).

genAction(delayDo(Lc,Actn,ExTp,ValTp,ErTp),Contract,Cont,Exp,Path) :-
  Unit = tple(Lc,[]),
  genReturn(Lc,Unit,ExTp,ValTp,ErTp,Contract,RtnUnit),
  genAction(Actn,Contract,Cont,NAct,Path),
  combineActs(Lc,RtnUnit,NAct,Contract,ExTp,Exp).

genAction(tryCatchDo(Lc,Bdy,Hndlr,ExTp,ValTp,ErTp),Contract,Cont,Exp,Path) :-
  typeOfCanon(Hndlr,HType),
  ConTp = tpExp(StTp,ValTp),
  genAction(Bdy,Contract,noDo(Lc),Body,Path),
  typeOfCanon(Body,BType),

  H = over(Lc,mtd(Lc,"_handle",funType(tupleType([BType,HType]),ConTp)),
	      true,[conTract(Contract,[StTp],[ErTp])]),
  HB = apply(Lc,H,tple(Lc,[Body,Hndlr]),ConTp),
  combineActs(Lc,HB,Cont,Contract,ExTp,Exp).
genAction(throwDo(Lc,A,ExTp,VlTp,ErTp),Contract,Cont,Exp,_) :-
  mkTypeExp(ExTp,[ErTp,VlTp],Tp),		% monadic type of thrown value
  Gen = over(Lc,mtd(Lc,"_raise",funType(tupleType([ErTp]),Tp)),
	     true,[conTract(Contract,[ExTp],[])]),
  combineActs(Lc,apply(Lc,Gen,tple(Lc,[A]),Tp),Cont,Contract,ExTp,Exp).
genAction(performDo(Lc,Ex,ExTp,VlTp,ErTp),Contract,Cont,Exp,_) :-
  genReturn(Lc,Ex,ExTp,VlTp,ErTp,Contract,Perf),
  combineActs(Lc,Perf,Cont,Contract,ExTp,Exp).
genAction(simpleDo(Lc,SimpleExp,ExTp),Contract,Cont,Exp,_) :-
  combineActs(Lc,SimpleExp,Cont,Contract,ExTp,Exp).
genAction(ifThenDo(Lc,Ts,Th,El,StTp,ValTp,ErTp),Contract,Cont,
	  cond(Lc,Tst,Then,Else,MTp),Path) :-
  mkTypeExp(StTp,[ErTp,ValTp],MTp),
  isIterableGoal(Ts),!,

  genAction(Th,Contract,Cont,Then,Path),
  genAction(El,Contract,Cont,Else,Path),

  genIterableGl(Ts,StTp,ValTp,ErTp,Contract,tpFun("star.core*option",1),Path,Tst).
genAction(ifThenDo(Lc,Ts,Th,El,StTp,ValTp,ErTp),Contract,Cont,
	  cond(Lc,Ts,Then,Else,MTp),Path) :-
  mkTypeExp(StTp,[ErTp,ValTp],MTp),
  genAction(Th,Contract,Cont,Then,Path),
  genAction(El,Contract,Cont,Else,Path).

/* Construct a local iterator function:
   let{
     loop() => do{ if C then { B; loop() } else  <Cont> 
   } in loop()
*/
genAction(whileDo(Lc,Ts,Body,StTp,ErTp),Contract,Cont,Exp,Path) :-
  packageVarName(Path,"loop",LclName),
  genNewName(Path,"lp",ThPath),
  genstr("loop",Fn),
  genNewName(Path,"Γ",ThNm),
  UnitTp = tupleType([]),
  mkTypeExp(StTp,[ErTp,UnitTp],LpTp),
  FnTp = funType(tupleType([]),LpTp),
  genAction(seqDo(Lc,
		  Body,
		  simpleDo(Lc,apply(Lc,v(Lc,Fn,FnTp),tple(Lc,[]),LpTp),StTp)),
	    Contract,noDo(Lc),Then,ThPath),
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
genAction(forDo(Lc,Tst,Body,StTp,ErTp),Contract,Cont,Exp,Path) :-
  Unit = tple(Lc,[]),
  genReturn(Lc,Unit,StTp,ErTp,Contract,Zed),
  genAction(Body,Contract,Zed,IterBody,Path),
  genCondition(Tst,Path,
	       do:genRtn(Lc,StTp,ErTp,Contract),
	       do:genSeq(Lc,StTp,ErTp,Contract),
	       do:genForBody(Lc,StTp,ErTp,Contract,IterBody),
%	       do:genRtn(Lc,StTp,ErTp,Contract),
	       unlifted(Unit),ForLoop),
  combineActs(Lc,ForLoop,Cont,Contract,StTp,Exp).
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

genUse(_Lc,_StTp,_ErTp,_Contract,Exp,_,Exp).

genRtn(_Lc,_,_,_,lifted(Exp),Exp).
genRtn(Lc,StTp,ErTp,Contract,unlifted(St),Exp) :-
  genReturn(Lc,St,StTp,ErTp,Contract,Exp).

genReturn(Lc,A,ExTp,VlTp,ErTp,Contract,apply(Lc,Gen,tple(Lc,[A]),MTp)) :-
  mkTypeExp(ExTp,[ErTp,VlTp],MTp),		% monadic type of returned value
  Gen = over(Lc,mtd(Lc,"_valis",funType(tupleType([VlTp]),MTp)),
	     true,[conTract(Contract,[ExTp],[])]).

genPerform(Lc,A,Tp,StTp,ErTp,Contract,apply(Lc,Perf,tple(Lc,[A]),Tp)) :-
  typeOfCanon(A,MdTp),!,
  Perf = over(Lc,mtd(Lc,"_perform",funType(tupleType([MdTp]),Tp)),
	      true,[conTract(Contract,[StTp],[ErTp])]).

genForBody(Lc,StTp,ErTp,Contract,IterBody,St,Exp) :-
  genRtn(Lc,StTp,ErTp,Contract,St,End),
  combineActs(Lc,IterBody,End,Contract,StTp,Exp).
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

combineActs(_,A1,noDo(_),_Contract,_,A1) :-!.
combineActs(Lc,A1,Cont,Contract,StTp,Exp) :-
  typeOfCanon(Cont,ConTp),
  anonVar(Lc,Anon,ATp),
  LTp = funType(tupleType([ATp]),ConTp),
  typeOfCanon(A1,A1Tp),
  Lam = lambda(Lc,equation(Lc,tple(Lc,[Anon]),
			   enm(Lc,"true",type("star.core*boolean")),Cont),LTp),
  Gen = over(Lc,mtd(Lc,"_sequence",funType(tupleType([A1Tp,LTp]),ConTp)),
	     true,[conTract(Contract,[StTp],[])]),
  Exp = apply(Lc,Gen,tple(Lc,[A1,Lam]),ConTp).



