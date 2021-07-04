:- module(do,[genPerform/6,genRtn/7,genReturn/7,genIterableGl/4]).

:- use_module(abstract).
:- use_module(misc).
:- use_module(canon).
:- use_module(types).
:- use_module(cnc).
:- use_module(freevars).
:- use_module(errors).

/* Implement the monadic transformation of do expressions */

genAction(seqDo(_,A,B),Contract,Cont,Exp,Path) :-
	genAction(B,Contract,Cont,RR,Path),
	genAction(A,Contract,RR,Exp,Path).
genAction(valisDo(Lc,A),_Contract,Cont,valisDo(Lc,A),_) :-
  (Cont = noDo(_) ; reportError("valis: %s must be last action",[A],Lc)).
%  genReturn(Lc,A,ExTp,VlTp,ErTp,Contract,Exp).
/* X<-E --> _sequence(E,(X)=>Cont) */
genAction(varDo(Lc,Ptn,Gen),_Contract,Cont,Exp,Path) :-
  (Cont = noDo(_) ->
   reportError("bind: %s.=%s may not be last action",[Ptn,Gen],Lc);
   true),
  genActionSequence(varDo(Lc,Ptn,Gen),Cont,Path,Exp).

genAction(tryCatchDo(Lc,Bdy,Hndlr,ExTp,ValTp,ErTp),Contract,Cont,Exp,Path) :-
  typeOfCanon(Hndlr,HType),
  mkTypeExp(ExTp,[ErTp,ValTp],ConTp),
  genAction(Bdy,Contract,noDo(Lc),Body,Path),
  typeOfCanon(Body,BType),
  H = over(Lc,mtd(Lc,"_handle",funType(tplType([BType,HType]),ConTp)),
	      true,[conTract(Contract,[ExTp],[])]),
  HB = apply(Lc,H,tple(Lc,[Body,Hndlr]),ConTp),
  combineActs(Lc,HB,Cont,Contract,ExTp,ErTp,Exp).
genAction(throwDo(Lc,A,ExTp,VlTp,ErTp),Contract,_Cont,
	  apply(Lc,Gen,tple(Lc,[A]),Tp),_) :-
  mkTypeExp(ExTp,[ErTp,VlTp],Tp),		% monadic type of thrown value
  Gen = over(Lc,mtd(Lc,"_raise",funType(tplType([ErTp]),Tp)),
	     true,[conTract(Contract,[ExTp],[])]).
genAction(performDo(Lc,Ex,ExTp,VlTp,ErTp),Contract,Cont,Exp,_) :-!,
  genReturn(Lc,Ex,ExTp,VlTp,ErTp,Contract,Perf),
  combineActs(Lc,Perf,Cont,Contract,ExTp,ErTp,Exp).
genAction(simpleDo(Lc,SimpleExp,ExTp,ErTp),Contract,Cont,Exp,_) :-!,
  combineActs(Lc,SimpleExp,Cont,Contract,ExTp,ErTp,Exp).
genAction(ifThenDo(Lc,Ts,Th,El,ExTp,ValTp,ErTp),Contract,Cont,
	  Exp,Path) :-
  mkTypeExp(ExTp,[ErTp,ValTp],MTp),
  isIterableGoal(Ts),!,
  genAction(Th,Contract,noDo(Lc),Then,Path),
  genAction(El,Contract,noDo(Lc),Else,Path),
  genIterableGl(Ts,Contract,Path,Tst),
  combineActs(Lc,cond(Lc,Tst,Then,Else,MTp),Cont,Contract,ExTp,ErTp,Exp).
genAction(ifThenDo(Lc,Tst,Th,El,ExTp,ValTp,ErTp),Contract,Cont,Exp,Path) :-
  mkTypeExp(ExTp,[ErTp,ValTp],MTp),
  genAction(Th,Contract,noDo(Lc),Then,Path),
  genAction(El,Contract,noDo(Lc),Else,Path),
  combineActs(Lc,cond(Lc,Tst,Then,Else,MTp),Cont,Contract,ExTp,ErTp,Exp).

/* Construct a local iterator function:
   let{
     loop() => do{ if C then { B; loop() } else  <Cont> 
   } in loop()
*/
genAction(whileDo(Lc,Ts,Body,StTp,ErTp),Contract,Cont,Exp,Path) :-
  packageVarName(Path,"loop",LclName),
  genNewName(Path,"lp",ThPath),
  genstr("loop",Fn),
  Unit = tple(Lc,[]),
  UnitTp = tplType([]),
  mkTypeExp(StTp,[ErTp,UnitTp],LpTp),
  FnTp = funType(tplType([]),LpTp),
  genAction(seqDo(Lc,
		  Body,
		  simpleDo(Lc,apply(Lc,v(Lc,Fn,FnTp),Unit,LpTp),StTp,ErTp)),
	    Contract,noDo(Lc),Then,ThPath),
  FF=funDef(Lc,Fn,LclName,FnTp,[],
	    [equation(Lc,tple(Lc,[]),none,cond(Lc,Ts,Then,Cont,LpTp))]),
  Exp = letExp(Lc, [FF], apply(Lc,v(Lc,Fn,FnTp),tple(Lc,[]),LpTp)).

/* Construct a local iterator function for do .. until .. :
   let{
     loop() => do{ B; if C then loop() else <Cont> }
   } in loop()
*/
genAction(untilDo(Lc,Ts,Body,StTp,ErTp),Contract,Cont,Exp,Path) :-
  packageVarName(Path,"loop",LclName),
  genNewName(Path,"lp",ThPath),
  genstr("loop",Fn),
  UnitTp = tplType([]),
  Unit = tple(Lc,[]),
  mkTypeExp(StTp,[ErTp,UnitTp],LpTp),
  FnTp = funType(tplType([]),LpTp),
  genAction(seqDo(Lc,
		  Body,
		  ifThenDo(Lc,Ts,
			   simpleDo(Lc,apply(Lc,v(Lc,Fn,FnTp),Unit,LpTp),StTp,ErTp),
			   Cont,
			   StTp,UnitTp,ErTp)),
	    Contract,noDo(Lc),Then,ThPath),
  FF=funDef(Lc,Fn,LclName,FnTp,[],
	    [equation(Lc,tple(Lc,[]),none,Then)]),
  Exp = letExp(Lc, [FF], apply(Lc,v(Lc,Fn,FnTp),tple(Lc,[]),LpTp)).



/*
   for C do {A}
  becomes:
  
  <iterator>( do{return ()}, (Lcls,St) => do {A; return St})
*/
genAction(forDo(Lc,Tst,Body,StTp,ErTp),Contract,Cont,Exp,Path) :-
  Unit = tple(Lc,[]),
  UnitTp = tplType([]),
  genReturn(Lc,Unit,StTp,UnitTp,ErTp,Contract,Zed),
  genAction(Body,Contract,Zed,IterBody,Path),
  genCondition(Tst,Path,
	       do:genRtn(Lc,StTp,UnitTp,ErTp,Contract),
	       do:genSeq(Lc,Path,StTp,UnitTp,ErTp,Contract),
	       do:genForBody(Lc,StTp,UnitTp,ErTp,Contract,IterBody),
	       unlifted(Unit),ForLoop),
  combineActs(Lc,ForLoop,Cont,Contract,StTp,ErTp,Exp).
genAction(noDo(_),_,Cont,Cont,_).  

genAction(ifThenDo(Lc,Tst,Th,El,ExTp,ValTp,ErTp),Contract,Cont,Exp,Path) :-
  mkTypeExp(ExTp,[ErTp,ValTp],MTp),
  genAction(Th,Contract,noDo(Lc),Then,Path),
  genAction(El,Contract,noDo(Lc),Else,Path),
  combineActs(Lc,cond(Lc,Tst,Then,Else,MTp),Cont,Contract,ExTp,ErTp,Exp).
genAction(caseDo(Lc,Gov,Cases,ExTp,ErTp),Contract,Cont,Exp,Path) :-
%  reportMsg("gen case %s",[caseDo(Lc,Gov,Cases,ExTp,ErTp)],Lc),
  mkTypeExp(ExTp,[ErTp,ExTp],Tp),
  map(Cases,do:genCase(Contract,Cont,Path),Eqns),
  combineActs(Lc,case(Lc,Gov,Eqns,Tp),Cont,Contract,ExTp,ErTp,Exp).


genActionSequence(A1,noDo(_),_,A1) :-!.
genActionSequence(noDo(_),A2,_,A2) :-!.
genActionSequence(A1,A2,seqDo(Lc,A1,A2),_) :-
  locOfAst(A1,Lc).


genCase(Contract,Cont,Path,equation(Lc,Arg,Guard,Act),equation(Lc,Arg,Guard,Exp)) :-
  genAction(Act,Contract,Cont,Exp,Path).

genVl(Lc,Ptn,ExTp,ErTp,Contract,_,Exp) :-
  typeOfCanon(Ptn,VlTp),
  genReturn(Lc,Ptn,ExTp,VlTp,ErTp,Contract,Exp).

genUse(_Lc,_StTp,_ErTp,_Contract,Exp,_,Exp).

genUnit(Lc,ExTp,ErTp,Contract,RtnUnit) :-
  Unit = tple(Lc,[]),
  UnitTp = tplType([]),
  genReturn(Lc,Unit,ExTp,UnitTp,ErTp,Contract,RtnUnit).

genRtn(_Lc,_,_,_,_,lifted(Exp),Exp).
genRtn(Lc,ExTp,VlTp,ErTp,Contract,unlifted(St),Exp) :-
  genReturn(Lc,St,ExTp,VlTp,ErTp,Contract,Exp).

genReturn(Lc,A,ExTp,VlTp,ErTp,Contract,apply(Lc,Gen,tple(Lc,[A]),MTp)) :-
  mkTypeExp(ExTp,[ErTp,VlTp],MTp),		% monadic type of returned value
  Gen = over(Lc,mtd(Lc,"_valis",funType(tplType([VlTp]),MTp)),
	     true,[conTract(Contract,[ExTp],[])]).

genPerform(Lc,A,Tp,ExTp,Contract,apply(Lc,Perf,tple(Lc,[A]),Tp)) :-
  typeOfCanon(A,MdTp),!,
  Perf = over(Lc,mtd(Lc,"_perform",funType(tplType([MdTp]),Tp)),
	      true,[conTract(Contract,[ExTp],[])]).
%  reportMsg("perform -> %s",[Perf]).

genForBody(Lc,StTp,VlTp,ErTp,Contract,IterBody,St,Exp) :-
  genRtn(Lc,StTp,VlTp,ErTp,Contract,St,End),
  combineActs(Lc,IterBody,End,Contract,StTp,ErTp,Exp).
  %reportMsg("for body-> %s",[Exp]).

genSeq(Lc,Path,ExStTp,ErTp,Contract,St,Init,Reslt,Exp) :-
  typeOfCanon(St,ATp),
  mkTypeExp(ExStTp,[ErTp,ATp],MdTp),
  LTp = funType(tplType([ATp]),MdTp),
  Lam = lambda(Lc,LamLbl,equation(Lc,tple(Lc,[St]),none,Reslt),LTp),
  Gen = over(Lc,mtd(Lc,"_sequence",funType(tplType([MdTp,LTp]),MdTp)),
	     true,[conTract(Contract,[ExStTp],[])]),
  genRtn(Lc,ExStTp,ATp,ErTp,Contract,Init,Initial),
  Exp = apply(Lc,Gen,tple(Lc,[Initial,Lam]),MdTp),
  lambdaLbl(Path,"bind",LamLbl).

combineActs(Lc,noDo(_),noDo(_),Contract,ExTp,ErTp,RtnUnit) :-!,
  genUnit(Lc,ExTp,ErTp,Contract,RtnUnit).
combineActs(_,noDo(_),A1,_Contract,_,_,A1) :-!.
combineActs(_,A1,noDo(_),_Contract,_,_,A1) :-!.
combineActs(Lc,A1,Cont,Contract,StTp,_ErTp,Exp) :-
  typeOfCanon(Cont,ConTp),
  anonVar(Lc,Anon,ATp),
  LTp = funType(tplType([ATp]),ConTp),
  typeOfCanon(A1,A1Tp),
  Lam = lambda(Lc,equation(Lc,tple(Lc,[Anon]),none,Cont),LTp),
  Gen = over(Lc,mtd(Lc,"_sequence",funType(tplType([A1Tp,LTp]),ConTp)),
	     true,[conTract(Contract,[StTp],[])]),
  Exp = apply(Lc,Gen,tple(Lc,[A1,Lam]),ConTp).


/*
  * 'iterable' conditions become a match on the result of a search
  *
  * becomes:
  *
  * either(some(PtnV)) .= <genCondition>(C,either(none),...)
  *
  * This will need more optimization ...
*/

genIterableGl(Cond,Contract,Path,match(Lc,Ptn,Gl)) :-
  locOfCanon(Cond,Lc),
  EitherTp = tpFun("star.either*either",2),
  OptionTp = tpFun("star.core*option",1),

  goalVars(Cond,Vrs),
  VTpl = tple(Lc,Vrs),
  typeOfCanon(VTpl,VlTp),

  UnitTp = tplType([]),

  mkTypeExp(OptionTp,[VlTp],OptTp),
  mkTypeExp(EitherTp,[UnitTp,OptTp],MTp),

  Unit = apply(Lc,v(Lc,"either",funType(tplType([OptTp]),MTp)),
	       tple(Lc,[enm(Lc,"none",OptTp)]),MTp),

  genReturn(Lc,Unit,EitherTp,OptTp,UnitTp,Contract,Zed),

  Ptn = apply(Lc,v(Lc,"either",funType(tplType([OptTp]),MTp)),
	      tple(Lc,[apply(Lc,v(Lc,"some",funType(tplType([VlTp]),OptTp)),
			    tple(Lc,[VTpl]),OptTp)]),MTp),
  
  genCondition(Cond,Path,
	       do:genRtn(Lc,EitherTp,OptTp,UnitTp,Contract),
	       checker:genSeq(Lc,Path,Contract,EitherTp,UnitTp),
	       do:genVl(Lc,Ptn,EitherTp,UnitTp,Contract),
	       lifted(Zed),Seq),
  genPerform(Lc,Seq,MTp,EitherTp,Contract,Gl).
%  reportMsg("iterable goal %s ->\n%s",[Cond,match(Lc,Ptn,Gl)]).
