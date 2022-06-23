:- module(makeAction,[makeAction/3]).

:- use_module(abstract).
:- use_module(wff).
:- use_module(misc).
:- use_module(errors).
:- use_module(astdisp).
:- use_module(location).

/* makeAction(A,Cont,Rp) */
makeAction(A,Cont,Ax) :-
  isBraceTuple(A,_,[E]),!,
  makeAction(E,Cont,Ax).
makeAction(A,some((_,Cont)),Cont) :-
  isIden(A,_,"nothing"),!.
makeAction(A,none,Ac) :-
  isIden(A,Lc,"nothing"),!,
  unitTpl(Lc,U),
  makeReturn(Lc,U,Ac).
makeAction(A,some((_,Cont)),Cont) :-
  isBraceTuple(A,_,[]),!.
makeAction(A,none,Ac) :-
  isBraceTuple(A,Lc,[]),!,
  unitTpl(Lc,U),
  makeReturn(Lc,U,Ac).
makeAction(A,Cont,Ac) :-
  isActionSeq(A,Lc,L,R),!,
  makeAction(R,Cont,Rx),
  makeAction(L,some((Lc,Rx)),Ac).
makeAction(A,Cont,Ac) :-
  isActionSeq(A,_,L),!,
  makeAction(L,Cont,Ac).
makeAction(A,none,Ac) :-
  isValis(A,Lc,E),!,
  makeReturn(Lc,E,Ac).
makeAction(A,some((OLc,_)),Ac) :-
  isValis(A,Lc,E),
  reportError("%s must be last action, previous action at %s",[ast(A),loc(OLc)],Lc),
  makeReturn(Lc,E,Ac).
makeAction(A,_,Ac) :-
  isRaise(A,Lc,E),!,
  makeThrow(Lc,E,Ac).
makeAction(A,none,Ac) :-
  isBind(A,Lc,L,R),!,
  reportError("%s may not be last action",[ast(A)],Lc),
  unitTpl(Lc,U),
  makeSequence(Lc,R,L,U,Ac).
makeAction(A,some((_,Cont)),Ac) :-
  isBind(A,Lc,L,R),!,
  makeSequence(Lc,R,L,Cont,Ac).
makeAction(A,none,Ac) :-
  isMatch(A,Lc,L,R),
  isAnon(L,_),!,
  roundTuple(Lc,[L],Arg),
  unitTpl(Lc,U),
  makeReturn(Lc,U,Cont),
  mkEquation(Lc,Arg,none,Cont,Lam),
  roundTerm(Lc,Lam,[R],Ac).
makeAction(A,none,Ac) :-
  isMatch(A,Lc,L,R),!,
  reportError("%s may not be last action",[ast(A)],Lc),
  roundTuple(Lc,[L],Arg),
  mkEquation(Lc,Arg,none,Arg,Lam),
  roundTerm(Lc,Lam,[R],Ac).
makeAction(A,some((CLc,Cont)),Ac) :-
  isMatch(A,Lc,L,R),!,
  roundTuple(Lc,[L],Arg),
  mkEquation(CLc,Arg,none,Cont,Lam),
  roundTerm(Lc,Lam,[R],Ac).
makeAction(A,none,Ac) :-
  isOptionMatch(A,Lc,L,R),!,
  reportError("%s may not be last action",[ast(A)],Lc),
  roundTuple(Lc,[L],Arg),
  mkEquation(Lc,Arg,none,Arg,Lam),
  roundTerm(Lc,Lam,[R],Ac).
makeAction(A,some((CLc,Cont)),Ac) :-
  isOptionMatch(A,Lc,L,R),!,
  unary(Lc,"some",L,OL),
  roundTuple(Lc,[OL],Arg),
  mkEquation(CLc,Arg,none,Cont,Lam),
  roundTerm(Lc,Lam,[R],Ac).
/*
   A[F:T] := R
  becomes
  A := _splice(A!,F,T,R)
*/

makeAction(A,Cont,Ac) :-
  isSplice(A,Lc,S,F,T,R),!,
  unary(Lc,"!",S,Src),
  nary(Lc,"_splice",[Src,F,T,R],Rep),
  assignment(Lc,S,Rep,As),
  combine(As,Cont,Ac).
makeAction(A,Cont,Ac) :-
  isTryCatch(A,Lc,B,H),!,
  makeAction(B,none,Bx),
  makeHandler(H,Hx),
  binary(Lc,"_catch",Bx,Hx,HH),
  combine(HH,Cont,Ac).
makeAction(A,Cont,Ac) :-
  isIfThenElse(A,Lc,T,L,R),!,
  makeAction(L,none,Th),
  makeAction(R,none,El),
  conditional(Lc,T,Th,El,Cc),
  combine(Cc,Cont,Ac).
makeAction(A,Cont,Ac) :-
  isIfThen(A,Lc,T,L),!,
  makeAction(L,none,Th),
  unitTpl(Lc,U),
  makeReturn(Lc,U,El),
  conditional(Lc,T,Th,El,Cc),
  combine(Cc,Cont,Ac).
/* Construct a local loop function for while:
let{.
  loop() => do{ if T then { B; loop() }}
.} in loop()
*/
makeAction(A,Cont,Ac) :-
  isWhileDo(A,Lc,T,B),!,
  genstr("loop",Lp),
  mkZeroary(Lc,Lp,FnCall),
  mkActionSeq(Lc,B,FnCall,BB),
  mkIfThen(Lc,T,BB,Bx),
  makeAction(Bx,none,Bd),
  mkEquation(Lc,FnCall,none,Bd,Eqn),
  mkLetRec(Lc,[Eqn],FnCall,Lx),
  combine(Lx,Cont,Ac).
/* Construct a local loop function for until:
let{.
  loop() => do{ B; if T then loop()}
.} in loop()
*/
makeAction(A,Cont,Ac) :-
  isUntilDo(A,Lc,B,T),!,
  genstr("loop",Lp),
  mkZeroary(Lc,Lp,FnCall),
  mkIfThen(Lc,T,FnCall,Lpx),
  mkActionSeq(Lc,B,Lpx,Bx),
  makeAction(Bx,none,Bd),
  mkEquation(Lc,FnCall,none,Bd,Eqn),
  mkLetRec(Lc,[Eqn],FnCall,Lx),
  combine(Lx,Cont,Ac).
/*
  for X in L do Act
  becomes
  _iter(L,return()),let{
    lP(X,St) => _sequence(St,(X)=>_sequence(Act,(_)=>_valis(())))
    lP(_,St) => St

  } in lP)
*/
makeAction(A,Cont,Ax) :-
  isForDo(A,Lc,X,L,Act),!,
  unitTpl(Lc,U),
  genIden(Lc,Lp),
  mkAnon(Lc,Anon),
  genIden(Lc,St),
  makeReturn(Lc,U,UA),
  mkSequence(Lc,Act,UA,As),
  makeAction(As,none,XA),
%  reportMsg("body action %s",[ast(XA)],Lc),
  mkEquation(Lc,tuple(Lc,"()",[Anon]),none,XA,L1),
  binary(Lc,"_sequence",St,L1,S1),
  buildEquation(Lc,Lp,[X,St],none,S1,Lp1),
  buildEquation(Lc,Lp,[Anon,St],none,St,Lp2),
  mkLetDef(Lc,[Lp1,Lp2],Lp,IFn),
  ternary(Lc,"_iter",L,UA,IFn,IA),
%  reportMsg("for loop %s becomes %s",[ast(A),ast(IA)],Lc),
  combine(IA,Cont,Ax).
makeAction(A,Cont,Ac) :-
  isIntegrity(A,Lc,Tst),!,
  makeAssert(Lc,Tst,Assrt),
  makeAction(Assrt,Cont,Ac).
makeAction(A,Cont,Ac) :-
  isShow(A,Lc,E),
  makeShow(Lc,E,Sh),
  makeAction(Sh,Cont,Ac).
/*
  perform A
  becomes
  _ <- A
  */
makeAction(A,Cont,Ac) :-
  isPerform(A,_Lc,AA),!,
  combine(AA,Cont,Ac).
makeAction(A,Cont,Ac) :-
  isLetDef(A,Lc,D,B),!,
  makeAction(B,Cont,Bc),
  mkLetDef(Lc,D,Bc,Ac).
makeAction(A,Cont,Ac) :-
  isLetRec(A,Lc,D,B),!,
  makeAction(B,Cont,Bc),
  mkLetRec(Lc,D,Bc,Ac).
makeAction(A,Cont,Ax) :-
  isCaseExp(A,Lc,G,Cs),!,
  map(Cs,macroRules:mkCase(Cont),Csx),
  caseExp(Lc,G,Csx,Ax).
makeAction(A,Cont,Ac) :-
  isRoundTerm(A,_,_),!,
  combine(A,Cont,Ac).
  
makeAction(A,_,A) :-
  locOfAst(A,Lc),
  reportError("%s is not a valid action",[ast(A)],Lc).

mkCase(Cont,A,Ax) :-
  isEquation(A,Lc,P,G,V),!,
  makeAction(V,Cont,VV),
  mkEquation(Lc,P,G,VV,Ax).
  
makeHandler(H,Hx) :-
  isBraceTuple(H,Lc,[A]),!,
  mkAnon(Lc,Anon),
  roundTuple(Lc,[Anon],Arg),
  makeAction(A,none,Ah),
  mkEquation(Lc,Arg,none,Ah,Hx).
makeHandler(H,H).

tryCatchMacro(T,action,Tx) :-
  isTryCatch(T,Lc,B,H),
  isBraceTuple(H,LLc,_),!,
  mkAnon(LLc,A),
  roundTuple(Lc,[A],Arg),
  unary(LLc,"do",H,HA),
  mkEquation(Lc,Arg,none,HA,Hx),
  mkTryCatch(Lc,B,Hx,Tx).

tryHandleMacro(T,action,Tx) :-
  isTryHandle(T,Lc,B,H),
  isBraceTuple(H,LLc,_),!,
  mkAnon(LLc,A),
  isName(Cont,LLc,"$k"),
  roundTuple(Lc,[A,Cont],Arg),
  unary(LLc,"do",H,HA),
  mkEquation(Lc,Arg,none,HA,Hx),
  mkTryHandle(Lc,B,Hx,Tx).

makeReturn(Lc,E,Ex) :-
  unary(Lc,"_valis",E,Ex).

makeThrow(Lc,E,Ex) :-
  unary(Lc,"_raise",E,Ex).

% bind X to A, evaluate Cnt
makeSequence(Lc,A,X,Cnt,Ex) :-
  roundTuple(Lc,[X],Arg),
  mkEquation(Lc,Arg,none,Cnt,Lam),
  binary(Lc,"_sequence",A,Lam,Ex).

combine(A,none,A) :-!.
combine(A,some((Lc,Cont)),Ex) :-
  mkAnon(Lc,Anon),
  makeSequence(Lc,A,Anon,Cont,Ex).

doMacro(A,expression,Ax) :-
%  reportMsg("Macro do term %s",[ast(A)]),
  isDoTerm(A,_Lc,X),!,
  makeAction(X,none,Ax).
%  reportMsg("Macro do term becomes %s",[ast(Ax)],Lc).


