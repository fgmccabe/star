:- module(macroRules,[build_main/2,
		      macroRl/3,
		      makeAction/3]).

:- use_module(abstract).
:- use_module(wff).
:- use_module(misc).
:- use_module(errors).
:- use_module(astdisp).
:- use_module(location).

macroRl("[]",pattern,macroRules:squarePtnMacro).
macroRl("[]",expression,macroRules:squareSequenceMacro).
macroRl("$[]",expression,macroRules:indexMacro).
macroRl("<||>",expression,macroRules:macroQuote).
macroRl("{}",expression,macroRules:comprehensionMacro).
macroRl("{}",expression,macroRules:mapLiteralMacro).
macroRl("{!!}",expression,macroRules:macroIotaComprehension).
macroRl("{??}",expression,macroRules:iterableGoalMacro).
macroRl("::",expression,macroRules:coercionMacro).
macroRl(":?",expression,macroRules:coercionMacro).
macroRl("*",expression,macroRules:multicatMacro).
macroRl("?",expression,macroRules:optionalPropagatetMacro).
macroRl("__pkg__",expression,macroRules:pkgNameMacro).
macroRl("__loc__",expression,macroRules:macroLocationExp).
macroRl("-",expression,macroRules:uminusMacro).
macroRl("^=",expression,macroRules:optionMatchMacro).
macroRl("^",expression,macroRules:unwrapExpMacro).
macroRl("!",expression,macroRules:binRefMacro).
macroRl(":=",action,macroRules:spliceAssignMacro).
macroRl(":=",action,macroRules:indexAssignMacro).
macroRl(":=",expression,macroRules:spliceAssignMacro).
macroRl(":=",expression,macroRules:indexAssignMacro).
macroRl("assert",expression,macroRules:assertMacro).
macroRl("assert",action,macroRules:assertMacro).
macroRl("show",action,macroRules:showMacro).
macroRl("show",expression,macroRules:showMacro).
macroRl("do",expression,macroRules:doMacro).
%macroRl("task",expression,macroRules:taskMacro).
macroRl("valof",expression,macroRules:valofMacro).
macroRl("ignore",action,macroRules:ignoreMacro).
%macroRl("try",action,macroRules:tryCatchMacro).
%macroRl("try",action,macroRules:tryHandleMacro).

build_main(As,Bs) :-
  look_for_signature(As,"main",Lc,Ms),
  \+look_for_signature(As,"_main",_,_),!,
  synthesize_main(Lc,Ms,As,Bs).
build_main(A,A).

look_for_signature([St|_],Nm,Lc,Ms) :-
  (isTypeAnnotation(St,Lc,V,T) ;
   (isPublic(St,_,I),isTypeAnnotation(I,Lc,V,T))),
  isIden(V,_,Nm),
  isFuncType(T,_,L,_),
  isTuple(L,_,Ms),!.
look_for_signature([_|As],Nm,Lc,Ms) :-
  look_for_signature(As,Nm,Lc,Ms).

synthesize_main(Lc,Ts,As,[MainTp,Main|As]) :-
  synthesize_coercions(Ts,Vs,Cs),
  list_pttrn(Lc,Vs,Arg),
  roundTerm(Lc,name(Lc,"_main"),[Arg],Lhs),
  roundTerm(Lc,name(Lc,"main"),Cs,Rhs),
  mkValof(Lc,Rhs,MnCall),
  eqn(Lc,Lhs,MnCall,Main),
  squareTerm(Lc,name(Lc,"cons"),[name(Lc,"string")],T1),
  roundTuple(Lc,[T1],T3),
  roundTuple(Lc,[],Unit),
  binary(Lc,"=>",T3,Unit,TU),
  binary(Lc,":",name(Lc,"_main"),TU,MainTp).
%  dispAst(Main).
  
synthesize_coercions([],[],[]).
synthesize_coercions([T|Ts],[V|Vs],[C|Cs]) :-
  locOfAst(T,Lc),
  genIden(Lc,V),
  coerce(Lc,V,T,C),
  synthesize_coercions(Ts,Vs,Cs).

list_pttrn(Lc,[],Arg) :-
  isSquareTuple(Arg,Lc,[]),!.
list_pttrn(Lc,Ts,Arg) :-
  reComma(Ts,As),
  isSquareTuple(Arg,Lc,[As]).

squareSequenceMacro(A,expression,Trm) :-
  isSquareTuple(A,Lc,Els),
  \+isMapLiteral(A,_,_),
  macroListEntries(Lc,Els,Trm,nilGen,consGen).

nilGen(Lc,name(Lc,"_nil")).

consGen(Lc,L,R,Trm) :-
  binary(Lc,"_cons",L,R,Trm).

appndGen(Lc,L,R,Trm) :-
  binary(Lc,"_apnd",L,R,Trm).

emptyGen(Lc,name(Lc,"_empty")).

putGen(Lc,Pr,R,Trm) :-
  isPair(Pr,_,F,V),
  ternary(Lc,"_put",R,F,V,Trm).

concGen(Lc,L,(F,V),Trm) :-
  ternary(Lc,"put",L,F,V,Trm).

squarePtnMacro(A,pattern,Ptn) :-
  isSquareTuple(A,Lc,Els),!,
  macroListEntries(Lc,Els,Ptn,genEofTest,genHedTest).

genEofTest(Lc,Trm) :-
  genIden(Lc,X),
  unary(Lc,"_eof",X,E),
  mkWhere(Lc,X,E,Trm).

genHedTest(Lc,L,R,Trm) :-
  mkWherePtn(Lc,tuple(Lc,"()",[L,R]),name(Lc,"_hdtl"),Trm).

macroListEntries(Lc,[],Trm,End,_) :-
  call(End,Lc,Trm).
macroListEntries(_,[Cns],Trm,_,Hed) :-
  isConsTerm(Cns,Lc,H,T),
  call(Hed,Lc,H,T,Trm).
macroListEntries(Lc,[E|L],Trm,End,Hed) :-
  macroListEntries(Lc,L,Tr,End,Hed),
  call(Hed,Lc,E,Tr,Trm).

indexMacro(T,expression,Rp) :-
  isIndexTerm(T,Lc,M,A),!,
  (isBinary(A,_,"->",Ky,Vl) ->
   ternary(Lc,"_put",M,Ky,Vl,Rp);
   isNegation(A,_,Ky) ->
   binary(Lc,"_remove",M,Ky,Rp);
   binary(Lc,"_index",M,A,Rp)).
indexMacro(T,expression,Rp) :-
  isSlice(T,Lc,M,Fr,To),
  ternary(Lc,"_slice",M,Fr,To,Rp).

mapLiteralMacro(A,expression,Trm) :-
  isMapLiteral(A,Lc,Prs),!,
  macroListEntries(Lc,Prs,Trm,emptyGen,putGen).

comprehensionMacro(T,expression,Rp) :-
  isComprehension(T,Lc,Bnd,Body),!,
  makeComprehension(Lc,Bnd,Body,Rp).

makeComprehension(Lc,Bnd,Body,Rp) :-
  makeCondition(Body,macroRules:passThru,macroRules:consResult(Lc,Bnd),grounded(name(Lc,"_nil")),Rp).

macroIotaComprehension(T,expression,Rp) :-
  isIotaComprehension(T,Lc,Bnd,Body),!,
  unary(Lc,"some",Bnd,Res),
  mkEnum(Lc,"none",Empty),
  makeCondition(Body,macroRules:passThru,macroRules:rtn(Res),grounded(Empty),Rp).

iterableGoalMacro(G,expression,Gx) :-
  isTestComprehension(G,_Lc,B),
  makeIterableGoal(B,Gx).

makeIterableGoal(G,Rp) :-
  locOfAst(G,Lc),
  mkEnum(Lc,"false",False),
  mkEnum(Lc,"true",True),
  makeCondition(G,macroRules:passThru,macroRules:rtn(True),grounded(False),Rp).

mkName(Lc,Nm,name(Lc,Nm)).

rtn(Vl,grounded(_),Vl).
rtn(_,lyfted(St),St).

passThru(grounded(X),X).
passThru(lyfted(X),X).

consResult(Lc,Bnd,grounded(St),Res) :-
  binary(Lc,"_cons",Bnd,St,Res).
consResult(_,_,lyfted(St),St).

makeCondition(A,Lift,Succ,Zed,Rp) :-
  isSearch(A,Lc,Ptn,Src),!,
  genIden(Lc,"sF",Sf),
  genIden(Lc,"St",St),
  mkAnon(Lc,Anon),
  roundTerm(Lc,Sf,[Ptn,St],S),
  roundTerm(Lc,Sf,[Anon,St],F),
  call(Succ,grounded(St),Sc),
  call(Lift,grounded(St),Lf),
  eqn(Lc,S,Sc,Eq1),
  eqn(Lc,F,Lf,Eq2),
  mkLetDef(Lc,[Eq1,Eq2],Sf,FF),
  call(Lift,Zed,ZZ),
%  unary(Lc,"_more",ZZ,ZI),
  ternary(Lc,"_iter",Src,ZZ,FF,Rp).
makeCondition(A,Lift,Succ,Zed,Rp) :-
  isConjunct(A,_,Lhs,Rhs),!,
  makeCondition(Lhs,Lift,macroRules:makeCondition(Rhs,Lift,Succ),Zed,Rp).
makeCondition(A,Lift,Succ,Zed,Rp) :-
  isDisjunct(A,_,Lhs,Rhs),!,
  makeCondition(Lhs,Lift,Succ,Zed,E1),
  makeCondition(Rhs,Lift,Succ,lyfted(E1),Rp).
makeCondition(A,Lift,Succ,Zed,Rp) :-
  isNegation(A,Lc,I),!,
  mkEnum(Lc,"true",True),
  mkEnum(Lc,"false",False),
  makeCondition(I,Lift,macroRules:rtn(True),grounded(False),Negated),
  call(Succ,Zed,Ok),
  call(Lift,Zed,NotOk),
  conditional(Lc,Negated,NotOk,Ok,Rp).
makeCondition(A,Lift,Succ,Zed,Rp) :-
  isForall(A,Lc,Lhs,Rhs),!,
  negation(Lc,Rhs,RR),
  conjunct(Lc,Lhs,RR,R1),
  negation(Lc,R1,RI),
  makeCondition(RI,Lift,Succ,Zed,Rp).
makeCondition(A,Lift,Succ,Zed,Rp) :-
  isTuple(A,[I]),!,
  makeCondition(I,Lift,Succ,Zed,Rp).
makeCondition(A,Lift,Succ,Zed,Rp) :-
  locOfAst(A,Lc),
  call(Succ,Zed,ZZ),
  call(Lift,Zed,OO),
  conditional(Lc,A,ZZ,OO,Rp).

coercionMacro(Term,expression,N) :-
  isCoerce(Term,Lc,L,R),!,
  unary(Lc,"_coerce",L,LT),
  unary(Lc,"_optval",LT,OLT),
  binary(Lc,":",OLT,R,N).
coercionMacro(Term,expression,N) :-
  isOptCoerce(Term,Lc,L,R),!,
  unary(Lc,"_coerce",L,LT),
  sqUnary(Lc,"option",R,OR),
  binary(Lc,":",LT,OR,N).

multicatMacro(T,expression,Tx) :-
  isUnary(T,Lc,"*",I),!,
  unary(Lc,"_multicat",I,Tx).

% ?E[^X] (i.e., E containing subexpression ^X) -> (some(V).=X?some(E[^V])||none)

optionalPropagatetMacro(T,expression,Tx) :-
  isUnary(T,Lc,"?",I),
  genIden(Lc,V),
  propFindReplace(I,V,R,X),!,
  unary(Lc,"some",V,AP),
  match(Lc,AP,X,PX),
  mkEnum(Lc,"none",Empty),
  unary(Lc,"some",R,RO),
  conditional(Lc,PX,RO,Empty,Tx).

propFindReplace(A,V,R,X) :-
  astFindReplace(A,macroRules:getProp(V,X),R).

getProp(V,X,A,V) :-
  isUnary(A,_,"^",X),!.

uminusMacro(T,expression,Tx) :-
  isUnaryMinus(T,Lc,A),
  isInteger(A,_,Ix),!,
  IIx is -Ix,
  mkInteger(Lc,IIx,Tx).
uminusMacro(T,expression,Tx) :-
  isUnaryMinus(T,Lc,A),
  isFloat(A,_,Dx),!,
  MDx is -Dx,
  mkFloat(Lc,MDx,Tx).
uminusMacro(T,expression,Tx) :-
  isUnaryMinus(T,Lc,A),
  isBigInt(A,_,Bx),!,
  negateString(Bx,Mx),
  mkBigInt(Lc,Mx,Tx).
uminusMacro(T,expression,Tx) :-
  isUnaryMinus(T,Lc,A),!,
  unary(Lc,"__minus",A,Tx).

optionMatchMacro(T,expression,Tx) :-
  isOptionMatch(T,Lc,P,E),!,
  unary(Lc,"some",P,SP),
  match(Lc,SP,E,Tx).

unwrapExpMacro(T,expression,Tx) :-
  isBinary(T,Lc,"^",L,R),isIden(L,_,Con),!,
  genIden(Lc,V),
  unary(Lc,Con,V,Ptn),
  match(Lc,Ptn,R,Mtch),
  mkWhere(Lc,V,Mtch,Tx).

pkgNameMacro(T,expression,string(Lc,P)) :-
  isName(T,Lc,"__pkg__"),
  lcPk(Lc,P).

macroLocationExp(T,expression,Loc) :-
  isName(T,Lc,"__loc__"),!,
  mkLoc(Lc,Loc).

  
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
  isThrow(A,Lc,E),!,
  makeThrow(Lc,E,Ac).
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
makeAction(A,Cont,Ac) :-
  isResume(A,_,_,_),!,
  combine(A,Cont,Ac).
makeAction(A,Cont,Ac) :-
  isIgnore(A,Lc,I),!,
  makeIgnore(Lc,I,Ig),
  makeAction(Ig,Cont,Ac).
  
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

/*
 assert C 
becomes
 perform assrt(()=>C,"failed: C",Loc)
*/
assertMacro(T,_,Act) :-
  isIntegrity(T,Lc,Cond),!,
  makeAssert(Lc,Cond,Act).

makeAssert(Lc,Cond,Act) :-
  ast2String(Lc,Cond,Msg),
  locOfAst(Cond,CLc),
  eqn(Lc,tuple(Lc,"()",[]),Cond,Lam),
  mkLoc(CLc,Loc),
  roundTerm(Lc,name(Lc,"assrt"),[Lam,Msg,Loc],Assert),
  mkPerform(Lc,Assert,Act).

/*
 show E 
becomes
  shwMsg(()=>E,"E",Lc)
*/
showMacro(T,_,Act) :-
  isShow(T,Lc,Exp),!,
  makeShow(Lc,Exp,Act).

makeShow(Lc,Exp,Act) :-
  ast2String(Lc,Exp,Txt),
  locOfAst(Exp,ELc),
  mkLoc(ELc,Loc),
  eqn(Lc,tuple(Lc,"()",[]),Exp,Lam),
  roundTerm(Lc,name(Lc,"shwMsg"),[Lam,Txt,Loc],Act).

/*
   A[F:T] := R
  becomes
  A := _splice(A!,F,T,R)
*/

spliceAssignMacro(A,action,Act) :-
  isSplice(A,Lc,S,F,T,R),!,
  unary(Lc,"!",S,Src),
  nary(Lc,"_splice",[Src,F,T,R],Rep),
  assignment(Lc,S,Rep,Act).

/*
  A[Ix] := R
  becomes
  A := _put(A!,Ix,R)
*/
indexAssignMacro(A,_,Act) :-
  isAssignment(A,Lc,L,R),
  isIndexTerm(L,LLc,C,I),!,
  unary(LLc,"!",C,CC),
  ternary(LLc,"_put",CC,I,R,Repl),
  binary(Lc,":=",C,Repl,Act).

valofMacro(T,expression,Tx) :-
  isValof(T,Lc,A),!,
  (isBraceTuple(A,_,[As]) ->
   makeAction(As,none,Ax),
   mkAnon(Lc,Anon),
   unitTpl(Lc,U),
   squareTerm(Lc,name(Lc,"result"),[U,Anon],RTp),
   typeAnnotation(Lc,Ax,RTp,AA);
   A=AA),
  unary(Lc,"_perform",AA,Tx).

ignoreMacro(T,action,Tx) :-
  isIgnore(T,Lc,A),!,
  makeIgnore(Lc,A,Tx).

makeIgnore(Lc,A,Act) :-
  mkAnon(Lc,An),
  match(Lc,An,A,Act).
  
macroQuote(T,expression,Rp) :-
  isQuote(T,_,A),!,
  quoteExp(A,Rp).

quoteExp(A,I) :-
  isUnary(A,_,"$",I),!.
quoteExp(name(Lc,Id),I) :-
  unary(Lc,"_name",string(Lc,Id),I).
quoteExp(qnme(Lc,Id),I) :-
  unary(Lc,"_qnme",string(Lc,Id),I).
quoteExp(integer(Lc,Ix),I) :-
  unary(Lc,"_integer",integer(Lc,Ix),I).
quoteExp(float(Lc,Dx),I) :-
  unary(Lc,"_float",float(Lc,Dx),I).
quoteExp(char(Lc,Cp),I) :-
  unary(Lc,"_char",char(Lc,Cp),I).
quoteExp(string(Lc,Sx),I) :-
  unary(Lc,"_string",string(Lc,Sx),I).
quoteExp(tuple(Lc,Lb,Els),Rp) :-
  map(Els,macroRules:quoteExp,QEls),
  macroListEntries(Lc,QEls,R,nilGen,consGen),
  binary(Lc,"_tuple",string(Lc,Lb),R,Rp).
quoteExp(app(Lc,O,A),Rp) :-
  quoteExp(O,Oq),
  quoteExp(A,Aq),
  binary(Lc,"_apply",Oq,Aq,Rp).

/*
  A![Ix]
  becomes
  (A!)[Ix]
*/
binRefMacro(T,expression,Rp) :-
  isBinary(T,Lc,"!",L,B),
  isSquareTuple(B,_,[A]),!,
  cellRef(Lc,L,LR),
  squareTerm(Lc,LR,[A],Rp).
  
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

/*
 * Synthesize a type from an unnamed brace term
*/
% braceTypeMacro(E,Ex) :-
%   isBraceTuple(E,Lx,Els),!,
%   collectEntries(Es,TpPairs),
%   sort(Es,macroRule:cmpTpl,SEs),
%   project0_3(SEs,Nms),
%   interleave(Nms,".",Ns),
%   concateStrings(Ns,TpNm),
%   braceTerm(Lx,name(Lx,TpNm),SEs,TpBdy),
%   project2_3(SEs,Vrs),
%   squareTerm(Lx,name(Lc,TpNm),[Vrs],TpHd).


% collectEntries([],[]).
% collectEntries([E|Els],[(Nm,An,TV)|TVs]) :-
%   ruleName(E,var(Nm),value),
%   locOfAst(E,Lc),
%   genIden(Lc,Nm,TV),
%   typeAnnotation(Lc,name(Lc,Nm),TV,An),
%   collectEntries(Els,TVs).

