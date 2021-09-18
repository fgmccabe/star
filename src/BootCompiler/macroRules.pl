:- module(macroRules,[build_main/2,
		      macroRl/3]).

:- use_module(abstract).
:- use_module(wff).
:- use_module(misc).
:- use_module(errors).
:- use_module(astdisp).
:- use_module(location).

macroRl("[]",pattern,macroRules:macroSquarePtn).
macroRl("[]",expression,macroRules:macroListComprehension).
macroRl("[]",expression,macroRules:macroSquareTuple).
macroRl("$[]",expression,macroRules:indexMacro).
macroRl("<||>",expression,macroRules:macroQuote).
macroRl("{}",expression,macroRules:macroComprehension).
macroRl("{!!}",expression,macroRules:macroIotaComprehension).
macroRl("{??}",expression,macroRules:macroIterableGoal).
macroRl("::",expression,macroRules:macroCoercion).
macroRl(":?",expression,macroRules:macroCoercion).
macroRl("__pkg__",expression,macroRules:pkgNameMacro).
macroRl("__loc__",expression,macroRules:macroLocationExp).
macroRl("-",expression,macroRules:uminusMacro).
macroRl("^=",expression,macroRules:optionMatchMacro).
macroRl("^",pattern,macroRules:optionPtnMacro).
macroRl("^",expression,macroRules:unwrapExpMacro).
macroRl("!",expression,macroRules:binRefMacro).
macroRl(":=",action,macroRules:spliceAssignMacro).
macroRl(":=",action,macroRules:indexAssignMacro).
macroRl("assert",action,macroRules:assertMacro).
macroRl("show",action,macroRules:showMacro).
macroRl("do",action,macroRules:forLoopMacro).
macroRl("valof",expression,macroRules:valofMacro).
macroRl("try",action,macroRules:tryCatchMacro).

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
  squareTerm(Lc,name(Lc,"cons"),[name(Lc,"chars")],T1),
  roundTuple(Lc,[T1],T3),
  roundTuple(Lc,[],Unit),
  binary(Lc,"=>",T3,Unit,TU),
  binary(Lc,":",name(Lc,"_main"),TU,MainTp).
%  dispAst(Main).
  
synthesize_coercions([],[],[]).
synthesize_coercions([T|Ts],[V|Vs],[C|Cs]) :-
  locOfAst(T,Lc),
  genIden(Lc,V),
  unary(Lc,"chrs_",V,VS),
  coerce(Lc,VS,T,C),
  synthesize_coercions(Ts,Vs,Cs).

list_pttrn(Lc,[],Arg) :-
  isSquareTuple(Arg,Lc,[]),!.
list_pttrn(Lc,Ts,Arg) :-
  reComma(Ts,As),
  isSquareTuple(Arg,Lc,[As]).

macroSquareTuple(A,expression,Trm) :-
  isSquareTuple(A,Lc,Els),
  \+isMapLiteral(A,_,_),
  \+isListComprehension(A,_,_,_),!,
  macroListEntries(Lc,Els,Trm,nilGen,consGen).
macroSquareTuple(A,expression,Trm) :-
  isMapLiteral(A,Lc,Prs),
  macroListEntries(Lc,Prs,Trm,emptyGen,putGen).

nilGen(Lc,name(Lc,"_nil")).

consGen(Lc,L,R,Trm) :-
  binary(Lc,"_cons",L,R,Trm).

appndGen(Lc,L,R,Trm) :-
  binary(Lc,"_apnd",L,R,Trm).

emptyGen(Lc,name(Lc,"_empty")).

putGen(Lc,(F,V),R,Trm) :-
  ternary(Lc,"_put",R,F,V,Trm).

concGen(Lc,L,(F,V),Trm) :-
  ternary(Lc,"put",L,F,V,Trm).

macroSquarePtn(A,pattern,Ptn) :-
  isSquareTuple(A,Lc,Els),!,
  macroListEntries(Lc,Els,Ptn,genEofTest,genHedTest).

genEofTest(Lc,Trm) :-
  genIden(Lc,X),
  unary(Lc,"_eof",X,E),
  mkWhere(Lc,X,E,Trm).

genHedTest(Lc,L,R,Trm) :-
  mkWherePtn(Lc,tuple(Lc,"()",[L,R]),name(Lc,"_hdtl"),Trm).

genTailTest(Lc,L,R,Trm) :-
  mkWherePtn(Lc,tuple(Lc,"()",[L,R]),name(Lc,"_back"),Trm).

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

macroComprehension(T,expression,Rp) :-
  isComprehension(T,Lc,Bnd,Body),!,
  makeComprehension(Lc,Bnd,Body,Rp).

macroListComprehension(T,expression,Rp) :-
  isListComprehension(T,Lc,Bnd,Body),!,
  makeComprehension(Lc,Bnd,Body,C),
  mkAnon(Lc,Anon),
  squareTerm(Lc,name(Lc,"cons"),[Anon],LTp),
  typeAnnotation(Lc,C,LTp,Rp).

makeComprehension(Lc,Bnd,Body,Rp) :-
  makeCondition(Body,macroRules:passThru,macroRules:consResult(Lc,Bnd),grounded(name(Lc,"_nil")),Rp).

macroIotaComprehension(T,expression,Rp) :-
  isIotaComprehension(T,Lc,Bnd,Body),!,
  unary(Lc,"some",Bnd,Res),
  mkEnum(Lc,"none",Empty),
  makeCondition(Body,macroRules:passThru,macroRules:rtn(Res),grounded(Empty),Rp).

macroIterableGoal(G,expression,Gx) :-
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
  locOfAst(A,Lc),
  call(Succ,Zed,ZZ),
  call(Lift,Zed,OO),
  conditional(Lc,A,ZZ,OO,Rp).

glVars(G,E,V,Vx) :-
  isSearch(G,_,P,_),!,
  ptnVars(P,E,V,Vx).
glVars(G,E,V,Vx) :-
  isMatch(G,_,P,_),!,
  ptnVars(P,E,V,Vx).
glVars(G,E,V,Vx) :-
  isOptionMatch(G,_,P,_),!,
  ptnVars(P,E,V,Vx).
glVars(G,E,V,Vx) :-
  isConjunct(G,_,L,R),!,
  glVars(L,E,V,V0),
  glVars(R,E,V0,Vx).
glVars(G,E,V,Vx) :-
  isDisjunct(G,_,L,R),!,
  glVars(L,E,[],V0),
  glVars(R,E,[],V1),
  intersect(V0,V1,V2),
  merge(V2,V,Vx).
glVars(G,E,V,Vx) :-
  isConditional(G,_,T,L,R),!,
  glVars(T,E,[],V0),
  glVars(L,E,V0,V1),
  glVars(R,E,[],V2),
  intersect(V1,V2,V3),
  merge(V3,V,Vx).
glVars(G,_,Vx,Vx) :-
  isForall(G,_,_,_),!.
glVars(G,_,Vx,Vx) :-
  isNegation(G,_,_),!.
glVars(G,E,V,Vx) :-
  isTuple(G,_,[G0]),!,
  glVars(G0,E,V,Vx).
glVars(_,_,Vx,Vx).

ptnVars(T,_,Vx,Vx) :-
  isAnon(T,_),!.
ptnVars(T,E,V,Vx) :-
  isIden(T,_,Nm),!,
  (is_member(Nm,E) -> V=Vx ; add_mem(Nm,V,Vx)).
ptnVars(T,_,V,V) :-
  isInteger(T,_,_),!.
ptnVars(T,_,V,V) :-
  isFloat(T,_,_),!.
ptnVars(T,_,V,V) :-
  isChars(T,_,_),!.
ptnVars(T,_,V,V) :-
  isEnum(T,_,_),!.
ptnVars(T,E,V,Vx) :-
  isRoundTerm(T,_,Args),!,
  ptnListVars(Args,E,V,Vx).
ptnVars(T,E,V,Vx) :-
  isRoundTuple(T,_,Args),!,
  ptnListVars(Args,E,V,Vx).
ptnVars(T,E,V,Vx) :-
  isSquareTuple(T,_,Args),!,
  ptnListVars(Args,E,V,Vx).
ptnVars(T,E,V,Vx) :-
  isWhere(T,_,P,G),!,
  ptnVars(P,V,V0),
  glVars(G,E,V0,Vx).
ptnVars(_,_,V,V).

ptnListVars([],_,Vx,Vx) :-!.
ptnListVars([T|R],E,V,Vx) :-
  ptnVars(T,E,V,V0),
  ptnListVars(R,E,V0,Vx).

macroCoercion(Term,expression,N) :-
  isCoerce(Term,Lc,L,R),!,
  unary(Lc,"_coerce",L,LT),
  unary(Lc,"_optval",LT,OLT),
  binary(Lc,":",OLT,R,N).
macroCoercion(Term,expression,N) :-
  isOptCoerce(Term,Lc,L,R),!,
  unary(Lc,"_coerce",L,LT),
  sqUnary(Lc,"option",R,OR),
  binary(Lc,":",LT,OR,N).

uminusMacro(T,expression,Tx) :-
  isUnaryMinus(T,Lc,A),!,
  (isInteger(A,LLc,Ix) ->
   IIx is -Ix,
   mkInteger(LLc,IIx,Tx);
   isFloat(A,LLc,Dx) ->
   MDx is -Dx,
   mkFloat(LLc,MDx,Tx);
   unary(Lc,"__minus",A,Tx)).   

optionMatchMacro(T,expression,Tx) :-
  isOptionMatch(T,Lc,P,E),!,
  unary(Lc,"some",P,SP),
  match(Lc,SP,E,Tx).

optionPtnMacro(T,pattern,Tx) :-
  isOptionPtn(T,Lc,Pt,Ex),
  mkWherePtn(Lc,Pt,Ex,Tx).

unwrapExpMacro(T,expression,Tx) :-
  isBinary(T,Lc,"^",L,R),isIden(L,_,Con),!,
  genIden(Lc,V),
  unary(Lc,Con,V,Ptn),
  match(Lc,Ptn,R,Mtch),
  mkWhere(Lc,V,Mtch,Tx).

pkgNameMacro(T,expression,Trm) :-
  isName(T,Lc,"__pkg__"),
  lcPk(Lc,P),
  mkSSChars(Lc,P,Trm).

macroLocationExp(T,expression,Loc) :-
  isName(T,Lc,"__loc__"),!,
  mkLoc(Lc,Loc).

/*
  for X in L do A
  becomes
  { _It .= _iterator(L);
    while X^=current(_It) do{
      A;
      _advance(_It)
    }
  }
*/

forLoopMacro(A,action,Ax) :-
  isForDo(A,Lc,P,C,B),!,
  genIden(Lc,It),
  roundTerm(Lc,name(Lc,"_iterator"),[C],Itor),
  match(Lc,It,Itor,S1),
  roundTerm(Lc,name(Lc,"_current"),[It],Cr),
  optionMatch(Lc,P,Cr,WCnd),
  roundTerm(Lc,name(Lc,"_advance"),[It],Ad),
  mkActionSeq(Lc,B,Ad,B1),
  mkWhileDo(Lc,WCnd,B1,S2),
  mkActionSeq(Lc,S1,S2,S3),
  braceTuple(Lc,[S3],Ax).
  

/*
 assert C 
becomes
 perform assrt(()=>C,"failed: C",Loc)
*/
assertMacro(T,action,Act) :-
  isIntegrity(T,Lc,Cond),!,
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
showMacro(T,action,Act) :-
  isShow(T,Lc,Exp),!,
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
indexAssignMacro(A,action,Act) :-
  isAssignment(A,Lc,L,R),
  isIndexTerm(L,LLc,C,I),!,
  unary(LLc,"!",C,CC),
  ternary(LLc,"_put",CC,I,R,Repl),
  binary(Lc,":=",C,Repl,Act).

valofMacro(T,expression,Tx) :-
  isValof(T,Lc,A),!,
  unary(Lc,"_valof",A,Tx).
  
macroQuote(T,expression,Rp) :-
  isQuote(T,_,A),!,
  quoteExp(A,Rp).

quoteExp(A,I) :-
  isUnary(A,_,"$",I),!.
quoteExp(name(Lc,Id),I) :-
  mkSSChars(Lc,Id,S),
  unary(Lc,"_name",S,I).
quoteExp(integer(Lc,Ix),I) :-
  unary(Lc,"_integer",integer(Lc,Ix),I).
quoteExp(float(Lc,Dx),I) :-
  unary(Lc,"_float",float(Lc,Dx),I).
quoteExp(chars(Lc,Sx),I) :-
  mkSSChars(Lc,Sx,S),
  unary(Lc,"_string",S,I).
quoteExp(tuple(Lc,Lb,Els),Rp) :-
  map(Els,quoteExp,QEls),
  macroListEntries(Lc,QEls,R,nilGen,consGen),
  mkSSChars(Lc,Lb,S),
  binary(Lc,"_tuple",S,R,Rp).
quoteExp(app(Lc,O,A),Rp) :-
  quoteExp(O,Oq),
  quoteExp(A,Aq),
  bnary(Lc,"_apply",Oq,Aq,Rp).

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