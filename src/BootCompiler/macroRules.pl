:- module(macroRules,[build_main/2,
		      macroRl/3]).

:- use_module(abstract).
:- use_module(wff).
:- use_module(misc).
:- use_module(errors).
:- use_module(astdisp).

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
macroRl(":=",action,macroRules:spliceAssignMacro).
macroRl(":=",action,macroRules:indexAssignMacro).
macroRl("assert",action,macroRules:assertMacro).
macroRl("show",action,macroRules:showMacro).
macroRl("valof",expression,macroRules:valofMacro).

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
%  dispAst(Main),
%  dispAst(MainTp).
  
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

macroSquareTuple(A,expression,Trm) :-
  isSquareTuple(A,Lc,Els),
  \+isMapLiteral(A,_,_),!,
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
  isSlice(T,Lc,M,F,T),
  ternary(Lc,"_slice",M,F,T,Rp).

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
  glVars(G,[],[],V),
  locOfAst(G,Lc),
  map(V,macroRules:mkName(Lc),Vrs),
  roundTuple(Lc,Vrs,VrTpl),
  mkEnum(Lc,"none",None),
  unary(Lc,"some",VrTpl,Vl),
  makeCondition(G,macroRules:passThru,macroRules:rtn(Vl),grounded(None),Exp),
  optionMatch(Lc,VrTpl,Exp,Rp).

mkName(Lc,Nm,name(Lc,Nm)).

rtn(Vl,grounded(_),Vl).
rtn(_,lyfted(St),St).

passThru(grounded(X),X).

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
  isString(T,_,_),!.
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
  unary(Lc,"__minus",A,Tx).

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

pkgNameMacro(T,expression,string(Lc,P)) :-
  isName(T,Lc,"__pkg__"),
  lcPk(Lc,P).

macroLocationExp(T,expression,Loc) :-
  isName(T,Lc,"__loc__"),!,
  mkLoc(Lc,Loc).

/*
 assert C 
becomes
 perform assrt(()=>C,"failed: C",Loc)
*/
assertMacro(T,action,Act) :-
  isIntegrity(T,Lc,Cond),!,
  ast2String(Cond,Msg),
  locOfAst(Cond,CLc),
  eqn(Lc,tuple(Lc,"()",[]),Cond,Lam),
  mkLoc(CLc,Loc),
  roundTerm(Lc,name(Lc,"assrt"),[Lam,string(Lc,Msg),Loc],Assert),
  mkPerform(Lc,Assert,Act).

/*
 show E 
becomes
  shwMsg(()=>E,"E",Lc)
*/
showMacro(T,action,Act) :-
  isShow(T,Lc,Exp),!,
  ast2String(Exp,Txt),
  locOfAst(Exp,ELc),
  mkLoc(ELc,Loc),
  eqn(Lc,tuple(Lc,"()",[]),Exp,Lam),
  roundTerm(Lc,name(Lc,"shwMsg"),[Lam,string(Lc,Txt),Loc],Act).

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
  unary(Lc,"_name",string(Lc,Id),I).
quoteExp(integer(Lc,Ix),I) :-
  unary(Lc,"_integer",integer(Lc,Ix),I).
quoteExp(float(Lc,Dx),I) :-
  unary(Lc,"_float",float(Lc,Dx),I).
quoteExp(string(Lc,Sx),I) :-
  unary(Lc,"_string",string(Lc,Sx),I).
quoteExp(tuple(Lc,Lb,Els),Rp) :-
  map(Els,quoteExp,QEls),
  macroListEntries(Lc,QEls,R,nilGen,consGen),
  binary(Lc,"_tuple",string(Lc,Lb),R,Rp).
quoteExp(app(Lc,O,A),Rp) :-
  quoteExp(O,Oq),
  quoteExp(A,Aq),
  bnary(Lc,"_apply",Oq,Aq,Rp).

