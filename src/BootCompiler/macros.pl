:- module(macros,[makeComprehension/4,makeIterableGoal/2]).

:- use_module(abstract).
:- use_module(wff).
:- use_module(misc).
:- use_module(errors).
:- use_module(astdisp).

applyRls(A,Cx,St,Rp) :-
  macroKey(A,Ky),
  macroRl(Ky,Cx,Rl),
  call(Rl,Cx,Rs),!,
  testReslt(Rs,A,Cx,St,Rp).
applyRls(A,_,inactive,A).

testReslt(active(Rp),_,Cx,St,Rep) :-!,
  applyRls(Rp,Cx,St,Rep).
testReslt(inactive,A,_,inactive,A).

macroStmt(S,Rp) :- macroAst(S,statement,examineStmt,Rp).

examineStmt(S,Rp) :-
  isTypeAnnotation(S,Lc,V,T),!,
  macroType(T,Tx),
  typeAnnotation(Lc,V,Tx,Rp).
examineStmt(S,Rp) :-
  isPublic(S,Lc,I),!,
  macroStmt(I,II),
  disThroughGroup(II,abstract:unary(Lc,"public"),Rp).
examineStmt(S,Rp) :-
  isPrivate(S,Lc,I),!,
  macroStmt(I,II),
  disThroughGroup(II,abstract:unary(Lc,"private"),Rp).
examineStmt(S,S) :-
  isImport(S,_Lc,_I),!.
examineStmt(S,S) :-
  isOpen(S,_Lc,_I),!.
examineStmt(S,Rp) :-
  isDefn(S,Lc,P,V),!,
  macroPtn(P,PP),
  macroTerm(V,VV),
  mkDefn(Lc,PP,VV,Rp).
examineStmt(S,Rp) :-
  isAssignment(S,Lc,P,V),!,
  macroPtn(P,PP),
  macroTerm(V,VV),
  assignment(Lc,PP,VV,Rp).
examineStmt(S,Rp) :-
  isEquation(S,Lc,P,G,V),!,
  macroPtn(P,PP),
  macroGuard(G,GG),
  macroTerm(V,VV),
  mkEquation(Lc,PP,GG,VV,Rp).



disThroughGroup(S,C,Rp) :-
  isBraceTuple(S,Lc,Els),
  map(Els,C,NEls),
  braceTuple(Lc,NEls,Rp).
dispThroughGroup(S,C,Rp) :-
  call(C,S,Rp).

macroRl("[]",pattern,macroSeqPtn).
macroRl("[]",expression,macroListComprehension).
macroRl("[]",expression,macroSeqExpression).
macroRl("<||>",expression,macroQuote).
macroRl("{}",expression,macroComprehension).
macroRl("::",expression,macroCoercion).
macroRl(":?",expression,macroCoercion).
macroRl("__pkg__",expression,pkgNameMacro).
macroRl("-",expression,uMinusMacro).
macroRl("^=",expression,optionMatchMacro).
macroRl("^",pattern,optionPtnMacro).

macroKey(name(_,Nm),Nm).
macroKey(qnm(_,Nm),Nm).
macroKey(integer(_,_),"$integer").
macroKey(float(_,_),"$float").
macroKey(string(_,_),"$string").
macroKey(A,Ky) :- isTuple(A,_,[I]),!,
  macroKey(I,Ky).
macroKey(tuple(_,Op,_),Op).
macroKey(app(_,Op,_),Ky) :-
  macroKey(Op,Ky).

macroComprehension(T,expression,active(Rp)) :-
  isComprehension(T,Lc,Bnd,Body),!,
  makeComprehension(Lc,Bnd,Body,Rp).
macroComprehension(_,_,inactive).

makeComprehension(Lc,Bnd,Body,Rp) :-
  makeCondition(Body,macros:passThru,macros:consResult(Lc,Bnd),grounded(name(Lc,"_nil")),Rp).

makeIterableGoal(G,Rp) :-
  glVars(G,[],[],V),
  locOfAst(G,Lc),
  map(V,macros:mkName(Lc),Vrs),
  roundTuple(Lc,Vrs,VrTpl),
  enum(Lc,"none",None),
  roundTerm(Lc,"some",[VrTpl],Vl),
  makeCondition(G,macros:passThru,macros:rtn(Vl),grounded(None),Exp),
  optionMatch(Lc,VrTpl,Exp,Rp).

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
  makeCondition(Lhs,Lift,macros:makeCondition(Rhs,Lift,Succ),Zed,Rp).
makeCondition(A,Lift,Succ,Zed,Rp) :-
  isDisjunct(A,_,Lhs,Rhs),!,
  makeCondition(Lhs,Lift,Succ,Zed,E1),
  makeCondition(Rhs,Lift,Succ,lyfted(E1),Rp).
makeCondition(A,Lift,Succ,Zed,Rp) :-
  isNegation(A,_,I),!,
  makeCondition(I,Succ,Lift,Zed,Rp).
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
  isConjunct(G,_,T,L,R),!,
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
  isInteger(T,_),!.
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




