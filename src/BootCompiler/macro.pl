:- module(macro,[macroRewrite/2]).

% Implement features like the algegraic type definition via macro-replacement
% used in checking a theta environment before dependency analysis

:- use_module(abstract).
:- use_module(wff).
:- use_module(misc).
:- use_module(errors).


% rewrite a sequence of statements into another sequence.

macroRewrite([],[]).
macroRewrite([St|More],Stmts) :-
  isUnary(St,"public",Inner),
  isAlgebraicTypeStmt(Inner,Lc,Quants,Constraints,Head,Body),!,
  convertAlgebraic(Lc,macro:markPublic,Quants,Constraints,Head,Body,Stmts,S0),
  macroRewrite(More,S0).
macroRewrite([St|More],Stmts) :-
  isUnary(St,"private",Inner),
  isAlgebraicTypeStmt(Inner,Lc,Quants,Constraints,Head,Body),!,
  convertAlgebraic(Lc,macro:markPrivate,Quants,Constraints,Head,Body,Stmts,S0),
  macroRewrite(More,S0).
macroRewrite([St|More],Stmts) :-
  isAlgebraicTypeStmt(St,Lc,Quants,Constraints,Head,Body),!,
  convertAlgebraic(Lc,macro:noMark,Quants,Constraints,Head,Body,Stmts,S0),
  macroRewrite(More,S0).
macroRewrite([St|More],[St|Stmts]) :-
  isContractStmt(St,_,Quants,Constraints,Con,Els),!,
  generateAnnotations(Els,Quants,[Con|Constraints],Stmts,S0),
  macroRewrite(More,S0).
macroRewrite([St|More],[St|Stmts]) :-
  macroRewrite(More,Stmts).

markPublic(Lc,Stmt,PStmt) :-
  unary(Lc,"public",Stmt,PStmt).

markPrivate(Lc,Stmt,PStmt) :-
  unary(Lc,"private",Stmt,PStmt).

noMark(_,Stmt,Stmt).

% super simple conversion. Maybe later add support for auto generation of interfaces

convertAlgebraic(Lc,Mark,Quants,Constraints,Head,Body,[TypeRule|Elements],Tail) :-
  algebraicFace(Body,[],Els),
  isBraceTuple(Face,Lc,Els),
  typeExists(Lc,Quants,Constraints,Head,Face,FaceRule),
  call(Mark,Lc,FaceRule,TypeRule),
  convertConstructors(Body,Head,Mark,Quants,Constraints,Elements,Tail).

convertConstructors(Pair,Head,Mark,Quants,Constraints,Elements,Tail) :-
  isBinary(Pair,"|",L,R),
  convertConstructors(L,Head,Mark,Quants,Constraints,Elements,L1),
  convertConstructors(R,Head,Mark,Quants,Constraints,L1,Tail).
convertConstructors(Term,Head,Mark,Quants,Constraints,Elements,Tail) :-
  convertConstructor(Term,Head,Mark,Quants,Constraints,Elements,Tail).

convertConstructor(name(Lc,Nm),Tp,Mark,Quants,Constraints,[TpRule,BodyRule|Tail],Tail) :-
  wrapConstraints(Constraints,Lc,Tp,TpCon),
  wrapQuants(Quants,Lc,TpCon,QTp),
  hasType(Lc,Nm,QTp,TpRl),
  call(Mark,Lc,TpRl,TpRule),
  bodyRule(Lc,name(Lc,Nm),[],BodyRule). /* construct con(_,..,_) <= {} */
convertConstructor(Con,Tp,Mark,Quants,Constraints,[TpRule,BodyRule|Tail],Tail) :-
  isRound(Con,Nm,Args),!, /* Construct con:(T1,..,Tn)<=>Tp */
  locOfAst(Con,Lc),
  isTuple(ArgTypes,Lc,Args),
  consType(Lc,ArgTypes,Tp,ClassType),
  wrapConstraints(Constraints,Lc,ClassType,CTp),
  wrapQuants(Quants,Lc,CTp,QTp),
  hasType(Lc,Nm,QTp,TpRl),
  call(Mark,Lc,TpRl,TpRule),
  genAnonArgs(Args,AArgs),       /* Construct con(_,..,_) <= thing */
  roundTerm(Lc,Nm,AArgs,Hd),
  bodyRule(Lc,Hd,[],BodyRule).
convertConstructor(Con,Tp,Mark,Quants,Constraints,[TpRule,BodyRule|Tail],Tail) :-
  isBraceTerm(Con,Lc,Op,Els),
  isIden(Op,TpNm),
  pullIntegrities(Els,As,FldTps),
  isBraceTuple(Args,Lc,FldTps),
  consType(Lc,Args,Tp,ClassType),
  wrapConstraints(Constraints,Lc,ClassType,CTp),
  wrapQuants(Quants,Lc,CTp,QTp),
  hasType(Lc,TpNm,QTp,TpRl),
  call(Mark,Lc,TpRl,TpRule),
  genFieldArgs(FldTps,Defs,W),
  braceTerm(Lc,Op,Defs,Hd),
  rewriteList(As,W,Ass),
  bodyRule(Lc,Hd,Ass,BodyRule).

pullIntegrities([],[],[]).
pullIntegrities([T|L],[T|I],E) :-
  isIntegrity(T,_,_),
  pullIntegrities(L,I,E).
pullIntegrities([T|L],I,[T|E]) :-
  pullIntegrities(L,I,E).

algebraicFace(T,SoFar,Face) :-
  isBinary(T,"|",L,R),
  algebraicFace(L,SoFar,SF),
  algebraicFace(R,SF,Face).
algebraicFace(T,SoFar,SoFar) :-
  isRound(T,_,_),!.
algebraicFace(T,Face,Face) :-
  isIden(T,_,_),!.
algebraicFace(T,SoFar,Face) :-
  isBraceTerm(T,_,_,Args),
  pickupFields(Args,SoFar,Face).

pickupFields([],Face,Face).
pickupFields([T|M],SF,Face) :-
  isBinary(T,Lc,":",L,R),
  isIden(L,_,Nm),
  checkSoFar(Lc,Nm,R,SF,SF0),
  pickupFields(M,[T|SF0],Face).
pickupFields([T|M],F,Fx) :-
  isIntegrity(T,_,_),
  pickupFields(M,F,Fx).
pickupFields([T|M],SF,Face) :-
  locOfAst(T,Lc),
  reportError("invalid type field: %s",[T],Lc),
  pickupFields(M,SF,Face).

checkSoFar(_,_,_,[],[]).
checkSoFar(Lc,Nm,T,[P|L],L) :-
  isBinary(P,":",NN,RR),
  isIden(NN,_,Nm),
  (sameTerm(RR,T) ; reportError("field %s:%s must be identical to: %s",[Nm,T,RR],Lc)).
checkSoFar(Lc,Nm,T,[P|L],[P|LL]) :-
  checkSoFar(Lc,Nm,T,L,LL).

typeExists(Lc,Quants,Constraints,Hd,Body,Stmt) :-
  binary(Lc,"<~",Hd,Body,Rl),
  wrapConstraints(Constraints,Lc,Rl,ConRl),
  wrapQuants(Quants,Lc,ConRl,Stmt).

wrapQuants([],_,Rule,Rule).
wrapQuants(Q,Lc,Rl,Rule) :-
  listComma(Q,Lc,QV),
  unary(Lc,"all",QV,R1),
  binary(Lc,"~~",R1,Rl,Rule).

wrapConstraints([],_,Tp,Tp).
wrapConstraints(Con,Lc,Tp,ConTp) :-
  listComma(Con,Lc,CTp),
  binary(Lc,"|:",CTp,Tp,ConTp).

listComma([T],_,T).
listComma([T|R],Lc,CT) :-
  listComma(R,Lc,RR),
  binary(Lc,",",T,RR,CT).

genAnonArgs([],[]).
genAnonArgs([T|M],[name(Lc,"_")|A]) :- locOfAst(T,Lc), genAnonArgs(M,A).

genFieldArgs([],[],[]).
genFieldArgs([F|M],[FA|AR],[(Nm,V)|Fx]) :-
  isBinary(F,Lc,":",L,_),
  isIden(L,Nm),
  genstr("_",V),
  binary(Lc,"=",L,name(Lc,V),FA),
  genFieldArgs(M,AR,Fx).

hasType(Lc,Nm,Tp,St) :-
  binary(Lc,":",name(Lc,Nm),Tp,St).

consType(Lc,A,Res,Tp) :- binary(Lc,"<=>",A,Res,Tp).
funType(Lc,Args,Res,Tp) :- isTuple(A,Lc,Args),binary(Lc,"=>",A,Res,Tp).
ptnType(Lc,Args,Res,Tp) :- isTuple(A,Lc,Args),binary(Lc,"<=",A,Res,Tp).
genericType(Lc,Nm,Args,Tp) :- squareTerm(Lc,Nm,Args,Tp).

bodyRule(Lc,Hd,Els,Body) :-
  isBraceTuple(Rhs,Lc,Els),
  binary(Lc,"<=>",Hd,Rhs,Body).

thingType(name(Lc,"thing"),Lc).

generateAnnotations([],_,_,Stmts,Stmts).
generateAnnotations([Def|Els],Quants,Constraints,[Annot|Stmts],S0) :-
  isBinary(Def,Lc,":",N,Tp),
  wrapConstraints(Constraints,Lc,Tp,CTp),
  wrapQuants(Quants,Lc,CTp,MTp),
  binary(Lc,":",N,MTp,Annot),
  generateAnnotations(Els,Quants,Constraints,Stmts,S0).
generateAnnotations([_|Els],Quants,Constraints,Stmts,S0) :- % ignore things like assertions
  generateAnnotations(Els,Quants,Constraints,Stmts,S0).
