:-module(wff,[isAlgebraicTypeStmt/6,isQuantified/3,getQuantifiers/3,isConstrainedTp/3,
    isContractStmt/6,isImplementationStmt/6,isTypeExistsStmt/6,isTypeFunStmt/6,
    isImport/2, isMacro/2,
    isConstructorStmt/5,isGrammarRule/5,
    isIntegrity/3,isCondExpr/5,isEquation/5,isPtnRule/5,isDefn/5,isVarDefn/5,
    packageName/2,pkgName/2,sameLength/3,deComma/2,tupleize/4,
    rewrite/3,rewriteList/3]).
:-use_module(abstract).
:-use_module(misc).
:-use_module(errors).

isImport(St,M) :-
  isUnary(St,"public",I),!,
  isImport(I,M).
isImport(St,M) :-
  isUnary(St,"private",I),!,
  isImport(I,M).
isImport(St,M) :-
  isUnary(St,"import",M).

isMacro(St,M) :-
  isUnary(St,"#",M).

isAlgebraicTypeStmt(Stmt,Lc,Quants,Constraints,Head,Body) :-
  locOfAst(Stmt,Lc),
  getQuantifiers(Stmt,Quants,Inner),
  isConstrainedTp(Inner,Constraints,TpStmt),
  isBinary(TpStmt,"::=",Head,Body).

isAlgebraicTypeStmt(Term) :- isAlgebraicTypeStmt(Term,_,_,_,_,_).

isQuantified(T,V,B) :- isBinary(T,"~~",L,B), isUnary(L,"all",V).

getQuantifiers(T,LV,B) :- isQuantified(T,V,B), deComma(V,LV).
getQuantifiers(T,[],T).

isContractStmt(St,Lc,Quants,Constraints,Con,Body) :-
  isUnary(St,Lc,"contract",I),
  contractSpec(I,Quants,Constraints,Con,Body).

contractSpec(S,Quants,Constraints,Con,Body) :-
  isQuantified(S,V,B), deComma(V,Quants),
  contractSpec(B,_,Constraints,Con,Body).
contractSpec(S,[],Constraints,Con,Body) :-
  isBinary(S,"|:",L,R),
  deComma(L,Constraints),
  contractSpec(R,_,_,Con,Body).
contractSpec(S,[],[],Con,Body) :-
  isBinary(S,"::=",Con,B),
  isBraceTuple(B,_,Body).

isImplementationStmt(St,Lc,Quants,Constraints,Con,Body) :-
  isUnary(St,Lc,"implementation",I),
  implSpec(I,Quants,Constraints,Con,Body).

implSpec(S,Quants,Constraints,Con,Body) :-
  isQuantified(S,V,B), deComma(V,Quants),
  implSpec(B,_,Constraints,Con,Body).
implSpec(S,[],Constraints,Con,Body) :-
  isBinary(S,"|:",L,R),
  deComma(L,Constraints),
  implSpec(R,_,_,Con,Body).
implSpec(S,[],[],Con,Body) :-
  isBinary(S,"=>",Con,B),
  isBraceTuple(B,_,Body).

isConstrainedTp(T,C,R) :-
  isBinary(T,"|:",L,R),
  deComma(L,C).
isConstrainedTp(T,[],T).

deComma(T,LL) :-
  isBinary(T,",",L,R),
  deComma(L,Lf),
  deComma(R,Rf),
  concat(Lf,Rf,LL).
deComma(T,[T]).

isTypeExistsStmt(St,Lc,Q,Cx,L,R) :-
  isQuantified(St,V,B),
  deComma(V,Q),
  isConstrainedTp(B,Cx,Inn),
  isTypeExistsStmt(Inn,Lc,_,_,L,R).
isTypeExistsStmt(St,Lc,[],[],L,R) :-
  isBinary(St,Lc,"<~",L,R).

isTypeFunStmt(St,Lc,Q,C,L,R) :-
  isQuantified(St,V,B),
  deComma(V,Q),
  isConstrainedTp(B,C,Inn),
  isTypeFunStmt(Inn,Lc,_,_,L,R).
isTypeFunStmt(St,Lc,[],[],L,R) :-
  isBinary(St,Lc,"~>",L,R).

isConstructorStmt(St,Lc,Hd,Cond,R) :-
  isBinary(St,Lc,"<=>",H,R),
  (isBinary(H,"&&",Hd,Cond) ; H=Hd,Cond=name(Lc,"true")),!.

isIntegrity(St,Lc,C) :-
  isUnary(St,Lc,"assert",C).

isCondExpr(Term,Lc,Cond,Th,El) :-
  isBinary(Term,Lc,"|",L,El),
  isBinary(L,_,"?",Cond,Th).

isEquation(Trm,Lc,Lhs,Cond,Rhs) :-
  isBinary(Trm,Lc,"=>",L,Rhs),
  (isBinary(L,"&&",Lhs,Cond) | Lhs=L, Cond=name(Lc,"true")).

isPtnRule(Trm,Lc,Lhs,Cond,Rhs) :-
  isBinary(Trm,Lc,"<=",L,Rhs),
  (isBinary(L,"&&",Lhs,Cond) | Lhs=L, Cond=name(Lc,"true")).

isGrammarRule(Trm,Lc,Lhs,PushBack,Rhs) :-
  isBinary(Trm,Lc,"-->",L,Rhs),
  (isBinary(L,",",Lhs,PushBack) | Lhs=L, PushBack=tuple(Lc,"[]",[])).

isDefn(Trm,Lc,Lhs,Cond,Rhs) :-
  isBinary(Trm,Lc,"=",L,Rhs),
  (isBinary(L,"&&",Lhs,Cond) | Lhs=L, Cond=name(Lc,"true")).

isVarDefn(Trm,Lc,Lhs,Cond,Rhs) :-
  isBinary(Trm,Lc,":=",L,Rhs),
  (isBinary(L,"&&",Lhs,Cond) | Lhs=L, Cond=name(Lc,"true")).

packageName(T,Pkg) :- isIden(T,Pkg).
packageName(T,Pkg) :- isString(T,Pkg).
packageName(T,Pkg) :- isBinary(T,".",L,R),
  packageName(L,LP),
  packageName(R,RP),
  string_concat(LP,".",I),
  string_concat(I,RP,Pkg).

pkgName(T,pkg(Pkg,v(Version))) :-
  isBinary(T,"#",L,R),
  packageName(L,Pkg),
  packageVersion(R,Version).
pkgName(T,pkg(Pkg,defltVersion)) :-
  packageName(T,Pkg).

packageVersion(T,Pkg) :- isIden(T,Pkg).
packageVersion(T,Pkg) :- isString(T,Pkg).
packageVersion(integer(_,Ix),Pkg) :- atom_string(Ix,Pkg).
packageVersion(T,Pkg) :- isBinary(T,".",L,R),
  packageVersion(L,LP),
  packageVersion(R,RP),
  string_concat(LP,".",I),
  string_concat(I,RP,Pkg).

sameLength(L1,L2,_) :- length(L1,L), length(L2,L),!.
sameLength(L1,_,Lc) :-
  length(L1,L),
  reportError("expecting %s elements",[L],Lc).

tupleize(app(_,name(_,","),tuple(_,"()",[L,R])), Lc, Op, tuple(Lc,Op,[L|Rest])) :-
    getTupleArgs(R,Rest).
tupleize(T,Lc,Op,tuple(Lc,Op,[T])).

getTupleArgs(app(_,name(_,","),tuple(_,"()",[L,R])), [L|Rest]) :-
    getTupleArgs(R,Rest).
getTupleArgs(T,[T]).

rewriteList([],_,[]).
rewriteList([T|L],Q,[WT|WL]) :-
  rewrite(T,Q,WT),
  rewriteList(L,Q,WL).

rewrite(name(Lc,Nm),Q,name(Lc,WNm)) :- is_member((Nm,WNm),Q).
rewrite(name(Lc,Nm),_,name(Lc,Nm)).
rewrite(integer(Lc,N),_,integer(Lc,N)).
rewrite(float(Lc,N),_,float(Lc,N)).
rewrite(string(Lc,Nm),_,string(Lc,Nm)).
rewrite(tuple(Lc,Nm,Els),Q,tuple(Lc,Nm,WEls)) :- rewriteList(Els,Q,WEls).
rewrite(app(Lc,Op,Arg),Q,app(Lc,WOp,WArg)) :- rewrite(Op,Q,WOp), rewrite(Arg,Q,WArg).
