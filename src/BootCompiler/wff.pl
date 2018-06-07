:-module(wff,[isAlgebraicTypeStmt/6,isConstructor/3,
    isQuantified/3,isXQuantified/3,reUQuant/3,reXQuant/3,
    isConstrained/3,reConstrain/3,
    isContractStmt/6,isImplementationStmt/6,
    isTypeExistsStmt/6,isTypeFunStmt/6,isTypeAnnotation/4,isTypeLambda/4,
    isImport/3, isMacro/3,isPrivate/3,isPublic/3,
    isDefault/3,isDefault/4,
    isIntegrity/3,isShow/3,isOpen/3,
    isConditional/5,
    isEquation/5,isDefn/4,isAssignment/4,
    isWhere/4,isCoerce/4,isFieldAcc/4,isVarRef/3,isOptionPtn/4,
    isConjunct/4,isDisjunct/4,isNegation/3,isMatch/4,isParse/4,isNTLookAhead/3,
    isLetDef/4,isMacroRule/4,
    whereTerm/4,
    packageName/2,pkgName/2,
    isComma/4,deComma/2,reComma/2,
    rewrite/3,rewriteList/3]).
:- use_module(abstract).
:- use_module(misc).

isImport(St,Lc,M) :-
  isUnary(St,Lc,"public",I),!,
  isImport(I,_,M).
isImport(St,Lc,M) :-
  isUnary(St,Lc,"private",I),!,
  isImport(I,_,M).
isImport(St,Lc,M) :-
  isUnary(St,Lc,"import",M).

isPrivate(St,Lc,I) :-
  isUnary(St,Lc,"private",I).

isPublic(St,Lc,I) :-
  isUnary(St,Lc,"public",I).

isMacro(St,Lc,M) :-
  isUnary(St,Lc,"#",M).

isTypeAnnotation(St,Lc,V,T) :-
  isBinary(St,Lc,":",V,T).

isAlgebraicTypeStmt(Stmt,Lc,Q,Cx,Head,Body) :-
  isBinary(Stmt,Lc,"::=",Lhs,Body),
  getQuantifiers(Lhs,Q,Inner),
  isConstrainedTp(Inner,Cx,Head).

isConstructor(C,Lc,Nm) :-
  isQuantified(C,_,I),
  isConstructor(I,Lc,Nm).
isConstructor(C,Lc,Nm) :-
  isXQuantified(C,_,I),
  isConstructor(I,Lc,Nm).
isConstructor(C,Lc,Nm) :-
  isIden(C,Lc,Nm).
isConstructor(C,Lc,Nm) :-
  isRound(C,Lc,Nm,_).
isConstructor(C,Lc,Nm) :-
  isBrace(C,Lc,Nm,_).

isQuantified(T,Q,B) :-
  isBinary(T,_,"~~",L,B),
  isUnary(L,_,"all",V),
  deComma(V,Q).

isXQuantified(T,Q,B) :-
  isBinary(T,_,"~~",L,B),
  isUnary(L,_,"exists",V),
  deComma(V,Q).

reUQuant([],T,T).
reUQuant(Q,T,QT) :-
  reComma(Q,V),
  locOfAst(T,Lc),
  unary(Lc,"all",V,QV),
  binary(Lc,"~~",QV,T,QT).

reXQuant([],T,T).
reXQuant(Q,T,QT) :-
  reComma(Q,V),
  locOfAst(T,Lc),
  unary(Lc,"exists",V,QV),
  binary(Lc,"~~",QV,T,QT).

getQuantifiers(T,Q,B) :- isQuantified(T,Q,B),!.
getQuantifiers(T,Q,T) :- isSquareTerm(T,_,Q), !.
getQuantifiers(T,[],T).

isContractStmt(St,Lc,Quants,Constraints,Con,Body) :-
  isUnary(St,Lc,"contract",I),
  isBinary(I,_,"::=",Lhs,B),
  isBraceTuple(B,_,Body),
  contractSpec(Lhs,Quants,Constraints,Con).

contractSpec(S,Quants,Constraints,Con) :-
  isQuantified(S,Quants,B),
  contractSpec(B,_,Constraints,Con).
contractSpec(S,[],Constraints,Con) :-
  isBinary(S,_,"|:",L,R),
  deComma(L,Constraints),
  contractSpec(R,_,_,Con).
contractSpec(S,[],[],S) :-
  isSquareTerm(S,_,_).

isImplementationStmt(St,Lc,Q,Cx,Con,Body) :-
  isUnary(St,Lc,"implementation",I),
  implSpec(I,Q,Cx,Con,Body).

implSpec(S,Quants,Constraints,Con,Body) :-
  isQuantified(S,Quants,B),
  implSpec(B,_,Constraints,Con,Body).
implSpec(S,[],Constraints,Con,Body) :-
  isBinary(S,_,"|:",L,R),
  deComma(L,Constraints),
  implSpec(R,_,_,Con,Body).
implSpec(S,[],[],Con,Body) :-
  isBinary(S,_,"=>",Con,Body),
  (isBraceTuple(Body,_,_) ; isQBraceTuple(Body,_,_)).

isConstrainedTp(T,C,R) :-
  isConstrained(T,R,C),!.
isConstrainedTp(T,[],T).

isComma(T,Lc,L,R) :-
  isBinary(T,Lc,",",L,R).

deComma(T,LL) :-
  isBinary(T,_,",",L,R),
  deComma(L,Lf),
  deComma(R,Rf),
  concat(Lf,Rf,LL).
deComma(T,[T]).

reComma([T],T).
reComma([F|M],T) :-
  reComma(M,T1),
  locOfAst(F,Lc),
  binary(Lc,",",F,T1,T).

isConstrained(Tp,T,Cx) :-
  isBinary(Tp,_,"|:",L,T),
  deComma(L,Cx).

reConstrain([],Tp,Tp).
reConstrain(Cx,T,CT) :-
  reComma(Cx,C),
  locOfAst(T,Lc),
  binary(Lc,"|:",C,T,CT).

isTypeExistsStmt(St,Lc,Q,Cx,L,R) :-
  isQuantified(St,Q,B),
  isConstrainedTp(B,Cx,Inn),
  isTypeExistsStmt(Inn,Lc,_,_,L,R).
isTypeExistsStmt(St,Lc,Q,[],T,R) :-
  isBinary(St,Lc,"<~",L,R),
  getQuantifiers(L,Q,T).

isTypeFunStmt(St,Lc,Q,C,L,R) :-
  isQuantified(St,Q,B),
  isConstrainedTp(B,C,Inn),
  isTypeFunStmt(Inn,Lc,_,_,L,R).
isTypeFunStmt(St,Lc,[],[],L,R) :-
  isBinary(St,Lc,"~>",L,R).

isTypeLambda(St,Lc,L,R) :-
  isBinary(St,Lc,"~>",L,R),
  isTuple(L,_,_).

isIntegrity(St,Lc,C) :-
  isUnary(St,Lc,"assert",C).

isDefault(St,Lc,Lhs) :-
  isUnary(St,Lc,"default",Lhs).

isDefault(St,Lc,Ptn,Val) :-
  isDefn(St,Lc,Lhs,Val),
  isUnary(Lhs,_,"default",Ptn).

isShow(St,Lc,Ex) :-
  isUnary(St,Lc,"show",Ex).

isOpen(St,Lc,Ex) :-
  isUnary(St,Lc,"open",Ex).

isConditional(Term,Lc,Cond,Th,El) :-
  isBinary(Term,Lc,"|",L,El),
  isBinary(L,_,"?",Cond,Th).

isEquation(Trm,Lc,Lhs,Cond,Rhs) :-
  isBinary(Trm,Lc,"=>",L,Rhs),
  (isWhere(L,_,Lhs,Cond) ; L=Lhs, Cond=name(Lc,"true")).

isMacroRule(Trm,Lc,Lhs,Cond) :-
  isBinary(Trm,Lc,"==>",Lhs,Cond).

isDefn(Trm,Lc,Lhs,Rhs) :-
  isBinary(Trm,Lc,"=",Lhs,Rhs).

isLetDef(Trm,Lc,Body,Exp) :-
  isBinary(Trm,Lc,"in",app(_,name(_,"let"),Body),Exp),
  (isBraceTuple(Body,_,_);isQBraceTuple(Body,_,_)),!.

isAssignment(Trm,Lc,Lhs,Rhs) :-
  isBinary(Trm,Lc,":=",Lhs,Rhs).

isWhere(Trm,Lc,Lhs,Rhs) :-
  isBinary(Trm,Lc,"where",Lhs,Rhs).

whereTerm(Lc,Lhs,Rhs,Trm) :-
  binary(Lc,"where",Lhs,Rhs,Trm).

isOptionPtn(Trm,Lc,Ptn,Opt) :-
  isBinary(Trm,Lc,"?=",Opt,Ptn),!.

isCoerce(Trm,Lc,Lhs,Rhs) :-
  isBinary(Trm,Lc,"::",Lhs,Rhs).

isConjunct(Trm,Lc,L,R) :-
  isBinary(Trm,Lc,"&&",L,R).

isDisjunct(Trm,Lc,L,R) :-
  isBinary(Trm,Lc,"||",L,R).

isNegation(Trm,Lc,L) :-
  isUnary(Trm,Lc,"\\+",L).

isMatch(Trm,Lc,P,E) :-
  isBinary(Trm,Lc,".=",P,E),!.
isMatch(Trm,Lc,P,E) :-
  isBinary(Trm,Lc,"=.",E,P).

isParse(Trm,Lc,N,E) :-
  isBinary(Trm,Lc,".~",N,E).

isFieldAcc(Trm,Lc,R,Fld) :-
  isBinary(Trm,Lc,".",R,F),
  isIden(F,Fld).

isNTLookAhead(Trm,Lc,N) :-
  isUnary(Trm,Lc,"+",N).

isVarRef(Trm,Lc,In) :-
  isUnary(Trm,Lc,"!",In).

packageName(T,Pkg) :- isIden(T,Pkg).
packageName(T,Pkg) :- isString(T,Pkg).
packageName(T,Pkg) :- isBinary(T,_,".",L,R),
  packageName(L,LP),
  packageName(R,RP),
  string_concat(LP,".",I),
  string_concat(I,RP,Pkg).

pkgName(T,pkg(Pkg,ver(Version))) :-
  isBinary(T,_,"#",L,R),
  packageName(L,Pkg),
  packageVersion(R,Version).
pkgName(T,pkg(Pkg,defltVersion)) :-
  packageName(T,Pkg).

packageVersion(T,Pkg) :- isIden(T,Pkg).
packageVersion(T,Pkg) :- isString(T,Pkg).
packageVersion(integer(_,Ix),Pkg) :- atom_string(Ix,Pkg).
packageVersion(T,Pkg) :- isBinary(T,_,".",L,R),
  packageVersion(L,LP),
  packageVersion(R,RP),
  string_concat(LP,".",I),
  string_concat(I,RP,Pkg).

rewriteList([],_,[]).
rewriteList([T|L],Q,[WT|WL]) :-
  rewrite(T,Q,WT),
  rewriteList(L,Q,WL).

rewrite(name(Lc,Nm),Q,name(Lc,WNm)) :-
  is_member((Nm,WNm),Q).
rewrite(name(Lc,Nm),_,name(Lc,Nm)).
rewrite(integer(Lc,N),_,integer(Lc,N)).
rewrite(float(Lc,N),_,float(Lc,N)).
rewrite(string(Lc,Nm),_,string(Lc,Nm)).
rewrite(tuple(Lc,Nm,Els),Q,tuple(Lc,Nm,WEls)) :-
  rewriteList(Els,Q,WEls).
rewrite(app(Lc,Op,Arg),Q,app(Lc,WOp,WArg)) :-
  rewrite(Op,Q,WOp),
  rewrite(Arg,Q,WArg).
