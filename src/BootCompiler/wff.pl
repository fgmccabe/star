:-module(wff,[isAlgebraicTypeStmt/6,isConstructor/3,
	      isQuantified/3,isXQuantified/3,reUQuant/3,reXQuant/3,
	      isConstrained/3,reConstrain/3,
	      isContractStmt/6,isImplementationStmt/6,
	      isTypeExistsStmt/6,isTypeFunStmt/6,isTypeAnnotation/4,
	      isTypeLambda/4,
	      isImport/3, isMacro/3,isPrivate/3,isPublic/3,
	      isDefault/3,isDefault/4,
	      isIntegrity/3,isShow/3,isOpen/3,
	      isConditional/5,conditional/5,isOfTerm/4,
	      isEquation/5,isDefn/4,isAssignment/4,eqn/5,
	      isCurriedRule/5,ruleHead/4,
	      isWhere/4,isCoerce/4,
	      isFieldAcc/4,isVarRef/3,
	      isOptionPtn/4,isOptionMatch/4,optionMatch/4,
	      isConjunct/4,isDisjunct/4,
	      isForall/4,isNegation/3,isMatch/4,isSearch/4,isIxSearch/5,
	      isAbstraction/4,isListAbstraction/4,
	      isParseTerm/3,isNTLookAhead/3,
	      isDoTerm/3,isDoTerm/2,isDoTerm/1,isTaskTerm/3,isActionTerm/3,
	      isBind/4,isValof/3,isThrow/3,isReturn/3,isHandle/4,isTryCatch/4,
	      isIfThenElse/5,isIfThen/4,isWhileDo/4,isForDo/4,
	      isActionSeq/4,isActionSeq/3,
	      isLetDef/4,mkLetDef/4,
	      whereTerm/4,
	      packageName/2,pkgName/2,
	      isComma/4,deComma/2,reComma/2,
	      mergeCond/4,
	      findVars/3]).
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
  isBinary(S,_,"=>",Con,Body).

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

isConditional(Term,Lc,Tst,Th,El) :-
  isBinary(Term,Lc,"?",Tst,Rhs),
  isBinary(Rhs,_,"||",Th,El).

conditional(Lc,Tst,Th,El,Cond) :-
  binary(Lc,"||",Th,El,Rhs),
  binary(Lc,"?",Tst,Rhs,Cond).

isOfTerm(Term,Lc,Lbl,R) :-
  isBinary(Term,Lc,"of",Lbl,R),
  isSquareTuple(R,_,_),
  isIden(Lbl,_,_).

ruleHead(Trm,Hd,Cond,IsDeflt) :-
  isTuple(Trm,_,[A]),!,
  ruleHead(A,Hd,Cond,IsDeflt).
ruleHead(Trm,Hd,Cond,IsDeflt) :-
  isWhere(Trm,_,Lhs,Cond),!,
  ruleHead(Lhs,Hd,_,IsDeflt).
ruleHead(Trm,Hd,Cond,true) :-
  isDefault(Trm,_,H),!,
  ruleHead(H,Hd,Cond,_).
ruleHead(Trm,Trm,name(Lc,"true"),false) :-
  isRound(Trm,Lc,_,_).

mWhere(_,Hd,name(_,"true"),Hd) :- !.
mWhere(Lc,H,Cond,Hd) :-
  binary(Lc,"where",H,Cond,Hd).

isEquation(Trm,Lc,Lhs,Cond,Rhs) :-
  isBinary(Trm,Lc,"=>",L,Rhs),
  (isWhere(L,_,Lhs,Cond) ; L=Lhs, Cond=name(Lc,"true")).

eqn(Lc,Lhs,name(_,"true"),Rhs,Eqn) :-!,
  binary(Lc,"=>",Lhs,Rhs,Eqn).
eqn(Lc,Args,Cond,Rhs,Eqn) :-
  whereTerm(Lc,Args,Cond,Lhs),
  binary(Lc,"=>",Lhs,Rhs,Eqn).

% refactor f(A)(B) where C => D to f(A) where C => (B)=>D
isCurriedRule(St,Lc,Op,Cond,Body) :-
  isBinary(St,Lc,"=>",L,R),!,
  ruleHead(L,H,Cond,_),
  isRound(H,_,Op,Args),
  isRound(Op,_,_,_),
  eqn(Lc,tuple(Lc,"()",Args),name(Lc,"true"),R,Body).

isDefn(Trm,Lc,Lhs,Rhs) :-
  isBinary(Trm,Lc,"=",Lhs,Rhs).

isLetDef(Trm,Lc,Body,Exp) :-
  isBinary(Trm,Lc,"in",app(_,name(_,"let"),Body),Exp),
  (isBraceTuple(Body,_,_);isQBraceTuple(Body,_,_)),!.

mkLetDef(Lc,Els,Bnd,Let) :-
  braceTerm(Lc,name(Lc,"let"),Els,Body),
  binary(Lc,"in",Body,Bnd,Let).

isAssignment(Trm,Lc,Lhs,Rhs) :-
  isBinary(Trm,Lc,":=",Lhs,Rhs).

isWhere(Trm,Lc,Lhs,Rhs) :-
  isBinary(Trm,Lc,"where",Lhs,Rhs).

whereTerm(Lc,Lhs,Rhs,Trm) :-
  binary(Lc,"where",Lhs,Rhs,Trm).

isOptionPtn(Trm,Lc,Ptn,Opt) :-
  isBinary(Trm,Lc,"^",Opt,Ptn),!.

isOptionMatch(Trm,Lc,Ptn,Vl) :-
  isBinary(Trm,Lc,"^=",Ptn,Vl).

optionMatch(Lc,Ptn,Exp,Term) :-
  binary(Lc,"^=",Ptn,Exp,Term).

isCoerce(Trm,Lc,Lhs,Rhs) :-  isBinary(Trm,Lc,"::",Lhs,Rhs).

isConjunct(Trm,Lc,L,R) :-
  isBinary(Trm,Lc,"&&",L,R).

isDisjunct(Trm,Lc,L,R) :-
  isBinary(Trm,Lc,"||",L,R).

isNegation(Trm,Lc,L) :-
  isUnary(Trm,Lc,"\\+",L).

isForall(Trm,Lc,L,R) :-
  isBinary(Trm,Lc,"*>",L,R).

isMatch(Trm,Lc,P,E) :-
  isBinary(Trm,Lc,".=",P,E),!.
isMatch(Trm,Lc,P,E) :-
  isBinary(Trm,Lc,"=.",E,P).

isIxSearch(Trm,Lc,Ky,Pt,E) :-
  isBinary(Trm,Lc,"in",L,E),
  isBinary(L,_,"->",Ky,Pt),!.

isSearch(Trm,Lc,P,E) :-
  isBinary(Trm,Lc,"in",P,E),
  \+isBinary(P,_,"->",_,_),!.

isAbstraction(Trm,Lc,Bnd,Body) :-
  isBraceTuple(Trm,Lc,[T]),
  isBinary(T,_,"|",Bnd,Body).

isListAbstraction(Trm,Lc,Bnd,Body) :-
  isSquareTuple(Trm,Lc,[T]),
  isBinary(T,_,"|",Bnd,Body).

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

headName(H,Nm) :-
  isBinary(H,_,"^",L,_),
  headName(L,Nm).
headName(H,Nm) :-
  isIden(H,_,Nm).
headName(H,Nm) :-
  isRoundTerm(H,N,_),
  isIden(N,_,Nm).

findVars(name(Lc,V),SoFar,Vrs) :-
  is_member(name(_,V),SoFar) -> Vrs=SoFar ; Vrs = [name(Lc,V)|SoFar].
findVars(app(_,_,Args),SoFar,Vrs) :-
  findVars(Args,SoFar,Vrs).
findVars(tuple(_,_,Els),SoFar,Vrs) :-
  rfold(Els,wff:findVars,SoFar,Vrs).
findVars(integer(_,_),Vrs,Vrs).
findVars(float(_,_),Vrs,Vrs).
findVars(string(_,_),Vrs,Vrs).

mergeCond(name(_,"true"),R,_,R) :-!.
mergeCond(L,name(_,"true"),_,L) :-!.
mergeCond(L,R,Lc,Cnd) :-
  binary(Lc,"&&",L,R,Cnd).

isDoTerm(A,Lc,Stmts) :-
  isUnary(A,Lc,"do",R),
  isBraceTuple(R,_,[Stmts]),!.

isDoTerm(A,Lc) :-
  isUnary(A,Lc,"do",_),!.

isDoTerm(A) :-
  isUnary(A,_,"do",_),!.

isActionTerm(A,Lc,Stmts) :-
  isBrace(A,Lc,"action",[Stmts]).

isTaskTerm(A,Lc,Stmts) :-
  isBrace(A,Lc,"task",[Stmts]).

isBind(T,Lc,B,E) :-
  isBinary(T,Lc,"<-",B,E),!.

isReturn(A,Lc,E) :-
  isUnary(A,Lc,"return",E).

isValof(A,Lc,E) :-
  isUnary(A,Lc,"valof",E).

isThrow(A,Lc,E) :-
  isUnary(A,Lc,"throw",E).

isTryCatch(A,Lc,B,H) :-
  isUnary(A,Lc,"try",I),
  isBinary(I,_,"catch",B,H).

isHandle(A,Lc,L,R) :-
  isBinary(A,Lc,">>>",L,R).

isIfThenElse(A,Lc,Ts,Th,El) :-
  isBinary(A,Lc,"else",Lhs,El),!,
  isBinary(Lhs,_,"then",LL,Th),
  isUnary(LL,_,"if",Ts).

isIfThen(A,Lc,Ts,Th) :-
  isBinary(A,Lc,"then",LL,Th),!,
  isUnary(LL,_,"if",Ts).

isWhileDo(A,Lc,Ts,Bd) :-
  isBinary(A,Lc,"do",LL,Bd),
  isUnary(LL,_,"while",Ts),!.

isForDo(A,Lc,Ts,Bd) :-
  isBinary(A,Lc,"do",LL,Bd),
  isUnary(LL,_,"for",Ts),!.

isActionSeq(A,Lc,S1,S2) :-
  isBinary(A,Lc,";",S1,S2).
isActionSeq(A,Lc,S) :-
  isUnary(A,Lc,";",S).

isParseTerm(A,Lc,Stmt) :-
  isApply(A,Lc,name(_,"prse"),Args),
  isBraceTuple(Args,_,[Stmt]).
