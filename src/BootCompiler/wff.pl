:-module(wff,[isAnnotation/4,isQuote/3,
	      isAlgebraicTypeStmt/6,mkAlgebraicTypeStmt/6,
	      isConstructor/3,
	      isConstructorType/6,constructorType/6,
	      isRoundCon/6,isBraceCon/6,
	      isQuantified/3,isXQuantified/3,reUQuant/3,reXQuant/3,
	      isConstrained/3,reConstrain/3,
	      isContractStmt/6,contractStmt/6,
	      isImplementationStmt/6,implementationStmt/6,
	      implementedContractName/2,
	      isTypeExists/4,typeExists/4,
	      isTypeExistsStmt/6,typeExistsStmt/6,isTypeFunStmt/6,typeFunStmt/6,
	      isTypeAnnotation/4,typeAnnotation/4,isTypeField/4,mkTypeField/4,
	      isTypeLambda/4,typeLambda/4,typeName/2,
	      isFuncType/4,funcType/4,isContType/4,mkContType/4,
	      mkSqType/4,
	      isEnum/3,mkEnum/3,isAnon/2,mkAnon/2,
	      isImport/3, isPrivate/3,isPublic/3,mkPrivate/3,mkPublic/3,
	      isDefault/3,isDefault/4,mkDefault/3,
	      isLiteralInteger/3,isLiteralFloat/3,isLiteralBigInt/3,
	      isIntegrity/3,isShow/3,isOpen/3,mkOpen/3,
	      isConditional/5,conditional/5,
	      isEquation/4,isEquation/5,mkEquation/5,
	      buildEquation/6,
	      isDefn/4,isAssignment/4,isRef/3,mkRef/3,isCellRef/3,cellRef/3,
	      isSequence/4,mkSequence/4,
	      assignment/4,eqn/4,eqn/5,
	      mkDefn/4,mkLoc/2,
	      isGl/1,isIterableGl/1,
	      ruleName/3,headName/2,
	      isWhere/4,mkWhere/4,mkWherePtn/4,mkWhereEquality/3,
	      isCoerce/4,coerce/4,isOptCoerce/4,optCoerce/4,
	      isFieldAcc/4,fieldAcc/4,isIndexTerm/4,mkIndexTerm/4,
	      isRecordUpdate/5,recordUpdate/5,
	      isSlice/5,isSplice/6,
	      isOptionMatch/4,optionMatch/4,
	      isConjunct/4,conjunct/4,isDisjunct/4,disjunct/4,
	      isForall/4,mkForall/4,isNegation/3,negation/3,
	      isMatch/4,match/4,isSearch/4,search/4,isBind/4,mkBind/4,
	      isMapLiteral/3,mkMapLiteral/3,
	      isComprehension/4,mkComprehension/4,
	      isIotaComprehension/4,
	      isTestComprehension/3,mkTestComprehension/3,
	      isCaseExp/4,caseExp/4,
	      isSuspend/4,isSuspend/5,isResume/5,isRetire/3,isRetire/4,
	      mkSuspend/4,mkSuspend/5,mkResume/5,mkRetire/3,mkRetire/4,
	      isTaskTerm/3,mkTaskTerm/3,isActionTerm/3,mkActionTerm/3,
	      isResultTerm/3,mkResultTerm/3,
	      isDoTerm/3,mkDoTerm/3,
	      isValof/3,mkValof/3,isPerform/3,mkPerform/3,
	      isRaise/3,mkRaise/3,isThrow/3,mkThrow/3,isValis/3,mkValis/3,
	      isIgnore/3,mkIgnore/3,
	      isTryCatch/4,mkTryCatch/4,
	      isTryHandle/4,mkTryHandle/4,
	      isIfThenElse/5,isIfThen/4,mkIfThenElse/5,mkIfThen/4,
	      isWhileDo/4,isUntilDo/4,isForDo/4,isForDo/5,
	      mkWhileDo/4,mkUntilDo/4,mkForDo/5,
	      isActionSeq/4,isActionSeq/3,mkActionSeq/4,
	      isLetDef/4,isLetRec/4,mkLetDef/4,mkLetRec/4,
	      whereTerm/4,
	      packageName/2,pkgName/2,
	      collectImports/3,
	      isComma/4,comma/4,deComma/2,reComma/2,
	      isCons/4,mkCons/4,
	      isPair/4,pair/4,
	      isUnaryMinus/3,
	      unitTpl/2,
	      dlName/2,dtName/2]).
:- use_module(abstract).
:- use_module(misc).
:- use_module(operators).

isAnnotation(St,Lc,none,A) :-
  isUnary(St,Lc,"@",A).
isAnnotation(St,Lc,some(L),R) :-
  isBinary(St,Lc,"@",L,R).

isImport(St,Lc,M) :-
  isUnary(St,Lc,"public",I),!,
  isImport(I,_,M).
isImport(St,Lc,M) :-
  isUnary(St,Lc,"private",I),!,
  isImport(I,_,M).
isImport(St,Lc,M) :-
  isUnary(St,Lc,"import",M).

findImport(St,_,Spec) :-
  isPrivate(St,_Lc,I),!,
  findImport(I,private,Spec).
findImport(St,_,Spec) :-
  isPublic(St,_,I),!,
  findImport(I,public,Spec).
findImport(St,Viz,importPk(Lc,Viz,Pkg)) :-
  isImport(St,Lc,P),!,
  pkgName(P,Pkg).

collectImports([],[],[]) :-!.
collectImports([A|As],[Spec|I],Oth) :-
  findImport(A,private,Spec),!,
  collectImports(As,I,Oth).
collectImports([A|As],I,[A|Oth]) :-
  collectImports(As,I,Oth).

isPrivate(St,Lc,I) :-
  isUnary(St,Lc,"private",I).

isPublic(St,Lc,I) :-
  isUnary(St,Lc,"public",I).

mkPrivate(Lc,E,Ex) :-
  unary(Lc,"private",E,Ex).

mkPublic(Lc,E,Ex) :-
  unary(Lc,"public",E,Ex).

isTypeAnnotation(St,Lc,V,T) :-
  isBinary(St,Lc,":",V,T),!.

typeAnnotation(Lc,V,T,St) :-
  binary(Lc,":",V,T,St).

isAlgebraicTypeStmt(Stmt,Lc,Q,Cx,Head,Body) :-
  isBinary(Stmt,Lc,"::=",Lhs,Body),
  getQuantifiers(Lhs,Q,Inner),
  isConstrainedTp(Inner,Cx,Head).

mkAlgebraicTypeStmt(Lc,Q,Cx,Head,Body,S) :-
  reConstrain(Cx,Head,H0),
  reUQuant(Q,H0,H1),
  binary(Lc,"::=",H1,Body,S).

isConstructor(C,Lc,Nm) :-
  isQuantified(C,_,I),
  isConstructor(I,Lc,Nm).
isConstructor(C,Lc,Nm) :-
  isXQuantified(C,_,I),
  isConstructor(I,Lc,Nm).
isConstructor(C,Lc,Nm) :-
  isIden(C,Lc,Nm).
isConstructor(C,Lc,Nm) :-
  isEnum(C,Lc,E),
  isIden(E,_,Nm).
isConstructor(C,Lc,Nm) :-
  isRound(C,Lc,Nm,_).
isConstructor(C,Lc,Nm) :-
  isBrace(C,Lc,Nm,_).

isRoundCon(C,XQ,XC,Lc,Nm,Els) :-
  isCon(C,abstract:isRoundTerm,XQ,XC,Lc,Nm,Els).

isBraceCon(C,XQ,XC,Lc,Nm,Els) :-
  isCon(C,abstract:isBraceTerm,XQ,XC,Lc,Nm,Els).

isCon(C,Tst,[],[],Lc,Nm,Els) :-
  call(Tst,C,Lc,N,Els),
  isIden(N,Nm).
isCon(C,Tst,XQ,XC,Lc,Nm,Els) :-
  isXQuantified(C,XQ,I),
  isCon(I,Tst,_,XC,Lc,Nm,Els).
isCon(C,Tst,XQ,XC,Lc,Nm,Els) :-
  isConstrained(C,I,XC),
  isCon(I,Tst,XQ,_,Lc,Nm,Els).

isConstructorType(A,Lc,U,X,L,R) :-
  isQuantified(A,U,I),!,
  isConstructorType(I,Lc,_U,X,L,R).
isConstructorType(A,Lc,U,X,L,R) :-
  isXQuantified(A,X,I),!,
  isConstructorType(I,Lc,U,_X,L,R).
isConstructorType(A,Lc,[],[],L,R) :-
  isBinary(A,Lc,"<=>",L,R).

constructorType(Lc,U,X,L,R,Tp) :-
  binary(Lc,"<=>",L,R,T0),
  reXQuant(X,T0,T1),
  reUQuant(U,T1,Tp).

isEnum(C,Lc,Id) :-
  isUnary(C,Lc,".",Id).

mkEnum(Lc,Id,E) :-
  unary(Lc,".",name(Lc,Id),E).

isAnon(name(Lc,"_"),Lc).

mkAnon(Lc,name(Lc,"_")).

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
  isContractSpec(Lhs,Quants,Constraints,Con).

contractStmt(Lc,Q,C,T,B,St) :-
  contractSpec(Lc,Q,C,T,Spec),
  braceTuple(Lc,B,Body),
  binary(Lc,"::=",Spec,Body,S0),
  unary(Lc,"contract",S0,St).

isContractSpec(S,Quants,Constraints,Con) :-
  isQuantified(S,Quants,B),
  isContractSpec(B,_,Constraints,Con).
isContractSpec(S,[],Constraints,Con) :-
  isBinary(S,_,"|:",L,R),
  deComma(L,Constraints),
  isContractSpec(R,_,_,Con).
isContractSpec(S,[],[],S) :-
  isSquareTerm(S,_,_).

contractSpec(_Lc,Q,C,Tp,Spec) :-
  reConstrain(C,Tp,T0),
  reUQuant(Q,T0,Spec).

isImplementationStmt(St,Lc,Q,Cx,Con,Body) :-
  isUnary(St,Lc,"implementation",I),
  isImplSpec(I,Q,Cx,Con,Body).

isImplSpec(S,Quants,Constraints,Con,Body) :-
  isQuantified(S,Quants,B),
  isImplSpec(B,_,Constraints,Con,Body).
isImplSpec(S,[],Constraints,Con,Body) :-
  isBinary(S,_,"|:",L,R),
  deComma(L,Constraints),
  isImplSpec(R,_,_,Con,Body).
isImplSpec(S,[],[],Con,Body) :-
  isBinary(S,_,"=>",Con,Body).

implementationStmt(Lc,Q,Cx,Con,Body,St) :-
  binary(Lc,"=>",Con,Body,S0),
  reConstrain(Cx,S0,S1),
  reUQuant(Q,S1,S2),
  unary(Lc,"implementation",S2,St).

implementedContractName(Sq,INm) :-
  isSquare(Sq,Nm,A),
  appStr(Nm,S0,S1),
  marker(over,M),
  surfaceNames(A,M,S1,[]),
  string_chars(INm,S0).

surfaceNames([],_,S,S).
surfaceNames([T|_],Sep,S0,Sx) :-
  isBinary(T,_,"->>",L,_),!,
  deComma(L,Els),
  surfaceNames(Els,Sep,S0,Sx).
surfaceNames([T|L],Sep,S0,Sx) :-
  surfaceName(T,SN),
  appStr(Sep,S0,S1),
  appStr(SN,S1,S2),
  surfaceNames(L,Sep,S2,Sx).

surfaceName(N,Nm) :-
  isIden(N,Nm).
surfaceName(N,Nm) :-
  isSquare(N,Nm,_).
surfaceName(T,Nm) :-
  isQuantified(T,_,B),
  surfaceName(B,Nm).
surfaceName(T,Nm) :-
  isTypeLambda(T,_,_,Rhs),
  surfaceName(Rhs,Nm).
surfaceName(T,Nm) :-
  isTuple(T,_,A),
  length(A,Ar),
  swritef(Nm,"()%d",[Ar]).

isConstrainedTp(T,C,R) :-
  isConstrained(T,R,C),!.
isConstrainedTp(T,[],T).

isFuncType(T,Lc,Lh,Rh) :-
  isBinary(T,Lc,"=>",Lh,Rh).

funcType(Lc,L,R,Tp) :-
  binary(Lc,"=>",L,R,Tp).

isContType(T,Lc,Lh,Rh) :-
  isBinary(T,Lc,"=>>",Lh,Rh).

mkContType(Lc,L,R,Tp) :-
  binary(Lc,"=>>",L,R,Tp).

mkSqType(Lc,Nm,Els,Tp) :-
  squareTerm(Lc,name(Lc,Nm),Els,Tp).

isComma(T,Lc,L,R) :-
  isBinary(T,Lc,",",L,R).

comma(Lc,L,R,T) :-
  binary(Lc,",",L,R,T).

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

isCons(T,Lc,L,R) :-
  isBinary(T,Lc,",..",L,R).

mkCons(Lc,L,R,T) :-
  binary(Lc,",..",L,R,T).

isPair(T,Lc,L,R) :-
  isBinary(T,Lc,"->",L,R).

pair(Lc,L,R,T) :-
  binary(Lc,"->",L,R,T).

isConstrained(Tp,T,Cx) :-
  isBinary(Tp,_,"|:",L,T),
  deComma(L,Cx).

reConstrain([],Tp,Tp).
reConstrain(Cx,T,CT) :-
  reComma(Cx,C),
  locOfAst(T,Lc),
  binary(Lc,"|:",C,T,CT).

isUnaryMinus(T,Lc,A) :-
  isUnary(T,Lc,"-",A).

isTypeExists(A,Lc,L,R) :-
  isBinary(A,Lc,"<~",L,R).

typeExists(Lc,L,R,A) :-
  binary(Lc,"<~",L,R,A).

isTypeExistsStmt(St,Lc,Q,Cx,L,R) :-
  isQuantified(St,Q,B),
  isConstrainedTp(B,Cx,Inn),
  isTypeExistsStmt(Inn,Lc,_,_,L,R).
isTypeExistsStmt(St,Lc,Q,[],T,R) :-
  isBinary(St,Lc,"<~",L,R),
  getQuantifiers(L,Q,T).

typeExistsStmt(Lc,Q,C,L,R,S) :-
  binary(Lc,"<~",L,R,S0),
  reConstrain(C,S0,S1),
  reUQuant(Q,S1,S).

isTypeFunStmt(St,Lc,Q,C,L,R) :-
  isQuantified(St,Q,B),
  isConstrainedTp(B,C,Inn),
  isTypeFunStmt(Inn,Lc,_,_,L,R).
isTypeFunStmt(St,Lc,[],[],L,R) :-
  isBinary(St,Lc,"~>",L,R).

typeFunStmt(Lc,Q,C,L,R,S) :-
  binary(Lc,"~>",L,R,S0),
  reConstrain(C,S0,S1),
  reUQuant(Q,S1,S).

isTypeField(St,Lc,L,R) :-
  isUnary(St,Lc,"type",Lhs),
  isBinary(Lhs,_,":",L,R).

mkTypeField(Lc,L,R,St) :-
  binary(Lc,":",L,R,S0),
  unary(Lc,"type",S0,St).

typeName(Tp,Nm) :-
  isBinary(Tp,_,"|:",_,R),
  typeName(R,Nm).
typeName(Tp,Nm) :- isSquare(Tp,Nm,_), \+ isKeyword(Nm).
typeName(Tp,Nm) :- isName(Tp,Nm), \+ isKeyword(Nm).
typeName(Tp,"=>") :- isBinary(Tp,_,"=>",_,_).
typeName(Tp,Nm) :- isTuple(Tp,_,A),
  length(A,Ar),
  swritef(Nm,"()%d",[Ar]).

isTypeLambda(St,Lc,L,R) :-
  isBinary(St,Lc,"~>",L,R),
  isTuple(L,_,_).

typeLambda(Lc,L,R,T) :-
  binary(Lc,"~>",L,R,T).

isIntegrity(St,Lc,C) :-
  isUnary(St,Lc,"assert",C).

isDefault(St,Lc,Lhs) :-
  isUnary(St,Lc,"default",Lhs).

isDefault(St,Lc,Ptn,Val) :-
  isDefn(St,Lc,Lhs,Val),
  isUnary(Lhs,_,"default",Ptn).

mkDefault(Lc,I,T) :-
  unary(Lc,"default",I,T).

isShow(St,Lc,Ex) :-
  isUnary(St,Lc,"show",Ex).

isOpen(St,Lc,Ex) :-
  isUnary(St,Lc,"open",Ex).

mkOpen(Lc,I,Trm) :-
  unary(Lc,"open",I,Trm).

isConditional(Term,Lc,Tst,Th,El) :-
  isBinary(Term,Lc,"?",Tst,Rhs),
  isBinary(Rhs,_,"||",Th,El).

conditional(Lc,Tst,Th,El,Cond) :-
  binary(Lc,"||",Th,El,Rhs),
  binary(Lc,"?",Tst,Rhs,Cond).

ruleName(St,var(Nm),value) :-
  headOfRule(St,Hd),
  headName(Hd,Nm).

headOfRule(St,Hd) :-
  isDefn(St,_,Hd,_),!.
headOfRule(St,Hd) :-
  isAssignment(St,_,Hd,_),!.
headOfRule(St,Hd) :-
  isEquation(St,_,Hd,_,_),!.

headName(Head,Nm) :-
  isRoundTerm(Head,Op,_),
  headName(Op,Nm).
headName(Head,Nm) :-
  isBrace(Head,_,Nm,_).
headName(Name,Nm) :-
  isIden(Name,Nm),
  \+isKeyword(Nm).
headName(tuple(_,"()",[Name]),Nm) :-
  headName(Name,Nm).
headName(Head,Nm) :-
  isDefault(Head,_,Lhs),
  headName(Lhs,Nm).

isEquation(Trm,Lc,Lhs,Cond,Rhs) :-
  isBinary(Trm,Lc,"=>",L,Rhs),
  (isWhere(L,_,Lhs,G), Cond=some(G) ; L=Lhs, Cond=none).

mkEquation(Lc,Lhs,none,Rhs,Eqn) :-
  binary(Lc,"=>",Lhs,Rhs,Eqn).
mkEquation(Lc,Args,some(G),Rhs,Eqn) :-
  whereTerm(Lc,Args,G,Lhs),
  binary(Lc,"=>",Lhs,Rhs,Eqn).
  
isEquation(Trm,Lc,Lhs,Rhs) :-
  isBinary(Trm,Lc,"=>",Lhs,Rhs).

buildEquation(Lc,Nm,Args,Cond,Exp,Eqn) :-
  roundTerm(Lc,Nm,Args,T),
  mkEquation(Lc,T,Cond,Exp,Eqn).
  
eqn(Lc,Args,Cond,Rhs,Eqn) :-
  whereTerm(Lc,Args,Cond,Lhs),
  binary(Lc,"=>",Lhs,Rhs,Eqn).
eqn(Lc,Lhs,Rhs,Eqn) :-
  binary(Lc,"=>",Lhs,Rhs,Eqn).

isDefn(Trm,Lc,Lhs,Rhs) :-
  isBinary(Trm,Lc,"=",Lhs,Rhs).

mkDefn(Lc,L,R,Trm) :-
  binary(Lc,"=",L,R,Trm).

isLetDef(Trm,Lc,Els,Exp) :-
  isBinary(Trm,Lc,"in",app(_,name(_,"let"),Body),Exp),
  isBraceTuple(Body,_,Els),!.

isLetRec(Trm,Lc,Els,Exp) :-
  isBinary(Trm,Lc,"in",app(_,name(_,"let"),Body),Exp),
  isQBraceTuple(Body,_,Els),!.

mkLetDef(Lc,Els,Bnd,Let) :-
  braceTerm(Lc,name(Lc,"let"),Els,Body),
  binary(Lc,"in",Body,Bnd,Let).

mkLetRec(Lc,Els,Bnd,Let) :-
  qbraceTerm(Lc,name(Lc,"let"),Els,Body),
  binary(Lc,"in",Body,Bnd,Let).

isCaseExp(Trm,Lc,Exp,Cases) :-
  isUnary(Trm,Lc,"case",L),
  isBinary(L,_,"in",Exp,R),
  isBraceTuple(R,_,Cases).

caseExp(Lc,Exp,Cases,Trm) :-
  braceTuple(Lc,Cases,R),
  binary(Lc,"in",Exp,R,C0),
  unary(Lc,"case",C0,Trm).

isAssignment(Trm,Lc,Lhs,Rhs) :-
  isBinary(Trm,Lc,":=",Lhs,Rhs).

isRef(Trm,Lc,Rhs) :-
  isUnary(Trm,Lc,"ref",Rhs).

mkRef(Lc,Rhs,Trm) :-
  unary(Lc,"ref",Rhs,Trm).

isCellRef(Trm,Lc,Rhs) :-
  isUnary(Trm,Lc,"!",Rhs).

cellRef(Lc,Rhs,Trm) :-
  unary(Lc,"!",Rhs,Trm).

assignment(Lc,Lhs,Rhs,Stmt) :-
  binary(Lc,":=",Lhs,Rhs,Stmt).

isWhere(Trm,Lc,Lhs,Rhs) :-
  isBinary(Trm,Lc,"where",Lhs,Rhs).

mkWhere(Lc,L,R,T) :-
  binary(Lc,"where",L,R,T).

mkWherePtn(Lc,Ptn,Ex,Ptrn) :-
  genIden(Lc,V), % create a new variable
  nary(Lc,Ex,[V],Cl), % call pattern generator
  unary(Lc,"some",Ptn,Lhs),
  binary(Lc,".=",Lhs,Cl,Test), % Ex(V)=.some(Ptn)
  binary(Lc,"where",V,Test,Ptrn).

mkWhereEquality(Lc,name(ILc,V),Ptrn) :-
  genIden(Lc,V,VV),
  binary(Lc,"==",VV,name(ILc,V),Test),
  binary(Lc,"where",VV,Test,Ptrn).

whereTerm(_,Lhs,Cond,Lhs) :-
  isEnum(Cond,_,name(_,"true")),!.
whereTerm(Lc,Lhs,Rhs,Trm) :-
  binary(Lc,"where",Lhs,Rhs,Trm).

isOptionMatch(Trm,Lc,Ptn,Vl) :-
  isBinary(Trm,Lc,"^=",Ptn,Vl).

optionMatch(Lc,Ptn,Exp,Term) :-
  binary(Lc,"^=",Ptn,Exp,Term).

isCoerce(Trm,Lc,Lhs,Rhs) :-  isBinary(Trm,Lc,"::",Lhs,Rhs).

coerce(Lc,Lhs,Rhs,Trm) :- binary(Lc,"::",Lhs,Rhs,Trm).

isOptCoerce(Trm,Lc,Lhs,Rhs) :-  isBinary(Trm,Lc,":?",Lhs,Rhs).

optCoerce(Lc,Lhs,Rhs,Trm) :- binary(Lc,":?",Lhs,Rhs,Trm).

isSequence(Trm,Lc,Lhs,Rhs) :-
  isBinary(Trm,Lc,";",Lhs,Rhs).

mkSequence(Lc,Lhs,Rhs,Trm) :-
  binary(Lc,";",Lhs,Rhs,Trm).

isLiteralInteger(integer(Lc,Ix),Lc,Ix) :-!.
isLiteralInteger(I,Lc,Nx) :-
  isUnary(I,Lc,"-",integer(_,Ix)),!,
  Nx is -Ix.

isLiteralFloat(float(Lc,Dx),Lc,Dx) :-!.
isLiteralFloat(T,Lc,Nx) :-
  isUnary(T,Lc,"-",float(_,Dx)),!,
  Nx is -Dx.

isLiteralBigInt(bigint(Lc,Ix),Lc,Ix) :-!.

isConjunct(Trm,Lc,L,R) :-
  isBinary(Trm,Lc,"&&",L,R).

conjunct(Lc,L,R,Trm) :-
  binary(Lc,"&&",L,R,Trm).

isDisjunct(Trm,Lc,L,R) :-
  isBinary(Trm,Lc,"||",L,R).

disjunct(Lc,L,R,Trm) :-
  binary(Lc,"||",L,R,Trm).

isNegation(Trm,Lc,L) :-
  isUnary(Trm,Lc,"~",L).

negation(Lc,L,Trm) :-
  unary(Lc,"~",L,Trm).

isForall(Trm,Lc,L,R) :-
  isBinary(Trm,Lc,"*>",L,R).

mkForall(Lc,L,R,Trm) :-
  binary(Lc,"*>",L,R,Trm).

isMatch(Trm,Lc,P,E) :-
  isBinary(Trm,Lc,".=",P,E),!.

match(Lc,L,R,T) :-
  binary(Lc,".=",L,R,T).

isBind(Trm,Lc,L,R) :-
  isBinary(Trm,Lc,"<-",L,R).

mkBind(Lc,L,R,Trm) :-
  binary(Lc,"<-",L,R,Trm).

isSearch(Trm,Lc,Ptn,Gen) :-
  isBinary(Trm,Lc,"in",Ptn,Gen).

search(Lc,L,R,T) :-
  binary(Lc,"in",L,R,T).

isMapLiteral(T,Lc,Prs) :-
  isBraceTuple(T,Lc,[E]),
  deComma(E,Prs),
  check_implies(misc:is_member(Pr,Prs),wff:isPair(Pr,_,_,_)),!.
isMapLiteral(T,Lc,[]) :-
  isBraceTuple(T,Lc,[]).

mkMapLiteral(Lc,[],M) :-!,
  braceTuple(Lc,[],M).
mkMapLiteral(Lc,Prs,M) :-
  reComma(Prs,I),
  braceTuple(Lc,[I],M).

collectPair(T,(F,E)) :-
  isPair(T,_,F,E).

isComprehension(Trm,Lc,Bnd,Body) :-
  isBraceTuple(Trm,Lc,[T]),
  isBinary(T,_,"|",Bnd,Body).

mkComprehension(Lc,Bnd,Bdy,Trm) :-
  binary(Lc,"|",Bnd,Bdy,El),
  braceTuple(Lc,[El],Trm).

isTestComprehension(Trm,Lc,Body) :-
  isUnary(Trm,Lc,"{??}",Body).

mkTestComprehension(Lc,Body,Trm) :-
  unary(Lc,"{??}",Body,Trm).

isIotaComprehension(Trm,Lc,Bnd,Body) :-
  isUnary(Trm,Lc,"{!!}",B),
  isBinary(B,_,"|",Bnd,Body).

mkIotaComprehension(Lc,Bnd,Body,Trm) :-
  binary(Lc,"|",Bnd,Body,B),
  unary(Lc,"{!!}",B,Trm).

isFieldAcc(Trm,Lc,Rc,Fld) :-
  isBinary(Trm,Lc,".",Rc,F),
  isIden(F,Fld),!.
isFieldAcc(Trm,Lc,Rc,Fld) :-
  isBinary(Trm,Lc,"!.",L,F),
  isIden(F,Fld),!,
  unary(Lc,"!",L,Rc).

fieldAcc(Lc,Rc,F,T) :-
  binary(Lc,".",Rc,F,T).

isRecordUpdate(Trm,Lc,Rc,Fld,Vl) :-
  isBinary(Trm,Lc,"<<-",Lft,Vl),
  isFieldAcc(Lft,_,Rc,Fld).

recordUpdate(Lc,Rc,Fld,Vl,T) :-
  fieldAcc(Lc,Rc,name(Lc,Fld),Lhs),
  binary(Lc,"<<-",Lhs,Vl,T).

isIndexTerm(Trm,Lc,Lhs,Rhs) :-
  isSquareTerm(Trm,Lc,Lhs,[Rhs]),
  \+isBinary(Rhs,_,":",_,_),!.

mkIndexTerm(Lc,L,R,Trm) :-
  squareTerm(Lc,L,R,Trm).

isSlice(Trm,Lc,Lhs,Frm,To) :-
  isSquareTerm(Trm,Lc,Lhs,[Rhs]),
  isBinary(Rhs,_,":",Frm,To),!.

isSplice(Trm,Lc,S,F,T,R) :-
  isAssignment(Trm,Lc,L,R), % S[F:T]:=R
  isSquareTerm(L,_,S,X),
  isBinary(X,_,":",F,T),!.

isGl(C) :-
  isConjunct(C,_,_,_),!.
isGl(C) :-
  isDisjunct(C,_,_,_),!.
isGl(C) :-
  isNegation(C,_,_),!.
isGl(C) :-
  isForall(C,_,_,_),!.
isGl(C) :-
  isSearch(C,_,_,_),!.
isGl(C) :-
  isMatch(C,_,_,_),!.

isIterableGl(C) :-
  isConjunct(C,_,L,R),!,
  (isIterableGl(L) ; isIterableGl(R)).
isIterableGl(C) :-
  isDisjunct(C,_,L,R),!,
  (isIterableGl(L) ; isIterableGl(R)).
isIterableGl(C) :-
  isNegation(C,_,L),!,
  isIterableGl(L).
isIterableGl(C) :-
  isForall(C,_,L,R),!,
  (isIterableGl(L) ; isIterableGl(R)).
isIterableGl(C) :-
  isSearch(C,_,_,_),!.

packageName(T,Pkg) :- isIden(T,Pkg).
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
packageVersion(integer(_,Ix),Pkg) :- atom_string(Ix,Pkg).
packageVersion(T,Pkg) :- isBinary(T,_,".",L,R),
  packageVersion(L,LP),
  packageVersion(R,RP),
  string_concat(LP,".",I),
  string_concat(I,RP,Pkg).

isResultTerm(A,Lc,Stmts) :-
  isBrace(A,Lc,"result",[Stmts]).

mkResultTerm(Lc,S,T) :-
  braceTerm(Lc,name(Lc,"result"),[S],T).

isActionTerm(A,Lc,Stmts) :-
  isBrace(A,Lc,"action",[Stmts]).

mkActionTerm(Lc,S,T) :-
  braceTerm(Lc,name(Lc,"action"),[S],T).

isTaskTerm(A,Lc,Stmts) :-
  isBrace(A,Lc,"task",[Stmts]).

mkTaskTerm(Lc,S,T) :-
  braceTerm(Lc,name(Lc,"task"),[S],T).

isDoTerm(A,Lc,Stmts) :-
  isUnary(A,Lc,"do",I),
  isBraceTuple(I,_,[Stmts]).

mkDoTerm(Lc,A,Trm) :-
  braceTuple(Lc,[A],S0),
  unary(Lc,"do",S0,Trm).

isValis(A,Lc,E) :-
  (isUnary(A,Lc,"valis",E) ; isUnary(A,Lc,"return",E)),!.

mkValis(Lc,A,E) :-
  unary(Lc,"valis",A,E).

isValof(A,Lc,E) :-
  isUnary(A,Lc,"valof",E).

mkValof(Lc,A,E) :-
  unary(Lc,"valof",A,E).

isPerform(A,Lc,E) :-
  isUnary(A,Lc,"perform",E).

mkPerform(Lc,A,E) :-
  unary(Lc,"perform",A,E).

isRaise(A,Lc,E) :-
  isUnary(A,Lc,"raise",E).

mkRaise(Lc,A,E) :-
  unary(Lc,"raise",A,E).

isThrow(A,Lc,E) :-
  isUnary(A,Lc,"throw",E).

mkThrow(Lc,A,E) :-
  unary(Lc,"throw",A,E).

isIgnore(A,Lc,E) :-
  isUnary(A,Lc,"ignore",E).

mkIgnore(Lc,A,E) :-
  unary(Lc,"ignore",A,E).

isTryCatch(A,Lc,B,H) :-
  isUnary(A,Lc,"try",I),
  isBinary(I,_,"catch",B,H).

mkTryCatch(Lc,B,H,A) :-
  binary(Lc,"catch",B,H,A0),
  unary(Lc,"try",A0,A).

isTryHandle(A,Lc,B,H) :-
  isUnary(A,Lc,"try",I),
  isBinary(I,_,"handle",B,H).

mkTryHandle(Lc,B,H,A) :-
  binary(Lc,"handle",B,H,A0),
  unary(Lc,"try",A0,A).

isIfThenElse(A,Lc,Ts,Th,El) :-
  isBinary(A,Lc,"else",Lhs,El),!,
  isBinary(Lhs,_,"then",LL,Th),
  isUnary(LL,_,"if",Ts).

mkIfThenElse(Lc,T,L,R,S) :-
  unary(Lc,"if",T,Ts),
  binary(Lc,"then",Ts,L,Lh),
  binary(Lc,"else",Lh,R,S).

isIfThen(A,Lc,Ts,Th) :-
  isBinary(A,Lc,"then",LL,Th),!,
  isUnary(LL,_,"if",Ts).

mkIfThen(Lc,T,L,S) :-
  unary(Lc,"if",T,Ts),
  binary(Lc,"then",Ts,L,S).

isWhileDo(A,Lc,Ts,Bd) :-
  isBinary(A,Lc,"do",LL,Bd),
  isUnary(LL,_,"while",Ts),!.

mkWhileDo(Lc,T,B,S) :-
  unary(Lc,"while",T,Ts),
  binary(Lc,"do",Ts,B,S).

isUntilDo(A,Lc,Bd,Ts) :-
  isBinary(A,Lc,"until",B,Ts),
  isUnary(B,_,"do",Bd),!.

mkUntilDo(Lc,T,B,S) :-
  unary(Lc,"do",B,B0),
  binary(Lc,"until",B0,T,S).

isForDo(A,Lc,C,Bd) :-
  isBinary(A,Lc,"do",LL,Bd),
  isUnary(LL,_,"for",C),!.

isForDo(A,Lc,El,It,Bd) :-
  isBinary(A,Lc,"do",LL,Bd),
  isUnary(LL,_,"for",Ts),
  isBinary(Ts,_,"in",El,It),!.

mkForDo(Lc,El,It,B,S) :-
  binary(Lc,"in",El,It,T),
  unary(Lc,"for",T,Ts),
  binary(Lc,"do",Ts,B,S).

isActionSeq(A,Lc,S1,S2) :-
  isBinary(A,Lc,";",S1,S2).
isActionSeq(A,Lc,S) :-
  isUnary(A,Lc,";",S).

mkActionSeq(Lc,S1,S2,T) :-
  binary(Lc,";",S1,S2,T).

isSuspend(A,Lc,E,C) :-
  isUnary(A,Lc,"suspend",L),
  isBinary(L,_,"in",E,R),
  isBraceTuple(R,_,C).

isSuspend(A,Lc,T,E,C) :-
  isBinary(A,Lc,"suspend",T,L),
  isBinary(L,_,"in",E,R),
  isBraceTuple(R,_,C).

mkSuspend(Lc,E,C,A) :-
  braceTuple(Lc,C,R),
  binary(Lc,"in",E,R,L),
  unary(Lc,"suspend",L,A).
mkSuspend(Lc,T,E,C,A) :-
  braceTuple(Lc,C,R),
  binary(Lc,"in",E,R,L),
  binary(Lc,"suspend",T,L,A).

isResume(A,Lc,T,E,C) :-
  isBinary(A,Lc,"resume",T,L),
  isBinary(L,_,"in",E,R),
  isBraceTuple(R,_,C).

mkResume(Lc,E,C,A) :-
  braceTuple(Lc,C,R),
  binary(Lc,"in",E,R,L),
  unary(Lc,"resume",L,A).
mkResume(Lc,T,E,C,A) :-
  braceTuple(Lc,C,R),
  binary(Lc,"in",E,R,L),
  binary(Lc,"resume",T,L,A).

isRetire(A,Lc,E) :-
  isUnary(A,Lc,"retire",E).
isRetire(A,Lc,T,E) :-
  isBinary(A,Lc,"retire",T,E).

mkRetire(Lc,E,A) :-
  unary(Lc,"retire",E,A).
mkRetire(Lc,T,E,A) :-
  binary(Lc,"retire",T,E,A).

mkLoc(Lc,T) :-
  Lc=loc(Pk,Line,Col,Off,Ln),
  roundTerm(Lc,name(Lc,"locn"),
	     [string(Lc,Pk),
	      integer(Lc,Line),
	      integer(Lc,Col),
	      integer(Lc,Off),
	      integer(Lc,Ln)],T).

isQuote(Trm,Lc,Body) :-
  isUnary(Trm,Lc,"<||>",Body).

unitTpl(Lc,Unit) :-
  roundTuple(Lc,[],Unit).

dlName(name(Lc,N),name(Lc,DlN)) :- dollarName(N,DlN).

dtName(name(Lc,Nm),name(Lc,DtNm)) :-
  dotName(Nm,DtNm).

