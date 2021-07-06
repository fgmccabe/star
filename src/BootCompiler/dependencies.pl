:- module(dependencies,[dependencies/3]).

:- use_module(topsort).
:- use_module(abstract).
:- use_module(errors).
:- use_module(misc).
:- use_module(operators).
:- use_module(wff).

dependencies(Dfs,Groups,Annots) :-
  allRefs(Dfs,[],ARefs),
  collectThetaRefs(Dfs,ARefs,Annots,Defs),
  topsort(Defs,Groups,misc:same).
%  showGroups(Groups).

allRefs([(con(N),_,[St])|Defs],SoFar,AllRefs) :-
  conRefs(con(N),St,SoFar,Rf0),
  allRefs(Defs,Rf0,AllRefs).
allRefs([(N,_,_)|Defs],SoFar,AllRefs) :-
  allRefs(Defs,[N|SoFar],AllRefs).
allRefs([],SoFar,SoFar).

conRefs(N,St,R,Rfs) :-
  isContractStmt(St,_,_,_,_,Els),
  rfold(Els,dependencies:conRef(N),R,Rfs).

conRef(N,St,Rfs,[(var(Nm),N)|Rfs]) :-
  isTypeAnnotation(St,_,V,_),!,
  isIden(V,Nm).
conRef(_,_,Rfs,Rfs).

collectThetaRefs([],_,_,[]).
collectThetaRefs([(cns(Nm),Lc,[Def])|Defs],AllRefs,Annots,[(cns(Nm),Refs,Lc,[Def])|Dfns]) :-
  collectTypeRefs(Def,AllRefs,[],Refs),
  collectThetaRefs(Defs,AllRefs,Annots,Dfns).
collectThetaRefs([(Defines,Lc,Def)|Defs],AllRefs,Annots,[(Defines,Refs,Lc,Def)|Dfns]) :-
  collectStmtRefs(Def,AllRefs,Annots,[],Refs),
  collectThetaRefs(Defs,AllRefs,Annots,Dfns).

collectStmtRefs([],_,_,Refs,Refs).
collectStmtRefs([St|Stmts],All,Annots,R,Refs) :-
  collStmtRefs(St,All,Annots,R,R0),!,
  collectStmtRefs(Stmts,All,Annots,R0,Refs).

collStmtRefs(St,All,_,R,Rfs) :-
  isTypeAnnotation(St,_,_,Tp),!,
  collectTypeRefs(Tp,All,R,Rfs).
collStmtRefs(St,All,Annots,R,Rx) :-
  isPublic(St,_,I),!,
  collStmtRefs(I,All,Annots,R,Rx).
collStmtRefs(St,All,Annots,R,Rx) :-
  isPrivate(St,_,I),!,
  collStmtRefs(I,All,Annots,R,Rx).
collStmtRefs(St,All,Annots,SoFar,Refs) :-
  isDefn(St,_,H,Exp),
  collectAnnotRefs(H,All,Annots,SoFar,R0),
  collectHeadRefs(H,All,R0,R1),
  collectTermRefs(Exp,All,R1,Refs).
collStmtRefs(St,All,Annots,SoFar,Refs) :-
  isEquation(St,_,H,Cond,Exp),
  collectAnnotRefs(H,All,Annots,SoFar,R0),
  collectHeadRefs(H,All,R0,R1),
  collectGuardRefs(Cond,All,R1,R2),
  collectTermRefs(Exp,All,R2,Refs).
collStmtRefs(C,All,_,R,Refs) :-
  isAlgebraicTypeStmt(C,_,_,Cx,_,_Body),
  collConstraints(Cx,All,R,Refs).
%  collectConstructorRefs(Body,All,Rf0,Refs).
collStmtRefs(C,All,_,R,Refs) :-
  isConstructorStmt(C),
  collectTypeRefs(C,All,R,Refs).
collStmtRefs(St,All,_,R0,Refs) :-
  isTypeExistsStmt(St,_,_,_,_,Tp),
  collectTypeRefs(Tp,All,R0,Refs).
collStmtRefs(St,All,_,R0,Refs) :-
  isTypeFunStmt(St,_,_,_,_,Tp),
  collectTypeRefs(Tp,All,R0,Refs).
collStmtRefs(St,All,_,R,Refs) :-
  isContractStmt(St,_,_,Cx,_,Defs),
  collConstraints(Cx,All,R,R0),
  collectFaceTypes(Defs,All,R0,Refs).
collStmtRefs(St,All,_,R0,Refs) :-
  isImplementationStmt(St,_,_,C,Con,B),
  collImplementationRefs(C,Con,B,All,R0,Refs).
collStmtRefs(St,All,_,R,Rx) :-
  isIntegrity(St,_,Inner),
  collectCondRefs(Inner,All,R,Rx).
collStmtRefs(St,All,_,R,Rx) :-
  isShow(St,_,Inner),
  collectTermRefs(Inner,All,R,Rx).
collStmtRefs(St,_,_,R,R) :-
  locOfAst(St,Lc),
  reportError("Cannot fathom definition %s",[St],Lc).

collectConstructorRefs(Body,All,Rf,Refs) :-
  isBinary(Body,_,"|",L,R),!,
  collectConstructorRefs(L,All,Rf,Rf0),
  collectConstructorRefs(R,All,Rf0,Refs).
collectConstructorRefs(Body,_,Refs,Refs) :-
  isIden(Body,_,_),!.
collectConstructorRefs(Body,_,Refs,Refs) :-
  isEnum(Body,_,_),!.
collectConstructorRefs(Body,All,Rf,Refs) :-
  isRoundCon(Body,_,_,_,_,Args),!,
  collectTypeList(Args,All,Rf,Refs).
collectConstructorRefs(Body,All,Rf,Refs) :-
  isBraceCon(Body,_,_,_,_,Entries),!,
  collectFaceTypes(Entries,All,Rf,Refs).

collImplementationRefs(C,Con,B,All,R0,Refs) :-
  collConstraints(C,All,R0,R1),
  collectTypeRefs(Con,All,R1,R2),
  collectTermRefs(B,All,R2,Refs).

collectHeadRefs(Hd,All,R0,Refs) :-
  isWhere(Hd,_,L,C),
  collectHeadRefs(L,All,R0,R1),
  collectCondRefs(C,All,R1,Refs).
collectHeadRefs(Hd,All,R0,Refs) :-
  isRoundTerm(Hd,_,A),
  collectTermListRefs(A,All,R0,Refs).
collectHeadRefs(_,_,R,R).

isConstructorStmt(C) :-
  isQuantified(C,_,I),
  isConstructorStmt(I).
isConstructorStmt(C) :-
  isXQuantified(C,_,I),
  isConstructorStmt(I).
isConstructorStmt(C) :-
  isPrivate(C,_,I),
  isConstructorStmt(I).
isConstructorStmt(C) :-
  isPublic(C,_,I),
  isConstructorStmt(I).
isConstructorStmt(C) :-
  isBinary(C,_,"<=>",_,_).

collConRefs(C,_,Refs,Refs) :-
  isIden(C,_,_).
collConRefs(C,All,R,Refs) :-
  isRoundTerm(C,_,Els),
  collectTypeList(Els,All,R,Refs).
collConRefs(C,All,R,Refs) :-
  isBraceTerm(C,_,_,A),
  collectFaceTypes(A,All,R,Refs).

collectClassRefs(Defs,All,SoFar,Refs) :-
  locallyDefined(Defs,All,Rest),
  collectStmtRefs(Defs,Rest,[],SoFar,Refs).

collectAnnotRefs(H,All,Annots,SoFar,Refs) :-
  headName(H,Nm),
  is_member((Nm,Tp),Annots),!,
  collectTypeRefs(Tp,All,SoFar,Refs).
collectAnnotRefs(_,_,_,Refs,Refs).

collConstraints([],_,R,R).
collConstraints([C|L],All,R,Refs) :-
  collConstraint(C,All,R,R0),
  collConstraints(L,All,R0,Refs).

collConstraint(C,All,SoFar,Refs) :-
  isBinary(C,_,",",L,R),
  collConstraint(L,All,SoFar,R0),
  collConstraint(R,All,R0,Refs).
collConstraint(C,All,Refs,[con(Nm)|Refs]) :-
  isSquare(C,_,Nm,_),
  is_member(con(Nm),All).
collConstraint(_,_,Refs,Refs).

locallyDefined([],All,All).
locallyDefined([St|Stmts],All,Rest) :-
  removeLocalDef(St,All,A0),
  locallyDefined(Stmts,A0,Rest).

removeLocalDef(St,All,Rest) :-
  ruleName(St,Nm,value),
  subtract(Nm,All,Rest).
removeLocalDef(_,All,All).

collectNmRef(N,_All,Rfs,Rfs) :-
  is_member(N,Rfs),!.
collectNmRef(N,All,Rfs,[N|Rfs]) :-
  is_member(N,All),!.
collectNmRef(N,All,Rfs,[M|Rfs]) :-
  is_member((N,M),All),!.
collectNmRef(_,_,Rfs,Rfs).

collectGuardRefs(none,_,Rfs,Rfs) :-!.
collectGuardRefs(some(G),A,R0,Refs) :-
  collectCondRefs(G,A,R0,Refs).

collectCondRefs(C,A,R0,Refs) :-
  isConjunct(C,_,L,R),
  collectCondRefs(L,A,R0,R1),
  collectCondRefs(R,A,R1,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isConditional(C,_,T,L,R),
  collectCondRefs(T,A,R0,R1),
  collectCondRefs(L,A,R1,R2),
  collectCondRefs(R,A,R2,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isDisjunct(C,_,L,R),
  collectCondRefs(L,A,R0,R1),
  collectCondRefs(R,A,R1,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isNegation(C,_,R),
  collectCondRefs(R,A,R0,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isTuple(C,[Inner]),
  collectCondRefs(Inner,A,R0,Refs).
collectCondRefs(C,A,R0,Refs) :-
  collectTermRefs(C,A,R0,Refs).

collectTermRefs(E,A,R0,Refs) :-
  isTypeAnnotation(E,_,L,R),
  collectTermRefs(L,A,R0,R1),
  collectTypeRefs(R,A,R1,Refs).
collectTermRefs(E,A,R0,Refs) :-
  isCoerce(E,_,L,R),
  collectTermRefs(L,A,R0,R1),
  collectTypeRefs(R,A,R1,Refs).
collectTermRefs(E,A,R0,Refs) :-
  isOptCoerce(E,_,L,R),
  collectTermRefs(L,A,R0,R1),
  collectTypeRefs(R,A,R1,Refs).
collectTermRefs(V,A,Rfs,Refs) :-
  isName(V,Nm),
  collectNmRef(var(Nm),A,Rfs,Rf0),
  collectNmRef(cns(Nm),A,Rf0,Refs).
collectTermRefs(T,A,Rfs,Refs) :-
  isEnum(T,_,I),!,
  isName(I,Nm),
  collectNmRef(cns(Nm),A,Rfs,Refs).
collectTermRefs(T,A,R,Rx) :-
  isLetDef(T,_,B,Ex),!,
  collectLetRefs(B,Ex,A,R,Rx).
collectTermRefs(T,A,R0,Refs) :-
  isRoundTerm(T,O,Args),
  collectTermRefs(O,A,R0,R1),
  collectTermListRefs(Args,A,R1,Refs).
collectTermRefs(T,A,R0,Refs) :-
  isTuple(T,Els),
  collectTermListRefs(Els,A,R0,Refs).
collectTermRefs(T,A,R0,Refs) :-
  isSquareTuple(T,_,Els),
  collectTermListRefs(Els,A,R0,Refs).
collectTermRefs(T,A,Rf,Refs) :-
  isCaseExp(T,_,G,C),!,
  collectTermRefs(G,A,Rf,R0),
  collectCaseRefs(C,collectTermRefs,A,R0,Refs).
collectTermRefs(T,A,R0,Refs) :-
  isAbstraction(T,_,B,G),!,
  collectTermRefs(B,A,R0,R1),
  collectCondRefs(G,A,R1,Refs).
collectTermRefs(T,A,R0,Refs) :-
  isListAbstraction(T,_,B,G),!,
  collectTermRefs(B,A,R0,R1),
  collectCondRefs(G,A,R1,Refs).
collectTermRefs(T,A,R0,Refs) :-
  isValof(T,_,E),!,
  collectTermRefs(E,A,R0,Refs).
collectTermRefs(T,A,R0,Refs) :-
  isDoTerm(T,_,Stmts),!,
  collectDoRefs(Stmts,A,R0,Refs).
collectTermRefs(T,A,R0,Refs) :-
  isActionTerm(T,_,Stmts),!,
  collectDoRefs(Stmts,A,R0,Refs).
collectTermRefs(T,A,R0,Refs) :-
  isTaskTerm(T,_,Stmts),!,
  collectDoRefs(Stmts,A,R0,Refs).
collectTermRefs(T,A,R0,Refs) :-
  isScriptTerm(T,_,Stmts),!,
  collectDoRefs(Stmts,A,R0,Refs).
collectTermRefs(app(_,Op,Args),All,R,Refs) :-
  collectTermRefs(Op,All,R,R0),
  collectTermRefs(Args,All,R0,Refs).
collectTermRefs(T,All,R,Refs) :-
  isBraceTuple(T,_,Els),
  collectClassRefs(Els,All,R,Refs).
collectTermRefs(T,All,R,Refs) :-
  isQBraceTuple(T,_,Els),
  collectStmtRefs(Els,All,[],R,Refs).
collectTermRefs(T,All,R,Refs) :-
  isBraceTerm(T,_,Op,Els),
  collectTermRefs(Op,All,R,R0),
  collectClassRefs(Els,All,R0,Refs).
collectTermRefs(T,All,R,Refs) :-
  isQBraceTerm(T,_,Op,Els),
  collectTermRefs(Op,All,R,R0),
  collectStmtRefs(Els,All,[],R0,Refs).
collectTermRefs(T,All,R,Refs) :-
  isSquareTerm(T,Op,A),
  collectTermRefs(Op,All,R,R0),
  collectIndexRefs(A,All,R0,Refs).
collectTermRefs(T,All,R0,Refs) :-
  isEquation(T,_,L,C,R),
  collectTermRefs(L,All,R0,R1),
  collectGuardRefs(C,All,R1,R2),
  collectTermRefs(R,All,R2,Refs).
collectTermRefs(T,All,R,Refs) :-
  isRef(T,_,A),!,
  collectTermRefs(A,All,R,Refs).
collectTermRefs(_,_,Refs,Refs).

collectTermListRefs([],_,Refs,Refs).
collectTermListRefs([E|L],A,R0,Refs) :-
  collectTermRefs(E,A,R0,R1),
  collectTermListRefs(L,A,R1,Refs).

collectLetRefs(B,Ex,A,R,Rx) :-
  collectTermRefs(B,A,R,R0),
  collectTermRefs(Ex,A,R0,Rx).

collectIndexRefs([A],All,R,Refs) :-
  isBinary(A,_,"->",Ky,Vl),!,
  collectTermRefs(Ky,All,R,R0),
  collectTermRefs(Vl,All,R0,Refs).
collectIndexRefs([A],All,R,Refs) :-
  isNegation(A,_,Ky),!,
  collectTermRefs(Ky,All,R,Refs).
collectIndexRefs(A,All,R,Refs) :-
  collectTermListRefs(A,All,R,Refs).

collectFaceRefs([],_,R,R).
collectFaceRefs([St|L],All,R0,Refs) :-
  collectStmtRefs(St,All,[],R0,R1),
  collectFaceRefs(L,All,R1,Refs).

collectCaseRefs([],_,_,Rf,Rf) :-!.
collectCaseRefs([E|Cs],C,A,Rf,Refs) :-
  isEquation(E,_,L,Cond,R),
  collectTermRefs(L,A,Rf,R0),
  collectGuardRefs(Cond,A,R0,R1),
  call(C,R,A,R1,R2),
  collectCaseRefs(Cs,C,A,R2,Refs).

collectDoRefs(T,All,Rf,Rfx) :-
  isActionSeq(T,_,L,R),!,
  collectDoRefs(L,All,Rf,Rf1),
  collectDoRefs(R,All,Rf1,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  isBraceTuple(T,_,[St]),!,
  collectDoRefs(St,All,Rf,Rfx).
collectDoRefs(T,_,Rf,Rf) :-
  isBraceTuple(T,_,[]),!.
collectDoRefs(T,All,Rf,Rfx) :-
  isMatch(T,_,L,R),!,
  collectTermRefs(L,All,Rf,Rf0),
  collectTermRefs(R,All,Rf0,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  isDefn(T,_,L,R),!,
  collectTermRefs(L,All,Rf,Rf0),
  collectTermRefs(R,All,Rf0,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  isIfThenElse(T,_,Tt,H,E),!,
  collectTermRefs(Tt,All,Rf,Rf0),
  collectDoRefs(H,All,Rf0,Rf1),
  collectDoRefs(E,All,Rf1,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  isIfThen(T,_,Tt,H),!,
  collectTermRefs(Tt,All,Rf,Rf0),
  collectDoRefs(H,All,Rf0,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  isCaseExp(T,_,Exp,Cases),!,
  collectTermRefs(Exp,All,Rf,R0),
  collectCaseRefs(Cases,collectDoRefs,All,R0,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  isWhileDo(T,_,Tt,B),!,
  collectTermRefs(Tt,All,Rf,Rf0),
  collectDoRefs(B,All,Rf0,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  isUntilDo(T,_,B,Tt),!,
  collectTermRefs(Tt,All,Rf,Rf0),
  collectDoRefs(B,All,Rf0,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  isForDo(T,_,Tt,B),!,
  collectTermRefs(Tt,All,Rf,Rf0),
  collectDoRefs(B,All,Rf0,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  isTryCatch(T,_,L,R),!,
  collectDoRefs(L,All,Rf,Rf1),
  collectCatchRefs(R,All,Rf1,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  isThrow(T,_,E),!,
  collectTermRefs(E,All,Rf,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  isValis(T,_,E),!,
  collectTermRefs(E,All,Rf,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  collectTermRefs(T,All,Rf,Rfx).

collectCatchRefs(T,All,Rf,Rfx) :-
  isBraceTuple(T,_,[St]),!,
  collectDoRefs(St,All,Rf,Rfx).
collectCatchRefs(T,All,Rf,Rfx) :-
  collectTermRefs(T,All,Rf,Rfx).

collectTypeRefs(V,All,SoFar,Refs) :-
  isIden(V,Nm),
  collectTypeName(Nm,All,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isSquareTerm(T,Op,A),
  collectTypeRefs(Op,All,SoFar,R0),
  collectTypeList(A,All,R0,Refs).
collectTypeRefs(St,_,SoFar,SoFar) :-
  isBinary(St,_,"@",_,_).
collectTypeRefs(St,_,SoFar,SoFar) :-
  isUnary(St,_,"@",_).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBinary(T,_,"=>",L,R),
  collectTypeRefs(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBinary(T,_,"<=>",L,R),
  collectTypeRefs(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Refs).
collectTypeRefs(T,All,SoFar,Rest) :-
  isBinary(T,_,"|:",L,R),
  collConstraint(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Rest).
collectTypeRefs(T,All,SoFar,Rest) :-
  isBinary(T,_,"->>",L,R),
  collectTypeRefs(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Rest).
collectTypeRefs(T,All,SoFar,Refs) :-
  isTypeLambda(T,_,L,R),
  collectTypeRefs(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Refs).
collectTypeRefs(C,All,SoFar,Refs) :-
  isBinary(C,_,",",L,R),
  collectTypeRefs(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Refs).
collectTypeRefs(T,All,R,Refs) :-
  isBinary(T,_,".",L,_),
  collectTermRefs(L,All,R,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isUnary(T,_,"ref",L),
  collectTypeRefs(L,All,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBraceTuple(T,_,A),
  collectFaceTypes(A,All,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBraceTerm(T,_,_,A),
  collectFaceTypes(A,All,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isQuantified(T,Q,A),
  filterOutQ(Q,All,SubAll),
  collectTypeRefs(A,SubAll,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isXQuantified(T,Q,A),
  filterOutQ(Q,All,SubAll),
  collectTypeRefs(A,SubAll,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isTuple(T,Args),
  collectTypeList(Args,All,SoFar,Refs).
collectTypeRefs(T,All,R,Rx) :-
  isApply(T,_,Op,Arg),
  collectTypeRefs(Op,All,R,R0),
  collectTypeRefs(Arg,All,R0,Rx).

filterOutQ([],All,All).
filterOutQ([I|Q],All,Ax) :-
  isIden(I,_,Nm),
  filterOut(tpe(Nm),All,A0),
  filterOutQ(Q,A0,Ax).
filterOutQ([I|Q],All,Ax) :-
  isBinary(I,_,"/",V,_),
  isIden(V,_,Nm),
  filterOut(tpe(Nm),All,A0),
  filterOutQ(Q,A0,Ax).

filterOut(Q,A,Ax) :-
  filter(A,dependencies:notEq(Q),Ax).

notEq(A,B) :- A\=B.

collectTypeList([],_,Refs,Refs).
collectTypeList([T|List],All,SoFar,Refs) :-
  isTypeAnnotation(T,_,_,Tp),!,
  collectTypeRefs(Tp,All,SoFar,R0),
  collectTypeList(List,All,R0,Refs).
collectTypeList([Tp|List],All,SoFar,Refs) :-
  collectTypeRefs(Tp,All,SoFar,R0),
  collectTypeList(List,All,R0,Refs).

collectFaceTypes([],_,Refs,Refs).
collectFaceTypes([T|List],All,R,Refs) :-
  collectFaceType(T,All,R,R0),
  collectFaceTypes(List,All,R0,Refs).

collectFaceType(T,All,R,Refs) :-
  isUnary(T,_,"type",I),
  collectFaceType(I,All,R,Refs).
collectFaceType(T,All,R,Refs) :-
  isTypeAnnotation(T,_,_,Tp),
  collectTypeRefs(Tp,All,R,Refs).
collectFaceType(_,_,R,R).

collectTypeName(Nm,All,Refs,[tpe(Nm)|Refs]) :-
  is_member(tpe(Nm),All),
  \+is_member(tpe(Nm),Refs),!.
collectTypeName(_,_,Refs,Refs).

collectLabelRefs(Lb,All,R0,Refs) :- collectTermRefs(Lb,All,R0,Refs).

showGroups([]).
showGroups([G|M]) :-
  reportMsg("Group:",[]),
  showGroup(G),
  showGroups(M).

showGroup([]).
showGroup([(Def,Lc,St)|M]) :-
  reportMsg("Def %s",[Def],Lc),
  reportMsg("Statement(s) %s",[St],Lc),
  showGroup(M).

showRefs(Msg,Refs) :-
  reportMsg("%s references: %s",[Msg,Refs]).
