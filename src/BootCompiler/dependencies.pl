:- module(dependencies,[dependencies/3,collectDefinitions/4]).

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

collectDefinitions([St|Stmts],Defs,P,A) :-
  collectDefinition(St,Stmts,S0,Defs,D0,P,P0,A,A0,dependencies:nop),
  collectDefinitions(S0,D0,P0,A0).
collectDefinitions([],[],[],[]).

collectDefinition(St,Stmts,Stmts,[(cns(V),Lc,[Tp])|Defs],Defs,P,Px,[(V,T)|A],A,Export) :-
  isTypeAnnotation(St,Lc,L,T),
  (isIden(L,V),Ex=Export; isPrivate(L,_,V1),isIden(V1,V),Ex=dependencies:nop),
  isConstructorType(T,_,Tp),!,
  call(Ex,var(V),P,Px).
collectDefinition(St,Stmts,Stmts,Defs,Defs,P,Px,[(V,T)|A],A,Export) :-
  isTypeAnnotation(St,Lc,L,T),
  (isIden(L,V) ->
   call(Export,var(V),P,Px) ;
   isPrivate(L,_,V1), isIden(V1,V) ->
   call(dependencies:nop,var(V),P,Px);
   reportError("cannot understand type annotation of %s",[L],Lc),
   P=Px).
collectDefinition(St,Stmts,Stx,Defs,Dfx,P,P,A,Ax,_) :-
  isPrivate(St,_,Inner),
  collectDefinition(Inner,Stmts,Stx,Defs,Dfx,P,_,A,Ax,dependencies:nop).
collectDefinition(St,Stmts,Stx,Defs,Dfx,P,Px,A,Ax,_) :-
  isPublic(St,_,Inner),
  collectDefinition(Inner,Stmts,Stx,Defs,Dfx,P,Px,A,Ax,dependencies:export).
collectDefinition(St,Stmts,Stmts,[(Nm,Lc,[St])|Defs],Defs,P,Px,A,Ax,Export) :-
  isContractStmt(St,Lc,Quants,Constraints,Con,Els),
  generateAnnotations(Els,Quants,[Con|Constraints],A,Ax),
  contractName(Con,Nm),
  call(Export,Nm,P,Px).
collectDefinition(St,Stmts,Stmts,[(Nm,Lc,[St])|Defs],Defs,P,Px,A,A,Export) :-
  isImplementationStmt(St,Lc,_,_,N,_),
  implementedContractName(N,Nm),
  call(Export,Nm,P,Px).
collectDefinition(St,Stmts,Stmts,Defs,Defs,Px,Px,A,A,_) :-
  isBinary(St,_,"@",_,_).
collectDefinition(St,Stmts,Stmts,Defs,Defs,Px,Px,A,A,_) :-
  isUnary(St,_,"@",_).
collectDefinition(St,Stmts,Stmts,[(tpe(Nm),Lc,[St])|Defs],Defs,P,Px,A,A,Export) :-
  isTypeExistsStmt(St,Lc,_,_,L,_),
  typeName(L,Nm),
  call(Export,tpe(Nm),P,Px).
collectDefinition(St,Stmts,Stmts,[(tpe(Nm),Lc,[St])|Defs],Defs,P,Px,A,A,Export) :-
  isTypeFunStmt(St,Lc,_,_,L,_),
  typeName(L,Nm),
  call(Export,tpe(Nm),P,Px).
collectDefinition(St,Stmts,Stmts,Defs,Dfx,P,Px,A,Ax,Export) :-
  isAlgebraicTypeStmt(St,_,_,_,_,_),
  reformAlgebraic(St,Defs,Dfx,A,Ax,Export,P,Px).
collectDefinition(St,Stmts,Stx,[(Nm,Lc,[St|Defn])|Defs],Defs,P,Px,A,A,Export) :-
  ruleName(St,Nm,Kind),
  locOfAst(St,Lc),
  collectDefines(Stmts,Kind,Stx,Nm,Defn),
  call(Export,Nm,P,Px).
collectDefinition(St,Stmts,Stmts,Defs,Defs,P,P,A,A,_) :-
  locOfAst(St,Lc),
  reportError("Cannot fathom %s",[St],Lc).

export(Nm,[Nm|P],P).
nop(_,P,P).

% Pull apart an algebraic type def into its pieces
reformAlgebraic(St,[(tpe(Nm),Lc,[TpRule])|Lst],Lx,A,Ax,Export,P,Px) :-
  isAlgebraicTypeStmt(St,Lc,Quants,Constraints,Head,Body),
  typeName(Head,Nm),
  algebraicFace(Body,Face),
%  reportMsg("face of %s is %s, quants = %s",[TpRule,Face,Quants],Lc),
  binary(Lc,"<~",Head,Face,TRl),
  reConstrain(Constraints,TRl,CTrl),
  reUQuant(Quants,CTrl,TpRule),
  call(Export,tpe(Nm),P,P0),
  buildConstructors(Body,Quants,Constraints,Nm,Head,Lst,Lx,A,Ax,Export,P0,Px).

algebraicFace(C,F) :-
  isBinary(C,_,"|",L,R),!,
  algebraicFace(L,F0),
  algebraicFace(R,F1),
  combineFaces(F0,F1,F).
algebraicFace(C,E) :-
  isIden(C,Lc,_),
  braceTuple(Lc,[],E).
algebraicFace(C,E) :-
  isEnum(C,Lc,_),
  braceTuple(Lc,[],E).
algebraicFace(C,Face) :-
  isRoundCon(C,_,_,Lc,_,_,dependencies:nop,_,_),
  braceTuple(Lc,[],Face).
algebraicFace(C,Face) :-
  isBraceCon(C,XQ,XC,Lc,_,Els,dependencies:nop,_,_),
  pullOthers(Els,Entries,_Asserts,_Defaults),
  braceTuple(Lc,Entries,F),
  reConstrain(XC,F,CF),
  reXQuant(XQ,CF,Face).
algebraicFace(C,Face) :-
  isPrivate(C,_,I),
  algebraicFace(I,Face).
algebraicFace(C,Face) :-
  isPublic(C,_,I),
  algebraicFace(I,Face).
algebraicFace(C,Face) :-
  isXQuantified(C,_,I),
  algebraicFace(I,Face).
algebraicFace(C,Face) :-
  isConstrained(C,I,_),
  algebraicFace(I,Face).

combineFaces(F0,F,F) :-
  isEmptyBrace(F0).
combineFaces(F,F0,F) :-
  isEmptyBrace(F0).
combineFaces(F0,F1,F2) :-
  isBraceTuple(F0,Lc,Els0),
  isBraceTuple(F1,_,Els1),
  mergeFields(Els0,Els1,Els),!,
  braceTuple(Lc,Els,F2).

mergeFields([],F,F).
mergeFields(F,[],F).
mergeFields([A|As],B,[A|Fs]) :-
  mergeField(A,B,B1),
  mergeFields(As,B1,Fs).

mergeField(A,B,Bx) :-
  isTypeAnnotation(A,_,N,Tp),
  isIden(N,_,Nm),
  checkFields(Nm,Tp,B,Bx),!.

checkFields(_,_,[],[]).
checkFields(Nm,Tp,[B|Bs],Bx) :-
  isTypeAnnotation(B,_,N,Tp2),
  (isIden(N,Lc,Nm) ->
   Bs=Bx,
   (sameTerm(Tp,Tp2);
    reportError("type of field %s mismatch, %s!=%s",[Nm,Tp,Tp2],Lc));
   Bx=[B|Bx1],
   checkFields(Nm,Tp,Bs,Bx1)).
checkFields(Nm,Tp,[B|Bs],[B|Bx]) :-
  checkFields(Nm,Tp,Bs,Bx).

buildConstructors(Body,Quants,Constraints,Nm,Tp,Lst,Lx,A,Ax,Export,P,Px) :-
  isBinary(Body,_,"|",L,R),
  buildConstructors(L,Quants,Constraints,Nm,Tp,Lst,L0,A,A0,Export,P,P0),
  buildConstructors(R,Quants,Constraints,Nm,Tp,L0,Lx,A0,Ax,Export,P0,Px).
buildConstructors(C,Quants,Constraints,Nm,Tp,[St|L0],Lx,A,Ax,Export,P,Px) :-
  buildConstructor(C,Quants,Constraints,Nm,Tp,St,L0,Lx,A,Ax,Export,P,Px).
buildConstructors(C,_,_,_,_,Defs,Defs,Ax,Ax,_,Px,Px) :-
  locOfAst(C,Lc),
  reportError("invalid constructor: %s",[C],Lc).

buildConstructor(N,Quants,Constraints,_,Tp,(cns(Nm),Lc,[St]),Lx,Lx,[(Nm,St)|Ax],Ax,Export,P,Px) :-
  isEnum(N,Lc,En),!,
  isIden(En,_,Nm),
  roundTuple(Lc,[],Hd),
  binary(Lc,"<=>",Hd,Tp,CnTp),
  reConstrain(Constraints,CnTp,Rl),
  reUQuant(Quants,Rl,St),
  call(Export,var(Nm),P,Px).
buildConstructor(N,Quants,Constraints,_,Tp,(cns(Nm),Lc,[St]),Lx,Lx,[(Nm,St)|Ax],Ax,Export,P,Px) :-
  isIden(N,Lc,Nm),
  reportMsg("use .%s instead of %s when defining enum",[N,N],Lc),
  roundTuple(Lc,[],Hd),
  binary(Lc,"<=>",Hd,Tp,CnTp),
  reConstrain(Constraints,CnTp,Rl),
  reUQuant(Quants,Rl,St),
  call(Export,var(Nm),P,Px).
buildConstructor(C,Quants,Constraints,_,Tp,(cns(Nm),Lc,[St]),Lx,Lx,[(Nm,St)|Ax],Ax,Export,P,Px) :-
  isRoundCon(C,_,_,Lc,Nm,Args,Export,P,Px),
  roundTuple(Lc,Args,Hd),
  binary(Lc,"<=>",Hd,Tp,Rl),
  reConstrain(Constraints,Rl,CRl),
  reUQuant(Quants,CRl,St).
buildConstructor(C,Quants,Constraints,_,Tp,(cns(Nm),Lc,[St]),Lx,Lx,[(Nm,St)|Ax],Ax,Export,P,Px) :-
  isBraceCon(C,XQ,XC,Lc,Nm,Els,Export,P,Px),
  pullOthers(Els,Entries,_Asserts,_Defaults),
  braceTuple(Lc,Entries,Hd),
  reConstrain(XC,Hd,XHd),
  reXQuant(XQ,XHd,QHd),
  binary(Lc,"<=>",QHd,Tp,Rl),
  reConstrain(Constraints,Rl,CRl),
  reUQuant(Quants,CRl,St).
buildConstructor(C,Quants,Constraints,Nm,Tp,St,L,Lx,A,Ax,_,P,Px) :-
  isPrivate(C,_,I),
  buildConstructor(I,Quants,Constraints,Nm,Tp,St,L,Lx,A,Ax,dependencies:nop,P,Px).
buildConstructor(C,Quants,Constraints,Nm,Tp,St,L,Lx,A,Ax,_,P,Px) :-
  isPublic(C,_,I),
  buildConstructor(I,Quants,Constraints,Nm,Tp,St,L,Lx,A,Ax,dependencies:export,P,Px).

isRoundCon(C,XQ,XC,Lc,Nm,Els,Export,P,Px) :-
  isCon(C,abstract:isRoundTerm,XQ,XC,Lc,Nm,Els,Export,P,Px).

isBraceCon(C,XQ,XC,Lc,Nm,Els,Export,P,Px) :-
  isCon(C,abstract:isBraceTerm,XQ,XC,Lc,Nm,Els,Export,P,Px).

isCon(C,Tst,[],[],Lc,Nm,Els,Export,P,Px) :-
  call(Tst,C,Lc,N,Els),
  isIden(N,Nm),
  call(Export,var(Nm),P,Px).
isCon(C,Tst,XQ,XC,Lc,Nm,Els,Export,P,Px) :-
  isXQuantified(C,XQ,I),
  isCon(I,Tst,_,XC,Lc,Nm,Els,Export,P,Px).
isCon(C,Tst,XQ,XC,Lc,Nm,Els,Export,P,Px) :-
  isConstrained(C,I,XC),
  isCon(I,Tst,XQ,_,Lc,Nm,Els,Export,P,Px).

pullOthers([],[],[],[]).
pullOthers([St|Els],Entries,[St|Asserts],Deflts) :-
  isIntegrity(St,_,_),!,
  pullOthers(Els,Entries,Asserts,Deflts).
pullOthers([St|Els],Entries,Asserts,[St|Deflts]) :-
  isDefault(St,_,_,_),!,
  pullOthers(Els,Entries,Asserts,Deflts).
pullOthers([St|Els],[St|Entries],Asserts,Deflts) :-
  pullOthers(Els,Entries,Asserts,Deflts).

ruleName(St,var(Nm),value) :-
  headOfRule(St,Hd),
  headName(Hd,Nm).

contractName(St,con(Nm)) :-
  isSquare(St,Nm,_).

implementedContractName(Sq,imp(INm)) :-
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

% project contract members out
generateAnnotations([],_,_,Ax,Ax).
generateAnnotations([Def|Els],Quants,Constraints,[(Nm,MTp)|A],Ax) :-
  isTypeAnnotation(Def,_,N,Tp),
  isIden(N,_,Nm),
  reConstrain(Constraints,Tp,CTp),
  reUQuant(Quants,CTp,MTp),
  generateAnnotations(Els,Quants,Constraints,A,Ax).
generateAnnotations([_|Els],Quants,Constraints,A,Ax) :- % ignore things like assertions
  generateAnnotations(Els,Quants,Constraints,A,Ax).

collectDefines([St|Stmts],Kind,OSt,Nm,[St|Defn]) :-
  ruleName(St,Nm,Kind),
  collectDefines(Stmts,Kind,OSt,Nm,Defn).
collectDefines([St|Stmts],Kind,[St|OSt],Nm,Defn) :-
  collectDefines(Stmts,Kind,OSt,Nm,Defn).
collectDefines(Stmts,_,Stmts,_,[]).

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
  isName(Name,Nm),
  \+isKeyword(Nm).
headName(tuple(_,"()",[Name]),Nm) :-
  headName(Name,Nm).
headName(Head,Nm) :-
  isDefault(Head,_,Lhs),
  headName(Lhs,Nm).

typeName(Tp,Nm) :-
  isBinary(Tp,_,"|:",_,R),
  typeName(R,Nm).
typeName(Tp,Nm) :- isSquare(Tp,Nm,_), \+ isKeyword(Nm).
typeName(Tp,Nm) :- isName(Tp,Nm), \+ isKeyword(Nm).
typeName(Tp,"=>") :- isBinary(Tp,_,"=>",_,_).
typeName(Tp,Nm) :- isTuple(Tp,_,A),
  length(A,Ar),
  swritef(Nm,"()%d",[Ar]).

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
  isAssignment(St,_,H,Exp),
  collectAnnotRefs(H,All,Annots,SoFar,R0),
  collectHeadRefs(H,All,R0,R1),
  collectTermRefs(Exp,All,R1,Refs).
collStmtRefs(St,All,Annots,SoFar,Refs) :-
  isEquation(St,_,H,Cond,Exp),
  collectAnnotRefs(H,All,Annots,SoFar,R0),
  collectHeadRefs(H,All,R0,R1),
  collectCondRefs(Cond,All,R1,R2),
  collectTermRefs(Exp,All,R2,Refs).
collStmtRefs(C,All,_,R,Refs) :-
  isAlgebraicTypeStmt(C,_,_,Cx,_,_),
  collConstraints(Cx,All,R,Refs).
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
  isDoTerm(T,_,Stmts),!,
  collectDoRefs(Stmts,A,R0,Refs).
collectTermRefs(T,A,R0,Refs) :-
  isActionTerm(T,_,Stmts),!,
  collectDoRefs(Stmts,A,R0,Refs).
collectTermRefs(T,A,R0,Refs) :-
  isTaskTerm(T,_,Stmts),!,
  collectDoRefs(Stmts,A,R0,Refs).
collectTermRefs(T,_A,Refs,Refs) :-
  isDoTerm(T,_),!.
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
  collectCondRefs(C,All,R1,R2),
  collectTermRefs(R,All,R2,Refs).
collectTermRefs(T,All,R,Refs) :-
  isRef(T,_,A),!,
  collectIndexRefs(A,All,R,Refs).
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
  collectStmtRefs(St,All,R0,R1),
  collectFaceRefs(L,All,R1,Refs).

collectCaseRefs([],_,_,Rf,Rf) :-!.
collectCaseRefs([E|Cs],C,A,Rf,Refs) :-
  isEquation(E,_,L,Cond,R),
  collectTermRefs(L,A,Rf,R0),
  collectTermRefs(Cond,A,R0,R1),
  call(C,R,A,R1,R2),
  collectCaseRefs(Cs,C,A,R2,Refs).

collectDoRefs(T,All,Rf,Rfx) :-
  isActionSeq(T,_,L,R),!,
  collectDoRefs(L,All,Rf,Rf1),
  collectDoRefs(R,All,Rf1,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  isBraceTuple(T,_,[St]),!,
  collectDoRefs(St,All,Rf,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  isBind(T,_,bind(L),R),!,
  collectTermRefs(L,All,Rf,Rf0),
  collectTermRefs(R,All,Rf0,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  isDefn(T,_,L,R),!,
  collectTermRefs(L,All,Rf,Rf0),
  collectTermRefs(R,All,Rf0,Rfx).
collectDoRefs(T,All,Rf,Rfx) :-
  isAssignment(T,_,L,R),!,
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
