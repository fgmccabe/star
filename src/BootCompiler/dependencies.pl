:- module(dependencies,[dependencies/6]).

:- use_module(topsort).
:- use_module(abstract).
:- use_module(errors).
:- use_module(misc).
:- use_module(keywords).
:- use_module(wff).

dependencies(Els,Groups,Public,Annots,Imports,Other) :-
  collectDefinitions(Els,Dfs,Public,Annots,Imports,Other),
  allRefs(Dfs,[],AllRefs),
  collectThetaRefs(Dfs,AllRefs,Annots,Defs),
  topsort(Defs,Groups,misc:same),
  showGroups(Groups).

collectDefinitions([St|Stmts],Defs,P,A,I,Other) :-
  collectDefinition(St,Stmts,S0,Defs,D0,P,P0,A,A0,I,I0,Other,O0,dependencies:nop),
  collectDefinitions(S0,D0,P0,A0,I0,O0).
collectDefinitions([],[],[],[],[],[]).

collectDefinition(St,Stmts,Stmts,Defs,Defs,P,P,A,A,[St|I],I,Other,Other,_) :-
  isImport(St,_).
collectDefinition(St,Stmts,Stmts,Defs,Defs,P,P,A,A,I,I,[St|Other],Other,_) :-
  isMacro(St,_).
collectDefinition(St,Stmts,Stmts,Defs,Defs,P,P,A,A,I,I,[St|Other],Other,_) :-
  isIntegrity(St,_,_).
collectDefinition(St,Stmts,Stmts,Defs,Defs,P,P,A,A,I,I,[St|Other],Other,_) :-
  isUnary(St,"show",_).
collectDefinition(St,Stmts,Stmts,Defs,Defs,P,Px,[(V,St)|A],A,I,I,Other,Other,Export) :-
  isBinary(St,":",L,_),
  isIden(L,V),
  call(Export,var(V),P,Px).
collectDefinition(St,Stmts,Stx,Defs,Dfx,P,P,A,Ax,I,Ix,O,Ox,_) :-
  isUnary(St,"private",Inner),
  collectDefinition(Inner,Stmts,Stx,Defs,Dfx,P,_,A,Ax,I,Ix,O,Ox,dependencies:nop).
collectDefinition(St,Stmts,Stx,Defs,Dfx,P,Px,A,Ax,I,Ix,O,Ox,_) :-
  isUnary(St,"public",Inner),
  collectDefinition(Inner,Stmts,Stx,Defs,Dfx,P,Px,A,Ax,I,Ix,O,Ox,dependencies:export).
collectDefinition(St,Stmts,Stmts,[(Nm,Lc,[St])|Defs],Defs,P,Px,A,A,I,I,O,O,Export) :-
  isContractStmt(St,Lc,_,_,Hd,_),
  contractName(Hd,Nm),
  call(Export,Nm,P,Px).
collectDefinition(St,Stmts,Stmts,[(Nm,Lc,[St])|Defs],Defs,P,Px,A,A,I,I,O,O,Export) :-
  isImplementationStmt(St,Lc,_,_,N,_),
  implementedContractName(N,Nm),
  call(Export,Nm,P,Px).
collectDefinition(St,Stmts,Stmts,Defs,Defs,Px,Px,A,A,I,I,O,O,_) :-
  isBinary(St,"@",_,_).
collectDefinition(St,Stmts,Stmts,Defs,Defs,Px,Px,A,A,I,I,O,O,_) :-
  isUnary(St,"@",_).
collectDefinition(St,Stmts,Stmts,[(tpe(Nm),Lc,[St])|Defs],Defs,P,Px,A,A,I,I,O,O,Export) :-
  isTypeExistsStmt(St,Lc,_,_,L,_),
  typeName(L,Nm),
  call(Export,Nm,P,Px).
collectDefinition(St,Stmts,Stmts,[(tpe(Nm),Lc,[St])|Defs],Defs,P,Px,A,A,I,I,O,O,Export) :-
  isTypeFunStmt(St,Lc,_,_,L,_),
  typeName(L,Nm),
  call(Export,Nm,P,Px).
collectDefinition(St,Stmts,Stmts,[(cns(Nm),Lc,[St])|Defs],Defs,P,Px,A,A,I,I,O,O,Export) :-
  isConstructorStmt(St,Lc,Hd,_,_),
  headName(Hd,Nm),
  call(Export,Nm,P,Px).
collectDefinition(St,Stmts,Stx,[(Nm,Lc,[St|Defn])|Defs],Defs,P,Px,A,A,I,I,O,O,Export) :-
  ruleName(St,Nm,Kind),
  locOfAst(St,Lc),
  collectDefines(Stmts,Kind,Stx,Nm,Defn),
  call(Export,Nm,P,Px).
collectDefinition(St,Stmts,Stmts,Defs,Defs,P,P,A,A,I,I,O,O,_) :-
  locOfAst(St,Lc),
  reportError("Cannot fathom %s",[St],Lc).

export(Nm,[Nm|P],P).
nop(_,P,P).

ruleName(St,var(Nm),value) :-
  headOfRule(St,Hd),
  headName(Hd,Nm).

contractName(St,con(Nm)) :-
  isSquare(St,Nm,_).

implementedContractName(Sq,imp(INm)) :-
  isSquare(Sq,Nm,A),
  appStr(Nm,S0,S1),
  marker(conTract,M),
  surfaceNames(A,M,S1,[]),
  string_chars(INm,S0).

surfaceNames([],_,S,S).
surfaceNames([T|_],Sep,S0,Sx) :-
  isBinary(T,Lc,"->>",L,_),!,
  tupleize(L,"()",Lc,tuple(_,_,Els)),
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
  isTuple(T,_,A),
  length(A,Ar),
  swritef(Nm,"()%d",[Ar]).

collectDefines([St|Stmts],Kind,OSt,Nm,[St|Defn]) :-
  ruleName(St,Nm,Kind),
  collectDefines(Stmts,Kind,OSt,Nm,Defn).
collectDefines([St|Stmts],Kind,[St|OSt],Nm,Defn) :-
  collectDefines(Stmts,Kind,OSt,Nm,Defn).
collectDefines(Stmts,_,Stmts,_,[]).

headOfRule(St,Hd) :-
  isBinary(St,"=",Hd,_).
headOfRule(St,Hd) :-
  isBinary(St,":=",Hd,_).
headOfRule(St,Hd) :-
  isBinary(St,"=>",H,_),
  (isBinary(H,"&&",Hd,_) ; H=Hd),!.
headOfRule(St,Hd) :-
  isBinary(St,"<=",H,_),
  (isBinary(H,"&&",Hd,_) ; H=Hd),!.
headOfRule(St,Hd) :-
  isBraceTerm(St,_,Hd,_),!.
headOfRule(St,Hd) :-
  isBinary(St,"-->",H,_),
  (isBinary(H,",",Hd,_) ; H=Hd),!.

headName(Head,Nm) :-
  isRoundTerm(Head,Op,_),
  headName(Op,Nm).
headName(Head,Nm) :-
  isBrace(Head,Nm,_).
headName(Name,Nm) :-
  isName(Name,Nm),
  \+isKeyword(Nm).
headName(tuple(_,"()",[Name]),Nm) :-
  headName(Name,Nm).

typeName(Tp,Nm) :-
  isBinary(Tp,"|:",_,R),
  typeName(R,Nm).
typeName(Tp,Nm) :- isSquare(Tp,Nm,_), \+ isKeyword(Nm).
typeName(Tp,Nm) :- isName(Tp,Nm), \+ isKeyword(Nm).
typeName(Tp,"=>") :- isBinary(Tp,"=>",_,_).
typeName(Tp,"<=") :- isBinary(Tp,"<=",_,_).
typeName(Tp,Nm) :- isTuple(Tp,_,A),
  length(A,Ar),
  swritef(Nm,"()%d",[Ar]).

allRefs([(N,_,_)|Defs],SoFar,AllRefs) :-
  allRefs(Defs,[N|SoFar],AllRefs).
allRefs([],SoFar,SoFar).

collectThetaRefs([],_,_,[]).
collectThetaRefs([(Defines,Lc,Def)|Defs],AllRefs,Annots,[(Defines,Refs,Lc,Def)|Dfns]) :-
  collectStmtRefs(Def,AllRefs,Annots,Refs,[]),
  collectThetaRefs(Defs,AllRefs,Annots,Dfns).

collectStmtRefs([],_,_,Refs,Refs).
collectStmtRefs([St|Stmts],All,Annots,SoFar,Refs) :-
  collRefs(St,All,Annots,SoFar,S0),
  collectStmtRefs(Stmts,All,Annots,S0,Refs).

collRefs(St,All,Annots,SoFar,Refs) :-
  isBinary(St,"=",H,Exp),
  collectAnnotRefs(H,All,Annots,SoFar,R0),
  collectHeadRefs(H,All,R0,R1),
  collectExpRefs(Exp,All,R1,Refs).
collRefs(St,All,Annots,SoFar,Refs) :-
  isBinary(St,":=",H,Exp),
  collectAnnotRefs(H,All,Annots,SoFar,R0),
  collectHeadRefs(H,All,R0,R1),
  collectExpRefs(Exp,All,R1,Refs).
collRefs(St,All,Annots,SoFar,Refs) :-
  isBinary(St,"=>",H,Exp),
  collectAnnotRefs(H,All,Annots,SoFar,R0),
  collectHeadRefs(H,All,R0,R1),
  collectExpRefs(Exp,All,R1,Refs).
collRefs(St,All,Annots,SoFar,Refs) :-
  isBinary(St,"-->",H,Body),
  collectAnnotRefs(H,All,Annots,SoFar,R0),
  collectGrHeadRefs(H,All,R0,R1),
  collectNTRefs(Body,All,R1,Refs).
collRefs(St,All,Annots,SoFar,Refs) :-
  isBinary(St,"<=",H,Exp),
  collectAnnotRefs(H,All,Annots,SoFar,R0),
  collectHeadRefs(H,All,R0,R1),
  collectLabelRefs(Exp,All,R1,Refs).
collRefs(St,All,Annots,R0,Refs) :-
  isBinary(St,_,"<=>",H,R),
  collectAnnotRefs(H,All,Annots,R0,R1),
  collectHeadRefs(H,All,R1,R2),
  collectExpRefs(R,All,R2,Refs).
collRefs(St,All,_,R0,Refs) :-
  isTypeExistsStmt(St,_,_,_,_,Tp),
  collectTypeRefs(Tp,All,R0,Refs).
collRefs(St,All,_,R0,Refs) :-
  isTypeFunStmt(St,_,_,_,_,Tp),
  collectTypeRefs(Tp,All,R0,Refs).
collRefs(St,All,_,R,Refs) :-
  isContractStmt(St,_,_,Cx,_,Defs),
  collConstraints(Cx,All,R,R0),
  collectFaceTypes(Defs,All,R0,Refs).
collRefs(St,All,_,R0,Refs) :-
  isImplementationStmt(St,_,_,C,Con,Defs),
  collConstraints(C,All,R0,R1),
  collectTypeRefs(Con,All,R1,R2),
  collectClassRefs(Defs,All,R2,Refs).
collRefs(St,All,_,R,Rx) :-
  isIntegrity(St,_,Inner),
  collectCondRefs(Inner,All,R,Rx).
collRefs(St,_,_,R,R) :-
  locOfAst(St,Lc),
  reportError("Cannot fathom %s",[St],Lc).

collectHeadRefs(Hd,All,R0,Refs) :-
  isBinary(Hd,"&&",L,R),
  collectHeadRefs(L,All,R0,R1),
  collectCondRefs(R,All,R1,Refs).
collectHeadRefs(Hd,All,R0,Refs) :-
  isRoundTerm(Hd,_,A),
  collectPtnListRefs(A,All,R0,Refs).
collectHeadRefs(Hd,All,R0,Refs) :-
  isBraceTerm(Hd,_,_,Els),
  collectPtnListRefs(Els,All,R0,Refs).
collectHeadRefs(_,_,R,R).

collectGrHeadRefs(Hd,All,R0,Refs) :-
  isBinary(Hd,",",L,R),!,
  collectHeadRefs(L,All,R0,R1),
  collectPtnRefs(R,All,R1,Refs).
collectGrHeadRefs(Hd,All,R0,Refs) :-
  collectHeadRefs(Hd,All,R0,Refs).

collectNTRefs(T,All,R,Refs) :-
  isSquareTuple(T,_,Els),
  collectExpListRefs(Els,All,R,Refs).
collectNTRefs(T,All,R,Refs) :-
  isRoundTuple(T,_,Els),
  collectNTRefList(Els,All,R,Refs).
collectNTRefs(T,All,R,Refs) :-
  isBraceTuple(T,_,Els),
  collectCondListRefs(Els,All,R,Refs).
collectNTRefs(T,All,Rf,Refs) :-
  isBinary(T,"|",L,R),
  isBinary(L,"?",LL,LR),
  collectNTRefs(LL,All,Rf,R0),
  collectNTRefs(LR,All,R0,R1),
  collectNTRefs(R,All,R1,Refs).
collectNTRefs(T,All,Rf,Refs) :-
  isBinary(T,"|",L,R),
  collectNTRefs(L,All,Rf,R0),
  collectNTRefs(R,All,R0,Refs).
collectNTRefs(T,All,Rf,Refs) :-
  isBinary(T,",",L,R),
  collectNTRefs(L,All,Rf,R0),
  collectNTRefs(R,All,R0,Refs).
collectNTRefs(T,All,Rf,Refs) :-
  isUnary(T,"!",L),
  collectNTRefs(L,All,Rf,Refs).
collectNTRefs(T,All,Rf,Refs) :-
  isUnary(T,"\\+",L),
  collectNTRefs(L,All,Rf,Refs).
collectNTRefs(T,All,Rf,Refs) :-
  isUnary(T,"+",L),
  collectNTRefs(L,All,Rf,Refs).
collectNTRefs(T,All,Rf,Refs) :-
  isBinary(T,"@@",L,R),
  collectNTRefs(L,All,Rf,R0),
  collectCondRefs(R,All,R0,Refs).
collectNTRefs(T,All,Rf,Refs) :-
  isBinary(T,"=",L,R),
  collectExpRefs(L,All,Rf,R0),
  collectExpRefs(R,All,R0,Refs).
collectNTRefs(T,All,Rf,Refs) :-
  isBinary(T,"\\=",L,R),
  collectExpRefs(L,All,Rf,R0),
  collectExpRefs(R,All,R0,Refs).
collectNTRefs(T,All,Rf,Refs) :-
  isRoundTerm(T,_,O,A),
  collectExpRefs(O,All,Rf,R0),
  collectExpListRefs(A,All,R0,Refs).
collectNTRefs(T,_,Rf,Rf) :-
  isString(T,_).
collectNTRefs(T,_,Rf,Rf) :-
  isIden(T,_,"eof").

collectNTRefList([],_,R,R).
collectNTRefList([C|L],All,R,Refs) :-
  collectNTRefs(C,All,R,R0),
  collectNTRefList(L,All,R0,Refs).

collectClassRefs(Defs,All,SoFar,Refs) :-
  locallyDefined(Defs,All,Rest),
  collectStmtRefs(Defs,Rest,[],SoFar,Refs).

collectAnnotRefs(H,All,Annots,SoFar,Refs) :-
  headName(H,Nm),
  is_member((Nm,Annot),Annots),!,
  isBinary(Annot,":",_,Tp),
  collectTypeRefs(Tp,All,SoFar,Refs).
collectAnnotRefs(_,_,_,Refs,Refs).

collConstraints(C,All,SoFar,Refs) :-
  isBinary(C,",",L,R),
  collConstraints(L,All,SoFar,R0),
  collConstraints(R,All,R0,Refs).
collConstraints(C,All,[con(Nm)|Refs],Refs) :-
  isSquare(C,_,Nm,_),
  is_member(con(Nm),All).
collConstraints(_,_,Refs,Refs).

locallyDefined([],All,All).
locallyDefined([St|Stmts],All,Rest) :-
  removeLocalDef(St,All,A0),
  locallyDefined(Stmts,A0,Rest).

removeLocalDef(St,All,Rest) :-
  ruleName(St,Nm,value),
  subtract(Nm,All,Rest).
removeLocalDef(_,All,All).

collectCondRefs(C,A,R0,Refs) :-
  isBinary(C,",",L,R),
  collectCondRefs(L,A,R0,R1),
  collectCondRefs(R,A,R1,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isBinary(C,"|",L,R),
  isBinary(L,"?",T,Th),
  collectCondRefs(T,A,R0,R1),
  collectCondRefs(Th,A,R1,R2),
  collectCondRefs(R,A,R2,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isBinary(C,"|",L,R),
  collectCondRefs(L,A,R0,R1),
  collectCondRefs(R,A,R1,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isBinary(C,"*>",L,R),
  collectCondRefs(L,A,R0,R1),
  collectCondRefs(R,A,R1,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isUnary(C,"!",R),
  collectCondRefs(R,A,R0,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isUnary(C,"\\+",R),
  collectCondRefs(R,A,R0,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isBinary(C,"%%",L,R),
  isBinary(R,"~",St,Re),
  collectNTRefs(L,A,R0,R1),
  collectExpRefs(St,A,R1,R2),
  collectExpRefs(Re,A,R2,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isBinary(C,"%%",L,R),
  collectNTRefs(L,A,R0,R1),
  collectExpRefs(R,A,R1,Refs).
collectCondRefs(C,A,R0,Refs) :-
  isTuple(C,[Inner]),
  collectCondRefs(Inner,A,R0,Refs).
collectCondRefs(C,A,R0,Refs) :-
  collectExpRefs(C,A,R0,Refs).

collectCondListRefs([],_,R,R).
collectCondListRefs([C|L],All,R,Refs) :-
  collectCondRefs(C,All,R,R0),
  collectCondListRefs(L,All,R0,Refs).

collectExpRefs(E,A,R0,Refs) :-
  isBinary(E,":",L,R),
  collectExpRefs(L,A,R0,R1),
  collectTypeRefs(R,A,R1,Refs).
collectExpRefs(V,A,[var(Nm)|Refs],Refs) :-
  isName(V,Nm),
  is_member(var(Nm),A).
collectExpRefs(T,A,R0,Refs) :-
  isRoundTerm(T,O,Args),
  collectExpRefs(O,A,R0,R1),
  collectExpListRefs(Args,A,R1,Refs).
collectExpRefs(T,A,R0,Refs) :-
  isTuple(T,Els),
  collectExpListRefs(Els,A,R0,Refs).
collectExpRefs(T,A,R0,Refs) :-
  isSquareTuple(T,Lc,Els),
  collectExpRefs(name(Lc,"[]"),A,R0,R1),
  collectExpRefs(name(Lc,",.."),A,R1,R2), % special handling for list notation
  collectExpListRefs(Els,A,R2,Refs).
collectExpRefs(app(_,Op,Args),All,R,Refs) :-
  collectExpRefs(Op,All,R,R0),
  collectExpRefs(Args,All,R0,Refs).
collectExpRefs(T,All,R,Refs) :-
  isBraceTuple(T,_,Els),
  collectClassRefs(Els,All,R,Refs).
collectExpRefs(T,All,R,Refs) :-
  isSquareTerm(T,Op,A),
  collectExpRefs(Op,All,R,R0),
  collectIndexRefs(A,All,R0,Refs).
collectExpRefs(T,All,R0,Refs) :-
  isBinary(T,"=>",L,R),
  collectExpRefs(L,All,R0,R1),
  collectExpRefs(R,All,R1,Refs).
collectExpRefs(T,All,R0,Refs) :-
  isBinary(T,":-",L,R),
  collectExpRefs(L,All,R0,R1),
  collectCondRefs(R,All,R1,Refs).
collectExpRefs(T,All,R0,Refs) :-
  isBinary(T,"-->",L,R),
  collectExpRefs(L,All,R0,R1),
  collectNTRefs(R,All,R1,Refs).
collectExpRefs(_,_,Refs,Refs).

collectExpListRefs([],_,Refs,Refs).
collectExpListRefs([E|L],A,R0,Refs) :-
  collectExpRefs(E,A,R0,R1),
  collectExpListRefs(L,A,R1,Refs).

collectIndexRefs([A],All,R,Refs) :-
  isBinary(A,_,"->",Ky,Vl),!,
  collectExpRefs(Ky,All,R,R0),
  collectExpRefs(Vl,All,R0,Refs).
collectIndexRefs([A],All,R,Refs) :-
  isUnary(A,_,"\\+",Ky),!,
  collectExpRefs(Ky,All,R,Refs).
collectIndexRefs(A,All,R,Refs) :-
  collectExpListRefs(A,All,R,Refs).

collectPtnListRefs([],_,Refs,Refs).
collectPtnListRefs([E|L],A,R0,Refs) :-
  collectPtnRefs(E,A,R0,R1),
  collectPtnListRefs(L,A,R1,Refs).

collectPtnRefs(P,A,R0,Refs) :-
  isBinary(P,"@@",L,R),
  collectPtnRefs(L,A,R0,R1),
  collectCondRefs(R,A,R1,Refs).
collectPtnRefs(P,A,R0,Refs) :-
  isBinary(P,":",L,R),
  collectPtnRefs(L,A,R0,R1),
  collectTypeRefs(R,A,R1,Refs).
collectPtnRefs(V,A,[var(Nm)|Refs],Refs) :-
  isIden(V,Nm),
  is_member(var(Nm),A).
collectPtnRefs(app(_,Op,Args),All,R0,Refs) :-
  collectExpRefs(Op,All,R0,R1),
  collectPtnRefs(Args,All,R1,Refs).
collectPtnRefs(T,All,R0,Refs) :-
  isBraceTuple(T,_,Els),
  collectFaceRefs(Els,All,R0,Refs).
collectPtnRefs(tuple(_,_,Args),All,R0,Refs) :-
  collectPtnListRefs(Args,All,R0,Refs).
collectPtnRefs(_,_,Refs,Refs).

collectFaceRefs([],_,R,R).
collectFaceRefs([St|L],All,R0,Refs) :-
  collectStmtRefs(St,All,R0,R1),
  collectFaceRefs(L,All,R1,Refs).

collectTypeDefRefs(T,All,SoFar,Refs) :-
  isBinary(T,"|",L,R),
  collectTypeDefRefs(L,All,SoFar,R0),
  collectTypeDefRefs(R,All,R0,Refs).
collectTypeDefRefs(St,_,SoFar,SoFar) :-
  isBinary(St,"@",_,_).
collectTypeDefRefs(St,_,SoFar,SoFar) :-
  isUnary(St,"@",_,_).
collectTypeDefRefs(T,All,R0,Refs) :-
  isRoundTerm(T,_,A),
  collectTypeList(A,All,R0,Refs).
collectTypeDefRefs(T,All,R0,Refs) :-
  isBraceTerm(T,_,_,A),
  collectFaceTypes(A,All,R0,Refs).
collectTypeDefRefs(T,_,Refs,Refs) :-
  isIden(T,_).

collectTypeRefs(V,All,SoFar,Refs) :-
  isIden(V,Nm),
  collectTypeName(Nm,All,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isSquare(T,Nm,A),
  collectTypeName(Nm,All,SoFar,R0),
  collectTypeList(A,All,R0,Refs).
collectTypeRefs(St,_,SoFar,SoFar) :-
  isBinary(St,"@",_,_).
collectTypeRefs(St,_,SoFar,SoFar) :-
  isUnary(St,"@",_,_).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBinary(T,"=>",L,R),
  collectTypeRefs(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBinary(T,"<=",L,R),
  collectTypeRefs(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBinary(T,"<=>",L,R),
  collectTypeRefs(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBinary(T,"-->",L,R),
  collectTypeRefs(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Refs).
collectTypeRefs(T,All,SoFar,Rest) :-
  isBinary(T,"|:",L,R),
  collConstraints(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Rest).
collectTypeRefs(T,All,SoFar,Rest) :-
  isBinary(T,"->>",L,R),
  collectTypeRefs(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Rest).
collectTypeRefs(C,All,SoFar,Refs) :-
  isBinary(C,",",L,R),
  collectTypeRefs(L,All,SoFar,R0),
  collectTypeRefs(R,All,R0,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isUnary(T,"ref",L),
  collectTypeRefs(L,All,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBraceTuple(T,_,A),
  collectFaceTypes(A,All,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isBraceTerm(T,_,_,A),
  collectFaceTypes(A,All,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isQuantified(T,_,A),
  collectTypeRefs(A,All,SoFar,Refs).
collectTypeRefs(T,All,SoFar,Refs) :-
  isTuple(T,Args),
  collectTypeList(Args,All,SoFar,Refs).

collectTypeList([],_,Refs,Refs).
collectTypeList([T|List],All,SoFar,Refs) :-
  isBinary(T,":",_,Tp),!,
  collectTypeRefs(Tp,All,SoFar,R0),
  collectTypeList(List,All,R0,Refs).
collectTypeList([Tp|List],All,SoFar,Refs) :-
  collectTypeRefs(Tp,All,SoFar,R0),
  collectTypeList(List,All,R0,Refs).

collectFaceTypes([],_,Refs,Refs).
collectFaceTypes([T|List],All,SoFar,Refs) :-
  isBinary(T,":",_,Tp),
  collectTypeRefs(Tp,All,SoFar,R0),
  collectFaceTypes(List,All,R0,Refs).

collectTypeName(Nm,All,[tpe(Nm)|SoFar],SoFar) :-
  is_member(tpe(Nm),All),!.
collectTypeName(_,_,Refs,Refs).

collectLabelRefs(Lb,All,R0,Refs) :- collectExpRefs(Lb,All,R0,Refs).

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
