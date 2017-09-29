:- module(parsetype,[parseType/3,
  parseTypeDef/4,
  parseTypeRule/4,parseTypeCore/3,parseContract/4,parseContractConstraint/4,rewriteConstraints/4,bindAT/4]).

:- use_module(abstract).
:- use_module(dict).
:- use_module(errors).
:- use_module(freshen).
:- use_module(misc).
:- use_module(unify).
:- use_module(wff).
:- use_module(types).

parseType(T,Env,Type) :-
  parseType(T,Env,[],[],Cons,Tp),
  wrapConstraints(Cons,Tp,Type).

parseType(Tp,Env,B,C,C,PT) :-
  isQuantified(Tp,V,BT),!,
  parseBound(V,B,B0,PT,Inner),
  parseType(BT,Env,B0,[],C0,BTp),
  wrapConstraints(C0,BTp,Inner).
parseType(F,Env,B,C0,Cx,Tp) :-
  isBinary(F,"|:",L,R),!,
  parseConstraint(L,Env,B,C0,C1),
  parseType(R,Env,B,C1,Cx,Tp).
parseType(Nm,Env,B,C0,Cx,Tp) :-
  isIden(Nm,Lc,Id), !,
  parseTypeName(Lc,Id,Env,B,C0,Cx,Tp).
parseType(Sq,Env,B,C0,Cx,Type) :-
  isSquare(Sq,Lc,N,Args),!,
  parseTypeSquare(Lc,N,Args,Env,B,C0,Cx,Type).
parseType(F,Env,B,C0,Cx,funType(AT,RT)) :-
  isBinary(F,"=>",L,R),
  isTuple(L,LA),!,
  parseTypes(LA,Env,B,C0,C1,AT),
  parseType(R,Env,B,C1,Cx,RT).
parseType(F,Env,B,C0,Cx,ptnType(AT,RT)) :-
  isBinary(F,"<=",L,R),
  isTuple(L,LA),!,
  parseTypes(LA,Env,B,C0,C1,AT),
  parseType(R,Env,B,C1,Cx,RT).
parseType(F,Env,B,C0,Cx,grammarType(AT,RT)) :-
  isBinary(F,"-->",L,R),
  isTuple(L,LA),!,
  parseTypes(LA,Env,B,C0,C1,AT),
  parseType(R,Env,B,C1,Cx,RT).
parseType(F,Env,B,C0,Cx,consType(AT,RT)) :-
  isBinary(F,"<=>",L,R),
  isTuple(L,LA),!,
  parseTypes(LA,Env,B,C0,C1,AT),!,
  parseType(R,Env,B,C1,Cx,RT).
parseType(F,Env,B,C0,Cx,refType(Tp)) :-
  isUnary(F,"ref",L),
  parseType(L,Env,B,C0,Cx,Tp).
parseType(T,Env,B,C0,Cx,tupleType(AT)) :-
  isTuple(T,[A]),
  isTuple(A,Inner),!,
  parseTypes(Inner,Env,B,C0,Cx,AT).
parseType(T,Env,B,C0,Cx,AT) :-
  isTuple(T,[A]),!,
  parseType(A,Env,B,C0,Cx,AT).
parseType(T,Env,B,C0,Cx,tupleType(AT)) :-
  isTuple(T,A),!,
  parseTypes(A,Env,B,C0,Cx,AT).
parseType(T,Env,B,C0,Cx,faceType(AT)) :-
  isBraceTuple(T,_,L),!,
  parseTypeFields(L,Env,B,C0,Cx,[],AT).
parseType(T,_,_,Cx,Cx,anonType) :-
  locOfAst(T,Lc),
  reportError("cannot understand type %s",[T],Lc).

parseTypeName(_,"_",_,_,C,C,anonType).
parseTypeName(_,"void",_,_,C,C,voidType).
parseTypeName(_,"this",_,_,C,C,thisType).
parseTypeName(_,Id,_,Q,C,C,Tp) :- is_member((Id,Tp),Q),!.
parseTypeName(_,Id,Env,Q,C,C,Tp) :-
  isType(Id,Env,tpDef(_,TpSpec,_)),
  freshen(TpSpec,voidType,Q,_,Tp).
parseTypeName(Lc,Id,_,_,C,C,anonType) :-
  reportError("type %s not declared",[Id],Lc).

parseTypeSquare(Lc,Id,Args,Env,Q,C0,Cx,Tp) :-
  parseTypeName(Lc,Id,Env,Q,C0,C1,Op),
  parseTypes(Args,Env,Q,C1,Cx,ArgTps),
  applyTypeExp(Lc,Op,ArgTps,Env,Tp).

applyTypeExp(_,typeExp(Op,ATps),ArgTps,Env,typeExp(DOp,ArgTps)) :-
  deRef(Op,DOp),
  length(ArgTps,Ar),
  validTypeOp(DOp,Ar),
  smList(ATps,ArgTps,Env).
applyTypeExp(Lc,Op,ArgTps,_,voidType) :-
  reportError("type %s not applicable to args %s",[Op,ArgTps],Lc).

validTypeOp(kFun(_,Ar),Ar).
validTypeOp(tpFun(_,Ar),Ar).

bindAT([],_,Q,Q).
bindAT(_,[],Q,Q).
bindAT([kVar(V)|L],[Tp|TL],Q,Qx) :-
  bindAT(L,TL,[(V,Tp)|Q],Qx).
bindAT([kFun(N,Ar)|L1],[kFun(N2,Ar)|L2],Q,Qx) :-
  bindAT(L1,L2,[(N,kFun(N2,Ar))|Q],Qx).
bindAT([kFun(V,Ar)|L],[tpFun(Nm,Ar)|TL],Q,Qx) :-
  bindAT(L,TL,[(V,tpFun(Nm,Ar))|Q],Qx).

rewriteConstraints([],_,Cx,Cx).
rewriteConstraints([Con|Cons],Q,C0,Cx) :-
  frshnConstraint(Con,Q,FCon),
  addConstraint(FCon,C0,C1),
  rewriteConstraints(Cons,Q,C1,Cx).

parseBound(P,BV,Bound,QT,Inner) :-
  isBinary(P,",",L,R),
  parseBound(L,BV,B0,QT,Q0),
  parseBound(R,B0,Bound,Q0,Inner).
parseBound(V,B,[(N,kVar(N))|B],univType(kVar(N),Inner),Inner) :-
  isIden(V,N).
parseBound(V,B,[(N,kFun(N,Ar))|B],univType(kFun(Nm,Ar),Inner),Inner) :-
  isBinary(V,"/",L,R),
  isInteger(R,Ar),
  isIden(L,_,Nm).
parseBound(T,B,B,Inner,Inner) :-
  locOfAst(T,Lc),
  reportError("invalid bound variable: %s",[T],Lc).

parseBoundTpVars([],Q,Q).
parseBoundTpVars([V|L],Q,Qx) :-
  parseBoundVar(V,Q,Q0),
  parseBoundTpVars(L,Q0,Qx).

parseBoundVar(N,Q,[(Nm,kVar(Nm))|Q]) :-
  isIden(N,Nm).
parseBoundVar(N,Q,[(Nm,kFun(Nm,Ar))|Q]) :-
  isBinary(N,"/",L,R),
  isInteger(R,Ar),
  isIden(L,Nm).
parseBoundVar(N,Q,Q) :-
  locOfAst(N,Lc),
  reportError("invalid bound variable: %s",[N],Lc).

parseTypeFace(T,Env,Bound,C0,Cx,faceType(AT)) :-
  isBraceTuple(T,_,L),
  parseTypeFields(L,Env,Bound,C0,Cx,[],AT).
parseTypeFace(T,_,_,Cx,Cx,faceType([])) :-
  locOfAst(T,Lc),
  reportError("%s is not a type interface",[T],Lc).

parseConstraint(T,Env,B,C0,Cx) :-
  isBinary(T,",",L,R),
  parseConstraint(L,Env,B,C0,C1),
  parseConstraint(R,Env,B,C1,Cx).
parseConstraint(T,Env,B,C0,Cx) :-
  isBinary(T,"<~",L,R),
  parseType(L,Env,B,C0,C1,TV),
  parseTypeFace(R,Env,B,C1,C2,faceType(AT)),
  addConstraint(implementsFace(TV,AT),C2,Cx).
parseConstraint(Sq,Env,B,C0,Cx) :-
  isSquare(Sq,Lc,N,Args),
  parseContractArgs(Args,Env,B,C0,C1,ArgTps,Deps),
  ( parseContractName(Lc,N,Env,B,conTract(Op,_ATs,_Dps)) ->
      addConstraint(conTract(Op,ArgTps,Deps),C1,Cx) ;
      reportError("contract %s not declared",[N],Lc),
      Cx=C1).
parseConstraint(T,_,B,B,C,C) :-
  locOfAst(T,Lc),
  reportError("invalid type constraint %s",[T],Lc).

parseConstraints([],_,_,C,C).
parseConstraints([Ct|L],E,Q,C,Cx) :-
  parseConstraint(Ct,E,Q,C,C0),
  parseConstraints(L,E,Q,C0,Cx).

parseContractConstraint(T,Env,N,Con) :-
  parseContractConstraint(T,Env,[],[],Cons,N,Tp),!,
  wrapConstraints(Cons,Tp,Con).

parseContractConstraint(Tp,Env,B,C,C,N,PT) :-
  isQuantified(Tp,V,BT),
  parseBound(V,B,B0,PT,Inner),
  parseContractConstraint(BT,Env,B0,[],C0,N,BTp),
  wrapConstraints(C0,BTp,Inner).
parseContractConstraint(Tp,Env,B,C0,Cx,N,Cn) :-
  isBinary(Tp,"|:",L,R),
  parseConstraint(L,Env,B,C0,C1),
  parseContractConstraint(R,Env,B,C1,Cx,N,Cn).
parseContractConstraint(Sq,Env,Q,C0,Cx,N,conTract(Op,ArgTps,Deps)) :-
  isSquare(Sq,Lc,N,Args),
  parseContractArgs(Args,Env,Q,C0,Cx,ArgTps,Deps),
  ( parseContractName(Lc,N,Env,Q,conTract(Op,ATs,Dps)) ->
      sameType(tupleType(ATs),tupleType(ArgTps),Env),
      sameType(tupleType(Dps),tupleType(Deps),Env)
    | reportError("contract %s not declared",[N],Lc), Op = N).

addConstraint(Con,C0,C0) :- is_member(Con,C0),!.
addConstraint(Con,C0,[Con|C0]).

parseContractName(_,Id,Env,_,FTp) :-
  getContract(Id,Env,contract(_,_,Con,_,_)),
  freshenConstraint(Con,FTp).

parseContractArgs([A],Env,B,C0,Cx,Args,Deps) :-
  isBinary(A,"->>",L,R),!,
  deComma(L,LA),
  deComma(R,RA),
  parseTypes(LA,Env,B,C0,C1,Args),
  parseTypes(RA,Env,B,C1,Cx,Deps).
parseContractArgs(A,Env,B,C0,Cx,Args,[]) :-
  parseTypes(A,Env,B,C0,Cx,Args).

parseTypes([],_,_,C,C,[]).
parseTypes([A|AT],Env,B,C0,Cx,[Atype|ArgTypes]) :-
  parseType(A,Env,B,C0,C1,Atype),
  parseTypes(AT,Env,B,C1,Cx,ArgTypes).

parseTypeFields([],_,_,Cx,Cx,Flds,Flds).
parseTypeFields([F|L],Env,Bound,C0,Cx,Flds,Fields) :-
  isBinary(F,":",Nm,FT),
  isIden(Nm,Fld),
  parseType(FT,Env,Bound,C0,C1,FldTp),
  parseTypeFields(L,Env,Bound,C1,Cx,[(Fld,FldTp)|Flds], Fields).
parseTypeFields([F|L],Env,Bound,C,Cx,Flds,Fields) :-
  isBinary(F,"@",_,_),
  parseTypeFields(L,Env,Bound,C,Cx,Flds, Fields).
parseTypeFields([F|L],Env,Bound,C,Cx,Flds,Fields) :-
  isUnary(F,"@",_,_),
  parseTypeFields(L,Env,Bound,C,Cx,Flds, Fields).

parseContract(T,Env,Path,contract(Nm,ConNm,Spec,FullSpec,Face)) :-
  isContractStmt(T,_,Quants,C0,Con,Els),
  parseBoundTpVars(Quants,[],Q),
  parseContractSpec(Con,Q,C0,Cx,Env,Spc,Nm,ConNm,Path),
  parseTypeFields(Els,Env,Q,Cx,_,[],Fc),
  reQuant(Q,faceType(Fc),Face),
  reQuant(Q,Spc,Spec),
  moveConstraints(SpcC,Cx,Spc),
  reQuant(Q,SpcC,FullSpec).

% reapply quantifiers to a type to get full form
reQuant([],Tp,Tp).
reQuant([(Nm,_)|M],Tp,QTp) :-
  reQuant(M,univType(kVar(Nm),Tp),QTp).

wrapConstraints([],Tp,Tp).
wrapConstraints([Con|C],Tp,WTp) :-
  wrapConstraints(C,constrained(Tp,Con),WTp).

parseContractSpec(T,Q,C0,Cx,Env,conTract(ConNm,ArgTps,Deps),Nm,ConNm,Path) :-
  isSquare(T,_,Nm,A),
  parseContractArgs(A,Env,Q,C0,Cx,ArgTps,Deps),
  marker(conTract,Marker),
  subPath(Path,Marker,Nm,ConNm).

parseTypeDef(St,E,Path,typeDef(Lc,Nm,Type,FaceRule)) :-
  isTypeExistsStmt(St,Lc,Quants,Ct,Hd,Body),
  parseBoundTpVars(Quants,[],Q),
  parseTypeHead(Hd,Q,Tp,Nm,Path),
  isBraceTuple(Body,_,Els),
  parseConstraints(Ct,E,Q,[],C0),
  parseTypeFields(Els,E,Q,C0,Cx,[],F),
  wrapConstraints(Cx,typeExists(Tp,faceType(F)),Rl),
  reQuant(Q,Rl,FaceRule),
  reQuant(Q,Tp,Type).
parseTypeDef(St,E,Path,typeDef(Lc,Nm,Type,FaceRule)) :-
  isTypeFunStmt(St,Lc,Quants,Ct,Hd,Bd),
  parseBoundTpVars(Quants,[],Q),
  parseConstraints(Ct,E,Q,[],C0),
  parseTypeHead(Hd,Q,Tp,Nm,Path),
  parseType(Bd,E,Q,C0,Cx,RpTp),
  wrapConstraints(Cx,typeLambda(Tp,RpTp),Rl),
  reQuant(Q,Rl,FaceRule),
  reQuant(Q,Tp,Type).

computeFaceOfCons(T,Q,E,F0,Fx) :-
  isBinary(T,"|",L,R),
  computeFaceOfCons(L,Q,E,F0,F1),
  computeFaceOfCons(R,Q,E,F1,Fx).
computeFaceOfCons(T,Q,E,F0,Fx) :-
  isBraceTerm(T,_,_,Els),
  parseTypeFields(Els,E,Q,[],_,F0,Fx).
computeFaceOfCons(_,_,_,F,F).

parseTypeRule(St,Env,Rule,Path) :-
  parseTypeRule(St,[],[],_,Env,Rule,Path).

parseTypeRule(St,B,C,C,Env,Rule,Path) :-
  isQuantified(St,V,Body),
  parseBound(V,B,B0,Rule,Rl),
  parseTypeRule(Body,B0,[],Cx,Env,Inner,Path),
  wrapConstraints(Cx,Inner,Rl).
parseTypeRule(St,B,C0,Cx,Env,Rule,Path) :-
  isBinary(St,"|:",L,R),
  parseConstraint(L,Env,B,C0,C1),
  parseTypeRule(R,B,C1,Cx,Env,Rule,Path).
parseTypeRule(St,B,C0,Cx,Env,typeExists(Lhs,Rhs),Path) :-
  isBinary(St,"<~",L,R),
  parseTypeHead(L,B,Lhs,_,Path),!,
  parseType(R,Env,B,C0,Cx,Tp),
  deRef(Tp,DTp),
  getFace(DTp,Env,Rhs).

getFace(faceType(F),_,faceType(F)) :- !.
getFace(type(Nm),Env,F) :-
  isType(Nm,Env,tpDef(_,_,FR)),
  moveQuants(FR,_,FQR),
  moveConstraints(FQR,_,typeExists(_,F)).
getFace(typeExp(Op,Args),Env,Face) :-
  deRef(Op,type(Nm)),
  isType(Nm,Env,tpDef(_,_,FR)),
  moveQuants(FR,_,FQR),
  moveConstraints(FQR,_,typeExists(typeExp(_,AT),F)),
  bindAT(AT,Args,[],BB),
  rewriteType(F,BB,Face).
getFace(type(Nm),Env,Face) :- !,
  isType(Nm,Env,tpDef(_,_,FaceRule)),
  freshen(FaceRule,voidType,[],_,typeExists(Lhs,Face)),
  sameType(type(Nm),Lhs,Env),!.
getFace(T,Env,faceType(Face)) :- isUnbound(T), !,
  constraints(T,C),
  collectImplements(C,Env,[],Face).

parseTypeCore(St,Type,Path) :-
  isTypeExistsStmt(St,_,Quants,_,Head,_),
  parseBoundTpVars(Quants,[],Q),
  parseTypeHead(Head,Q,Tp,_,Path),
  reQuant(Q,Tp,Type).
parseTypeCore(St,Type,Path) :-
  isTypeFunStmt(St,_,Quants,_,Head,_),
  parseBoundTpVars(Quants,[],Q),
  parseTypeHead(Head,Q,Tp,_,Path),
  reQuant(Q,Tp,Type).

parseTypeHead(N,_,type(TpNm),Nm,Path) :-
  isIden(N,_,Nm),
  marker(type,Marker),
  subPath(Path,Marker,Nm,TpNm).
parseTypeHead(N,B,typeExp(tpFun(TpNm,Ar),Args),Nm,Path) :-
  isSquare(N,_,Nm,A),
  parseHeadArgs(A,B,Args),
  length(Args,Ar),
  marker(type,Marker),
  subPath(Path,Marker,Nm,TpNm).

parseHeadArgs([],_,[]).
parseHeadArgs([H|L],B,[V|Args]) :-
  isIden(H,Lc,Nm),
  (is_member((Nm,V),B) ; reportError("type argument %s not quantified ",[H],Lc)),
  parseHeadArgs(L,B,Args).
