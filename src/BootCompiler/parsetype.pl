:- module(parsetype,[parseType/3,parseType/6,
		     parseBoundTpVars/3,reQuant/3,reQuantX/3,wrapConstraints/3,
		     parseTypeHead/5,
		     parseTypeCore/3,
		     parseContract/4,parseTypeDef/6,
		     pickTypeTemplate/2,typeTemplate/3,
		     parseConstraints/5,parseContractConstraint/6,bindAT/4]).

:- use_module(abstract).
:- use_module(dict).
:- use_module(errors).
:- use_module(freshen).
:- use_module(misc).
:- use_module(unify).
:- use_module(wff).
:- use_module(types).
:- use_module(vartypes).

parseType(T,Env,Type) :-
  parseType(T,Env,[],[],Cons,Tp),
  wrapConstraints(Cons,Tp,Type).

parseType(Tp,Env,B,C,C,PT) :-
  isQuantified(Tp,V,BT),!,
  parseBoundTpVars(V,[],B0),
  concat(B,B0,Q),
  parseType(BT,Env,Q,[],C0,BTp),
  wrapConstraints(C0,BTp,Inner),
  reQuant(B0,Inner,PT).
parseType(Tp,Env,B,C,C,PT) :-
  isXQuantified(Tp,V,BT),!,
  parseBoundTpVars(V,[],B0),
  concat(B,B0,Q),
  parseType(BT,Env,Q,[],C0,BTp),
  wrapConstraints(C0,BTp,Inner),
  reQuantX(B0,Inner,PT).
parseType(F,Env,B,C0,Cx,Tp) :-
  isConstrained(F,T,C),!,
  parseConstraints(C,Env,B,C0,C1),
  parseType(T,Env,B,C1,Cx,Tp).
parseType(Nm,Env,B,C0,Cx,Tp) :-
  isIden(Nm,Lc,Id), !,
  parseTypeName(Lc,Id,Env,B,C0,Cx,Tp).
parseType(Sq,Env,Q,C0,Cx,Tp) :-
  isSquareTerm(Sq,Lc,N,Args),!,
  parseType(N,Env,Q,C0,C1,Op),
  parseTypes(Args,Env,Q,C1,C2,ArgTps),
  freshen(Op,Env,Qx,OOp),
  applyTypeFun(Lc,OOp,ArgTps,Env,C2,Cx,T),
  reBind(Qx,Env,T,Tp).
parseType(F,Env,B,C0,Cx,funType(AT,RT)) :-
  isBinary(F,_,"=>",L,R),
  parseArgType(L,Env,B,C0,C1,AT),
  parseType(R,Env,B,C1,Cx,RT).
parseType(F,Env,B,C0,Cx,consType(AT,RT)) :-
  isBinary(F,_,"<=>",L,R),
  parseArgType(L,Env,B,C0,C1,AT),!,
  parseType(R,Env,B,C1,Cx,RT).
parseType(F,Env,B,C0,Cx,refType(Tp)) :-
  isUnary(F,_,"ref",L),
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
parseType(T,Env,B,Cx,Cx,faceType(AT,FT)) :-
  isBraceTuple(T,_,L),!,
  parseTypeFields(L,Env,B,[],AT,[],FT).
parseType(T,Env,B,C,Cx,typeLambda(AT,RT)) :-
  isTypeLambda(T,_,L,R),
  parseArgType(L,Env,B,C,C0,AT),
  parseType(R,Env,B,C0,Cx,RT).
parseType(Term,Env,_,C,Cx,Tp) :-
  isFieldAcc(Term,Lc,L,Fld),
  isIden(L,Nm),
  isVar(Nm,Env,vrEntry(_,_,RcTp,Fc)),!,
  call(Fc,Env,Face),
  freshen(Face,Env,_,FFace),
  moveConstraints(FFace,C0,faceType(_,Types)),
  fieldInFace(Types,Fld,RcTp,Lc,Tp),
  concat(C0,C,Cx).
parseType(Trm,Env,B,C,Cx,Tp) :-
  isRoundTerm(Trm,Lc,Op,[L,R]),  %% Special case for binary to allow type aliases
  squareTerm(Lc,Op,[L,R],TT),
  parseType(TT,Env,B,C,Cx,Tp),!.
parseType(T,_,_,Cx,Cx,anonType) :-
  locOfAst(T,Lc),
  reportError("cannot understand type %s",[T],Lc).

parseArgType(T,Env,Q,C,Cx,tupleType(AT)) :-
  isTuple(T,A),!,
  parseTypes(A,Env,Q,C,Cx,AT).
parseArgType(T,Env,Q,C,Cx,Tp) :-
  parseType(T,Env,Q,C,Cx,Tp).

fieldInFace(Fields,Nm,_,_,Tp) :-
  is_member((Nm,Tp),Fields),!.
fieldInFace(_,Nm,RcTp,Lc,anonType) :-
  reportError("type %s not declared in %s",[Nm,RcTp],Lc).

parseTypeName(_,"_",_,_,C,C,Tp) :- newTypeVar("_",Tp).
parseTypeName(_,"void",_,_,C,C,voidType).
parseTypeName(_,"this",_,_,C,C,thisType).
parseTypeName(_,Id,_,Q,C,C,Tp) :- is_member((Id,Tp),Q),!.
parseTypeName(_,Id,Env,_,C,C,Tp) :-
  isType(Id,Env,tpDef(_,T,TpDf)),
  (TpDf=typeLambda(tupleType([]),Tp);T=Tp).
parseTypeName(Lc,Id,_,_,C,C,anonType) :-
  reportError("type %s not declared",[Id],Lc).

applyTypeFun(_,kFun(T,Ar),Args,_,Cx,Cx,Tp) :-
  length(Args,Ar),!,
  mkTypeExp(kFun(T,Ar),Args,Tp).
applyTypeFun(_,tFun(T,B,Ar,Id),Args,_,Cx,Cx,Tp) :-
  length(Args,AAr),AAr=<Ar,!,
  mkTypeExp(tFun(T,B,Ar,Id),Args,Tp).
applyTypeFun(_,tpFun(T,Ar),Args,_,Cx,Cx,Tp) :-
  length(Args,AAr),AAr=<Ar,!,
  mkTypeExp(tpFun(T,Ar),Args,Tp).
applyTypeFun(Lc,constrained(Tp,Ct),ArgTps,Env,C,Cx,ATp) :-
  applyTypeFun(Lc,Tp,ArgTps,Env,[Ct|C],Cx,ATp),!.
applyTypeFun(Lc,typeLambda(L,Tp),[A|ArgTps],Env,C,Cx,RTp) :-
  sameType(L,A,Env),!,
  applyTypeFun(Lc,Tp,ArgTps,Env,C,Cx,RTp).
applyTypeFun(_,Tp,[],_,C,C,Tp).
applyTypeFun(Lc,Op,ArgTps,_,Cx,Cx,voidType) :-
  reportError("type %s not applicable to args %s",[Op,ArgTps],Lc).

bindAT([],_,Q,Q).
bindAT(_,[],Q,Q).
bindAT([kVar(V)|L],[Tp|TL],Q,Qx) :-
  bindAT(L,TL,[(V,Tp)|Q],Qx).
bindAT([kFun(N,Ar)|L1],[kFun(N2,Ar)|L2],Q,Qx) :-
  bindAT(L1,L2,[(N,kFun(N2,Ar))|Q],Qx).
bindAT([kFun(V,Ar)|L],[tpFun(Nm,Ar)|TL],Q,Qx) :-
  bindAT(L,TL,[(V,tpFun(Nm,Ar))|Q],Qx).

parseBoundTpVars([],Q,Q).
parseBoundTpVars([V|L],Q,Qx) :-
  parseBoundVar(V,Q,Q0),
  parseBoundTpVars(L,Q0,Qx).

parseBoundVar(N,Q,[(Nm,kVar(Nm))|Q]) :-
  isIden(N,Nm).
parseBoundVar(N,Q,[(Nm,kFun(Nm,Ar))|Q]) :-
  isBinary(N,_,"/",L,R),
  isInteger(R,Ar),
  isIden(L,Nm).
parseBoundVar(N,Q,Q) :-
  locOfAst(N,Lc),
  reportError("invalid bound variable: %s",[N],Lc).

% reapply quantifiers to a type to get full form
reQuant([],Tp,Tp).
reQuant([(_,KV)|M],Tp,QTp) :-
  reQuant(M,allType(KV,Tp),QTp).

reQuantX([],Tp,Tp).
reQuantX([(_,KV)|M],Tp,QTp) :-
  reQuantX(M,existType(KV,Tp),QTp).

reBind([],_,Tp,Tp).
reBind([(Nm,TV)|Q],Env,T,Tp) :-
  isUnboundFVar(TV,Ar),!,
  sameType(TV,kFun(Nm,Ar),Env),
  reBind(Q,Env,allType(kFun(Nm,Ar),T),Tp).
reBind([(Nm,TV)|Q],Env,T,Tp) :-
  isUnbound(TV),!,
  sameType(TV,kVar(Nm),Env),
  reBind(Q,Env,allType(kVar(Nm),T),Tp).
reBind([_|Q],Env,T,Tp) :-
  reBind(Q,Env,T,Tp).

wrapConstraints([],Tp,Tp).
wrapConstraints([Con|C],Tp,WTp) :-
  wrapConstraints(C,constrained(Tp,Con),WTp).

parseTypeFace(T,Env,Bound,AT,FT) :-
  isBraceTuple(T,_,L),
  parseTypeFields(L,Env,Bound,[],AT,[],FT).
parseTypeFace(T,_,_,[],[]) :-
  locOfAst(T,Lc),
  reportError("%s is not a type interface",[T],Lc).

parseConstraint(T,Env,B,C0,Cx) :-
  isBinary(T,_,",",L,R),
  parseConstraint(L,Env,B,C0,C1),
  parseConstraint(R,Env,B,C1,Cx).
parseConstraint(T,Env,B,C0,Cx) :-
  isBinary(T,_,"<~",L,R),
  parseType(L,Env,B,C0,C1,TV),
  parseType(R,Env,B,C1,C2,AT),
  addConstraint(implementsFace(TV,AT),C2,Cx).
parseConstraint(Sq,Env,B,C0,Cx) :-
  isSquare(Sq,Lc,N,Args),
  parseContractArgs(Args,Env,B,C0,C1,ArgTps,Deps),
  ( parseContractName(Lc,N,Env,B,contractExists(conTract(Op,_ATs,_Dps),_)) ->
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

parseContractConstraint(Quants,Cons,Sq,Env,Op,ConSpec) :-
  isSquare(Sq,Lc,N,Args),
  parseBoundTpVars(Quants,[],Q),
  parseConstraints(Cons,Env,Q,[],C0),
  parseContractArgs(Args,Env,Q,C0,C1,ArgTps,Deps),
  ( parseContractName(Lc,N,Env,Q,contractExists(conTract(Op,ATs,Dps),IFace)) ->
      ( sameType(tupleType(ATs),tupleType(ArgTps),Env),
        simplifyType(tupleType(ATs),Env,C1,C2,tupleType(As)),
        sameType(tupleType(Dps),tupleType(Deps),Env) ->
          simplifyType(tupleType(Dps),Env,C2,Cx,tupleType(Ds)),
          moveConstraints(CC,Cx,contractExists(conTract(Op,As,Ds),IFace)),
          reQuant(Q,CC,ConSpec);
          reportError("implementation does not match contract %s",[Op],Lc),
          fail);
    reportError("contract %s not declared",[N],Lc), fail).

addConstraint(Con,C0,C0) :- is_member(Con,C0),!.
addConstraint(Con,C0,[Con|C0]).

parseContractName(_,Id,Env,_,FCon) :-
  getContract(Id,Env,conDef(_,_,Con)),!,
  freshen(Con,Env,_,FCon).

parseContractArgs([A],Env,B,C0,Cx,Args,Deps) :-
  isBinary(A,_,"->>",L,R),!,
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

parseTypeFields([],_,_,Flds,Flds,Tps,Tps).
parseTypeFields([F|L],Env,Bound,Flds,Fields,Tps,Types) :-
  parseTypeField(F,Env,Bound,Flds,F0,Tps,T0),
  parseTypeFields(L,Env,Bound,F0,Fields,T0,Types).

parseTypeField(F,Env,Bound,Flds,[(Fld,FldTp)|Flds],Types,Types) :-
  isTypeAnnotation(F,_,Nm,FT),
  isIden(Nm,Lc,Fld),
  parseType(FT,Env,Bound,[],Cx,FldTp),
  (Cx=[] -> true ; reportError("unexpected constraints in field type %s",[Nm],Lc)).
parseTypeField(S,Env,Bound,Flds,Flds,Types,[(Fld,FldTp)|Types]) :-
  isUnary(S,_,"type",F),
  isTypeAnnotation(F,_,Nm,FT),
  isIden(Nm,Lc,Fld),
  parseType(FT,Env,Bound,[],Cx,FldTp),
  (Cx=[] -> true ; reportError("unexpected constraints in field type %s",[Nm],Lc)).
parseTypeField(F,_,_,Fields,Fields,Types,Types) :-
  isBinary(F,_,"@",_,_).
parseTypeField(F,_,_,Fields,Fields,Types,Types) :-
  isBinary(F,_,"@",_,_,_).
parseTypeField(FS,_,_,Fields,Fields,Types,Types) :-
  locOfAst(FS,Lc),
  reportError("invalid field type %s",[FS],Lc).

parseContract(T,Env,Path,conDef(Nm,ConNm,ConRule)) :-
  isContractStmt(T,_,Quants,C0,Con,Els),
  parseBoundTpVars(Quants,[],Q),
  parseContractSpec(Con,Q,C0,Cx,Env,SpC,Nm,ConNm,Path),
  parseTypeFields(Els,Env,Q,[],Fc,[],Tps),
  moveConstraints(Crl,Cx,contractExists(SpC,faceType(Fc,Tps))),
  reQuant(Q,Crl,ConRule).

parseContractSpec(T,Q,C0,Cx,Env,conTract(ConNm,ArgTps,Deps),Nm,ConNm,Path) :-
  isSquare(T,_,Nm,A),
  parseContractArgs(A,Env,Q,C0,Cx,ArgTps,Deps),
  marker(conTract,Marker),
  subPath(Path,Marker,Nm,ConNm).

parseTypeDef(St,[Defn|Dx],Dx,E,Ev,Path) :-
  isTypeExistsStmt(St,Lc,Quants,Ct,Hd,Body),!,
  parseTypeExists(Lc,Quants,Ct,Hd,Body,Defn,E,Ev,Path).
parseTypeDef(St,[Defn|Dx],Dx,E,Ev,Path) :-
  isTypeFunStmt(St,Lc,Quants,Ct,Hd,Bd),
  parseTypeFun(Lc,Quants,Ct,Hd,Bd,Defn,E,Ev,Path).

parseTypeExists(Lc,Quants,Ct,Hd,Body,typeDef(Lc,Nm,Type,FaceRule),E,Ev,Path) :-
  parseBoundTpVars(Quants,[],Q),
  parseTypeHead(Hd,Q,Tp,Nm,Path),
  parseConstraints(Ct,E,Q,[],C0),
  parseType(Body,E,Q,C0,Cx,RTp),
  wrapConstraints(Cx,typeExists(Tp,RTp),Rl),
  reQuant(Q,Rl,FaceRule),
  reQuant(Q,Tp,Type),
  pickTypeTemplate(Type,Tmp),
  declareType(Nm,tpDef(Lc,Tmp,FaceRule),E,Ev).

parseTypeFun(Lc,Quants,Ct,Hd,Bd,typeDef(Lc,Nm,Type,Rule),E,Ev,Path) :-
  parseBoundTpVars(Quants,[],Q),
  parseConstraints(Ct,E,Q,[],C0),
  parseTypeHead(Hd,Q,Tp,Nm,Path),
  parseType(Bd,E,Q,C0,Cx,RpTp),
  pickTypeTemplate(Tp,Type),
  mkTypeLambda(Tp,RpTp,Lam),
  wrapConstraints(Cx,Lam,Rl),
  reQuant(Q,Rl,Rule),
  declareType(Nm,tpDef(Lc,Type,Rule),E,Ev).

mkTypeLambda(tpExp(Op,A),Tp,typeLambda(A,RRTp)) :-
  mkTypeLambda(Op,Tp,RRTp).
mkTypeLambda(tpFun(_,_),Tp,Tp).
mkTypeLambda(type(_),Tp,typeLambda(tupleType([]),Tp)).

pickTypeTemplate(existType(_,Tp),Tmp) :-
  pickTypeTemplate(Tp,Tmp).
pickTypeTemplate(allType(_,Tp),XTp) :-
  pickTypeTemplate(Tp,XTp).
pickTypeTemplate(typeExists(Lhs,_),Tmp) :-
  pickTypeTemplate(Lhs,Tmp).
pickTypeTemplate(typeLambda(Lhs,_),Tmp) :-
  pickTypeTemplate(Lhs,Tmp).
pickTypeTemplate(constrained(Tp,_),Tmp) :-
  pickTypeTemplate(Tp,Tmp).
pickTypeTemplate(type(Nm),type(Nm)).
pickTypeTemplate(tpExp(Op,_),Tmp) :-
  pickTypeTemplate(Op,Tmp).
pickTypeTemplate(tpFun(Nm,Ar),tpFun(Nm,Ar)).
pickTypeTemplate(kFun(Nm,Ar),kFun(Nm,Ar)).

typeFunTemplate(Nm,Ix,existType(_,Tp),Tmp) :-
  typeFunTemplate(Nm,Ix,Tp,Tmp).
typeFunTemplate(Nm,Ix,allType(_,Tp),XTp) :-
  typeFunTemplate(Nm,Ix,Tp,XTp).
typeFunTemplate(Nm,Ix,typeLambda(Lhs,_),Tmp) :-
  Ix1 is Ix+1,
  typeFunTemplate(Nm,Ix1,Lhs,Tmp).
typeFunTemplate(Nm,0,_,type(Nm)).
typeFunTemplate(Nm,Ix,_,tpFun(Nm,Ix)).

typeTemplate(_Nm,Tp,Tmp) :-
  pickTypeTemplate(Tp,Tmp),!.
typeTemplate(Nm,Tp,Tmp) :-
  typeFunTemplate(Nm,0,Tp,Tmp).

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
parseTypeCore(St,Type,Path) :-
  isAlgebraicTypeStmt(St,_,Quants,_,Hd,_),
  parseBoundTpVars(Quants,[],Q),
  parseTypeHead(Hd,Q,Tp,_,Path),
  reQuant(Q,Tp,Type).

parseTypeHead(N,_,type(TpNm),Nm,Path) :-
  isIden(N,_,Nm),
  marker(type,Marker),
  subPath(Path,Marker,Nm,TpNm).
parseTypeHead(N,B,Tp,Nm,Path) :-
  isSquare(N,_,Nm,A),
  parseHeadArgs(A,B,Args),
  length(Args,Ar),
  marker(type,Marker),
  subPath(Path,Marker,Nm,TpNm),
  mkTypeExp(tpFun(TpNm,Ar),Args,Tp).
parseTypeHead(N,B,Tp,Nm,Path) :-
  isRoundTerm(N,Lc,Op,Els),
  squareTerm(Lc,Op,Els,TT),
  parseTypeHead(TT,B,Tp,Nm,Path).

parseHeadArgs([],_,[]).
parseHeadArgs([H|L],B,[V|Args]) :-
  isIden(H,Lc,Nm),
  (is_member((Nm,V),B) ; reportError("type argument %s not quantified ",[H],Lc)),
  parseHeadArgs(L,B,Args).
