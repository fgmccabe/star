:- module(parsetype,[parseType/3,
		     parseTypeCore/4,
		     parseContract/6,parseTypeDef/6,
		     typeTemplate/3,
		     parseContractConstraint/7,
		     genBraceType/6,buildBraceAccessors/8,
		     unwrapType/4,wrapType/4]).

:- use_module(abstract).
:- use_module(canon).
:- use_module(dict).
:- use_module(errors).
:- use_module(freshen).
:- use_module(misc).
:- use_module(unify).
:- use_module(wff).
:- use_module(types).

parseType(T,Env,Type) :-
  parseType(T,Env,[],Cons,[],Tp),
  wrapConstraints(Cons,Tp,Type).

parseType(Tp,Env,B,C,C,PT) :-
  isQuantified(Tp,V,BT),!,
  parseBoundTpVars(V,B0),
  concat(B,B0,Q),
  parseType(BT,Env,Q,C0,[],BTp),
  wrapType(B0,C0,[],[],BTp,PT).
parseType(Tp,Env,B,C,C,PT) :-
  isXQuantified(Tp,V,BT),!,
  parseBoundTpVars(V,B0),
  concat(B,B0,Q),
  parseType(BT,Env,Q,C0,[],BTp),
  wrapType([],[],B0,C0,BTp,PT).
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
  freshen(Op,Env,_Qx,OOp),
  doTypeFun(Lc,OOp,ArgTps,Env,C2,Cx,Tp).
parseType(F,Env,B,C0,Cx,funType(AT,RT)) :-
  isFuncType(F,_,L,R),
  parseArgType(L,Env,B,C0,C1,AT),
  parseType(R,Env,B,C1,Cx,RT).
parseType(F,Env,B,C0,Cx,consType(AT,RT)) :-
  isConstructorType(F,_,_,_,L,R),!, % quantifiers already handled
  parseArgType(L,Env,B,C0,C1,AT),!,
  parseType(R,Env,B,C1,Cx,RT).
parseType(F,Env,B,C0,Cx,throwsType(AT,RT)) :-
  isThrows(F,_,L,R),
  parseType(L,Env,B,C0,C1,AT),
  parseType(R,Env,B,C1,Cx,RT).
parseType(F,Env,B,C0,Cx,refType(Tp)) :-
  isRef(F,_,L),
  parseType(L,Env,B,C0,Cx,Tp).
parseType(T,Env,B,C0,Cx,tplType(AT)) :-
  isTuple(T,[A]),
  isTuple(A,Inner),!,
  parseTypes(Inner,Env,B,C0,Cx,AT).
parseType(T,Env,B,C0,Cx,AT) :-
  isTuple(T,[A]),!,
  parseType(A,Env,B,C0,Cx,AT).
parseType(T,Env,B,C0,Cx,tplType(AT)) :-
  isTuple(T,A),!,
  parseTypes(A,Env,B,C0,Cx,AT).
parseType(T,Env,B,Cx,Cx,faceType(AT,FT)) :-
  isBraceTuple(T,_,L),!,
  parseTypeFields(L,Env,B,[],AT,[],FT).
parseType(T,Env,B,C,Cx,typeLambda(AT,RT)) :-
  isTypeLambda(T,_,L,R),
  parseArgType(L,Env,B,C,C0,AT),
  parseType(R,Env,B,C0,Cx,RT).
parseType(Term,Env,_,Cx,Cx,Tp) :-
  isFieldAcc(Term,Lc,L,Fld),
  parseFieldAccType(Lc,L,Fld,Env,Tp).
parseType(Trm,Env,B,C,Cx,Tp) :-
  isRoundTerm(Trm,Lc,Op,[L,R]),  %% Special case for binary to allow type aliases
  squareTerm(Lc,Op,[L,R],TT),
  parseType(TT,Env,B,C,Cx,Tp),!.
parseType(T,_,_,Cx,Cx,anonType) :-
  locOfAst(T,Lc),
  reportError("cannot understand type %s",[ast(T)],Lc).

parseFieldAccType(Lc,L,Fld,Env,Tp) :-
  isIden(L,Nm),
  (getVarTypeFace(Lc,Nm,Env,VTp) ->
   faceOfType(VTp,Lc,Env,faceType(_,Types)),
   (fieldInFace(Types,Fld,VTp,Lc,Tp) ;
    reportError("%s not part of type of %s:%s",[id(Fld),ast(L),tpe(VTp)],Lc),
    newTypeVar("_",Tp));
   reportError("%s not a known variable",[ast(L)],Lc),
   newTypeVar("_",Tp)).

parseArgType(T,Env,Q,C,Cx,tplType(AT)) :-
  isTuple(T,A),!,
  parseTypes(A,Env,Q,C,Cx,AT).
parseArgType(T,Env,Q,C,Cx,Tp) :-
  parseType(T,Env,Q,C,Cx,Tp).

fieldInFace(Fields,Nm,_,_,Tp) :-
  is_member((Nm,Tp),Fields),!.
fieldInFace(_,Nm,RcTp,Lc,anonType) :-
  reportError("type %s not declared in %s",[id(Nm),tpe(RcTp)],Lc).

parseTypeName(_,"_",_,_,C,C,Tp) :- newTypeVar("_",Tp).
parseTypeName(_,"void",_,_,C,C,voidType).
parseTypeName(_,Id,_,Q,C,C,Tp) :- is_member((Id,Tp),Q),!.
parseTypeName(_,Id,Env,_,C,C,Tp) :-
  isType(Id,Env,tpDef(_,T,TpDf)),
  (isTypeLam(TpDf) ->
   freshen(TpDf,Env,_,TpL),
   (TpL=typeLambda(tplType([]),Tp) ; Tp = TpL);
   Tp=T).
parseTypeName(Lc,Id,_,_,C,C,anonType) :-
  reportError("type %s not declared",[id(Id)],Lc).

doTypeFun(_,typeLambda(tplType([]),Tp),[],_,Cx,Cx,Tp) :-!. % special case
doTypeFun(_,Op,[],_,Cx,Cx,Op).
doTypeFun(Lc,typeLambda(L,R),[A|Args],Env,C,Cx,Tp) :-
  sameType(L,A,Lc,Env),
  doTypeFun(Lc,R,Args,Env,C,Cx,Tp).
doTypeFun(Lc,constrained(CTp,Ct),Args,Env,C,Cx,Tp) :-
  doTypeFun(Lc,CTp,Args,Env,[Ct|C],Cx,Tp).
doTypeFun(Lc,Op,[A|Args],Env,C,Cx,Tp) :-
  doTypeFun(Lc,tpExp(Op,A),Args,Env,C,Cx,Tp).

parseBoundTpVars([],[]).
parseBoundTpVars([V|L],Q) :-
  parseBoundVar(V,Q,Q0),
  parseBoundTpVars(L,Q0).

parseBoundVar(N,[(Nm,kVar(Nm))|Q],Q) :-
  isIden(N,Nm),!.
parseBoundVar(N,[(Nm,kFun(Nm,Ar))|Q],Q) :-
  isBinary(N,_,"/",L,R),
  isInteger(R,_,Ar),
  isIden(L,Nm),!.
parseBoundVar(N,Q,Q) :-
  locOfAst(N,Lc),
  reportError("invalid quantifier variable: %s",[ast(N)],Lc).

% reapply quantifiers to a type to get full form
reUQnt([],Tp,Tp).
reUQnt([(_,KV)|M],Tp,allType(KV,QTp)) :-
  reUQnt(M,Tp,QTp).
reUQnt([KV|M],Tp,allType(KV,QTp)) :-
  reUQnt(M,Tp,QTp).

reXQnt([],Tp,Tp).
reXQnt([(_,KV)|M],Tp,existType(KV,QTp)) :-
  occursIn(KV,Tp),!,
  reXQnt(M,Tp,QTp).
reXQnt([_|M],Tp,QTp) :-
  reXQnt(M,Tp,QTp).

wrapConstraints([],Tp,Tp).
wrapConstraints([Con|C],Tp,constrained(WTp,Con)) :-
  wrapConstraints(C,Tp,WTp).

parseTypeFace(T,Env,Bound,AT,FT) :-
  isBraceTuple(T,_,L),
  parseTypeFields(L,Env,Bound,[],AT,[],FT).
parseTypeFace(T,_,_,[],[]) :-
  locOfAst(T,Lc),
  reportError("%s is not a type interface",[ast(T)],Lc).

parseConstraint(T,Env,B,C0,Cx) :-
  isBinary(T,_,"<~",L,R),
  parseType(L,Env,B,C0,C1,TV),
  parseType(R,Env,B,C1,[implementsFace(TV,AT)|Cx],AT).
parseConstraint(Sq,Env,B,C0,Cx) :-
  isSquare(Sq,Lc,N,Args),
  parseContractArgs(Args,Env,B,C0,C1,ArgTps,Deps),
  ( parseContractName(Lc,N,Env,B,contractExists(conTract(Op,_ATs,_Dps),_)) ->
    C1=[conTract(Op,ArgTps,Deps)|Cx];
    reportError("contract %s not declared",[id(N)],Lc),
    Cx=C1).
parseConstraint(T,_,B,B,C,C) :-
  locOfAst(T,Lc),
  reportError("invalid type constraint %s",[ast(T)],Lc).

parseConstraints([],_,_,C,C).
parseConstraints([Ct|L],E,Q,C,Cx) :-
  parseConstraint(Ct,E,Q,C,C0),
  parseConstraints(L,E,Q,C0,Cx).

parseContractConstraint(Quants,Cons,Sq,Env,N,Op,ConSpec) :-
  isSquare(Sq,Lc,N,Args),
  parseBoundTpVars(Quants,Q),
  parseConstraints(Cons,Env,Q,C0,[]),
  parseContractArgs(Args,Env,Q,C0,C1,ArgTps,Deps),
  ( parseContractName(Lc,N,Env,Q,contractExists(conTract(Op,ATs,Dps),IFace)) ->
      ( sameType(tplType(ATs),tplType(ArgTps),Lc,Env),
        simplifyType(tplType(ATs),Lc,Env,C1,C2,tplType(As)),
        sameType(tplType(Dps),tplType(Deps),Lc,Env) ->
          simplifyType(tplType(Dps),Lc,Env,C2,Cx,tplType(Ds)),
          putConstraints(Cx,contractExists(conTract(Op,As,Ds),IFace),CC),
          reUQnt(Q,CC,ConSpec);
          reportError("implementation does not match contract %s",[id(Op)],Lc),
          fail);
    reportError("contract %s not declared",[id(N)],Lc), fail).

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
  (Cx=[] -> true ; reportError("unexpected constraints in field type %s",[id(Nm)],Lc)).
parseTypeField(S,Env,Bound,Flds,Flds,Types,[(Fld,FldTp)|Types]) :-
  isTypeField(S,_,Nm,T),
  isIden(Nm,Lc,Fld),
  parseType(T,Env,Bound,[],Cx,FldTp),
  (Cx=[] -> true ; reportError("unexpected constraints in field type %s",[id(Nm)],Lc)).
parseTypeField(F,_,_,Fields,Fields,Types,Types) :-
  isUnary(F,_,"@",_).
parseTypeField(F,_,_,Fields,Fields,Types,Types) :-
  isBinary(F,_,"@",_,_).
parseTypeField(FS,_,_,Fields,Fields,Types,Types) :-
  locOfAst(FS,Lc),
  reportError("invalid field type %s",[ast(FS)],Lc).

parseContract(T,Env,Ev,Path,[conDef(Nm,ConNm,ConRule),
			     ConTpDef|Df],Dfx) :-
  isContractStmt(T,Lc,Quants,C0,Con,Els),
  parseBoundTpVars(Quants,Q),
  parseContractSpec(Con,Q,C0,Cx,Env,SpC,Nm,ConNm,Path),
  parseTypeFields(Els,Env,Q,[],Fs,[],Ts),
  sort(Fs,checker:cmpPair,SortedFlds),
  sort(Ts,checker:cmpPair,SortedTps),
  Face = faceType(SortedFlds,SortedTps),
  putConstraints(Cx,contractExists(SpC,Face),Crl),
  reUQnt(Q,Crl,ConRule),
  contractType(SpC,ConTp),
  wrapType(Q,Cx,[],[],typeExists(ConTp,Face),FaceRule),
  wrapType(Q,Cx,[],[],ConTp,CnType),
  ConTpDef = typeDef(Lc,ConNm,CnType,FaceRule),
%  reportMsg("contract type  %s",[ConTpDef]),
  dollarName(Nm,DlNm),
  dotName(Nm,DtNm),
  genBraceConstructor(Lc,SortedFlds,DlNm,DtNm,ConNm,Q,Cx,ConTp,Df,Df0,Env,Ev0),
%  reportMsg("contract type constructor %s",[CnsDef]),
  genBraceAccessors(Lc,Q,Cx,ConNm,ConTp,SortedFlds,SortedFlds,Df0,Dfx,Acc,[]),
  declareAccessors(Acc,Ev0,Ev).

parseContractSpec(T,Q,C0,Cx,Env,conTract(ConNm,ArgTps,Deps),Nm,ConNm,Path) :-
  isSquare(T,_,Nm,A),
  parseContractArgs(A,Env,Q,C0,Cx,ArgTps,Deps),
  contractName(Path,Nm,ConNm).

parseTypeDef(St,[Defn|Dx],Dx,E,Ev,Path) :-
  isTypeExistsStmt(St,Lc,Quants,Ct,Hd,Body),!,
%  reportMsg("parse type exists: %s",[St]),
  parseTypeExists(Lc,Quants,Ct,Hd,Body,Defn,E,Ev,Path).
parseTypeDef(St,[Defn|Dx],Dx,E,Ev,Path) :-
  isTypeFunStmt(St,Lc,Quants,Ct,Hd,Bd),
  parseTypeFun(Lc,Quants,Ct,Hd,Bd,Defn,E,Ev,Path).
parseTypeDef(St,Defs,Dx,E,Ev,Path) :-
  isAlgebraicTypeStmt(St,Lc,Quants,Constraints,Hd,Body),
  parseAlgebraicTypeDef(Lc,Quants,Constraints,Hd,Body,Defs,Dx,E,Ev,Path).

parseAlgebraicTypeDef(Lc,Quants,Constraints,Hd,Body,
		      [typeDef(Lc,Nm,Type,FaceRule)|D1],Dx,E,Ev,Path):-
  algebraicFace(Body,[],EQ,Face),
  parseBoundTpVars(EQ,XQ),
  parseBoundTpVars(Quants,Q),
  concat(XQ,Q,QV),
  parseTypeHead(Hd,QV,Tp,Nm,_Args,Path),
  parseConstraints(Constraints,E,QV,C0,[]),
%  reportMsg("algebraic type head %s",[tpe(Tp)],Lc),
  pickTypeTemplate(Tp,Type),
  parseType(Face,E,QV,C0,Cx,FceTp),
  wrapType([],[],XQ,[],FceTp,FaceTp),
%  reportMsg("face type %s",[tpe(FaceTp)],Lc),
  wrapType(Q,Cx,[],[],typeExists(Tp,FaceTp),FaceRule),
%  buildConsMap(Body,ConsMap,Path),
%  reportMsg("algebraic face rule %s",[tpe(FaceRule)],Lc),
  declareType(Nm,tpDef(Lc,Type,FaceRule),E,Ev0),
  tpName(Type,TpNm),
  buildAccessors(Lc,Q,XQ,Cx,Path,TpNm,Tp,FceTp,Body,D0,Dx,Acc0,[]),
  buildUpdaters(Lc,Q,XQ,Cx,Path,TpNm,Tp,FceTp,Body,D1,D0,Acc,Acc0),
  declareAccessors(Acc,Ev0,Ev).

declareAccessors(Acc,Ev,Evx) :-
  rfold(Acc,parsetype:declareAcc,Ev,Evx).

declareAcc(acc(Tp,Fld,AccName,AccFunTp),Env,Ev) :-
  declareFieldAccess(Tp,Fld,AccName,AccFunTp,Env,Ev).
declareAcc(upd(Tp,Fld,AccName,AccFunTp),Env,Ev) :-
  declareFieldUpdater(Tp,Fld,AccName,AccFunTp,Env,Ev).

buildAccessors(Lc,Q,XQ,Constraints,Path,TpNm,Tp,faceType(ElTps,_),Body,Defs,Dfx,Acc,Acx) :-
  genAccessors(Lc,Q,XQ,Constraints,Path,TpNm,Tp,ElTps,ElTps,Body,Defs,Dfx,Acc,Acx).

genAccessors(_,_,_,_,_,_,_,[],_,_,Defs,Defs,Acc,Acc).
genAccessors(Lc,Q,XQ,Cx,Path,TpNm,Tp,[(Fld,FldTp)|ElTps],AllElTps,Body,Defs,Dfx,Acc,Accx) :-
  genAccessor(Lc,Q,XQ,Cx,Path,TpNm,Tp,Fld,FldTp,Tp,AllElTps,Body,Defs,Df0,Acc,Ac0),
  genAccessors(Lc,Q,XQ,Cx,Path,TpNm,Tp,ElTps,AllElTps,Body,Df0,Dfx,Ac0,Accx).

genAccessor(Lc,Q,XQ,Cx,Path,TpNm,Tp,Fld,FldTp,Tp,AllElTps,Body,
	    [funDef(Lc,AccName,AccName,soft,AccFunTp,[],Eqns),
	     accDec(Tp,Fld,AccName,AccFunTp)|Defs],Defs,
	    [acc(Tp,Fld,AccName,AccFunTp)|Acc],Acc) :-
  mangleName(TpNm,field,Fld,AccName),
%  reXQnt(XQ,FldTp,XFldTp),
  putConstraints(Cx,funType(tplType([Tp]),FldTp),CxFunTp),
  concat(Q,XQ,TQ),
  reUQnt(TQ,CxFunTp,AccFunTp),
  accessorEquations(Lc,Path,Tp,Fld,FldTp,AllElTps,Body,Eqns,[]).
%  reportMsg("accessor defined %s:%s",[Fld,tpe(AccFunTp)],Lc),
%  reportMsg("accessor %s",[ss(canon:ssDf(0,funDef(Lc,AccName,AccName,soft,AccFunTp,[],Eqns)))],Lc).

accessorEquations(Lc,Path,Tp,Fld,FldTp,AllElTps,Body,Eqns,Eqx) :-
  isBinary(Body,_,"|",L,R),!,
  accessorEquations(Lc,Path,Tp,Fld,FldTp,AllElTps,L,Eqns,Eq0),
  accessorEquations(Lc,Path,Tp,Fld,FldTp,AllElTps,R,Eq0,Eqx).
accessorEquations(Lc,Path,Tp,Fld,FldTp,AllElTps,Body,Eqns,Eqx) :-
  isBraceCon(Body,_XQ,_XC,_,Nm,Args),
  fieldPresent(Fld,Args,_),!,
  mangleName(Path,class,Nm,ConNm),
  projectArgTypes(Args,AllElTps,ArgTps),
  genAccessorEquation(Lc,ConNm,Fld,FldTp,Tp,ArgTps,Eqns,Eqx).
accessorEquations(_,_,_,_,_,_,_,Eqx,Eqx).

genAccessorEquation(Lc,ConsNm,Fld,FldTp,Tp,AllElTps,
		    [rule(Lc,tple(Lc,[apply(Lc,
					    cons(Lc,ConsNm,
						 consType(ArgTps,Tp)),
					    tple(Lc,ArgPtns),Tp,none)]),
			  none,
			  XX)|Eqns],Eqns) :-
  XX = v(Lc,"XX",FldTp),  
  fillinElementPtns([(Fld,XX)],Lc,AllElTps,ArgPtns,ArgTps).

buildUpdaters(Lc,Q,XQ,Constraints,Path,TpNm,Tp,faceType(ElTps,_),Body,Defs,Dfx,Acc,Acx) :-
  genUpdaters(Lc,Q,XQ,Constraints,Path,TpNm,Tp,ElTps,ElTps,Body,Defs,Dfx,Acc,Acx).

genUpdaters(_,_,_,_,_,_,_,[],_,_,Defs,Defs,Acc,Acc).
genUpdaters(Lc,Q,XQ,Cx,Path,TpNm,Tp,[(Fld,FldTp)|ElTps],AllElTps,Body,Defs,Dfx,Acc,Accx) :-
  genUpdater(Lc,Q,XQ,Cx,Path,TpNm,Tp,Fld,FldTp,Tp,AllElTps,Body,Defs,Df0,Acc,Ac0),
  genUpdaters(Lc,Q,XQ,Cx,Path,TpNm,Tp,ElTps,AllElTps,Body,Df0,Dfx,Ac0,Accx).

genUpdater(Lc,Q,XQ,Cx,Path,TpNm,Tp,Fld,FldTp,Tp,AllElTps,Body,
	    [funDef(Lc,AccName,AccName,soft,AccFunTp,[],Eqns),
	     updDec(Tp,Fld,AccName,AccFunTp)|Defs],Defs,
	    [upd(Tp,Fld,AccName,AccFunTp)|Acc],Acc) :-
  mangleName(TpNm,over,Fld,AccName),
%  reXQnt(XQ,FldTp,XFldTp),
  putConstraints(Cx,funType(tplType([Tp,FldTp]),Tp),CxFunTp),
  concat(Q,XQ,TQ),
  reUQnt(TQ,CxFunTp,AccFunTp),
  updaterEquations(Lc,Path,Tp,Fld,FldTp,AllElTps,Body,Eqns,[]).
%  reportMsg("updater defined %s:%s",[Fld,tpe(AccFunTp)],Lc),
%  reportMsg("updater %s",[ss(canon:ssDf(0,funDef(Lc,AccName,AccName,soft,AccFunTp,[],Eqns)))],Lc).

updaterEquations(Lc,Path,Tp,Fld,FldTp,AllElTps,Body,Eqns,Eqx) :-
  isBinary(Body,_,"|",L,R),!,
  updaterEquations(Lc,Path,Tp,Fld,FldTp,AllElTps,L,Eqns,Eq0),
  updaterEquations(Lc,Path,Tp,Fld,FldTp,AllElTps,R,Eq0,Eqx).
updaterEquations(Lc,Path,Tp,Fld,FldTp,AllElTps,Body,Eqns,Eqx) :-
  isBraceCon(Body,_XQ,_XC,_,Nm,Args),
  fieldPresent(Fld,Args,_),!,
  mangleName(Path,class,Nm,ConNm),
  projectArgTypes(Args,AllElTps,ArgTps),
  genUpdaterEquation(Lc,ConNm,Fld,FldTp,Tp,ArgTps,Eqns,Eqx).
updaterEquations(_,_,_,_,_,_,_,Eqx,Eqx).

genUpdaterEquation(Lc,ConsNm,Fld,FldTp,Tp,AllElTps,
		   [rule(Lc,tple(Lc,[apply(Lc,
					    cons(Lc,ConsNm,
						 consType(ArgTps,Tp)),
					   tple(Lc,ArgPtns),Tp,none),
				     XX]),
			  none,
			 apply(Lc,
			       cons(Lc,ConsNm,consType(ArgTps,Tp)),
			       tple(Lc,ValPtns),Tp,none)
			 )|Eqns],Eqns) :-
  XX = v(Lc,"XX",FldTp),
  allArgs(AllElTps,Fld,XX,Lc,ArgTps,ArgPtns,ValPtns).

allArgs([],_,_,_,[],[],[]) :- !.
allArgs([(F,T)|As],F,V,Lc,[T|Ts],[anon(Lc,T)|Ps],[V|AAs]) :-
  allArgs(As,F,V,Lc,Ts,Ps,AAs).
allArgs([(Fn,T)|As],F,V,Lc,[T|Ts],[v(Lc,Fn,T)|Ps],[v(Lc,Fn,T)|AAs]) :-
  allArgs(As,F,V,Lc,Ts,Ps,AAs).

projectArgTypes([],_,[]).
projectArgTypes([A|As],AllTps,[(Nm,ATp)|Tps]) :-
  isTypeAnnotation(A,_,V,_),
  isIden(V,Nm),!,
  is_member((Nm,ATp),AllTps),!,
  projectArgTypes(As,AllTps,Tps).
projectArgTypes([S|As],AllTps,Tps) :-
  isTypeField(S,_,_,_),!,
  projectArgTypes(As,AllTps,Tps).
  
fieldPresent(Fld,[A|_],T) :-
  isBinary(A,_,":",L,T),
  isIden(L,_,Fld),!.
fieldPresent(Fld,[_|Args],T) :-
  fieldPresent(Fld,Args,T).

fillinElementPtns(Els,Lc,Flds,Args,ArgTps) :-
  rfold(Flds,parsetype:fillinElementPtn(Lc),Els,NEls),
  sort(NEls,parsetype:cmpVarDef,Elements),
  project1(Elements,Args),
  map(Args,canon:typeOfCanon,ArgTps).

buildBraceAccessors(Lc,Q,Cx,Tp,Defs,Dfx,Imps,Impx) :-
  tpName(Tp,ConNm),
  dollarName(ConNm,DlNm),
  deRef(Tp,faceType(ElTps,_)),
  genBraceAccessors(Lc,Q,Cx,DlNm,Tp,ElTps,ElTps,Defs,Dfx,Imps,Impx).
  
genBraceAccessors(_Lc,_Q,_Cx,_ConNm,_Tp,[],_,Defs,Defs,Imps,Imps).
genBraceAccessors(Lc,Q,Cx,ConNm,Tp,[(Fld,FldTp)|ElTps],AllElTps,Defs,Dfx,Imps,Imx) :-
  genBraceAccessor(Lc,Q,Cx,ConNm,Tp,Fld,FldTp,Tp,AllElTps,Defs,Df0,Imps,Im0),
  genBraceAccessors(Lc,Q,Cx,ConNm,Tp,ElTps,AllElTps,Df0,Dfx,Im0,Imx).

genBraceAccessor(Lc,Q,Cx,ConNm,Tp,Fld,FldTp,Tp,AllElTps,
		 [funDef(Lc,AccName,AccName,soft,AccFunTp,[],[Eqn]),AccDef|Defs],Defs,
		 [acc(Tp,Fld,AccName,AccFunTp)|Imx],Imx) :-
  tpName(Tp,TpNm),
  mangleName(TpNm,field,Fld,AccName),
  putConstraints(Cx,funType(tplType([Tp]),FldTp),CxFunTp),
  reUQnt(Q,CxFunTp,AccFunTp),
  XX = v(Lc,"XX",FldTp),  
  fillinElementPtns([(Fld,XX)],Lc,AllElTps,ArgPtns,ArgTps),
  AccDef = accDec(Tp,Fld,AccName,AccFunTp),
  Eqn=rule(Lc,tple(Lc,[apply(Lc,
			     cons(Lc,ConNm,
				  consType(ArgTps,Tp)),
			     tple(Lc,ArgPtns),Tp,none)]),
	   none,
	   XX).

genBraceType(Lc,Tp,Defs,Dfx,Env,Ev) :-
  moveQuants(Tp,_Q,In),
  getConstraints(In,_Cx,faceType(Flds,_)),
  sort(Flds,parsetype:cmpVarDef,SFlds),
  tpName(Tp,TpNm),
  lfold(SFlds,parsetype:newFieldVar,([],[]),(Fs,ArgQ)),
  BareFs = faceType(Fs,[]),
  reUQnt(Fs,BareFs,BrTp),
  length(Fs,Ar),
  mkTypeExp(tpFun(TpNm,Ar),ArgQ,Tp0),
  wrapType(Fs,[],[],[],typeExists(Tp0,BareFs),FaceRule),
  dotName(TpNm,DtNm),
  genBraceConstructor(Lc,Fs,TpNm,DtNm,TpNm,Fs,[],BareFs,Df,Dfx,Env,Ev),
  genBraceAccessors(Lc,Fs,[],TpNm,Tp0,Fs,Fs,Defs,
		    [typeDef(Lc,TpNm,BrTp,FaceRule)|Df],
		    _Imps,[]).

newFieldVar((Nm,_),(Fs,Args),([(Nm,kVar(Nm1))|Fs],[kVar(Nm1)|Args])) :-
  genstr("ϰ",Nm1).

genBraceConstructor(Lc,[],Nm,_,ConNm,Q,Cx,Tp,
		    [cnsDef(Lc,Nm,enm(Lc,Nm,ConTp))|Df],Df,Env,Ev) :-
  wrapType(Q,Cx,[],[],consType(faceType([],[]),Tp),ConTp),
  declareEnum(Lc,Nm,ConNm,ConTp,Env,Ev).
genBraceConstructor(Lc,Fields,Nm,DtNm,ConNm,Q,Cx,Tp,
		    [cnsDef(Lc,Nm,cons(Lc,ConNm,ConTp)),
		     cnsDef(Lc,DtNm,cons(Lc,DtNm,CnTp))|Df],Df,Env,Ev) :-
  wrapType(Q,Cx,[],[],consType(faceType(Fields,[]),Tp),ConTp),
  wrapType(Q,Cx,[],[],consType(tplType(FTps),Tp),CnTp),
  project1(Fields,FTps),
  declareCns(Lc,Nm,ConNm,ConTp,Env,Ev).

cmpVarDef((N1,_),(N2,_)) :-
  str_lt(N1,N2).

fillinElementPtn(_,(Nm,_),Els,Els) :-
  is_member((Nm,_),Els) ,!.
fillinElementPtn(Lc,(Nm,Tp),Els,[(Nm,anon(Lc,Tp))|Els]).

buildConsMap(Body,Map,Path) :-
  findCons(Body,Cns,[],Path),
  sort(Cns,str_lt,SortedNms),
  index_list(SortedNms,0,Map).

findCons(Body,Cons,Cnx,Path) :-
  isBinary(Body,_,"|",L,R),!,
  findCons(L,Cons,C0,Path),
  findCons(R,C0,Cnx,Path).
findCons(Body,[Id|Cnx],Cnx,Path) :-
  isIden(Body,_,Nm),!,
  mangleName(Path,class,Nm,Id).
findCons(Body,[Id|Cnx],Cnx,Path) :-
  isEnum(Body,_,E),
  isIden(E,_,Nm),!,
  mangleName(Path,class,Nm,Id).
findCons(Body,[Id|Cnx],Cnx,Path) :-
  isRoundCon(Body,_,_,_,Nm,_),!,
  mangleName(Path,class,Nm,Id).
findCons(Body,[Id|Cnx],Cnx,Path) :-
  isBraceCon(Body,_,_,_,Nm,_),!,
  mangleName(Path,class,Nm,Id).
  
wrapType(Q,Cx,XQ,XC,Tp,WTp) :-
  wrapConstraints(XC,Tp,Tp0),
  reXQnt(XQ,Tp0,Tp1),
  wrapConstraints(Cx,Tp1,CTp),
  reUQnt(Q,CTp,WTp).

wrapType(Q,Cx,ITp,Tp) :-
  wrapConstraints(Cx,ITp,T1),
  reUQnt(Q,T1,Tp).

unwrapType(Tp,Q,Cx,ITp) :-
  moveQuants(Tp,Q,Tp0),
  getConstraints(Tp0,Cx,ITp).

algebraicFace(C,Q,Qx,F) :-
  isBinary(C,_,"|",L,R),!,
  algebraicFace(L,Q,Q0,F0),
  algebraicFace(R,Q0,Qx,F1),
  combineFaces(F0,F1,F).
algebraicFace(C,Q,Q,E) :-
  isIden(C,Lc,_),
  braceTuple(Lc,[],E).
algebraicFace(C,Q,Q,E) :-
  isEnum(C,Lc,_),
  braceTuple(Lc,[],E).
algebraicFace(C,Q,Q,Face) :-
  isRoundCon(C,_,_,Lc,_,_),
  braceTuple(Lc,[],Face).
algebraicFace(C,Q,Qx,Face) :-
  isBraceCon(C,XQ,XC,Lc,_,Entries),
  braceTuple(Lc,Entries,F),
  concat(Q,XQ,Qx),
  reConstrain(XC,F,Face).
algebraicFace(C,Q,Qx,Face) :-
  isPrivate(C,_,I),
  algebraicFace(I,Q,Qx,Face).
algebraicFace(C,Q,Qx,Face) :-
  isPublic(C,_,I),
  algebraicFace(I,Q,Qx,Face).
algebraicFace(C,Q,Qx,Face) :-
  isXQuantified(C,EQ,I),
  concat(Q,EQ,Q0),
  algebraicFace(I,Q0,Qx,Face).
algebraicFace(C,Q,Qx,Face) :-
  isConstrained(C,I,_),
  algebraicFace(I,Q,Qx,Face).

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
    reportError("type of field %s mismatch, %s!=%s",[id(Nm),tpe(Tp),tpe(Tp2)],Lc));
   Bx=[B|Bx1],
   checkFields(Nm,Tp,Bs,Bx1)).
checkFields(Nm,Tp,[B|Bs],[B|Bx]) :-
  checkFields(Nm,Tp,Bs,Bx).
  
parseTypeExists(Lc,Quants,Ct,Hd,Body,typeDef(Lc,Nm,Type,FcRule),E,Ev,Path) :-
  parseBoundTpVars(Quants,Q),
  parseTypeHead(Hd,Q,Tp,Nm,_Args,Path),
  parseConstraints(Ct,E,Q,C0,[]),
  pickTypeTemplate(Tp,Type),
  declareType(Nm,tpDef(Lc,Type,typeExists(Type,faceType([],[]))),E,E0),
  parseType(Body,E0,Q,C0,Cx,RTp),
  wrapConstraints(Cx,typeExists(Tp,RTp),Rl),
  reUQnt(Q,Rl,FcRule),
  declareType(Nm,tpDef(Lc,Type,FcRule),E,Ev).

parseTypeFun(Lc,Quants,Ct,Hd,Bd,typeDef(Lc,Nm,Type,Rule),E,Ev,Path) :-
  parseBoundTpVars(Quants,Q),
  parseConstraints(Ct,E,Q,C0,[]),
  parseTypeHead(Hd,Q,Tp,Nm,_,Path),
  parseType(Bd,E,Q,C0,Cx,RpTp),
  pickTypeTemplate(Tp,Type),
  mkTypeLambda(Tp,RpTp,Lam),
  wrapConstraints(Cx,Lam,Rl),
  reUQnt(Q,Rl,Rule),
  declareType(Nm,tpDef(Lc,Type,Rule),E,Ev).

mkTypeLambda(tpExp(Op,A),Tp,RRTp) :-
  mkTypeLambda(Op,typeLambda(A,Tp),RRTp).
mkTypeLambda(tpFun(_,_),Tp,Tp).
mkTypeLambda(type(_),Tp,typeLambda(tplType([]),Tp)).

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

mkTpLambda([],Tp,Tp) :-!.
mkTpLambda([A|Args],Tp,typeLambda(A,LTp)) :-
  mkTpLambda(Args,Tp,LTp).

parseTypeCore(St,Type,_,Path) :-
  isTypeExistsStmt(St,_,Quants,_,Head,_),
  parseBoundTpVars(Quants,Q),
  parseTypeHead(Head,Q,Tp,_,Args,Path),
  mkTpLambda(Args,Tp,LTp),
  reUQnt(Q,LTp,Type).
parseTypeCore(St,Type,_Env,Path) :-
  isTypeFunStmt(St,_Lc,Quants,_Ct,Hd,_Bd),
  parseBoundTpVars(Quants,Q),
  parseTypeHead(Hd,Q,Tp,_,Args,Path),
  mkTpLambda(Args,Tp,LTp),
  reUQnt(Q,LTp,Type).
parseTypeCore(St,Type,_,Path) :-
  isAlgebraicTypeStmt(St,_,Quants,_,Hd,_),
  parseBoundTpVars(Quants,Q),
  parseTypeHead(Hd,Q,Tp,_,Args,Path),
  mkTpLambda(Args,Tp,LTp),
  reUQnt(Q,LTp,Type).

parseTypeHead(N,_,type(TpNm),Nm,[],Path) :-
  isIden(N,_,Nm),
  marker(type,Marker),
  subPath(Path,Marker,Nm,TpNm).
parseTypeHead(N,B,Tp,Nm,Args,Path) :-
  isSquare(N,_,Nm,A),
  parseHeadArgs(A,B,Args),
  length(Args,Ar),
  marker(type,Marker),
  subPath(Path,Marker,Nm,TpNm),
  mkTypeExp(tpFun(TpNm,Ar),Args,Tp).
parseTypeHead(N,B,Tp,Nm,Args,Path) :-
  isRoundTerm(N,Lc,Op,Els),
  squareTerm(Lc,Op,Els,TT),
  parseTypeHead(TT,B,Tp,Nm,Args,Path).

parseHeadArgs([],_,[]).
parseHeadArgs([H|L],B,[V|Args]) :-
  isIden(H,Lc,Nm),
  (is_member((Nm,V),B) ; reportError("type argument %s not quantified ",[ast(H)],Lc)),
  parseHeadArgs(L,B,Args).
