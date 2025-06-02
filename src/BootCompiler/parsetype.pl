:- module(parsetype,[parseType/3,
		     parseTypeCore/4,
		     parseContract/11,parseTypeDef/11,algebraicFace/4,
		     typeTemplate/3,
		     parseContractConstraint/7,
		     unwrapType/4,wrapType/4]).

:- use_module(abstract).
:- use_module(canon).
:- use_module(dict).
:- use_module(errors).
:- use_module(freshen).
:- use_module(meta).
:- use_module(misc).
:- use_module(unify).
:- use_module(wff).
:- use_module(types).

parseType(T,Env,Tp) :-
  parseType(T,Env,[],Tp).

parseType(T,Env,B,Tp) :-
  isQuantified(T,V,BT),!,
  parseBoundTpVars(V,B0),
  concat(B,B0,Q),
  parseType(BT,Env,Q,BTp),
  reUQnt(B0,BTp,Tp).
parseType(T,Env,B,Tp) :-
  isXQuantified(T,V,BT),!,
  parseBoundTpVars(V,B0),
  concat(B,B0,Q),
  parseType(BT,Env,Q,BTp),
  reXQnt(B0,BTp,Tp).
parseType(T,Env,B,Tp) :-
  isConstrained(T,T0,C),!,
  parseConstraints(C,Env,B,Cx,[]),
  parseType(T0,Env,B,BT),
  wrapConstraints(Cx,BT,Tp).
parseType(Nm,Env,Q,Tp) :-
  isIden(Nm,Lc,Id), !,
  parseTypeName(Lc,Id,Env,Q,Tp).
parseType(Sq,Env,Q,Tp) :-
  isSquareTerm(Sq,Lc,N,Args),!,
  parseType(N,Env,Q,Op),
  parseTypes(Args,Env,Q,ArgTps),
  freshen(Op,Env,_Qx,OOp),
  doTypeFun(Lc,OOp,ArgTps,Env,Tp).
parseType(F,Env,Q,FT) :-
  isFuncType(F,_,L,R),
  parseArgType(L,Env,Q,AT),
  parseResultType(R,Env,Q,AT,FT).
parseType(F,Env,Q,consType(AT,RT)) :-
  isConstructorType(F,_,_,_,L,R),!, % should be no quantifiers
  parseArgType(L,Env,Q,AT),!,
  parseType(R,Env,Q,RT).
parseType(F,Env,Q,Tp) :-
  isTaskType(F,_,A),
  parseType(A,Env,Q,AT),
  taskType(AT,Tp).
parseType(F,Env,Q,Tp) :-
  isRef(F,_,L),
  parseType(L,Env,Q,A),
  mkTypeExp(tpFun("ref",1),[A],Tp).
parseType(T,Env,Q,tplType(AT)) :-
  isTuple(T,[A]),
  isTuple(A,Inner),!,
  parseTypes(Inner,Env,Q,AT).
parseType(T,Env,Q,AT) :-
  isTuple(T,[A]),!,
  parseType(A,Env,Q,AT).
parseType(T,Env,Q,tplType(AT)) :-
  isTuple(T,A),!,
  parseTypes(A,Env,Q,AT).
parseType(T,Env,Q,faceType(AT,FT)) :-
  isBraceTuple(T,_,L),!,
  parseTypeFields(L,Env,Q,[],AT,[],FT).
parseType(T,Env,Q,typeLambda(AT,RT)) :-
  isTypeLambda(T,_,L,R),
  parseArgType(L,Env,Q,AT),
  parseType(R,Env,Q,RT).
parseType(Term,Env,_,Tp) :-
  isFieldAcc(Term,Lc,L,Fld),
  parseFieldAccType(Lc,L,Fld,Env,Tp).
parseType(Trm,Env,Q,Tp) :-
  isRoundTerm(Trm,Lc,Op,[L,R]),  %% Special case for binary to allow type aliases
  squareTerm(Lc,Op,[L,R],TT),
  parseType(TT,Env,Q,Tp),!.
parseType(T,_,_,anonType) :-
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

parseArgType(T,Env,Q,tplType(AT)) :-
  isTuple(T,A),!,
  parseTypes(A,Env,Q,AT).
parseArgType(T,Env,Q,Tp) :-
  parseType(T,Env,Q,Tp).

parseResultType(A,Env,Q,AT,funType(AT,RT,ET)) :-
  isThrows(A,_,L,R),
  parseType(L,Env,Q,RT),
  parseType(R,Env,Q,ET).
parseResultType(A,Env,Q,AT,funType(AT,RT)) :-
  parseType(A,Env,Q,RT).

fieldInFace(Fields,Nm,_,_,Tp) :-
  is_member((Nm,Tp),Fields),!.
fieldInFace(_,Nm,RcTp,Lc,anonType) :-
  reportError("type %s not declared in %s",[id(Nm),tpe(RcTp)],Lc).

parseTypeName(_,"_",_,_,Tp) :- newTypeVar("_",Tp).
parseTypeName(_,"void",_,_,voidType).
parseTypeName(_,Id,_,Q,Tp) :- is_member((Id,Tp),Q),!.
parseTypeName(_,Id,Env,_,Tp) :-
  isType(Id,Env,tpDef(_,T,TpDf,_)),
  (isTypeLam(TpDf) ->
   freshen(TpDf,Env,_,TpL),
   (TpL=typeLambda(tplType([]),Tp) ; Tp = TpL);
   Tp=T).
parseTypeName(Lc,Id,_,_,anonType) :-
  reportError("type %s not declared",[id(Id)],Lc).

doTypeFun(_,typeLambda(tplType([]),Tp),[],_,Tp) :-!. % special case
doTypeFun(_,Op,[],_,Op).
doTypeFun(Lc,typeLambda(L,R),[A|Args],Env,Tp) :-
  sameType(L,A,Lc,Env),
  doTypeFun(Lc,R,Args,Env,Tp).
doTypeFun(Lc,constrained(CTp,Ct),Args,Env,Tp) :-
  doTypeFun(Lc,CTp,Args,Env,BTp),
  wrapConstraints([Ct],BTp,Tp).
doTypeFun(Lc,Op,[A|Args],Env,Tp) :-
  doTypeFun(Lc,tpExp(Op,A),Args,Env,Tp).

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
  occursIn(KV,Tp),!,
  reUQnt(M,Tp,QTp).
reUQnt([_|M],Tp,QTp) :-
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

parseConstraint(T,Env,B,[implicit(Nm,Tp)|Cx],Cx) :-
  isDynamic(T,_,Nm,R),
  parseType(R,Env,B,Tp).
parseConstraint(T,Env,B,[implementsFace(TV,AT)|Cx],Cx) :-
  isTypeExists(T,_,L,R),
  parseType(L,Env,B,TV),
  parseType(R,Env,B,AT).
parseConstraint(Sq,Env,B,C,Cx) :-
  isSquare(Sq,Lc,N,Args),
  parseContractArgs(Args,Env,B,ArgTps,Deps),
  ( parseContractName(Lc,N,Env,B,contractExists(conTract(Op,_ATs,_Dps),_)) ->
    C=[conTract(Op,ArgTps,Deps)|Cx];
    reportError("contract %s not declared",[id(N)],Lc),
    C=Cx).
parseConstraint(Cn,Env,B,C,Cx) :-
  isIden(Cn,Lc,Nm),
  (parseContractName(Lc,Nm,Env,B,contractExists(conTract(Op,_ATs,_Dps),_)) ->
   C=[conTract(Op,[],[])|Cx];
   reportError("contract %s not declared",[id(Nm)],Lc),
   C=Cx).
parseConstraint(T,Env,B,C,Cx) :-
  isTuple(T,_,[El]),
  parseConstraint(El,Env,B,C,Cx).
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
  parseContractArgs(Args,Env,Q,ArgTps,Deps),
  ( parseContractName(Lc,N,Env,Q,contractExists(conTract(Op,ATs,Dps),IFace)) ->
      ( sameType(tplType(ATs),tplType(ArgTps),Lc,Env),
        simplifyType(tplType(ATs),Lc,Env,C0,C1,tplType(As)),
        sameType(tplType(Dps),tplType(Deps),Lc,Env) ->
	simplifyType(tplType(Dps),Lc,Env,C1,Cx,tplType(Ds)),
	putConstraints(Cx,contractExists(conTract(Op,As,Ds),IFace),CC),
	reUQnt(Q,CC,ConSpec);
	reportError("implementation does not match contract %s",[id(Op)],Lc),
	fail);
    reportError("contract %s not declared",[id(N)],Lc), fail).
parseContractConstraint(Quants,Cons,Sq,Env,N,Op,ConSpec) :-
  isIden(Sq,Lc,N),
  parseBoundTpVars(Quants,Q),
  parseConstraints(Cons,Env,Q,C0,[]),
  ( parseContractName(Lc,N,Env,Q,contractExists(conTract(Op,ATs,Dps),IFace)) ->
      ( sameType(tplType(ATs),tplType([]),Lc,Env),
        simplifyType(tplType(ATs),Lc,Env,C0,C2,tplType(As)),
        sameType(tplType(Dps),tplType([]),Lc,Env) ->
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

parseContractArgs([A],Env,B,Args,Deps) :-
  isBinary(A,_,"->>",L,R),!,
  deComma(L,LA),
  deComma(R,RA),
  parseTypes(LA,Env,B,Args),
  parseTypes(RA,Env,B,Deps).
parseContractArgs(A,Env,B,Args,[]) :-
  parseTypes(A,Env,B,Args).

parseTypes([],_,_,[]).
parseTypes([A|AT],Env,Q,[Atype|ArgTypes]) :-
  parseType(A,Env,Q,Atype),
  parseTypes(AT,Env,Q,ArgTypes).

parseTypeFields([],_,_,Flds,Flds,Tps,Tps).
parseTypeFields([F|L],Env,Bound,Flds,Fields,Tps,Types) :-
  parseTypeField(F,Env,Bound,Flds,F0,Tps,T0),
  parseTypeFields(L,Env,Bound,F0,Fields,T0,Types).

parseTypeField(F,Env,Q,Flds,[(Fld,FldTp)|Flds],Types,Types) :-
  isTypeAnnotation(F,_,Nm,FT),
  isIden(Nm,_,Fld),
  parseType(FT,Env,Q,FldTp).
parseTypeField(S,Env,Q,Flds,Flds,Types,[(Fld,FldTp)|Types]) :-
  isTypeField(S,_,Nm,T),
  isIden(Nm,_,Fld),
  parseType(T,Env,Q,FldTp).
parseTypeField(F,_,_,Fields,Fields,Types,Types) :-
  isUnary(F,_,"@",_).
parseTypeField(F,_,_,Fields,Fields,Types,Types) :-
  isBinary(F,_,"@",_,_).
parseTypeField(FS,_,_,Fields,Fields,Types,Types) :-
  locOfAst(FS,Lc),
  reportError("invalid field type %s",[ast(FS)],Lc).

parseContract(T,Env,Ev,Opts,Path,[conDef(Nm,ConNm,ConRule),
			     ConTpDef|Df],Dfx,Publish,Viz,Dc,Dcx) :-
  isContractStmt(T,Lc,Quants,Cx,Con,Els),
  parseBoundTpVars(Quants,Q),
  parseContractSpec(Con,Q,Env,SpC,Nm,ConNm,Path),
  parseTypeFields(Els,Env,Q,[],Fs,[],Ts),
  sort(Fs,checker:cmpPair,SortedFlds),
  sort(Ts,checker:cmpPair,SortedTps),
  Face = faceType(SortedFlds,SortedTps),
  putConstraints(Cx,contractExists(SpC,Face),Crl),
  reUQnt(Q,Crl,ConRule),
  contractType(SpC,ConTp),
  wrapType(Q,Cx,[],[],typeExists(ConTp,Face),FaceRule),
  wrapType(Q,Cx,[],[],ConTp,CnType),
  dollarName(Nm,DlNm),
  progTypeArity(ConTp,Ar),
  IxMap = [(lbl(ConNm,Ar),0)],
  ConTpDef = typeDef(Lc,DlNm,CnType,FaceRule,IxMap),
  checkOpt(Opts,traceCheck,parsetype:showContractType(ConTpDef)),
  genBraceConstructor(Lc,SortedFlds,DlNm,ConNm,Q,Cx,ConTp,Df,Df0,Env,Ev0,ConDecl),
  call(Publish,Viz,con(Nm),ConDecl,Dc,Dca),
%  reportMsg("contract type constructor %s",[CnsDef]),
  genBraceAccessors(Lc,Q,Cx,ConNm,ConTp,SortedFlds,SortedFlds,Df0,Dfx,Acc,[],
		    Publish,Viz,con(Nm),Dca,Dc0),
  declareAccessors(Acc,Ev0,Ev),
  call(Publish,Viz,con(Nm),contractDec(Nm,ConNm,ConRule),Dc0,Dc1),
  call(Publish,Viz,con(Nm),typeDec(ConNm,CnType,FaceRule,IxMap),Dc1,Dcx).

parseContractSpec(T,Q,Env,conTract(ConNm,ArgTps,Deps),Nm,ConNm,Path) :-
  isSquare(T,_,Nm,A),!,
  parseContractArgs(A,Env,Q,ArgTps,Deps),
  contractName(Path,Nm,ConNm).
parseContractSpec(T,_Q,_Env,conTract(ConNm,[],[]),Nm,ConNm,Path) :-
  isIden(T,_,Nm),!,
  contractName(Path,Nm,ConNm).

parseTypeDef(St,[Defn|Dx],Dx,E,Ev,Publish,Viz,Dc,Dcx,Opts,Path) :-
  isTypeExistsStmt(St,Lc,Quants,Ct,Hd,Body),!,
  checkOpt(Opts,traceCheck,showAst(Lc,"parse type exists: %s",[St])),
  parseTypeExists(Lc,Quants,Ct,Hd,Body,Defn,E,Ev,Publish,Viz,Dc,Dcx,Path).
parseTypeDef(St,[Defn|Dx],Dx,E,Ev,Publish,Viz,Dc,Dcx,_Opts,Path) :-
  isTypeFunStmt(St,Lc,Quants,Ct,Hd,Bd),
  parseTypeFun(Lc,Quants,Ct,Hd,Bd,Defn,E,Ev,Publish,Viz,Dc,Dcx,Path).
parseTypeDef(St,Defs,Dx,E,Ev,Publish,Viz,Dc,Dcx,Opts,Path) :-
  isAlgebraicTypeStmt(St,Lc,Quants,Constraints,Hd,Body),
  parseAlgebraicTypeDef(Lc,Quants,Constraints,Hd,Body,Defs,Dx,E,Ev,Publish,Viz,Dc,Dcx,Opts,Path).
parseTypeDef(St,Defs,Dx,E,Ev,Publish,Viz,Dc,Dcx,_Opts,Path) :-
  isStructTypeStmt(St,Lc,Q,X,Cx,Hd,Nm,Els),
  parseStructTypeDef(Lc,Q,X,Cx,Hd,Nm,Els,Defs,Dx,E,Ev,Publish,Viz,Dc,Dcx,Path).

parseAlgebraicTypeDef(Lc,Quants,Constraints,Hd,Body,[Defn|Dx],Dx,E,Ev,
		      Publish,Viz,Dc,Dcx,Opts,Path):-
  algebraicFace(Body,[],EQ,Face),
  parseBoundTpVars(EQ,XQ),
  parseBoundTpVars(Quants,Q),
  concat(XQ,Q,QV),
  parseTypeHead(Hd,QV,Tp,Nm,_Args,Path),
  parseConstraints(Constraints,E,QV,Cx,[]),
  checkOpt(Opts,traceCheck,showMsg(Lc,"algebraic type head %s",[tpe(Tp)])),
  pickTypeTemplate(Tp,Type),
  parseType(Face,E,QV,FceTp),
  wrapType([],[],XQ,[],FceTp,FaceTp),
  wrapType(Q,Cx,[],[],typeExists(Tp,FaceTp),FaceRule),
  buildConsMap(Body,ConsMap,Path),
  checkOpt(Opts,traceCheck,showMsg(Lc,"cons index map %s",[ConsMap])),
  checkOpt(Opts,traceCheck,showMsg(Lc,"algebraic face rule %s",[tpe(FaceRule)])),
  declareType(Nm,tpDef(Lc,Type,FaceRule,ConsMap),E,Ev),
%  tpName(Type,TpNm),
  Defn = typeDef(Lc,Nm,Type,FaceRule,ConsMap),
  Decl = typeDec(Nm,Type,FaceRule,ConsMap),
  call(Publish,Viz,tpe(Nm),Decl,Dc,Dcx).

parseStructTypeDef(Lc,Qs,Xs,Cs,Hd,BrNm,Fields,[TpDefn,CnDefn|Df],Dfx,Env,Envx,
		      Publish,Viz,Dc,Dcx,Path):-
  parseBoundTpVars(Xs,X),
  parseBoundTpVars(Qs,Q),
  parseConstraints(Cs,Env,Q,Cx,[]),

  parseTypeFields(Fields,Env,Q,[],Fs,[],Ts),
  sort(Fs,checker:cmpPair,SortedFlds),
  sort(Ts,checker:cmpPair,SortedTps),
  Face = faceType(SortedFlds,SortedTps),

  parseTypeHead(Hd,Q,Tp,Nm,_Args,Path),
  pickTypeTemplate(Tp,Type),
  wrapType(Q,Cx,X,[],typeExists(Tp,Face),FaceRule),
  wrapType(Q,Cx,[],[],consType(faceType(SortedFlds,SortedTps),Tp),ConTp),
%  reportMsg("constructor type %s",[tpe(ConTp)],Lc),

  mangleName(Path,class,BrNm,ConNm),
  ConDecl = cnsDec(BrNm,ConNm,ConTp),

  genBraceAccessors(Lc,Q,Cx,ConNm,Tp,SortedFlds,SortedFlds,Df,Df0,Acc,[],
		    Publish,Viz,tpe(Nm),Dc,Dc0),
  declareAccessors(Acc,Env,Ev1),
  genBraceUpdaters(Lc,Q,Cx,ConNm,Tp,SortedFlds,SortedFlds,Df0,Dfx,Ups,[],
		    Publish,Viz,tpe(Nm),Dc0,Dc1),
  declareAccessors(Ups,Ev1,Ev2),
  ConsIx = [(lbl(ConNm,Ar),0)],
  call(Publish,Viz,tpe(Nm),typeDec(Nm,Type,FaceRule,ConsIx),Dc1,Dc2),
  call(Publish,Viz,tpe(Nm),ConDecl,Dc2,Dcx),
  progTypeArity(Type,Ar),
  TpDefn = typeDef(Lc,Nm,Type,FaceRule,ConsIx),
  CnDefn = cnsDef(Lc,Nm,enm(Lc,ConNm,ConTp)),
  declareType(Nm,tpDef(Lc,Type,FaceRule,ConsIx),Ev2,Envx).

declareAccessors(Acc,Ev,Evx) :-
  rfold(Acc,parsetype:declareAcc,Ev,Evx).

declareAcc(acc(Tp,Fld,AccName,AccFunTp),Env,Ev) :-
  declareFieldAccess(Tp,Fld,AccName,AccFunTp,Env,Ev).
declareAcc(upd(Tp,Fld,AccName,AccFunTp),Env,Ev) :-
  declareFieldUpdater(Tp,Fld,AccName,AccFunTp,Env,Ev).

projectArgTypes([],_,[]).
projectArgTypes([A|As],AllTps,[(Nm,ATp)|Tps]) :-
  isTypeAnnotation(A,_,V,_),
  isIden(V,Nm),!,
  is_member((Nm,ATp),AllTps),!,
  projectArgTypes(As,AllTps,Tps).
projectArgTypes([S|As],AllTps,Tps) :-
  isTypeField(S,_,_,_),!,
  projectArgTypes(As,AllTps,Tps).
  
fillinElementPtns(Els,Lc,Flds,Args,ArgTps) :-
  rfold(Flds,parsetype:fillinElementPtn(Lc),Els,NEls),
  sort(NEls,parsetype:cmpVarDef,Elements),
  project1(Elements,Args),
  map(Args,canon:typeOfCanon,ArgTps).

fillinElementPtn(_,(Nm,_),Els,Els) :-
  is_member((Nm,_),Els) ,!.
fillinElementPtn(Lc,(Nm,Tp),Els,[(Nm,anon(Lc,Tp))|Els]).

genBraceAccessors(_Lc,_Q,_Cx,_ConNm,_Tp,[],_,Defs,Defs,Imps,Imps,_,_,_,Dc,Dc).
genBraceAccessors(Lc,Q,Cx,ConNm,Tp,[(Fld,FldTp)|ElTps],AllElTps,Defs,Dfx,Imps,Imx,
		  Publish,Viz,ExNm,Dc,Dcx) :-
  genBraceAccessor(Lc,Q,Cx,ConNm,Tp,Fld,FldTp,Tp,AllElTps,Defs,Df0,Imps,Im0,Publish,Viz,ExNm,Dc,Dc0),
  genBraceAccessors(Lc,Q,Cx,ConNm,Tp,ElTps,AllElTps,Df0,Dfx,Im0,Imx,Publish,Viz,ExNm,Dc0,Dcx).

genBraceAccessor(Lc,Q,Cx,ConNm,Tp,Fld,FldTp,Tp,AllElTps,
		 [funDef(Lc,AccName,AccName,soft,AccFunTp,[],[Eqn])|Defs],Defs,
		 [acc(Tp,Fld,AccName,AccFunTp)|Imx],Imx,
		 Publish,Viz,ExNm,Dc,Dcx) :-
  tpName(Tp,TpNm),
  mangleName(TpNm,field,Fld,AccName),
  putConstraints(Cx,funType(tplType([Tp]),FldTp),CxFunTp),
  reUQnt(Q,CxFunTp,AccFunTp),
  XX = v(Lc,"XX",FldTp),  
  fillinElementPtns([(Fld,XX)],Lc,AllElTps,ArgPtns,ArgTps),
  Eqn=rule(Lc,tple(Lc,[capply(Lc,
			      enm(Lc,ConNm,
				  consType(ArgTps,Tp)),
			      tple(Lc,ArgPtns),Tp)]),
	   none,
	   XX),
  call(Publish,Viz,ExNm,accDec(Tp,Fld,AccName,AccFunTp),Dc,Dcx).
%  dispDef(funDef(Lc,AccName,AccName,soft,AccFunTp,[],[Eqn])).

genBraceUpdaters(_Lc,_Q,_Cx,_ConNm,_Tp,[],_,Defs,Defs,Imps,Imps,_,_,_,Dc,Dc).
genBraceUpdaters(Lc,Q,Cx,ConNm,Tp,[(Fld,FldTp)|ElTps],AllElTps,Defs,Dfx,Imps,Imx,
		  Publish,Viz,ExNm,Dc,Dcx) :-
  genBraceUpdater(Lc,Q,Cx,ConNm,Tp,Fld,FldTp,Tp,AllElTps,Defs,Df0,Imps,Im0,Publish,Viz,ExNm,Dc,Dc0),
  genBraceUpdaters(Lc,Q,Cx,ConNm,Tp,ElTps,AllElTps,Df0,Dfx,Im0,Imx,Publish,Viz,ExNm,Dc0,Dcx).

genBraceUpdater(Lc,Q,Cx,ConNm,Tp,Fld,FldTp,Tp,AllElTps,
		 [funDef(Lc,AccName,AccName,soft,AccFunTp,[],[Eqn])|Defs],Defs,
		 [upd(Tp,Fld,AccName,AccFunTp)|Imx],Imx,
		 Publish,Viz,ExNm,Dc,Dcx) :-
  tpName(Tp,TpNm),
  mangleName(TpNm,over,Fld,AccName),
  putConstraints(Cx,funType(tplType([Tp,FldTp]),Tp),CxFunTp),
  reUQnt(Q,CxFunTp,AccFunTp),

  XX = v(Lc,"XX",FldTp),

  sort(AllElTps,parsetype:cmpVarDef,SortedTps),
  allArgs(SortedTps,Fld,XX,Lc,ArgTps,ArgPtns,ValPtns),

  Eqn = rule(Lc,tple(Lc,[capply(Lc,
				 enm(Lc,ConNm,
				     consType(ArgTps,Tp)),
				 tple(Lc,ArgPtns),Tp),
			  XX]),
			  none,
	      capply(Lc,
		     enm(Lc,ConNm,consType(ArgTps,Tp)),
		     tple(Lc,ValPtns),Tp)
	     ),

  Decl = updDec(Tp,Fld,AccName,AccFunTp),

  call(Publish,Viz,ExNm,Decl,Dc,Dcx).
%  dispDef(funDef(Lc,AccName,AccName,soft,AccFunTp,[],[Eqn])).

allArgs([],_,_,_,[],[],[]) :- !.
allArgs([(F,T)|As],F,V,Lc,[T|Ts],[anon(Lc,T)|Ps],[V|AAs]) :-
  allArgs(As,F,V,Lc,Ts,Ps,AAs).
allArgs([(Fn,T)|As],F,V,Lc,[T|Ts],[v(Lc,Fn,T)|Ps],[v(Lc,Fn,T)|AAs]) :-
  allArgs(As,F,V,Lc,Ts,Ps,AAs).

genBraceConstructor(Lc,[],Nm,ConNm,Q,Cx,Tp,
		    [cnsDef(Lc,Nm,enm(Lc,Nm,ConTp))|Df],Df,Env,Ev,cnsDec(Nm,ConNm,ConTp)) :-
  wrapType(Q,Cx,[],[],consType(faceType([],[]),Tp),ConTp),
  declareEnum(Lc,Nm,ConNm,ConTp,Env,Ev).
genBraceConstructor(Lc,Fields,Nm,ConNm,Q,Cx,Tp,
		    [cnsDef(Lc,Nm,enm(Lc,ConNm,ConTp))|Df],Df,Env,Ev,cnsDec(Nm,ConNm,ConTp)) :-
  wrapType(Q,Cx,[],[],consType(faceType(Fields,[]),Tp),ConTp),
  declareCns(Lc,Nm,ConNm,ConTp,Env,Ev).

cmpVarDef((N1,_),(N2,_)) :-
  str_lt(N1,N2).

lbl_lt(lbl(S1,_A1),lbl(S2,_A2)) :-
  str_lt(S1,S2).

buildConsMap(Body,Map,Path) :-
  findCons(Body,Cns,[],Path),
  sort(Cns,parsetype:lbl_lt,SortedNms),
  index_list(SortedNms,0,Map).

findCons(Body,Cons,Cnx,Path) :-
  isBinary(Body,_,"|",L,R),!,
  findCons(L,Cons,C0,Path),
  findCons(R,C0,Cnx,Path).
findCons(Body,Cons,Cnx,Path) :-
  isUnary(Body,_,"|",R),!,
  findCons(R,Cons,Cnx,Path).
findCons(Body,[lbl(Id,0)|Cnx],Cnx,Path) :-
  isIden(Body,_,Nm),!,
  mangleName(Path,class,Nm,Id).
findCons(Body,[lbl(Id,0)|Cnx],Cnx,Path) :-
  isEnum(Body,_,E),
  isIden(E,_,Nm),!,
  mangleName(Path,class,Nm,Id).
findCons(Body,[lbl(Id,Ar)|Cnx],Cnx,Path) :-
  isRoundCon(Body,_,_,_,Nm,Es),!,
  length(Es,Ar),
  mangleName(Path,class,Nm,Id).
findCons(Body,[lbl(Id,Ar)|Cnx],Cnx,Path) :-
  isBraceCon(Body,_,_,_,Nm,Es),!,
  braceConArity(Es,Ar),
  mangleName(Path,class,Nm,Id).

braceConArity([],Ar,Ar).
braceConArity([E|Es],Ar,Ax) :-
  isTypeAnnotation(E,_,_,_),!,
  Ar1 is Ar+1,
  braceConArity(Es,Ar1,Ax).
braceConArity([_E|Es],Ar,Ax) :-
  braceConArity(Es,Ar,Ax).

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
algebraicFace(C,Q,Qx,F) :-
  isUnary(C,_,"|",R),!,
  algebraicFace(R,Q,Qx,F).
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
  isXQuantified(C,EQ,I),
  concat(Q,EQ,Q0),
  algebraicFace(I,Q0,Qx,Face).
algebraicFace(C,Q,Qx,Face) :-
  isConstrained(C,I,_),
  algebraicFace(I,Q,Qx,Face).
algebraicFace(T,Q,Q,[]) :-
  locOfAst(T,Lc),
  reportError("invalid form of algebraic type definition, %s",[ast(T)],Lc).

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
  
parseTypeExists(Lc,Quants,Ct,Hd,Body,typeDef(Lc,Nm,Type,Rule,[]),E,Ev,Publish,Viz,Dc,Dcx,Path) :-
  parseBoundTpVars(Quants,Q),
  parseTypeHead(Hd,Q,Tp,Nm,_Args,Path),
  parseConstraints(Ct,E,Q,Cx,[]),
  pickTypeTemplate(Tp,Type),
  declareType(Nm,tpDef(Lc,Type,typeExists(Type,faceType([],[])),[]),E,E0),
  parseType(Body,E0,Q,RTp),
  wrapConstraints(Cx,typeExists(Tp,RTp),Rl),
  reUQnt(Q,Rl,Rule),
  declareType(Nm,tpDef(Lc,Type,Rule,[]),E,Ev),
  Decl = typeDec(Nm,Type,Rule,[]),
  call(Publish,Viz,tpe(Nm),Decl,Dc,Dcx).

parseTypeFun(Lc,Quants,Ct,Hd,Bd,typeDef(Lc,Nm,Type,Rule,[]),E,Ev,Publish,Viz,Dc,Dcx,Path) :-
  parseBoundTpVars(Quants,Q),
  parseConstraints(Ct,E,Q,Cx,[]),
  parseTypeHead(Hd,Q,Tp,Nm,_,Path),
  parseType(Bd,E,Q,RpTp),
  pickTypeTemplate(Tp,Type),
  mkTypeLambda(Tp,RpTp,Lam),
  wrapConstraints(Cx,Lam,Rl),
  reUQnt(Q,Rl,Rule),
  declareType(Nm,tpDef(Lc,Type,Rule,[]),E,Ev),
  call(Publish,Viz,tpe(Nm),typeDec(Nm,Type,Rule,[]),Dc,Dcx).

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
parseTypeCore(St,Type,_,Path) :-
  isStructTypeStmt(St,_,Quants,_,_,Hd,_,_),
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

showContractType(Def,_) :-
  reportMsg("contract type  %s",[canDef(Def)]).
  
