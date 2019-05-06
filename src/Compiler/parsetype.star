star.compiler.typeparse{
  import star.

  import star.compiler.ast.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.freshen.
  import star.compiler.location.
  import star.compiler.types.
  import star.compiler.unify.
  import star.compiler.wff.

  public parseType:(list[(string,tipe)],ast,dict,reports) => either[reports,tipe].

  parseType(Typ,Env,Rp) => let{
      parseT(Tp,Q) where (Lc,V,BT) ^= isQuantified(Tp) => do{
	  BV <- parseBoundTpVars(V);
	  In <- parseT(BT,Q++BV);
	  lift reQuant(BV,In)
	}.
      parseT(Tp,Q) where (Lc,V,BT) ^= isXQuantified(Tp) => do{
	  BV <- parseBoundTpVars(V);
	  In <- parseT(BT,Q++BV);
	  lift reQuantX(BV,In)
	}.
      parseT(Tp,Q) where (Lc,C,B) ^= isConstrained(Tp) => do{
	  Cx <- parseConstraints(C,Q);
	  Inn <- parseT(B,Q);
	  lift wrapConstraints(Cx,Inn)
	}
      parseT(Tp,Q) where (Lc,Nm) ^= isName(Tp) =>
	parseTypeName(Lc,Nm,Q).
      parseT(Tp,Q) where (Lc,O,Args) ^= isSquareTerm(Tp) => do{
	  Op <- parseT(O,Q);
	  ArgTps <- parseTps(Args,Q);
	  if (Qx,OOp) .= freshen(Op,[],Env) then {
	      Inn <- applyTypeFun(deRef(OOp),ArgTps,locOf(O));
		return rebind(Qx,Inn)
	    } else
	    throw reportError(Rp,"Could not freshen $(Op)",Lc)
	}

	  parseTps:(list[ast],list[(string,tipe)]) => either[reports,list[tipe]].
	  parseTps([],_) => either([]).
	  parseTps([T,..L],Q) => do{
		Tl <- parseT(T,Q);
		Tr <- parseTps(L,Q);
		lift [Tl,..Tr]
	      }

	  parseTypeName(_,"_",_) => either(newTypeVar("_")).
	  parseTypeName(Lc,"this",_) => either(thisType).
	  parseTypeName(_,Nm,Q) where (Nm,Tp) in Q => either(Tp).

    applyTypeFun(kFun(Nm,Ar),Args,_) where size(Args)=<Ar =>
      either(mkTypeExp(kFun(Nm,Ar),Args)).
    applyTypeFun(tFun(U,Ar,Nm),Args,_) where size(Args)=<Ar =>
      either(mkTypeExp(tFun(U,Ar,Nm),Args)).
    applyTypeFun(tpFun(Nm,Ar),Args,Lc) where size(Args)=<Ar =>
      either(mkTypeExp(tpFun(Nm,Ar),Args)).
    applyTypeFun(typeLambda(L,R),[A,..Args],Lc) where sameType(L,A,Env) =>
      applyTypeFun(R,Args,Lc).
    applyTypeFun(Tp,[],_) => either(Tp).
    applyTypeFun(Tp,Args,Lc) =>
      other(reportError(Rp,"type $(Tp) to applicable to $(Args)",Lc)).
    
    parseBoundTpVars:(list[ast])=>either[reports,list[(string,tipe)]].
    parseBoundTpVars([]) => either([]).
    parseBoundTpVars([V,..R]) =>
      parseBoundTpVars(R) >>= (L) =>
      parseBoundTpVar(V) >>= (Vr) => return [Vr,..L].
    
    parseBoundTpVar(Nm) where (_,Id) ^= isName(Nm) => either((Id,newTypeVar(Id))).
    parseBoundTpVar(FNm) where
	(_,Lhs,Rhs) ^= isBinary(FNm,"/") &&
    (_,Id) ^= isName(Lhs) &&
    (_,Ar) ^= isInt(Rhs) => either((Id,newTypeFun(Id,Ar))).
    parseBoundTpVar(O) default =>
      other(reportError(Rp,"invalid bound type variable $(O)",locOf(O))).

    parseConstraints:(list[ast],list[(string,tipe)])=>either[reports,list[constraint]].
    parseConstraints([],_) => either([]).
    parseConstraints([A,..As],Q) => do{
      Cn <- parseConstraint(A,Q);
      Cx <- parseConstraints(As,Q);
      lift [Cn,..Cx]
    }

    parseConstraint(A,Q) where (Lc,Lh,Rh) ^= isBinary(A,"<~") =>
      either(fieldConstraint(parseT(Lh,Q),parseT(Rh,Q))).
    parseConstraint(A,Q) where (Lc,Op,Args) ^= isSquareTerm(A) =>
      parseContractConstraint(Q,A,Env).

    rebind:(list[(string,tipe)],tipe)=>tipe.
    rebind([],T) => T.
    rebind([(Nm,TV),..L],T) where
	Ar ^= isUnboundFVar(TV) && sameType(TV,kFun(Nm,Ar),Env) =>
      rebind(L,allType(kFun(Nm,Ar),T)).
    rebind([(Nm,TV),..L],T) where sameType(TV,kVar(Nm),Env) =>
      rebind(L,allType(kVar(Nm),T)).

    wrapConstraints([],Tp)=>Tp.
    wrapConstraints([Cx,..Cs],Tp) => wrapConstraints(Cs,constrainedType(Tp,Cx)).
      
    
  } in parseT(Typ,[]).

  reQuant:(list[(string,tipe)],tipe) => tipe.
  reQuant([],Tp) => Tp.
  reQuant([(_,KV),..T],Tp) => reQuant(T,allType(KV,Tp)).

  reQuantX:(list[(string,tipe)],tipe) => tipe.
  reQuantX([],Tp) => Tp.
  reQuantX([(_,KV),..T],Tp) => reQuantX(T,existType(KV,Tp)).

  parseContractConstraint:(list[(string,tipe)],ast,dict,reports) =>
    either[reports,constraint].
  parseContractConstraint(Q,A,Env,Rp) where
      (Lc,Op,Ags) ^= isSquareTerm(A) => do{
    (Args,Deps) <- parseContractArgs(Q,Ags,Env,Rp);
    conConstraint(Con,ATs,Dps) <- parseContractName(Op,Env,Rp);
    if sameType(tupleType(Args),tupleType(ATs),Env) &&
    sameType(tupleType(Deps),tupleType(Dps),Env) then
      lift conConstraint(Con,Args,Deps)
    else
 throw reportError(Rp,"$(A) not consistent with contract $(Op)",Lc)
      }
  parseContractConstraint(_,A,Env,Rp) =>
    other(reportError(Rp,"$(A) is not a contract constraint",locOf(A))).

  parseContractName:(ast,dict,reports)=>either[reports,constraint].
    parseContractName(Op,Env,Rp) where (_,Id) ^= isName(Op) => do{
	Con <- findContract(Env,Id);
      }

  parseContractArgs:(list[(string,tipe)],list[ast],dict,reports) =>
    either[reports,(list[tipe],list[tipe])].
  parseContractArgs(Q,[A],Env,Rp) where
      (_,Lhs,Rhs) ^= isBinary(A,"->>") => do{
    LA <- parseTypes(Q,deComma(Lhs),Env,Rp);
    DA <- parseTypes(Q,deComma(Rhs),Env,Rp);
    lift (LA,DA)
      }
    
  parseTypes:(list[(string,tipe)],list[ast],dict,reports) => either[reports,list[tipe]].
  parseTypes(_,[],_,_) => either([]).
  parseTypes(Q,[T,..L],Env,Rp) => do{
    Tl <- parseType(Q,T,Env,Rp);
    Tr <- parseTypes(Q,L,Env,Rp);
    lift [Tl,..Tr]
  }

  

}
