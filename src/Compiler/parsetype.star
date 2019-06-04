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

  -- Convenience type decl
  tipes ~> cons[(string,tipe)].

  public parseType:(tipes,ast,dict,reports) => either[reports,tipe].

  parseType(Q0,Typ,Env,Rp) => let{
    parseT:(tipes,ast) => either[reports,tipe].
    parseT(Q,Tp) where (Lc,V,BT) ^= isQuantified(Tp) => do{
      BV <- parseBoundTpVars(V);
      In <- parseT(Q++BV,BT);
      valis reQuant(BV,In)
    }
    parseT(Q,Tp) where (Lc,V,BT) ^= isXQuantified(Tp) => do{
      BV <- parseBoundTpVars(V);
      In <- parseT(Q++BV,BT);
      valis reQuantX(BV,In)
    }
    parseT(Q,Tp) where (Lc,C,B) ^= isConstrained(Tp) => do{
      Cx <- parseConstraints(C,Q);
      Inn <- parseT(Q,B);
      valis wrapConstraints(Cx,Inn)
    }
    parseT(Q,Tp) where (Lc,Nm) ^= isName(Tp) =>
      parseTypeName(Q,Lc,Nm).
    parseT(Q,Tp) where (Lc,O,Args) ^= isSquareTerm(Tp) => do{
      Op <- parseT(Q,O);
      if (Qx,OOp) .= freshen(Op,[],Env) then {
	ArgTps <- parseTps(Q,Args);
	Inn <- doTypeFun(deRef(OOp),ArgTps,locOf(O));
	return rebind(Qx,Inn)
      }else
	  throw reportError(Rp,"Could not freshen $(Op)",Lc)
    }
    parseT(Q,T) where (Lc,Lhs,Rhs) ^= isBinary(T,"=>") => do{
      A <- parseArgType(Q,Lhs);
      R <- parseT(Q,Rhs);
      valis tpExp(tpExp(tpFun("=>",2),A),R)
    }
    parseT(Q,T) where (Lc,Lhs,Rhs) ^= isBinary(T,"<=>") => do{
      A <- parseArgType(Q,Lhs);
      R <- parseT(Q,Rhs);
      valis tpExp(tpExp(tpFun("<=>",2),A),R)
    }
    parseT(Q,T) where (Lc,Rhs) ^= isUnary(T,"ref") => do{
      R <- parseT(Q,Rhs);
      valis tpExp(tpFun("ref",1),R)
    }
    parseT(Q,T) where (Lc,[A]) ^= isTuple(T) => do{
      if (_,As) ^= isTuple(A) then{
	ArgTps <- parseTps(Q,As);
	valis tupleType(ArgTps)
      } else
	parseT(Q,A)
    }
    parseT(Q,T) where (_,As) ^= isTuple(T) => do{
      ArgTps <- parseTps(Q,As);
      valis tupleType(ArgTps)
    }
    parseT(Q,T) where (Lc,A) ^= isBrTuple(T) => do{
      (Flds,Tps) <- parseTypeFields(Q,A,[],[]);
      valis faceType(Flds::list[(string,tipe)],Tps::list[(string,tipe)])
    }
    parseT(Q,T) where (Lc,Lhs,Rhs) ^= isTypeLambda(T) => do{
      A <- parseArgType(Q,Lhs);
      R <- parseT(Q,Rhs);
      valis typeLambda(A,R)
    }
    -- TODO: field access of type
    parseT(Q,T) default =>
      other(reportError(Rp,"cannot parse type $(T)",locOf(T))).

    parseArgType(Q,A) where (_,As) ^= isTuple(A) => do{
      Args <- parseTps(Q,As);
      valis tupleType(Args)
    }
    parseArgType(Q,A) =>
      parseT(Q,A).
    
    parseTypeArgs:(locn,tipes,list[ast]) =>
      either[reports,(list[tipe],list[tipe])].
    parseTypeArgs(_,Q,[XX]) where (As,Ds)^=isDepends(XX) => do{
      Lhs <- parseTps(Q,As);
      Rhs <- parseTps(Q,Ds);
      valis (Lhs,Rhs)
    }.
    parseTypeArgs(_,Q,As) => do{
      ATps <- parseTps(Q,As);
      valis (ATps,[])
    }
    parseTypeArgs(Lc,_,As) =>
      other(reportError(Rp,"cannot parse argument types $(As)",Lc)).

    parseTps:(tipes,list[ast]) => either[reports,list[tipe]].
    parseTps(_,[]) => either([]).
    parseTps(Q,[T,..L]) => do{
      Tl <- parseT(Q,T);
      Tr <- parseTps(Q,L);
      valis [Tl,..Tr]
    }

    parseTypeFields:(tipes,list[ast],tipes,tipes) => either[reports,(tipes,tipes)].
    parseTypeFields(Q,[],Flds,Tps) => either((Flds,Tps)).
    parseTypeFields(Q,[F,..L],Flds,Tps) => do{
      (FF,TT) <- parseTypeField(Q,F,Flds,Tps);
      parseTypeFields(Q,L,FF,TT)
    }

    parseTypeField:(tipes,ast,tipes,tipes) => either[reports,(tipes,tipes)].
    parseTypeField(Q,F,Flds,Tps) where (_,Lhs,Rhs) ^= isTypeAnnotation(F) => do{
      if (ILc,Nm) ^= isName(Lhs) then {
	FTp<-parseT(Q,Rhs);
	valis ([(Nm,FTp),..Flds],Tps)
      } else
	throw reportError(Rp,"invalid lhs -- $(Lhs) -- of type annotation",locOf(Lhs))
    }
    parseTypeField(Q,F,Flds,Tps) where
	(_,A)^=isUnary(F,"type") &&
	(_,Lhs,Rhs) ^= isTypeAnnotation(A) => do{
	  if (ILc,Nm) ^= isName(Lhs) then {
	    FTp<-parseT(Q,Rhs);
	    valis (Flds,[(Nm,FTp),..Tps])
	  } else
	    throw reportError(Rp,"invalid lhs -- $(Lhs) -- of type annotation",locOf(Lhs))
	}.
    parseTypeField(Q,F,Flds,Tps) =>
      other(reportError(Rp,"invalid type field -- $(F)",locOf(F))).
	  
    parseTypeName(_,_,"_") => either(newTypeVar("_")).
    parseTypeName(_,_,"this") => either(thisType).
    parseTypeName(Q,_,Nm) where (Nm,Tp) in Q => either(Tp).
    parseTypeName(Q,_,Nm) where (_,T,TpDf) ^= findType(Env,Nm) =>
      either(typeLambda(_,_).=TpDf ? TpDf || T).
    parseTypeName(_,Lc,Nm) =>
      other(reportError(Rp,"type $(Nm) not declared",Lc)).
	    
    doTypeFun(typeLambda(tupleType([]),Tp),[],_) => either(Tp).
    doTypeFun(typeLambda(L,R),[A,..Args],Lc) where
	sameType(L,A,Env) =>
      doTypeFun(R,Args,Lc).
    doTypeFun(Op,[A,..Args],Lc) =>
      doTypeFun(tpExp(Op,A),Args,Lc).
    doTypeFun(Tp,[],_) => either(Tp).
    doTypeFun(Tp,Args,Lc) =>
      other(reportError(Rp,"type $(Tp) to applicable to $(Args)",Lc)).
    
    parseBoundTpVars:(list[ast])=>either[reports,tipes].
    parseBoundTpVars([]) => either([]).
    parseBoundTpVars([V,..R]) =>
      parseBoundTpVars(R) >>= (L) =>
      parseBoundTpVar(V) >>= (Vr) => return [Vr,..L].
    
    parseBoundTpVar(Nm) where (_,Id) ^= isName(Nm) => either((Id,kVar(Id))).
    parseBoundTpVar(FNm) where
	(_,Lhs,Rhs) ^= isBinary(FNm,"/") &&
	(_,Id) ^= isName(Lhs) &&
	(_,Ar) ^= isInt(Rhs) => either((Id,kFun(Id,Ar))).
    parseBoundTpVar(O) default =>
      other(reportError(Rp,"invalid bound type variable $(O)",locOf(O))).

    parseConstraints:(list[ast],tipes)=>either[reports,list[constraint]].
    parseConstraints([],_) => either([]).
    parseConstraints([A,..As],Q) => do{
      Cn <- parseConstraint(A,Q);
      Cx <- parseConstraints(As,Q);
      valis [Cn,..Cx]
    }

    parseConstraint(A,Q) where (Lc,Lh,Rh) ^= isBinary(A,"<~") => do{
      Bnd <- parseT(Q,Lh);
      Face <- parseT(Q,Rh);
      valis fieldConstraint(Bnd,Face).
    }
    parseConstraint(A,Q) where (Lc,Op,Args) ^= isSquareTerm(A) =>
      parseContractConstraint(Q,A,Env,Rp).

    rebind:(tipes,tipe)=>tipe.
    rebind([],T) => T.
    rebind([(Nm,TV),..L],T) where
	Ar ^= isUnboundFVar(TV) && sameType(TV,kFun(Nm,Ar),Env) =>
      rebind(L,allType(kFun(Nm,Ar),T)).
    rebind([(Nm,TV),..L],T) where sameType(TV,kVar(Nm),Env) =>
      rebind(L,allType(kVar(Nm),T)).

    wrapConstraints([],Tp)=>Tp.
    wrapConstraints([Cx,..Cs],Tp) => wrapConstraints(Cs,constrainedType(Tp,Cx)).
    
  } in parseT([],Typ).

  reQuant:(tipes,tipe) => tipe.
  reQuant([],Tp) => Tp.
  reQuant([(_,KV),..T],Tp) => reQuant(T,allType(KV,Tp)).

  reQuantX:(tipes,tipe) => tipe.
  reQuantX([],Tp) => Tp.
  reQuantX([(_,KV),..T],Tp) => reQuantX(T,existType(KV,Tp)).

  parseContractConstraint:(tipes,ast,dict,reports) =>
    either[reports,constraint].
  parseContractConstraint(Q,A,Env,Rp) where
      (Lc,Op,Ags) ^= isSquareTerm(A) && (_,Nm) ^= isName(Op) => do{
	if [Arg].=Ags && (Lc,L,R) ^= isBinary(Arg,"->>") then {
	  ArgTps <- parseTypes(Q,deComma(L),Env,Rp);
	  DepTps <- parseTypes(Q,deComma(R),Env,Rp);
	  valis typeConstraint(conTract(Nm,ArgTps,DepTps))
	} else {
	  ArgTps <- parseTypes(Q,Ags,Env,Rp);
	  valis typeConstraint(conTract(Nm,ArgTps,[]))
	}
      }.
  parseContractConstraint(_,A,Env,Rp) =>
    other(reportError(Rp,"$(A) is not a contract constraint",locOf(A))).

  parseContractName:(ast,dict,reports)=>either[reports,constraint].
  parseContractName(Op,Env,Rp) where (_,Id) ^= isName(Op) => do{
    if conDfn(_,_,_,Con) ^= findContract(Env,Id) then {
      valis typeConstraint(snd(freshen(Con,[],Env)))
    }
      else
	throw reportError(Rp,"contract $(Op) not defined",locOf(Op))
  }

  parseTypes:(tipes,list[ast],dict,reports) => either[reports,list[tipe]].
  parseTypes(_,[],_,_) => either([]).
  parseTypes(Q,[T,..L],Env,Rp) => do{
    Tl <- parseType(Q,T,Env,Rp);
    Tr <- parseTypes(Q,L,Env,Rp);
    valis [Tl,..Tr]
  }

}
