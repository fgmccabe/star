star.compiler.typeparse{
  import star.
  
  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.misc.
  import star.compiler.errors.
  import star.compiler.freshen.
  import star.compiler.location.
  import star.compiler.types.
  import star.compiler.unify.
  import star.compiler.wff.

  -- Convenience type decl
  tipes ~> cons[(string,tipe)].

  public parseType:(tipes,ast,dict,reports) => either[reports,tipe].

  parseType(Q,Tp,Env,Rp) where (Lc,V,BT) ^= isQuantified(Tp) => do{
    BV <- parseBoundTpVars(V,Rp);
    In <- parseType(Q++BV,BT,Env,Rp);
    valis reQuant(BV,In)
  }
  parseType(Q,Tp,Env,Rp) where (Lc,V,BT) ^= isXQuantified(Tp) => do{
    BV <- parseBoundTpVars(V,Rp);
    In <- parseType(Q++BV,BT,Env,Rp);
    valis reQuantX(BV,In)
  }
  parseType(Q,Tp,Env,Rp) where (Lc,C,B) ^= isConstrained(Tp) => do{
    Cx <- parseConstraints(C,Q,Env,Rp);
    Inn <- parseType(Q,B,Env,Rp);
    valis wrapConstraints(Cx,Inn)
  }
  parseType(Q,Tp,Env,Rp) where (Lc,Nm) ^= isName(Tp) => do{
    if Nm=="_" then
      valis newTypeVar("_")
    else if (Nm,VTp) in Q then
      valis VTp
    else if (_,T,TpRl) ^= findType(Env,Nm) then{
      if isLambdaRule(TpRl) then{
	(_,typeLambda(_,Rhs)) .= freshen(TpRl,Env);
	valis Rhs
      }
      else
      valis T
    }
    else
    throw reportError(Rp,"type $(Nm) not declared",Lc)
  }
  
  parseType(Q,Tp,Env,Rp) where (Lc,O,Args) ^= isSquareTerm(Tp) && (OLc,Nm)^=isName(O) => do{
    (Op,Rl) <- parseTypeName(Q,OLc,Nm,Env,Rp);
--    logMsg("type op $(Op) - $(Rl)");
    if (Qx,OOp) .= freshen(Op,Env) then {
--      logMsg("freshened op $(OOp)");
      if [A].=Args && (_,Lhs,Rhs)^=isBinary(A,"->>") then{
	ArgTps <- parseTypes(Q,deComma(Lhs),Env,Rp);
	DepTps <- parseTypes(Q,deComma(Rhs),Env,Rp);
	valis rebind(Qx,funDeps(mkTypeExp(deRef(OOp),ArgTps),DepTps),Env)
      }
      else{
	ArgTps <- parseTypes(Q,Args,Env,Rp);
--	logMsg("doing type fun $(OOp) agin $(ArgTps)");
	Inn .= mkTypeExp(deRef(OOp),ArgTps);
	if LmRl^=Rl then{
	  (_,typeLambda(L,Rhs)) .= freshen(LmRl,Env);
--	  logMsg("freshened rule $(typeLambda(L,Rhs))");
	  if sameType(L,Inn,Env) then{
--	    logMsg("type expression $(Inn)");
	    valis rebind(Qx,Rhs,Env)
	  }
	  else
	  throw reportError(Rp,"type rule for $(Nm) does not apply to $(Tp)",OLc)
	} else{
--	logMsg("result $(Inn)");
	  valis rebind(Qx,Inn,Env)
	}
      }
    } else
    throw reportError(Rp,"Could not freshen $(Op)",Lc)
  }
  parseType(Q,T,Env,Rp) where (Lc,Lhs,Rhs) ^= isBinary(T,"=>") => do{
    A <- parseArgType(Q,Lhs,Env,Rp);
    R <- parseType(Q,Rhs,Env,Rp);
    valis fnType(A,R)
  }
  parseType(Q,T,Env,Rp) where (Lc,Lhs,Rhs) ^= isBinary(T,"<=>") => do{
    A <- parseArgType(Q,Lhs,Env,Rp);
    R <- parseType(Q,Rhs,Env,Rp);
    valis consType(A,R)
  }
  parseType(Q,T,Env,Rp) where (Lc,Rhs) ^= isRef(T) => do{
    R <- parseType(Q,Rhs,Env,Rp);
    valis tpExp(tpFun("star.core*ref",1),R)
  }
  parseType(Q,T,Env,Rp) where (Lc,[A]) ^= isTuple(T) => do{
    if (_,As) ^= isTuple(A) then{
      ArgTps <- parseTypes(Q,As,Env,Rp);
      valis tupleType(ArgTps)
    } else
    parseType(Q,A,Env,Rp)
  }
  parseType(Q,T,Env,Rp) where (_,As) ^= isTuple(T) => do{
    ArgTps <- parseTypes(Q,As,Env,Rp);
    valis tupleType(ArgTps)
  }
  parseType(Q,T,Env,Rp) where (Lc,A) ^= isBrTuple(T) => do{
    (Flds,Tps) <- parseTypeFields(Q,A,[],[],Env,Rp);
    valis faceType(Flds,Tps)
  }
  parseType(Q,T,Env,Rp) where (Lc,Lhs,Rhs) ^= isTypeLambda(T) => do{
    A <- parseArgType(Q,Lhs,Env,Rp);
    R <- parseType(Q,Rhs,Env,Rp);
    valis typeLambda(A,R)
  }
  parseType(Q,T,Env,Rp) where (Lc,Lhs,Rhs) ^= isTypeExists(T) => do{
    A <- parseArgType(Q,Lhs,Env,Rp);
    R <- parseType(Q,Rhs,Env,Rp);
    valis typeExists(A,R)
  }
  -- TODO: field access of type
  parseType(Q,T,Env,Rp) default =>
    other(reportError(Rp,"cannot parse type $(T)",locOf(T))).

  parseArgType(Q,A,Env,Rp) where (_,As) ^= isTuple(A) => do{
    Args <- parseTypes(Q,As,Env,Rp);
    valis tupleType(Args)
  }
  parseArgType(Q,A,Env,Rp) =>
    parseType(Q,A,Env,Rp).
    
  parseTypeArgs:(locn,tipes,cons[ast],dict,reports) =>
    either[reports,(cons[tipe],cons[tipe])].
  parseTypeArgs(_,Q,[XX],Env,Rp) where (As,Ds)^=isDepends(XX) => do{
    Lhs <- parseTypes(Q,As,Env,Rp);
    Rhs <- parseTypes(Q,Ds,Env,Rp);
    valis (Lhs,Rhs)
  }.
  parseTypeArgs(_,Q,As,Env,Rp) => do{
    ATps <- parseTypes(Q,As,Env,Rp);
    valis (ATps,[])
  }
  parseTypeArgs(Lc,_,As,Env,Rp) =>
    other(reportError(Rp,"cannot parse argument types $(As)",Lc)).

  parseTypeFields:(tipes,cons[ast],tipes,tipes,dict,reports) =>
    either[reports,(tipes,tipes)].
  parseTypeFields(Q,[],Flds,Tps,_,_) => either((Flds,Tps)).
  parseTypeFields(Q,[A,..L],Flds,Tps,Env,Rp) where _ ^= isAnnotation(A) =>
    parseTypeFields(Q,L,Flds,Tps,Env,Rp).
  parseTypeFields(Q,[F,..L],Flds,Tps,Env,Rp) => do{
    (FF,TT) <- parseTypeField(Q,F,Flds,Tps,Env,Rp);
    parseTypeFields(Q,L,FF,TT,Env,Rp)
  }

  parseTypeField:(tipes,ast,tipes,tipes,dict,reports) => either[reports,(tipes,tipes)].
  parseTypeField(Q,F,Flds,Tps,Env,Rp) where (_,Lhs,Rhs) ^= isTypeAnnotation(F) => do{
    if (ILc,Nm) ^= isName(Lhs) then {
      FTp<-parseType(Q,Rhs,Env,Rp);
      valis ([(Nm,FTp),..Flds],Tps)
    } else
    throw reportError(Rp,"invalid lhs -- $(Lhs) -- of type annotation",locOf(Lhs)) -- 
  }
  parseTypeField(Q,F,Flds,Tps,Env,Rp) where
      (_,A)^=isUnary(F,"type") &&
      (_,Lhs,Rhs) ^= isTypeAnnotation(A) => do{
	if (ILc,Nm) ^= isName(Lhs) then {
	  FTp<-parseType(Q,Rhs,Env,Rp);
	  valis (Flds,[(Nm,FTp),..Tps])
	} else
	throw reportError(Rp,"invalid lhs -- $(Lhs) -- of type annotation",locOf(Lhs)) -- 
      }.
  parseTypeField(Q,F,Flds,Tps,Env,Rp) =>
    other(reportError(Rp,"invalid type field -- $(F)",locOf(F))). -- 
	  
  parseTypeName(_,_,"_",_,_) => either((newTypeVar("_"),.none)).
  parseTypeName(Q,_,Nm,_,_) where (Nm,Tp) in Q => either((Tp,.none)).
  parseTypeName(Q,_,Nm,Env,Rp) where (_,T,TpRl) ^= findType(Env,Nm) => do{
--    logMsg("is $(T) \: $(TpRl) a rule?");
    if isLambdaRule(TpRl) then 
      valis (T,some(TpRl))
    else
    valis (T,.none)}
  parseTypeName(_,Lc,Nm,Env,Rp) =>
    other(reportError(Rp,"type $(Nm) not declared",Lc)).

  public parseBoundTpVars:(cons[ast],reports)=>either[reports,tipes].
  parseBoundTpVars([],_) => either([]).
  parseBoundTpVars([V,..R],Rp) => do{
    L <- parseBoundTpVars(R,Rp);
    Vr <- parseBoundTpVar(V,Rp);
    valis [Vr,..L]
  }
    
  parseBoundTpVar(Nm,_) where (_,Id) ^= isName(Nm) => either((Id,nomnal(Id))).
  parseBoundTpVar(FNm,_) where
      (_,Lhs,Rhs) ^= isBinary(FNm,"/") &&
      (_,Id) ^= isName(Lhs) &&
      (_,Ar) ^= isInt(Rhs) => either((Id,kFun(Id,Ar))).
  parseBoundTpVar(O,Rp) default =>
    other(reportError(Rp,"invalid bound type variable $(O)",locOf(O))).

  public parseConstraints:(cons[ast],tipes,dict,reports)=>either[reports,cons[constraint]].
  parseConstraints([],_,_,_) => either([]).
  parseConstraints([A,..As],Q,Env,Rp) => do{
    Cn <- parseConstraint(A,Q,Env,Rp);
    Cx <- parseConstraints(As,Q,Env,Rp);
    valis [Cn,..Cx]
  }

  parseConstraint(A,Q,Env,Rp) where (Lc,Lh,Rh) ^= isBinary(A,"<~") => do{
    Bnd <- parseType(Q,Lh,Env,Rp);
    Face <- parseType(Q,Rh,Env,Rp);
    valis fieldConstraint(Bnd,Face).
  }
  parseConstraint(A,Q,Env,Rp) where (Lc,Op,Args) ^= isSquareTerm(A) => do{
    Tp<-parseContractConstraint(Q,A,Env,Rp);
    valis typeConstraint(Tp)
  }

  public rebind:(tipes,tipe,dict)=>tipe.
  rebind([],T,_) => T.
  rebind([(Nm,TV),..L],T,Env) where
      Ar ^= isUnboundFVar(TV) && sameType(TV,kFun(Nm,Ar),Env) =>
    rebind(L,allType(kFun(Nm,Ar),T),Env).
  rebind([(Nm,TV),..L],T,Env) where sameType(TV,nomnal(Nm),Env) =>
    rebind(L,allType(nomnal(Nm),T),Env).

  public wrapConstraints([],Tp)=>Tp.
  wrapConstraints([Cx,..Cs],Tp) => wrapConstraints(Cs,constrainedType(Tp,Cx)).
    
  public reQuant:(tipes,tipe) => tipe.
  reQuant([],Tp) => Tp.
  reQuant([(_,KV),..T],Tp) => reQuant(T,allType(KV,Tp)).

  public reQuantX:(tipes,tipe) => tipe.
  reQuantX([],Tp) => Tp.
  reQuantX([(_,KV),..T],Tp) => reQuantX(T,existType(KV,Tp)).

  public parseContractConstraint:(tipes,ast,dict,reports) =>
    either[reports,tipe].
  parseContractConstraint(Q,A,Env,Rp) where
      _ ^= isSquareTerm(A) =>  parseType(Q,A,Env,Rp).
  parseContractConstraint(_,A,Env,Rp) =>
    other(reportError(Rp,"$(A) is not a contract constraint",locOf(A))).

  parseContractName:(ast,dict,reports)=>either[reports,constraint].
  parseContractName(Op,Env,Rp) where (_,Id) ^= isName(Op) => do{
    if Con ^= findContract(Env,Id) then {
      valis typeConstraint(snd(freshen(Con,Env)))
    }
      else
	throw reportError(Rp,"contract $(Op) not defined",locOf(Op))
  }

  parseTypes:(tipes,cons[ast],dict,reports) => either[reports,cons[tipe]].
  parseTypes(_,[],_,_) => either([]).
  parseTypes(Q,[T,..L],Env,Rp) => do{
    Tl <- parseType(Q,T,Env,Rp);
    Tr <- parseTypes(Q,L,Env,Rp);
    valis [Tl,..Tr]
  }

  pickTypeTemplate:(tipe) => tipe.
  pickTypeTemplate(allType(_,Tp)) => pickTypeTemplate(Tp).
  pickTypeTemplate(existType(_,Tp)) => pickTypeTemplate(Tp).
  pickTypeTemplate(constrainedType(Hd,_)) => pickTypeTemplate(Hd).
  pickTypeTemplate(typeExists(Hd,_)) => pickTypeTemplate(Hd).
  pickTypeTemplate(typeLambda(Hd,_)) => pickTypeTemplate(Hd).
  pickTypeTemplate(nomnal(Hd)) => nomnal(Hd).
  pickTypeTemplate(tpFun(Nm,Ar)) => tpFun(Nm,Ar).
  pickTypeTemplate(kFun(Nm,Ar)) => kFun(Nm,Ar).
  pickTypeTemplate(tpExp(Op,_)) => pickTypeTemplate(Op).

  public parseTypeDef:(string,ast,dict,string,reports) => either[reports,(canonDef,dict)].
  parseTypeDef(Nm,St,Env,Path,Rp) where (Lc,V,C,H,B) ^= isTypeExistsStmt(St) => do{
    Q <- parseBoundTpVars(V,Rp);
    Tp <- parseTypeHead(Q,H,Env,Path,Rp);
    Cx <- parseConstraints(C,Q,Env,Rp);
    Tmplte .= pickTypeTemplate(Tp);
    Fce <- parseType(Q,B,declareType(Nm,some(Lc),Tmplte,typeExists(Tp,faceType([],[])),Env),Rp);
    TpRl .= reQuant(Q,reConstrainType(Cx,typeExists(Tp,Fce)));
    valis (typeDef(Lc,Nm,Tmplte,TpRl),declareType(Nm,some(Lc),Tmplte,TpRl,Env))
  }
  parseTypeDef(Nm,St,Env,Path,Rp) where (Lc,V,C,H,B) ^= isTypeFunStmt(St) => do{
    Q <- parseBoundTpVars(V,Rp);
    Tp <- parseTypeHead(Q,H,Env,Path,Rp);
    Cx <- parseConstraints(C,Q,Env,Rp);
    RTp <- parseType(Q,B,Env,Rp);
    
    Tmplte .= pickTypeTemplate(Tp);
    TpRl .= reQuant(Q,reConstrainType(Cx,typeLambda(Tp,RTp)));

    valis (typeDef(Lc,Nm,Tmplte,TpRl),declareType(Nm,some(Lc),Tmplte,TpRl,Env))
  }

  parseTypeHead:(tipes,ast,dict,string,reports) => either[reports,tipe].
  parseTypeHead(Q,Tp,Env,Path,Rp) where (Lc,Nm) ^= isName(Tp) => 
    either(nomnal(qualifiedName(Path,.typeMark,Nm))).
  parseTypeHead(Q,Tp,Env,Path,Rp) where
      (Lc,O,Args) ^= isSquareTerm(Tp) && (_,Nm) ^= isName(O) => do{
	if [A].=Args && (_,Lhs,Rhs)^=isBinary(A,"->>") then{
	  ArgTps <- parseHeadArgs(Q,deComma(Lhs),[],Env,Rp);
	  DepTps <- parseHeadArgs(Q,deComma(Rhs),[],Env,Rp);
	  Inn .= mkTypeExp(tpFun(qualifiedName(Path,.typeMark,Nm),size(ArgTps)),ArgTps);
	  valis funDeps(Inn,DepTps)
	}
	else{
	  ArgTps <- parseHeadArgs(Q,Args,[],Env,Rp);
	  valis mkTypeExp(tpFun(qualifiedName(Path,.typeMark,Nm),size(ArgTps)),ArgTps)
	}
      }.

  parseHeadArgs:(tipes,cons[ast],cons[tipe],dict,reports) => either[reports,cons[tipe]].
  parseHeadArgs(Q,[],ArgTps,_,_) => either(reverse(ArgTps)).
  parseHeadArgs(Q,[A,..As],Args,Env,Rp) where (_,Nm) ^= isName(A) =>
    parseHeadArgs(Q,As,[nomnal(Nm),..Args],Env,Rp).
  parseHeadArgs(Q,[A,.._],_,_,Rp) => other(reportError(Rp,"invalid argument in type: $(A)",locOf(A))).
      
  public parseConstructor(Nm,St,Env,Path,Rp) => do{
    Tp <- parseType([],St,Env,Rp);
    Lc .= locOf(St);
    FullNm .= qualifiedName(Path,.conMark,Nm);
--    logMsg("declaring $(Nm) as $(FullNm)\:$(Tp)");
    valis (cnsDef(Lc,Nm,FullNm,Tp),
      declareConstructor(Nm,FullNm,some(Lc),Tp,Env))
  }

  public parseContract:(ast,dict,string,reports) => either[reports,tipe].
  parseContract(St,Env,Path,Rp) where
      (Lc,Lhs,Els) ^= isContractStmt(St) &&
      (_,Nm,Q,C,T) ^= isContractSpec(Lhs) &&
      (_,Op,As) ^= isSquareTerm(T) &&
      (_,Id) ^= isName(Op) => do{
	BV <- parseBoundTpVars(Q,Rp);
	(Flds,Tps) <- parseTypeFields(BV,Els,[],[],Env,Rp);
	Face .= faceType(Flds,Tps);
	Con <- parseTypeHead(BV,T,Env,Path,Rp);
	valis reQuant(BV,typeExists(Con,Face))
      }
}
