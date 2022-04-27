star.compiler.typeparse{
  import star.
  
  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.dict.mgt.
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
    valis reQ(BV,In)
  }
  parseType(Q,Tp,Env,Rp) where (Lc,V,BT) ^= isXQuantified(Tp) => do{
    BV <- parseBoundTpVars(V,Rp);
    In <- parseType(Q++BV,BT,Env,Rp);
    valis reQX(BV,In)
  }
  parseType(Q,Tp,Env,Rp) where (Lc,C,B) ^= isConstrained(Tp) => do{
    Cx <- parseConstraints(C,Q,Env,Rp);
    Inn <- parseType(Q,B,Env,Rp);
    valis wrapConstraints(Cx,Inn)
  }
  parseType(Q,Tp,Env,Rp) where (Lc,Nm) ^= isName(Tp) => do{
    if Nm=="_" then
      valis newTypeVar("_")
    else if VTp ^= {! VTp | (Nm,VTp) in Q !} then
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
    raise reportError(Rp,"type $(Nm) not declared",Lc)
  }
  parseType(Q,Tp,Env,Rp) where (Lc,O,Args) ^= isSquareTerm(Tp) && (OLc,Nm)^=isName(O) => do{
    (Op,Rl) <- parseTypeName(Q,OLc,Nm,Env,Rp);
    if (_,OOp) .= freshen(Op,Env) then {
      ArgTps <- parseTypes(Q,Args,Env,Rp);
      doTypeFun(OOp,ArgTps,Env,Rp)
    }
    else
    raise reportError(Rp,"Could not freshen $(Op)",Lc)
  }
  parseType(Q,T,Env,Rp) where (Lc,Lhs,Rhs) ^= isFunctionType(T) => do{
    A <- parseArgType(Q,Lhs,Env,Rp);
    R <- parseType(Q,Rhs,Env,Rp);
    valis fnType(A,R)
  }
  parseType(Q,T,Env,Rp) where (Lc,Lhs,Rhs) ^= isContTp(T) => do{
    A <- parseArgType(Q,Lhs,Env,Rp);
    R <- parseType(Q,Rhs,Env,Rp);
    valis contType(A,R)
  }
  parseType(Q,T,Env,Rp) where (Lc,Lhs,Rhs) ^= isConstructorType(T) => do{
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
  parseType(Q,T,Env,Rp) where (Lc,Lhs,Rhs) ^= isFieldAcc(T) => do{
    if (_,Id) ^= isName(Lhs) && (_,Fld) ^= isName(Rhs) then{
      if RcType ^= findVarFace(Id,Env) && faceType(_,Tps).=deRef(RcType) then{
	if Ftp ^= {! Ftp | (Fld,Ftp) in Tps !} then
	  valis Ftp
	else
	raise reportError(Rp,"Field $(Rhs) not defined in type $(RcType)",Lc)
      } else
      raise reportError(Rp,"variable $(Id) not defined",Lc)
    } else
    raise reportError(Rp,"type access to non-var $(Lhs) not supported",Lc)
  }
  parseType(Q,T,Env,Rp) where (Lc,Op,[L,R]) ^= isRoundTerm(T) =>
    parseType(Q,squareTerm(Lc,Op,[L,R]),Env,Rp).
  parseType(Q,T,Env,Rp) default =>
    other(reportError(Rp,"cannot understand type $(T)",locOf(T))).

  parseArgType(Q,A,Env,Rp) where (_,As) ^= isTuple(A) => do{
    Args <- parseTypes(Q,As,Env,Rp);
    valis tupleType(Args)
  }
  parseArgType(Q,A,Env,Rp) =>
    parseType(Q,A,Env,Rp).
    
  parseTypeArgs:(option[locn],tipes,cons[ast],dict,reports) =>
    either[reports,(cons[tipe],cons[tipe])].
  parseTypeArgs(_,Q,[XX],Env,Rp) where (_,As,Ds)^=isDepends(XX) => do{
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

  applyTypeRule:(option[locn],typeRule,cons[tipe],dict,reports) => either[reports,tipe].
  applyTypeRule(_,typeLambda(tupleType([]),Tp),[],_,_) => either(Tp).
  applyTypeFun(Lc,typeLambda(L,R),[A,..As],Env,Rp) => do{
    if sameType(L,A,Env) then{
      doTypeFun(R,As,Env,Rp)
    } else {
      raise reportError(Rp,"Type rule $(typeLambda(L,R)) does not apply to $(A)",Lc)
    }
  }

  doTypeFun:(tipe,cons[tipe],dict,reports) => either[reports,tipe].
  doTypeFun(Op,[A,..As],Env,Rp) =>
    doTypeFun(tpExp(Op,A),As,Env,Rp).
  doTypeFun(Tp,[],_,Rp) => either(Tp).

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
    raise reportError(Rp,"invalid lhs -- $(Lhs) -- of type annotation",locOf(Lhs)) -- 
  }
  parseTypeField(Q,F,Flds,Tps,Env,Rp) where
      (_,A)^=isUnary(F,"type") &&
      (_,Lhs,Rhs) ^= isTypeAnnotation(A) => do{
	if (ILc,Nm) ^= isName(Lhs) then {
	  FTp<-parseType(Q,Rhs,Env,Rp);
	  valis (Flds,[(Nm,FTp),..Tps])
	} else
	raise reportError(Rp,"invalid lhs -- $(Lhs) -- of type annotation",locOf(Lhs)) -- 
      }.
  parseTypeField(Q,F,Flds,Tps,Env,Rp) =>
    other(reportError(Rp,"invalid type field -- $(F)",locOf(F))). -- 
	  
  parseTypeName(_,_,"_",_,_) => either((newTypeVar("_"),.none)).
  parseTypeName(Q,_,Nm,_,_) where Tp^={! Tp | (Nm,Tp) in Q !} => either((Tp,.none)).
  parseTypeName(Q,_,Nm,Env,Rp) where (_,T,TpRl) ^= findType(Env,Nm) => do{
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
    if faceType([(Fld,FTp)],_).=deRef(Face) then
      valis fieldConstraint(Bnd,Fld,FTp)
    else
    raise reportError(Rp,"invalid rhs:$(Rh) of field constraint, expecting $(Lh)<~{F:T}",Lc)
  }
  parseConstraint(A,Q,Env,Rp) => parseContractConstraint(Q,A,Env,Rp).
  
  public rebind:(tipes,tipe,dict)=>tipe.
  rebind([],T,_) => T.
  rebind([(Nm,TV),..L],T,Env) where
      Ar ^= isUnboundFVar(TV) && sameType(TV,kFun(Nm,Ar),Env) =>
    rebind(L,allType(kFun(Nm,Ar),T),Env).
  rebind([(Nm,TV),..L],T,Env) where sameType(TV,nomnal(Nm),Env) =>
    rebind(L,allType(nomnal(Nm),T),Env).

  public wrapConstraints([],Tp)=>Tp.
  wrapConstraints([Cx,..Cs],Tp) => wrapConstraints(Cs,constrainedType(Tp,Cx)).
    
  reQ:(tipes,tipe) => tipe.
  reQ([],Tp) => Tp.
  reQ([(_,KV),..T],Tp) => reQ(T,allType(KV,Tp)).

  reQX:(tipes,tipe) => tipe.
  reQX([],Tp) => Tp.
  reQX([(_,KV),..T],Tp) => reQX(T,existType(KV,Tp)).

  public parseContractConstraint:(tipes,ast,dict,reports) =>
    either[reports,constraint].
  parseContractConstraint(Q,A,Env,Rp) where
      (Lc,O,As) ^= isSquareTerm(A) && (_,Nm) ^= isName(O) => do{
	contractExists(Cn,_,_,_) <- parseContractName(O,Env,Rp);
--	logMsg("contract name $(typeKey(Cn))");
	if [AAs].=As && (_,L,R) ^= isBinary(AAs,"->>") then{
	  Tps <- parseTypes(Q,deComma(L),Env,Rp);
	  Dps <- parseTypes(Q,deComma(R),Env,Rp);
	  valis conTract(Cn,Tps,Dps)
	} else{
	  Tps <- parseTypes(Q,As,Env,Rp);
	  valis conTract(Cn,Tps,[])
	}
      }
  parseContractConstraint(_,A,Env,Rp) =>
    other(reportError(Rp,"$(A) is not a contract constraint",locOf(A))).

  parseContractName:(ast,dict,reports)=>either[reports,typeRule].
  parseContractName(Op,Env,Rp) where (_,Id) ^= isName(Op) => do{
    if Con ^= findContract(Env,Id) then {
      valis snd(freshen(Con,Env))
    }
      else
	raise reportError(Rp,"contract $(Op) not defined",locOf(Op))
  }

  public parseAccessorContract:(tipes,ast,dict,reports) =>
    either[reports,constraint].
  parseAccessorContract(Q,A,Env,Rp) where
      (Lc,O,As) ^= isSquareTerm(A) && (_,Nm) ^= isName(O) => do{
	if [AAs].=As && (_,L,R) ^= isBinary(AAs,"->>") then{
	  Tps <- parseTypes(Q,deComma(L),Env,Rp);
	  Dps <- parseTypes(Q,deComma(R),Env,Rp);
	  valis conTract(Nm,Tps,Dps)
	} else{
	  Tps <- parseTypes(Q,As,Env,Rp);
	  valis conTract(Nm,Tps,[])
	}
      }
  parseAccessorContract(_,A,Env,Rp) =>
    other(reportError(Rp,"$(A) is not a valid accessor contract",locOf(A))).


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
  pickTypeTemplate(nomnal(Hd)) => nomnal(Hd).
  pickTypeTemplate(tpFun(Nm,Ar)) => tpFun(Nm,Ar).
  pickTypeTemplate(kFun(Nm,Ar)) => kFun(Nm,Ar).
  pickTypeTemplate(tpExp(Op,_)) => pickTypeTemplate(Op).

  public parseTypeDef:(string,ast,dict,string,reports) =>
    either[reports,(cons[canonDef],cons[decl])].
  parseTypeDef(Nm,St,Env,Path,Rp) where (Lc,V,C,H,B) ^= isTypeExistsStmt(St) => do{
    Q <- parseBoundTpVars(V,Rp);
    Tp <- parseTypeHead(Q,H,Env,Path,Rp);
    Cx <- parseConstraints(C,Q,Env,Rp);
    Tmplte .= pickTypeTemplate(Tp);
    Fce <- parseType(Q,B,declareType(Nm,Lc,Tmplte,typeExists(Tp,faceType([],[])),Env),Rp);
    TpRl .= foldLeft(((_,QV),Rl)=>allRule(QV,Rl),typeExists(reConstrainType(Cx,Tp),Fce),Q);
    valis ([typeDef(Lc,Nm,Tmplte,TpRl)],[tpeDec(Lc,Nm,Tmplte,TpRl)])
  }
  parseTypeDef(Nm,St,Env,Path,Rp) where (Lc,V,C,H,B) ^= isTypeFunStmt(St) => do{
    Q <- parseBoundTpVars(V,Rp);
    Tp <- parseTypeHead(Q,H,Env,Path,Rp);
    Cx <- parseConstraints(C,Q,Env,Rp);
    RTp <- parseType(Q,B,Env,Rp);
    
    Tmplte .= pickTypeTemplate(Tp);
    TpRl .= foldLeft(((_,QV),Rl)=>allRule(QV,Rl),typeLambda(reConstrainType(Cx,Tp),RTp),Q);

    valis ([typeDef(Lc,Nm,Tmplte,TpRl)],[tpeDec(Lc,Nm,Tmplte,TpRl)])
  }

  parseTypeHead:(tipes,ast,dict,string,reports) => either[reports,tipe].
  parseTypeHead(Q,Tp,Env,Path,Rp) where (Lc,Nm) ^= isName(Tp) => 
    either(nomnal(qualifiedName(Path,.typeMark,Nm))).
  parseTypeHead(Q,Tp,Env,Path,Rp) where
      (Lc,O,Args) ^= isSquareTerm(Tp) && (_,Nm) ^= isName(O) => do{
	ArgTps <- parseHeadArgs(Q,Args,[],Env,Rp);
	valis mkTypeExp(tpFun(qualifiedName(Path,.typeMark,Nm),size(ArgTps)),ArgTps)
      }.

  parseHeadArgs:(tipes,cons[ast],cons[tipe],dict,reports) => either[reports,cons[tipe]].
  parseHeadArgs(Q,[],ArgTps,_,_) => either(reverse(ArgTps)).
  parseHeadArgs(Q,[A,..As],Args,Env,Rp) where (_,Nm) ^= isName(A) =>
    parseHeadArgs(Q,As,[nomnal(Nm),..Args],Env,Rp).
  parseHeadArgs(Q,[A,.._],_,_,Rp) => other(reportError(Rp,"invalid argument in type: $(A)",locOf(A))).
      
  parseConstructor:(string,ast,dict,string,reports) =>
    either[reports,(cons[canonDef],cons[decl])].
  public parseConstructor(Nm,St,Env,Path,Rp) => do{
    Tp <- parseType([],St,Env,Rp);
    Lc .= locOf(St);
    FullNm .= qualifiedName(Path,.conMark,Nm);
    valis ([cnsDef(Lc,Nm,FullNm,Tp)],
      [cnsDec(Lc,Nm,FullNm,Tp)])
  }

  parseContractHead:(tipes,ast,dict,string,reports) => either[reports,constraint].
  parseContractHead(Q,Tp,Env,Path,Rp) where
      (Lc,O,Args) ^= isSquareTerm(Tp) && (_,Nm) ^= isName(O) => do{
	if [A].=Args && (_,Lhs,Rhs)^=isBinary(A,"->>") then{
	  ArgTps <- parseHeadArgs(Q,deComma(Lhs),[],Env,Rp);
	  DepTps <- parseHeadArgs(Q,deComma(Rhs),[],Env,Rp);
	  valis conTract(qualifiedName(Path,.typeMark,Nm),ArgTps,DepTps)
	}
	else{
	  ArgTps <- parseHeadArgs(Q,Args,[],Env,Rp);
	  valis conTract(qualifiedName(Path,.typeMark,Nm),ArgTps,[])
	}
      }.

  public parseContract:(ast,dict,string,reports) => either[reports,
    (cons[canonDef],cons[decl])].
  parseContract(St,Env,Path,Rp) => do{
    (Lc,Q,C,T,Els) ^= isCntrctStmt(St);
    (_,Op,As) ^= isSquareTerm(T);
    (_,Id) ^= isName(Op);
    BV <- parseBoundTpVars(Q,Rp);
    (Flds,Tps) <- parseTypeFields(BV,Els,[],[],Env,Rp);
    Face .= faceType(Flds,Tps);
    conTract(Con,CTps,CDps) <- parseContractHead(BV,T,Env,Path,Rp);
    ConTp .= reQ(BV,mkConType(Con,CTps,CDps));
    ConRlTp .= foldLeft(((_,QV),Rl)=>allRule(QV,Rl),contractExists(Con,CTps,CDps,Face),BV);
    valis ([conDef(Lc,Id,tpName(ConTp),ConRlTp)],
      [conDec(Lc,Id,tpName(ConTp),ConRlTp)])
  }
}
