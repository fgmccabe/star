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

  public parseType:(tipes,ast,dict) => tipe.

  parseType(Q,Tp,Env) where (Lc,V,BT) ^= isQuantified(Tp) => valof{
    BV = parseBoundTpVars(V);
    In = parseType(Q++BV,BT,Env);
    valis reQ(BV,In)
  }
  parseType(Q,Tp,Env) where (Lc,V,BT) ^= isXQuantified(Tp) => valof{
    BV = parseBoundTpVars(V);
    In = parseType(Q++BV,BT,Env);
    valis reQX(BV,In)
  }
  parseType(Q,Tp,Env) where (Lc,C,B) ^= isConstrained(Tp) => valof{
    Cx = parseConstraints(C,Q,Env);
    Inn = parseType(Q,B,Env);
    valis wrapConstraints(Cx,Inn)
  }
  parseType(Q,Tp,Env) where (Lc,Nm) ^= isName(Tp) => valof{
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
    else{
      reportError("type $(Nm) not declared",Lc);
      valis .voidType
    }
  }
  parseType(Q,Tp,Env) where (Lc,O,Args) ^= isSquareTerm(Tp) && (OLc,Nm)^=isName(O) => valof{
    (Op,Rl) = parseTypeName(Q,OLc,Nm,Env);
    if (_,OOp) .= freshen(Op,Env) then {
      ArgTps = parseTypes(Q,Args,Env);
      valis doTypeFun(OOp,ArgTps,Env)
    }
    else{
      reportError("Could not freshen $(Op)",Lc);
      valis .voidType
    }
  }
  parseType(Q,T,Env) where (Lc,Lhs,Rhs) ^= isFunctionType(T) => valof{
    A = parseArgType(Q,Lhs,Env);
    R = parseType(Q,Rhs,Env);
    valis fnType(A,R)
  }
  parseType(Q,T,Env) where (Lc,Lhs,Rhs) ^= isThrows(T) => valof{
    A = parseType(Q,Lhs,Env);
    E = parseType(Q,Rhs,Env);
    valis throwsType(A,E)
  }
  parseType(Q,T,Env) where (Lc,Lhs,Rhs) ^= isConstructorType(T) => valof{
    A = parseArgType(Q,Lhs,Env);
    R = parseType(Q,Rhs,Env);
    valis consType(A,R)
  }
  parseType(Q,T,Env) where (Lc,Rhs) ^= isRef(T) =>
    refType(parseType(Q,Rhs,Env)).
  parseType(Q,T,Env) where (Lc,[A]) ^= isTuple(T) => valof{
    if (_,As) ^= isTuple(A) then{
      ArgTps = parseTypes(Q,As,Env);
      valis tupleType(ArgTps)
    } else
    valis parseType(Q,A,Env)
  }
  parseType(Q,T,Env) where (_,As) ^= isTuple(T) => valof{
    ArgTps = parseTypes(Q,As,Env);
    valis tupleType(ArgTps)
  }
  parseType(Q,T,Env) where (Lc,A) ^= isBrTuple(T) => valof{
    (Flds,Tps) = parseTypeFields(Q,A,[],[],Env);
    valis faceType(Flds,Tps)
  }
  parseType(Q,T,Env) where (Lc,Lhs,Rhs) ^= isFieldAcc(T) => valof{
    if (_,Id) ^= isName(Lhs) && (_,Fld) ^= isName(Rhs) then{
      if RcType ^= findVarFace(Id,Env) && faceType(_,Tps).=deRef(RcType) then{
	if Ftp ^= {! Ftp | (Fld,Ftp) in Tps !} then
	  valis Ftp
	else{
	  reportError("Field $(Rhs) not defined in type $(RcType)",Lc);
	  valis .voidType
	}
      } else{
	reportError("variable $(Id) not defined",Lc);
	valis .voidType
      }
    } else{
      reportError("type access to non-var $(Lhs) not supported",Lc);
      valis .voidType
    }
  }
  parseType(Q,T,Env) where (Lc,Op,[L,R]) ^= isRoundTerm(T) =>
    parseType(Q,squareTerm(Lc,Op,[L,R]),Env).
  parseType(Q,T,Env) default => valof{
    reportError("cannot understand type $(T)",locOf(T));
    valis .voidType
  }

  parseArgType(Q,A,Env) where (_,As) ^= isTuple(A) => valof{
    Args = parseTypes(Q,As,Env);
    valis tupleType(Args)
  }
  parseArgType(Q,A,Env) =>
    parseType(Q,A,Env).
    
  parseTypeArgs:(option[locn],tipes,cons[ast],dict) => (cons[tipe],cons[tipe]).
  parseTypeArgs(_,Q,[XX],Env) where (_,As,Ds)^=isDepends(XX) => valof{
    Lhs = parseTypes(Q,As,Env);
    Rhs = parseTypes(Q,Ds,Env);
    valis (Lhs,Rhs)
  }.
  parseTypeArgs(_,Q,As,Env) => valof{
    ATps = parseTypes(Q,As,Env);
    valis (ATps,[])
  }
  parseTypeArgs(Lc,_,As,Env) => valof{
    reportError("cannot parse argument types $(As)",Lc);
    valis ([],[])
  }.

  applyTypeRule:(option[locn],typeRule,cons[tipe],dict) => tipe.
  applyTypeRule(_,typeLambda(tupleType([]),Tp),[],_) => Tp.
  applyTypeFun(Lc,typeLambda(L,R),[A,..As],Env) => valof{
    if sameType(L,A,Env) then{
      valis doTypeFun(R,As,Env)
    } else {
      reportError("Type rule $(typeLambda(L,R)) does not apply to $(A)",Lc);
      valis .voidType
    }
  }

  doTypeFun:(tipe,cons[tipe],dict) => tipe.
  doTypeFun(Op,[A,..As],Env) =>
    doTypeFun(tpExp(Op,A),As,Env).
  doTypeFun(Tp,[],_) => Tp.

  parseTypeFields:(tipes,cons[ast],tipes,tipes,dict) => (tipes,tipes).
  parseTypeFields(Q,[],Flds,Tps,_) => (Flds,Tps).
  parseTypeFields(Q,[A,..L],Flds,Tps,Env) where _ ^= isAnnotation(A) =>
    parseTypeFields(Q,L,Flds,Tps,Env).
  parseTypeFields(Q,[F,..L],Flds,Tps,Env) => valof{
    (FF,TT) = parseTypeField(Q,F,Flds,Tps,Env);
    parseTypeFields(Q,L,FF,TT,Env)
  }

  parseTypeField:(tipes,ast,tipes,tipes,dict) => (tipes,tipes).
  parseTypeField(Q,F,Flds,Tps,Env) where (_,Lhs,Rhs) ^= isTypeAnnotation(F) => valof{
    if (ILc,Nm) ^= isName(Lhs) then {
      FTp=parseType(Q,Rhs,Env);
      valis ([(Nm,FTp),..Flds],Tps)
    } else{
      reportError("invalid lhs -- $(Lhs) -- of type annotation",locOf(Lhs));
      valis ([],[])
    }
  }
  parseTypeField(Q,F,Flds,Tps,Env) where
      (_,A)^=isUnary(F,"type") &&
      (_,Lhs,Rhs) ^= isTypeAnnotation(A) => valof{
	if (ILc,Nm) ^= isName(Lhs) then {
	  FTp=parseType(Q,Rhs,Env);
	  valis (Flds,[(Nm,FTp),..Tps])
	} else{
	  reportError("invalid lhs -- $(Lhs) -- of type annotation",locOf(Lhs));
	  valis ([],[])
	}
      }.
  parseTypeField(Q,F,Flds,Tps,Env) => valof{
    reportError("invalid type field -- $(F)",locOf(F));
    valis ([],[])
  }.
	  
  parseTypeName(_,_,"_",_) => (newTypeVar("_"),.none).
  parseTypeName(Q,_,Nm,_) where Tp^={! Tp | (Nm,Tp) in Q !} => (Tp,.none).
  parseTypeName(Q,_,Nm,Env) where (_,T,TpRl) ^= findType(Env,Nm) => valof{
    if isLambdaRule(TpRl) then 
      valis (T,some(TpRl))
    else
    valis (T,.none)}
  parseTypeName(_,Lc,Nm,Env) => valof{
    reportError("type $(Nm) not declared",Lc);
    valis (.voidType,.none)
  }

  public parseBoundTpVars:(cons[ast])=>tipes.
  parseBoundTpVars([]) => [].
  parseBoundTpVars([V,..R]) => valof{
    L = parseBoundTpVars(R);
    Vr = parseBoundTpVar(V);
    valis [Vr,..L]
  }
    
  parseBoundTpVar(Nm) where (_,Id) ^= isName(Nm) => (Id,nomnal(Id)).
  parseBoundTpVar(FNm) where
      (_,Lhs,Rhs) ^= isBinary(FNm,"/") &&
      (_,Id) ^= isName(Lhs) &&
      (_,Ar) ^= isInt(Rhs) => (Id,kFun(Id,Ar)).
  parseBoundTpVar(O) default => valof{
    reportError("invalid bound type variable $(O)",locOf(O));
    valis ("",.voidType)
  }

  public parseConstraints:(cons[ast],tipes,dict)=>cons[constraint].
  parseConstraints([],_,_) => [].
  parseConstraints([A,..As],Q,Env) => valof{
    Cn = parseConstraint(A,Q,Env);
    Cx = parseConstraints(As,Q,Env);
    valis [Cn,..Cx]
  }

  parseConstraint(A,Q,Env) where (Lc,Lh,Rh) ^= isBinary(A,"<~") => valof{
    Bnd = parseType(Q,Lh,Env);
    Face = parseType(Q,Rh,Env);
    if faceType([(Fld,FTp)],_).=deRef(Face) then
      valis fieldConstraint(Bnd,Fld,FTp)
    else{
      reportError("invalid rhs:$(Rh) of field constraint, expecting $(Lh)<~{F:T}",Lc);
      valis fieldConstraint(.voidType,"",.voidType)
    }
  }
  parseConstraint(A,Q,Env) => parseContractConstraint(Q,A,Env).
  
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

  public parseContractConstraint:(tipes,ast,dict) => constraint.
  parseContractConstraint(Q,A,Env) where
      (Lc,O,As) ^= isSquareTerm(A) && (_,Nm) ^= isName(O) => valof{
	contractExists(Cn,_,_,_) .= parseContractName(O,Env);
--	logMsg("contract name $(typeKey(Cn))");
	if [AAs].=As && (_,L,R) ^= isBinary(AAs,"->>") then{
	  Tps = parseTypes(Q,deComma(L),Env);
	  Dps = parseTypes(Q,deComma(R),Env);
	  valis conTract(Cn,Tps,Dps)
	} else{
	  Tps = parseTypes(Q,As,Env);
	  valis conTract(Cn,Tps,[])
	}
      }.
  parseContractConstraint(_,A,Env) => valof{
    reportError("$(A) is not a contract constraint",locOf(A));
    valis conTract("",[],[])
  }.

  parseContractName:(ast,dict)=>typeRule.
  parseContractName(Op,Env) where (_,Id) ^= isName(Op) => valof{
    if Con ^= findContract(Env,Id) then {
      valis snd(freshen(Con,Env))
    }
    else{
      reportError("contract $(Op) not defined",locOf(Op));
      valis typeExists(.voidType,.voidType)
    }
  }

  public parseAccessorContract:(tipes,ast,dict) => constraint.
  parseAccessorContract(Q,A,Env) where
      (Lc,O,As) ^= isSquareTerm(A) && (_,Nm) ^= isName(O) => valof{
	if [AAs].=As && (_,L,R) ^= isBinary(AAs,"->>") then{
	  Tps = parseTypes(Q,deComma(L),Env);
	  Dps = parseTypes(Q,deComma(R),Env);
	  valis conTract(Nm,Tps,Dps)
	} else{
	  Tps = parseTypes(Q,As,Env);
	  valis conTract(Nm,Tps,[])
	}
      }.
  parseAccessorContract(_,A,Env) => valof{
    reportError("$(A) is not a valid accessor contract",locOf(A));
    valis conTract("void",[],[])
  }.

  parseTypes:(tipes,cons[ast],dict) => cons[tipe].
  parseTypes(_,[],_) => [].
  parseTypes(Q,[T,..L],Env) => valof{
    Tl = parseType(Q,T,Env);
    Tr = parseTypes(Q,L,Env);
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
  pickTypeTemplate(throwsType(T,_)) => pickTypeTemplate(T).

  public parseTypeDef:(string,ast,dict,string) => (cons[canonDef],cons[decl]).
  parseTypeDef(Nm,St,Env,Path) where (Lc,V,C,H,B) ^= isTypeExistsStmt(St) => valof{
    logMsg("parse type exists $(St)");
    Q = parseBoundTpVars(V);
    Tp = parseTypeHead(Q,H,Env,Path);
    Cx = parseConstraints(C,Q,Env);
    Tmplte = pickTypeTemplate(Tp);
    Fce = parseType(Q,B,declareType(Nm,Lc,Tmplte,typeExists(Tp,faceType([],[])),Env));
    TpRl = foldLeft(((_,QV),Rl)=>allRule(QV,Rl),typeExists(reConstrainType(Cx,Tp),Fce),Q);
    valis ([typeDef(Lc,Nm,Tmplte,TpRl)],[tpeDec(Lc,Nm,Tmplte,TpRl)])
  }
  parseTypeDef(Nm,St,Env,Path) where (Lc,V,C,H,B) ^= isTypeFunStmt(St) => valof{
    logMsg("parse type fun $(St)");
    Q = parseBoundTpVars(V);
    Tp = parseTypeHead(Q,H,Env,Path);
    Cx = parseConstraints(C,Q,Env);
    RTp = parseType(Q,B,Env);
    
    Tmplte = pickTypeTemplate(Tp);
    TpRl = foldLeft(((_,QV),Rl)=>allRule(QV,Rl),typeLambda(reConstrainType(Cx,Tp),RTp),Q);

    valis ([typeDef(Lc,Nm,Tmplte,TpRl)],[tpeDec(Lc,Nm,Tmplte,TpRl)])
  }

  parseTypeHead:(tipes,ast,dict,string) => tipe.
  parseTypeHead(Q,Tp,Env,Path) where (Lc,Nm) ^= isName(Tp) => 
    nomnal(qualifiedName(Path,.typeMark,Nm)).
  parseTypeHead(Q,Tp,Env,Path) where
      (Lc,O,Args) ^= isSquareTerm(Tp) && (_,Nm) ^= isName(O) => valof{
	ArgTps = parseHeadArgs(Q,Args,[],Env);
	valis mkTypeExp(tpFun(qualifiedName(Path,.typeMark,Nm),size(ArgTps)),ArgTps)
      }.

  parseHeadArgs:(tipes,cons[ast],cons[tipe],dict) => cons[tipe].
  parseHeadArgs(Q,[],ArgTps,_) => reverse(ArgTps).
  parseHeadArgs(Q,[A,..As],Args,Env) where (_,Nm) ^= isName(A) =>
    parseHeadArgs(Q,As,[nomnal(Nm),..Args],Env).
  parseHeadArgs(Q,[A,.._],_,_) => valof{
    reportError("invalid argument in type: $(A)",locOf(A));
    valis []
  }.
      
  parseConstructor:(string,ast,dict,string) => (cons[canonDef],cons[decl]).
  public parseConstructor(Nm,St,Env,Path) => valof{
    Tp = parseType([],St,Env);
    Lc = locOf(St);
    FullNm = qualifiedName(Path,.conMark,Nm);
    valis ([cnsDef(Lc,Nm,FullNm,Tp)],
      [cnsDec(Lc,Nm,FullNm,Tp)])
  }

  parseContractHead:(tipes,ast,dict,string) => constraint.
  parseContractHead(Q,Tp,Env,Path) where
      (Lc,O,Args) ^= isSquareTerm(Tp) => valof{
	ConTp = parseType(Q,dollarName(O),Env);
	if [A].=Args && (_,Lhs,Rhs)^=isBinary(A,"->>") then{
	  ArgTps = parseHeadArgs(Q,deComma(Lhs),[],Env);
	  DepTps = parseHeadArgs(Q,deComma(Rhs),[],Env);
	  valis conTract(tpName(ConTp),ArgTps,DepTps)
	}
	else{
	  ArgTps = parseHeadArgs(Q,Args,[],Env);
	  valis conTract(tpName(ConTp),ArgTps,[])
	}
      }.

  public parseContract:(ast,dict,string) => (cons[canonDef],cons[decl]).
  parseContract(St,Env,Path) => valof{
    (Lc,Q,C,T,Els) ^= isCntrctStmt(St);
    (_,Op,As) ^= isSquareTerm(T);
    (_,Id) ^= isName(Op);
    BV = parseBoundTpVars(Q);
    (Flds,Tps) = parseTypeFields(BV,Els,[],[],Env);
    Face .= faceType(Flds,Tps);
    conTract(Con,CTps,CDps) .= parseContractHead(BV,T,Env,Path);
    logMsg("contract head $(mkConType(Con,CTps,CDps))");
    ConTp .= reQ(BV,mkConType(Con,CTps,CDps));
    ConRlTp .= foldLeft(((_,QV),Rl)=>allRule(QV,Rl),contractExists(Con,CTps,CDps,Face),BV);
    valis ([conDef(Lc,Id,tpName(ConTp),ConRlTp)],
      [conDec(Lc,Id,tpName(ConTp),ConRlTp)])
  }
}
