star.compiler.typeparse{
  import star.
  import star.sort.
  
  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.dict.mgt.
  import star.compiler.meta.
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

  parseType(Q,Tp,Env) where (Lc,V,BT) ?= isQuantified(Tp) => valof{
    BV = parseBoundTpVars(V);
    In = parseType(Q++BV,BT,Env);
    valis reQ(BV,In)
  }
  parseType(Q,Tp,Env) where (Lc,V,BT) ?= isXQuantified(Tp) => valof{
    BV = parseBoundTpVars(V);
    In = parseType(Q++BV,BT,Env);
    valis reQX(BV,In)
  }
  parseType(Q,Tp,Env) where (Lc,C,B) ?= isConstrained(Tp) => valof{
    Cx = parseConstraints(C,Q,Env);
    Inn = parseType(Q,B,Env);
    valis wrapConstraints(Cx,Inn)
  }
  parseType(Q,Tp,Env) where (Lc,Nm) ?= isName(Tp) => valof{
    if Nm=="_" then
      valis newTypeVar("_")
    else if VTp ?= {! VTp | (Nm,VTp) in Q !} then
      valis VTp
    else if (_,T,TpRl,_) ?= findType(Env,Nm) then{
      if isLambdaRule(TpRl) then{
	(_,.typeLambda(_,Rhs)) = freshen(TpRl,Env);
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
  parseType(Q,Tp,Env) where (Lc,O,Args) ?= isSquareTerm(Tp) && (OLc,Nm)?=isName(O) => valof{
    ArgTps = parseTypes(Q,Args,Env);
    (Op,Rl) = parseTypeName(Q,OLc,Nm,Env);
    if TpRl ?= Rl then{
      if (_,FrshRl) .= freshen(TpRl,Env) && (_,OOp) .= freshen(Op,Env) then{
	valis applyTypeRule(Lc,FrshRl,doTypeFun(OOp,ArgTps),Env)
      } else{
	reportError("Could not freshen type rule $(Rl)",Lc);
	valis .voidType
      }
    } else {
      if (_,OOp) .= freshen(Op,Env) then {
	valis doTypeFun(OOp,ArgTps)
      }
      else{
	reportError("Could not freshen $(Op)",Lc);
	valis .voidType
      }
    }
  }
  parseType(Q,T,Env) where (Lc,Lhs,Rhs) ?= isFunctionType(T) => valof{
    A = parseArgType(Q,Lhs,Env);
    R = parseType(Q,Rhs,Env);
    valis fnType(A,R)
  }
  parseType(Q,T,Env) where (Lc,Lhs,Rhs) ?= isConstructorType(T) => valof{
    A = parseArgType(Q,Lhs,Env);
    R = parseType(Q,Rhs,Env);
    valis consType(A,R)
  }
  parseType(Q,T,Env) where (Lc,Lhs,Rhs) ?= isContinType(T) => valof{
    A = parseArgType(Q,Lhs,Env);
    R = parseType(Q,Rhs,Env);
    valis continType(A,R)
  }
  parseType(Q,T,Env) where (Lc,Rhs) ?= isRef(T) =>
    refType(parseType(Q,Rhs,Env)).
  parseType(Q,T,Env) where (Lc,[A]) ?= isTuple(T) => valof{
    if (_,As) ?= isTuple(A) then{
      ArgTps = parseTypes(Q,As,Env);
      valis .tupleType(ArgTps)
    } else
    valis parseType(Q,A,Env)
  }
  parseType(Q,T,Env) where (_,As) ?= isTuple(T) => valof{
    ArgTps = parseTypes(Q,As,Env);
    valis .tupleType(ArgTps)
  }
  parseType(Q,T,Env) where (Lc,A) ?= isBrTuple(T) => valof{
    (Flds,Tps) = parseTypeFields(Q,A,[],[],Env);
    valis .faceType(Flds,Tps)
  }
  parseType(Q,T,Env) where (Lc,Lhs,Fld) ?= isFieldAcc(T) => valof{
    if (_,Id) ?= isName(Lhs) then{
      if RcType ?= findVarFace(Id,Env) && .faceType(_,Tps).=deRef(RcType) then{
	if Ftp ?= {! Ftp | (Fld,Ftp) in Tps !} then
	  valis Ftp
	else{
	  reportError("Field $(Fld) not defined in type $(RcType)",Lc);
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
  parseType(Q,T,Env) where (Lc,Op,[L,R]) ?= isRoundTerm(T) =>
    parseType(Q,squareTerm(Lc,Op,[L,R]),Env).
  parseType(Q,T,Env) default => valof{
    reportError("cannot understand type $(T)",locOf(T));
    valis .voidType
  }

  parseArgType(Q,A,Env) where (_,As) ?= isTuple(A) => valof{
    Args = parseTypes(Q,As,Env);
    valis .tupleType(Args)
  }
  parseArgType(Q,A,Env) =>
    parseType(Q,A,Env).
    
  parseTypeArgs:(option[locn],tipes,cons[ast],dict) => (cons[tipe],cons[tipe]).
  parseTypeArgs(_,Q,[XX],Env) where (_,As,Ds)?=isDepends(XX) => valof{
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

  applyTypeRule:(option[locn],typeRule,tipe,dict) => tipe.
  applyTypeRule(_,.typeLambda(.tupleType([]),Tp),_,_) => Tp.
  applyTypeRule(Lc,.typeLambda(L,R),A,Env) => valof{
    if sameType(L,A,Env) then{
      valis R
    } else {
      reportError("Type rule $(.typeLambda(L,R)) does not apply to $(A)",Lc);
      valis .voidType
    }
  }

  doTypeFun:(tipe,cons[tipe]) => tipe.
  doTypeFun(Op,[A,..As]) =>
    doTypeFun(.tpExp(Op,A),As).
  doTypeFun(Tp,[]) => Tp.

  parseTypeFields:(tipes,cons[ast],tipes,tipes,dict) => (tipes,tipes).
  parseTypeFields(Q,[],Flds,Tps,_) => (Flds,Tps).
  parseTypeFields(Q,[A,..L],Flds,Tps,Env) where _ ?= isAnnotation(A) =>
    parseTypeFields(Q,L,Flds,Tps,Env).
  parseTypeFields(Q,[A,..L],Flds,Tps,Env) => valof{
    (FF,TT) = parseTypeField(Q,A,Flds,Tps,Env);
    valis parseTypeFields(Q,L,FF,TT,Env)
  }

  parseTypeField:(tipes,ast,tipes,tipes,dict) => (tipes,tipes).
  parseTypeField(Q,F,Flds,Tps,Env) where (_,Lhs,Rhs) ?= isTypeAnnotation(F) => valof{
    if (ILc,Nm) ?= isName(Lhs) then {
      FTp=parseType(Q,Rhs,Env);
      valis ([(Nm,FTp),..Flds],Tps)
    } else{
      reportError("invalid lhs -- $(Lhs) -- of type annotation",locOf(Lhs));
      valis ([],[])
    }
  }
  parseTypeField(Q,F,Flds,Tps,Env) where (Lc,Lhs,Rhs) ?= isTypeExists(F) => valof{
    if (ILc,Nm) ?= isName(Lhs) then {
      FTp=parseType(Q,Rhs,Env);
      valis (Flds,[(Nm,FTp),..Tps])
    } else{
      reportError("invalid lhs -- $(Lhs) -- of type annotation",locOf(Lhs)); -- 
      valis ([],[])
    }
  }.
  parseTypeField(Q,F,Flds,Tps,Env) => valof{
    reportError("invalid type field -- $(F)",locOf(F));
    valis ([],[])
  }.
	  
  parseTypeName(_,_,"_",_) => (newTypeVar("_"),.none).
  parseTypeName(Q,_,Nm,_) where Tp?={! T | (Nm,T) in Q !} => (Tp,.none).
  parseTypeName(Q,_,Nm,Env) where (_,T,TpRl,_) ?= findType(Env,Nm) => valof{
--    logMsg("type $(Nm) has type rule $(TpRl)");
    if isLambdaRule(TpRl) then 
      valis (T,.some(TpRl))
    else
    valis (T,.none)
  }
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
    
  parseBoundTpVar(Nm) where (_,Id) ?= isName(Nm) => (Id,.nomnal(Id)).
  parseBoundTpVar(FNm) where
      (_,Lhs,Rhs) ?= isBinary(FNm,"/") &&
      (_,Id) ?= isName(Lhs) &&
      (_,Ar) ?= isInt(Rhs) => (Id,.kFun(Id,Ar)).
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

  parseConstraint(A,Q,Env) where (Lc,Lh,Rh) ?= isBinary(A,"<~") => valof{
    Bnd = parseType(Q,Lh,Env);
    Face = parseType(Q,Rh,Env);
    if .faceType([(Fld,FTp)],_).=deRef(Face) then
      valis .hasField(Bnd,Fld,FTp)
    else{
      reportError("invalid rhs:$(Rh) of field constraint, expecting $(Lh)<~{F:T}",Lc);
      valis .hasField(.voidType,"",.voidType)
    }
  }
  parseConstraint(A,Q,Env) where (Lc,Id,T) ?= isImplicit(A) => valof{
    Tp = parseType(Q,T,Env);
    valis .implicit(Id,Tp)
  }
  parseConstraint(A,Q,Env) => parseContractConstraint(Q,A,Env).
  
  public rebind:(tipes,tipe,dict)=>tipe.
  rebind([],T,_) => T.
  rebind([(Nm,TV),..L],T,Env) where
      Ar ?= isUnboundFVar(TV) && sameType(TV,.kFun(Nm,Ar),Env) =>
    rebind(L,.allType(.kFun(Nm,Ar),T),Env).
  rebind([(Nm,TV),..L],T,Env) where sameType(TV,.nomnal(Nm),Env) =>
    rebind(L,.allType(.nomnal(Nm),T),Env).

  public wrapConstraints([],Tp)=>Tp.
  wrapConstraints([Cx,..Cs],Tp) => wrapConstraints(Cs,.constrainedType(Tp,Cx)).
    
  reQ:(tipes,tipe) => tipe.
  reQ([],Tp) => Tp.
  reQ([(_,KV),..T],Tp) where occursIn(deRef(KV),Tp) => reQ(T,.allType(KV,Tp)).
  reQ([(_,KV),..T],Tp) => reQ(T,Tp).

  reQX:(tipes,tipe) => tipe.
  reQX([],Tp) => Tp.
  reQX([(_,KV),..T],Tp) where occursIn(KV,Tp) => reQX(T,.existType(KV,Tp)).
  reQX([(_,KV),..T],Tp) => reQX(T,Tp).

  public parseContractConstraint:(tipes,ast,dict) => constraint.
  parseContractConstraint(Q,A,Env) where
      (Lc,O,As) ?= isSquareTerm(A) && (OLc,Nm) ?= isName(O) => valof{
	Cn = parseContractName(OLc,Nm,Env);
--	logMsg("contract name $(typeKey(Cn))");
	if [AAs].=As && (_,L,R) ?= isBinary(AAs,"->>") then{
	  Tps = parseTypes(Q,deComma(L),Env);
	  Dps = parseTypes(Q,deComma(R),Env);
	  valis .conTract(Cn,Tps,Dps)
	} else{
	  Tps = parseTypes(Q,As,Env);
	  valis .conTract(Cn,Tps,[])
	}
      }.
  parseContractConstraint(_,A,Env) => valof{
    reportError("$(A) is not a contract constraint",locOf(A));
    valis .conTract("",[],[])
  }.

  parseContractName:(option[locn],string,dict)=>string.
  parseContractName(Lc,Id,Env) => valof{
    if Con ?= findContract(Env,Id) &&
	.contractExists(Cn,_,_,_) .= snd(freshen(Con,Env)) then
      valis Cn
    else{
      reportError("contract $(Id) not defined",Lc);
      valis Id
    }
  }

  public parseAccessorContract:(tipes,ast,dict) => constraint.
  parseAccessorContract(Q,A,Env) where
      (Lc,O,As) ?= isSquareTerm(A) && (_,Nm) ?= isName(O) => valof{
	if [AAs].=As && (_,L,R) ?= isBinary(AAs,"->>") then{
	  Tps = parseTypes(Q,deComma(L),Env);
	  Dps = parseTypes(Q,deComma(R),Env);
	  valis .conTract(Nm,Tps,Dps)
	} else{
	  Tps = parseTypes(Q,As,Env);
	  valis .conTract(Nm,Tps,[])
	}
      }.
  parseAccessorContract(_,A,Env) => valof{
    reportError("$(A) is not a valid accessor contract",locOf(A));
    valis .conTract("void",[],[])
  }.

  parseTypes:(tipes,cons[ast],dict) => cons[tipe].
  parseTypes(_,[],_) => [].
  parseTypes(Q,[T,..L],Env) => valof{
    Tl = parseType(Q,T,Env);
    Tr = parseTypes(Q,L,Env);
    valis [Tl,..Tr]
  }

  pickTypeTemplate:(tipe) => tipe.
  pickTypeTemplate(.allType(_,Tp)) => pickTypeTemplate(Tp).
  pickTypeTemplate(.existType(_,Tp)) => pickTypeTemplate(Tp).
  pickTypeTemplate(.constrainedType(Hd,_)) => pickTypeTemplate(Hd).
  pickTypeTemplate(.nomnal(Hd)) => .nomnal(Hd).
  pickTypeTemplate(.tpFun(Nm,Ar)) => .tpFun(Nm,Ar).
  pickTypeTemplate(.kFun(Nm,Ar)) => .kFun(Nm,Ar).
  pickTypeTemplate(.tpExp(Op,_)) => pickTypeTemplate(Op).

  public parseTypeDef:(string,ast,dict,string) => (cons[canonDef],cons[decl]).
  parseTypeDef(Nm,St,Env,Path) where (Lc,V,C,H,B) ?= isTypeExistsStmt(St) => valof{
--    logMsg("parse type exists $(St)");
    Q = parseBoundTpVars(V);
    (Tp,_) = parseTypeHead(Q,H,Env,Path);
    Cx = parseConstraints(C,Q,Env);
    Tmplte = pickTypeTemplate(Tp);
    Fce = parseType(Q,B,declareType(Nm,Lc,Tmplte,.typeExists(Tp,.faceType([],[])),Env));
    TpRl = foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeExists(reConstrainType(Cx,Tp),Fce),Q);
    valis ([.typeDef(Lc,Nm,Tmplte,TpRl)],[.tpeDec(Lc,Nm,Tmplte,TpRl)])
  }
  parseTypeDef(Nm,St,Env,Path) where (Lc,V,C,H,B) ?= isTypeFunStmt(St) => valof{
--    logMsg("parse type fun $(St)");
    Q = parseBoundTpVars(V);
    (Tp,_) = parseTypeHead(Q,H,Env,Path);
    Cx = parseConstraints(C,Q,Env);
    RTp = parseType(Q,B,Env);
    
    Tmplte = pickTypeTemplate(Tp);
    TpRl = foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeLambda(reConstrainType(Cx,Tp),RTp),Q);

    valis ([.typeDef(Lc,Nm,Tmplte,TpRl)],[.tpeDec(Lc,Nm,Tmplte,TpRl)])
  }
  parseTypeDef(Nm,St,Env,Path) where (Lc,V,C,H,B) ?= isAlgebraicTypeStmt(St) =>
    parseAlgebraicType(Lc,Nm,V,C,H,B,Env,Path).
  parseTypeDef(_,St,_,_) => valof{
    reportError("invalid type definition: $(St)",locOf(St));
    valis ([],[])
  }

  public parseTypeCore:(ast,dict,string) => option[(tipe,typeRule)].
  parseTypeCore(St,Env,Path) where (Lc,V,C,H,B) ?= isTypeExistsStmt(St) => valof{
    Q = parseBoundTpVars(V);
    (Tp,TArgs) = parseTypeHead(Q,H,Env,Path);
    Cx = parseConstraints(C,Q,Env);
    Tmplte = pickTypeTemplate(Tp);
    valis .some((Tmplte,foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeExists(reConstrainType(Cx,Tp),.faceType([],[])),Q)))
  }
  parseTypeCore(St,Env,Path) where (Lc,V,C,H,B) ?= isTypeFunStmt(St) => valof{
    Q = parseBoundTpVars(V);
    (Tp,TArgs) = parseTypeHead(Q,H,Env,Path);
    Cx = parseConstraints(C,Q,Env);
    Tmplte = pickTypeTemplate(Tp);
    valis .some((Tmplte,foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeExists(reConstrainType(Cx,Tp),.faceType([],[])),Q)))
  }
  parseTypeCore(St,Env,Path) where (Lc,V,C,H,_) ?= isAlgebraicTypeStmt(St) => valof{
    Q = parseBoundTpVars(V);
    (Tp,TArgs) = parseTypeHead(Q,H,Env,Path);
    Cx = parseConstraints(C,Q,Env);
    Tmplte = pickTypeTemplate(Tp);
    valis .some((Tmplte,foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeExists(reConstrainType(Cx,Tp),.faceType([],[])),Q)))
  }
  parseTypeCore(St,Env,Path) default => .none.

  parseTypeHead:(tipes,ast,dict,string) => (tipe,cons[tipe]).
  parseTypeHead(Q,Tp,Env,Path) where (Lc,Nm) ?= isName(Tp) => 
    (.nomnal(qualifiedName(Path,.typeMark,Nm)),[]).
  parseTypeHead(Q,Tp,Env,Path) where
      (Lc,O,Args) ?= isSquareTerm(Tp) && (_,Nm) ?= isName(O) => valof{
	ArgTps = parseHeadArgs(Q,Args,[],Env);
	valis (mkTypeExp(.tpFun(qualifiedName(Path,.typeMark,Nm),size(ArgTps)),ArgTps),ArgTps)
      }.

  parseHeadArgs:(tipes,cons[ast],cons[tipe],dict) => cons[tipe].
  parseHeadArgs(Q,[],ArgTps,_) => reverse(ArgTps).
  parseHeadArgs(Q,[A,..As],Args,Env) where (_,Nm) ?= isName(A) =>
    parseHeadArgs(Q,As,[.nomnal(Nm),..Args],Env).
  parseHeadArgs(Q,[A,.._],_,_) => valof{
    reportError("invalid argument in type: $(A)",locOf(A));
    valis []
  }.
      
  public parseConstructor:(string,ast,dict,string) => (cons[canonDef],cons[decl]).
  parseConstructor(Nm,St,Env,Path) => valof{
    -- logMsg("parse constructor $(St)");
    Tp = parseType([],St,Env);
    Lc = locOf(St);
    FullNm = qualifiedName(Path,.conMark,Nm);
    valis ([],[.cnsDec(Lc,Nm,FullNm,Tp)])
  }

  parseContractHead:(tipes,ast,dict,string) => (string,cons[tipe],cons[tipe]).
  parseContractHead(Q,Tp,Env,Path) where
      (Lc,O,Args) ?= isSquareTerm(Tp) => valof{
	ConTp = parseType(Q,hashName(O),Env);
	if [A].=Args && (_,Lhs,Rhs)?=isBinary(A,"->>") then{
	  ArgTps = parseHeadArgs(Q,deComma(Lhs),[],Env);
	  DepTps = parseHeadArgs(Q,deComma(Rhs),[],Env);
	  valis (tpName(ConTp),ArgTps,DepTps)
	}
	else{
	  ArgTps = parseHeadArgs(Q,Args,[],Env);
	  valis (tpName(ConTp),ArgTps,[])
	}
      }.

  public parseContract:(ast,dict,string) => (cons[canonDef],cons[decl]).
  parseContract(St,Env,Path) where (Lc,Lhs,Els) ?= isContractStmt(St) &&
      (_,Q,C,Id,As,Ds) ?= isContractSpec(Lhs) => valof{
	Qv = parseBoundTpVars(Q);

	Cx = parseConstraints(C,Qv,Env);
	ArgTps = parseHeadArgs(Qv,As,[],Env);
	DepTps = parseHeadArgs(Qv,Ds,[],Env);
	(Flds,Tps) = parseTypeFields(Qv,Els,[],[],Env);
	Face = .faceType(sort(Flds,cmpField),sort(Tps,cmpField));
	FullNm = qualifiedName(Path,.typeMark,Id);

	ConRl = foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.contractExists(FullNm,ArgTps,DepTps,Face),Qv);
	ConDec = .conDec(Lc,Id,FullNm,ConRl);

	if traceCanon! then
	  logMsg("Contract decl $(ConDec)");

	DlId = dlrName(Id);
	DlTp = makeTpExp(FullNm,ArgTps++DepTps);
	
	TypeRl = foldLeft(((_,QV),Rl) => .allRule(QV,Rl),.typeExists(DlTp,Face),Qv);
	TpeDec = .tpeDec(Lc,Id,.tpFun(FullNm,[|ArgTps|]+[|DepTps|]),TypeRl);

	ConConTp = reQ(Qv,wrapConstraints(Cx,consType(.faceType(Flds,Tps),DlTp)));

	if traceCanon! then
	  logMsg("Contract vars $(Qv)");
	
	if traceCanon! then
	  logMsg("Contract type $(ConConTp)");

	ConFullNm = qualifiedName(Path,.typeMark,DlId);
	ConCns = .cnsDec(Lc,DlId,ConFullNm,ConConTp);
	
	(ConAccs,AccDecs) = buildAccessors(Flds,mkBrTerm(Lc,.nme(Lc,DlId),Els),Qv,Cx,DlTp,Env,Path);
	valis ([.cnsDef(Lc,ConFullNm,0,ConConTp),..ConAccs],
	  [TpeDec,ConDec,ConCns,..AccDecs])
      }.
  parseContract(St,_,_) => valof{
    reportError("invalid contract definition $(St)",locOf(St));
    valis ([],[])
  }

  cmpField:((string,tipe),(string,tipe))=>boolean.
  cmpField((F1,_),(F2,_)) => F1<F2.

  public parseAlgebraicType:(option[locn],string,cons[ast],cons[ast],ast,ast,dict,string) => (cons[canonDef],cons[decl]).
  parseAlgebraicType(Lc,Nm,V,C,H,B,Env,Path) => valof{
    Q = parseBoundTpVars(V);
    (Tp,_) = parseTypeHead(Q,H,Env,Path);
    Cx = parseConstraints(C,Q,Env);
    Tmplte = pickTypeTemplate(Tp);

    (Fs,Ts,Cs) = parseAlgebraicFace(B,Q,Env,Path);
    if traceCanon! then
      logMsg("Algebraic face $(Fs)");
    
    TpRl = foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeExists(reConstrainType(Cx,Tp),.faceType(Fs,Ts)),Q);

    Css = sort(Cs,((F1,_),(F2,_))=>F1<F2);
    CMap = foldLeft(((F,_),(M,Ix))=>(M[F->Ix],Ix+1),(([]:map[string,integer]),0),Css).0;

    if traceCanon! then
      logMsg("Constructor map $(CMap)");

    (CDefs,CDecs) = buildConstructors(B,CMap,Q,Cx,Tp,Env,Path);
    (ADefs,ADecs) = buildAccessors(Fs,B,Q,Cx,Tp,Env,Path);
    (UDefs,UDecs) = buildUpdaters(Fs,B,Q,Cx,Tp,Env,Path);

    TDef = .typeDef(Lc,Nm,Tmplte,TpRl);
    TDec = .tpeDec(Lc,Nm,Tmplte,TpRl);

    if traceCanon! then
      logMsg("Type dec $(TDec)");

    valis ([TDef,..CDefs]++ADefs++UDefs,[TDec,..CDecs]++ADecs++UDecs)
  }
  parseAlgebraicType(Lc,Nm,_,_,_,_,_,_) => valof{
    reportError("invalid type definition of $(Nm)",Lc);
    valis ([],[])
  }

  parseAlgebraicFace:(ast,tipes,dict,string) => (tipes,tipes,cons[(string,ast)]).
  parseAlgebraicFace(A,Qs,Env,Path) => 
    algebraicFace(A,Qs,[],[],Env,Path).

  algebraicFace:(ast,tipes,tipes,tipes,dict,string) => (tipes,tipes,cons[(string,ast)]).
  algebraicFace(A,Qs,Fs,Ts,Env,Path) where (Lc,L,R) ?= isBinary(A,"|") => valof{
    (F1,T1,C1) = algebraicFace(L,Qs,Fs,Ts,Env,Path);
    (F2,T2,C2) = algebraicFace(R,Qs,Fs,Ts,Env,Path);
    valis (combineTypes(F1,F2,Env,Lc),combineTypes(T1,T2,Env,Lc),C1++C2)
  }
  algebraicFace(A,_,Fs,Ts,Env,Path) where (Lc,Op,_) ?= isRoundTerm(A) && (_,Id)?=isName(Op) =>
    ([],[],[(Id,A)]).
  algebraicFace(A,_,Fs,Ts,Env,Path) where (Lc,Id) ?= isEnumSymb(A) => ([],[],[(Id,A)]).
  algebraicFace(A,_,Fs,Ts,Env,Path) where (Lc,Op,_) ?= isEnumCon(A) && (_,Id) ?= isName(Op) => ([],[],[(Id,A)]).
  algebraicFace(A,Qs,Fs,Ts,Env,Path) where (Lc,Op,Els) ?= isBrTerm(A) && (_,Id) ?= isName(Op) => valof{
    (F,T) = parseTypeFields(Qs,Els,Fs,Ts,Env);
    valis (F,T,[(Id,A)])
  }
  algebraicFace(A,Qs,Fs,Ts,Env,Path) where (_,V,B) ?= isQuantified(A) => valof{
    BV = parseBoundTpVars(V);
    (F,T,Cs) = algebraicFace(B,BV++Qs,Fs,Ts,Env,Path);
    if [(Id,_)].=Cs then
      valis (F//(((Fld,Ftp))=>(Fld,reQ(BV,Ftp))),T,[(Id,A)])
    else{
      reportError("invalid case in algebraic type (quantifier)",locOf(A));
      valis ([],[],[])
    }      
  }
  algebraicFace(A,Qs,Fs,Ts,Env,Path) where (_,V,B) ?= isXQuantified(A) => valof{
    BV = parseBoundTpVars(V);
    (F,T,Cs) = algebraicFace(B,BV++Qs,Fs,Ts,Env,Path);
    if [(Id,_)].=Cs then
      valis (F//(((Fld,Ftp))=>(Fld,reQX(BV,Ftp))),T,[(Id,A)])
    else{
      reportError("invalid case in algebraic type (quantifier)",locOf(A));
      valis ([],[],[])
    }      
  }
  algebraicFace(A,Qs,Fs,Ts,Env,Path) default => valof{
    reportError("invalid case in algebraic type",locOf(A));
    valis ([],[],[])
  }

  combineTypes([],F2,_,_) => F2.
  combineTypes(F1,[],_,_) => F1.
  combineTypes([(Id,Tp),..Fs],Gs,Env,Lc) => [(Id,Tp),..combineTypes(Fs,mergeField(Id,Tp,Gs,Env,Lc),Env,Lc)].

  mergeField(_,_,[],_,_) => [].
  mergeField(Id,Tp,[(Id,T2),..As],Env,Lc) => valof{
    if sameType(Tp,T2,Env) then
      valis As
    else{
      reportError("type of field $(Id)\:$(Tp) incompatible with $(T2)",Lc);
      valis As
    }
  }
  mergeField(Id,Tp,[F2,..As],Env,Lc) => [F2,..mergeField(Id,Tp,As,Env,Lc)].

  buildConstructors:(ast,map[string,integer],tipes,cons[constraint],tipe,dict,string)=> (cons[canonDef],cons[decl]).

  buildConstructors(A,Mp,Qs,Cx,Tp,Env,Path) where (Lc,L,R) ?= isBinary(A,"|") => valof{
    (Dfl,Dcl) = buildConstructors(L,Mp,Qs,Cx,Tp,Env,Path);
    (Dfr,Dcr) = buildConstructors(R,Mp,Qs,Cx,Tp,Env,Path);
    valis (Dfl++Dfr,Dcl++Dcr)
  }
  buildConstructors(A,Mp,Qs,Cx,Tp,Env,Path) =>
    buildConstructor(A,Mp,Qs,Cx,Tp,Env,Path).

  buildConstructor:(ast,map[string,integer],tipes,cons[constraint],tipe,dict,string)=> (cons[canonDef],cons[decl]).
  
  buildConstructor(A,Mp,Qs,Cx,Tp,Env,Path) where (Lc,O,Els) ?= isBrTerm(A) &&
      (_,Nm) ?= isName(O) => valof{
	(Flds,Tps) = parseTypeFields(Qs,Els,[],[],Env);
	ConNm = qualifiedName(Path,.conMark,Nm);

	ConTp = reQ(Qs,wrapConstraints(Cx,consType(.faceType(Flds,Tps),Tp)));
	if traceCanon! then
	  logMsg("constructor: $(A)\:$(ConTp)[$Ix]");
	if Ix?=Mp[Nm] then
	  valis ([.cnsDef(Lc,ConNm,Ix,ConTp)],[.cnsDec(Lc,Nm,ConNm,ConTp)])
	else{
	  reportError("(internal) cant find #(Nm) in $(Mp)",Lc);
	  valis ([],[])
	}
      }.
  buildConstructor(A,Mp,Qs,Cx,Tp,Env,Path) where (Lc,O,Args) ?= isEnumCon(A) && (_,Nm) ?= isName(O) && Ix?=Mp[Nm] => valof{
    ConNm = qualifiedName(Path,.conMark,Nm);
    
    ConTp = reQ(Qs,wrapConstraints(Cx,consType(.tupleType(parseTypes(Qs,Args,Env)),Tp)));
    valis ([.cnsDef(Lc,ConNm,Ix,ConTp)],[.cnsDec(Lc,Nm,ConNm,ConTp)])
  }.
  buildConstructor(A,Mp,Qs,Cx,Tp,Env,Path) where (Lc,Nm) ?= isEnumSymb(A) && Ix?=Mp[Nm] => valof{
    ConNm = qualifiedName(Path,.conMark,Nm);
    
    ConTp = reQ(Qs,wrapConstraints(Cx,enumType(Tp)));
    valis ([.cnsDef(Lc,ConNm,Ix,ConTp)],[.cnsDec(Lc,Nm,ConNm,ConTp)])
  }.
  buildConstructor(A,Mp,Qs,Cx,Tp,Env,Path) where (Lc,B,C) ?= isQuantified(A) => valof{
    BV = parseBoundTpVars(B);
    (Df,Dc) = buildConstructor(C,Mp,BV++Qs,Cx,Tp,Env,Path);
    if [.cnsDef(LLc,ConNm,Ix,ConTp)] .= Df && [.cnsDec(LLc2,Nm,ConNm,CTp)] .= Dc then{
      valis ([.cnsDef(LLc,ConNm,Ix,reQ(BV,ConTp))],[.cnsDec(LLc2,Nm,ConNm,reQ(BV,CTp))])
    } else{
      reportError("invalid constructor case $(A)",locOf(A));
      valis ([],[])
    }
  }
  buildConstructor(A,Mp,Qs,Cx,Tp,Env,Path) where (Lc,B,C) ?= isXQuantified(A) => valof{
    BV = parseBoundTpVars(B);
    (Df,Dc) = buildConstructor(C,Mp,BV++Qs,Cx,Tp,Env,Path);
    if [.cnsDef(LLc,ConNm,Ix,ConTp)] .= Df && [.cnsDec(LLc2,Nm,ConNm,CTp)] .= Dc then{
      CnTp = reQX(BV,ConTp);
      valis ([.cnsDef(LLc,ConNm,Ix,CnTp)],[.cnsDec(LLc2,Nm,ConNm,CnTp)])
    } else{
      reportError("invalid constructor case $(A)",locOf(A));
      valis ([],[])
    }
  }
  buildConstructor(A,_,_,_,_,_,_) => valof{
    reportError("invalid constructor case $(A)",locOf(A));
    valis ([],[])
  }

  buildAccessors:(tipes,ast,tipes,cons[constraint],tipe,dict,string)=>(cons[canonDef],cons[decl]).
  buildAccessors(Fields,B,Q,Cx,RcTp,Env,Path) => let{.
    makeAccessor:(string,tipe,ast)=> (cons[canonDef],cons[decl]).
    makeAccessor(Fld,FldTp,B) => valof{
      AccFnTp = reQ(Q,wrapConstraints(Cx,funType([RcTp],FldTp))); -- TODO: may result optional
      Lc = locOf(B);
--      DefltEqn = .rule(Lc,.tple(Lc,[.anon(Lc,RcTp)]),.none,.enm(Lc,"none",optType(FldTp)));
      AcEqs = accessorEqns(B,Fld,FldTp,[/*DefltEqn*/]);

      AccFnNm = qualifiedName(tpName(RcTp),.fldMark,Fld);
      
      Acc = .varDef(Lc,AccFnNm,.lambda(Lc,AccFnNm,AcEqs,AccFnTp),Cx,AccFnTp);
      AccDec = .accDec(Lc,RcTp,Fld,AccFnNm,AccFnTp);
      AccFnDec = .funDec(Lc,AccFnNm,AccFnNm,AccFnTp);
      valis ([Acc],[AccDec,AccFnDec])
    }
    makeAccessor(_,_,_) default => ([],[]).

    accessorEqns:(ast,string,tipe,cons[rule[canon]]) => cons[rule[canon]].
    accessorEqns(TB,Fld,FldTp,SoFar) where (Lc,L,R)?=isBinary(TB,"|") =>
      accessorEqns(R,Fld,FldTp,accessorEqns(L,Fld,FldTp,SoFar)).
    accessorEqns(TB,Fld,FldTp,SoFar) where (Lc,CN,Els)?=isBrTerm(TB) && (_,CnNm)?=isName(CN) && isFieldOfFc(Els,Fld) => valof{
      Sorted = sort(Els,compEls);
      XX = .vr(Lc,"X",FldTp);
      ConArgs = projectArgTypes(Sorted,0,(FLc,_,ATp) => .anon(FLc,ATp),XX,Fld,Fields);
      
      Eqn = rule(Lc,.tple(Lc,[
	    .apply(Lc,.enm(Lc,CnNm,consType(.tupleType(ConArgs//typeOf),RcTp)),ConArgs,RcTp)]),
	.none,XX);
      valis [Eqn,..SoFar]
    }.
    accessorEqns(TB,Fld,RcTp,Eqns) where (_,_,I) ?= isXQuantified(TB) =>
      accessorEqns(I,Fld,RcTp,Eqns).
    accessorEqns(_,_,_,Eqns) default => Eqns.
  .} in collapsePairs(Fields//((Fld,FTp))=>makeAccessor(Fld,FTp,B)).


  buildUpdaters:(tipes,ast,tipes,cons[constraint],tipe,dict,string)=>(cons[canonDef],cons[decl]).
  buildUpdaters(Fields,B,Q,Cx,RcTp,Env,Path) => let{.
    makeUpdater:(string,tipe,ast)=> (cons[canonDef],cons[decl]).
    makeUpdater(Fld,FldTp,B) => valof{
      UpdFnTp = reQ(Q,wrapConstraints(Cx,funType([RcTp,FldTp],RcTp)));
      Lc = locOf(B);
      AcEqs = updaterEqns(B,Fld,FldTp,[]);

      UpdFnNm = qualifiedName(tpName(RcTp),.overMark,Fld);

      Upd = .varDef(Lc,UpdFnNm,.lambda(Lc,UpdFnNm,AcEqs,UpdFnTp),Cx,UpdFnTp);
      UpdDec = .updDec(Lc,RcTp,Fld,UpdFnNm,UpdFnTp);
      UpdFnDec = .funDec(Lc,UpdFnNm,UpdFnNm,UpdFnTp);
      valis ([Upd],[UpdDec,UpdFnDec])
    }
    makeUpdater(_,_,_) default => ([],[]).

    updaterEqns:(ast,string,tipe,cons[rule[canon]]) => cons[rule[canon]].
    updaterEqns(TB,Fld,FldTp,SoFar) where (Lc,L,R)?=isBinary(TB,"|") =>
      updaterEqns(R,Fld,FldTp,updaterEqns(L,Fld,FldTp,SoFar)).
    updaterEqns(TB,Fld,FldTp,SoFar) where (Lc,CN,Els)?=isBrTerm(TB) && (_,CnNm)?=isName(CN) && isFieldOfFc(Els,Fld) => valof{
      Sorted = sort(Els,compEls);
      XX = .vr(Lc,"X",FldTp);
      ConArgs = projectArgTypes(Sorted,0,(FLc,Ix,FTp)=>.vr(FLc,"X$(Ix)",FTp),.anon(Lc,FldTp),Fld,Fields);
      RepArgs = projectArgTypes(Sorted,0,(FLc,Ix,FTp)=>.vr(FLc,"X$(Ix)",FTp),XX,Fld,Fields);
      ConsTp = consType(.tupleType(ConArgs//typeOf),RcTp);
      
      Eqn = rule(Lc,.tple(Lc,[.apply(Lc,.enm(Lc,CnNm,ConsTp),ConArgs,RcTp),XX]),
	.none,.apply(Lc,.enm(Lc,CnNm,ConsTp),RepArgs,RcTp));
      valis [Eqn,..SoFar]
    }.
    updaterEqns(TB,Fld,RcTp,Eqns) where (_,_,I) ?= isXQuantified(TB) =>
      updaterEqns(I,Fld,RcTp,Eqns).
    updaterEqns(_,_,_,Eqns) default => Eqns.
  .} in collapsePairs(Fields//((Fld,FTp))=>makeUpdater(Fld,FTp,B)).

  isFieldOfFc([F,..Els],Fld) where (_,Fld) ?= isTypeAnnot(F) => .true.
  isFieldOfFc([_,..Els],Fld) => isFieldOfFc(Els,Fld).
  isFieldOfFc([],_) default => .false.

  isTypeAnnot(A) where (Lc,V,_) ?= isTypeAnnotation(A) && (_,Id) ?= isName(V) => .some((Lc,Id)).
  isTypeAnnot(_) default => .none.

  projectArgTypes([],_,_,_,_,_) => [].
  projectArgTypes([A,..As],Ix,Fn,X,F,Fs) where (Lc,V) ?= isTypeAnnot(A) && F == V =>
    [X,..projectArgTypes(As,Ix+1,Fn,X,F,Fs)].
  projectArgTypes([A,..As],Ix,Fn,X,F,Fs) where (Lc,V) ?= isTypeAnnot(A) && Tp?=pickFldTp(V,Fs) =>
    [Fn(Lc,Ix,Tp),..projectArgTypes(As,Ix+1,Fn,X,F,Fs)].
  projectArgTypes([_,..As],Ix,Fn,X,F,Fs) => projectArgTypes(As,Ix,Fn,X,F,Fs).


  pickFldTp(Id,Tps) => {! Tp | (Id,Tp) in Tps !}.

  compEls:(ast,ast)=>boolean.
  compEls(E1,E2) where (_,K1) ?= isTypeAnnot(E1) && (_,K2) ?= isTypeAnnot(E2) => K1<K2.
  compEls(_,_) default => .false.
}
