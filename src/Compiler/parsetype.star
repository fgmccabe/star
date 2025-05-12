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
  rules ~> cons[(string,typeRule)].

  public parseType:(ast,dict) => tipe.
  parseType(Tp,Env) where (Lc,V,BT) ?= isQuantified(Tp) => valof{
    BV = parseBoundTpVars(V);
    QEnv = declareTypeVars(BV,Env);
    In = parseType(BT,QEnv);
    valis reQ(BV,In)
  }
  parseType(Tp,Env) where (Lc,V,BT) ?= isXQuantified(Tp) => valof{
    BV = parseBoundTpVars(V);
    QEnv = declareTypeVars(BV,Env);
    In = parseType(BT,QEnv);
    valis reQX(BV,In)
  }
  parseType(Tp,Env) where (Lc,C,B) ?= isConstrained(Tp) => valof{
    Cx = parseConstraints(C,Env);
    Inn = parseType(B,Env);
    valis wrapConstraints(Cx,Inn)
  }
  parseType(Tp,Env) where (Lc,Nm) ?= isName(Tp) => valof{
    if Nm=="_" then
      valis newTypeVar("_")
    else if Nm=="void" then
      valis .voidType
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
  parseType(Tp,Env) where (Lc,O,Args) ?= isSquareTerm(Tp) && (OLc,Nm)?=isName(O) => valof{
    ArgTps = parseTypes(Args,Env);
    (Op,Rl) = parseTypeName(OLc,Nm,Env);
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
  parseType(T,Env) where (Lc,Lhs,Rhs) ?= isFunctionType(T) => valof{
    A = parseArgType(Lhs,Env);
    R = parseType(Rhs,Env);
    valis fnType(A,R)
  }
  parseType(T,Env) where (Lc,Lhs,Rhs,Ehs) ?= isThrwFunctionType(T) => valof{
    A = parseArgType(Lhs,Env);
    R = parseType(Rhs,Env);
    E = parseType(Ehs,Env);
    valis throwingType(A,R,E)
  }
  parseType(T,Env) where (Lc,Lhs,Rhs) ?= isConstructorType(T) => valof{
    A = parseArgType(Lhs,Env);
    R = parseType(Rhs,Env);
    valis consType(A,R)
  }
  parseType(T,Env) where (Lc,Lhs,Rhs) ?= isFiberType(T) => valof{
    R = parseType(Lhs,Env);
    S = parseType(Rhs,Env);
    valis fiberType(R,S)
  }
  parseType(T,Env) where (Lc,Rhs) ?= isRef(T) =>
    refType(parseType(Rhs,Env)).
  parseType(T,Env) where (Lc,[A]) ?= isTuple(T) => valof{
    if (_,As) ?= isTuple(A) then{
      ArgTps = parseTypes(As,Env);
      valis .tupleType(ArgTps)
    } else
    valis parseType(A,Env)
  }
  parseType(T,Env) where (_,As) ?= isTuple(T) => valof{
    ArgTps = parseTypes(As,Env);
    valis .tupleType(ArgTps)
  }
  parseType(T,Env) where (Lc,A) ?= isBrTuple(T) => valof{
    (Flds,Tps) = parseTypeFields(A,[],[],Env);
    valis .faceType(Flds,Tps)
  }
  parseType(T,Env) where (Lc,Lhs,Fld) ?= isFieldAcc(T) => valof{
    if traceCanon! then
      showMsg("field access: $(T)");
    if (_,Id) ?= isName(Lhs) then{
      if traceCanon! then
	showMsg("record id : $(Id)");
      if RcType ?= findVarFace(Id,Env) && .faceType(_,Tps).=deRef(RcType) then{
	if traceCanon! then
	  showMsg("record type: $(RcType)");
	if Rl ?= {! Rl | (Fld,Rl) in Tps !} then{
	  if traceCanon! then
	    showMsg("record type rulw: $(Rl)");

	  if (_,FrshRl) .= freshen(Rl,Env) then{
	    if traceCanon! then
	      showMsg("freshened rule: $(FrshRl)");

	    valis applyTypeRule(Lc,FrshRl,.nomnal(Fld),Env)
	  } else{
	    reportError("Could not freshen type rule $(Rl)",Lc);
	    valis .voidType
	  }
	} else {
	  reportError("$(Id) not part of type $(RcType)",Lc);
	  valis .voidType
	}
      }
      else{
	reportError("Field $(Fld) not defined in $(Id) type",Lc);
	valis .voidType
      }
    } else{
      reportError("type access to non-var $(Lhs) not supported",Lc);
      valis .voidType
    }
  }
  parseType(T,Env) where (Lc,Op,[L,R]) ?= isRoundTerm(T) =>
    parseType(squareTerm(Lc,Op,[L,R]),Env).
  parseType(T,Env) default => valof{
    reportError("cannot understand type $(T)",locOf(T));
    valis .voidType
  }

  parseArgType(A,Env) where (_,As) ?= isTuple(A) => valof{
    Args = parseTypes(As,Env);
    valis .tupleType(Args)
  }
  parseArgType(A,Env) =>
    parseType(A,Env).
    
  parseTypeArgs:(option[locn],cons[ast],dict) => (cons[tipe],cons[tipe]).
  parseTypeArgs(_,[XX],Env) where (_,As,Ds)?=isDepends(XX) => valof{
    Lhs = parseTypes(As,Env);
    Rhs = parseTypes(Ds,Env);
    valis (Lhs,Rhs)
  }.
  parseTypeArgs(_,As,Env) => valof{
    ATps = parseTypes(As,Env);
    valis (ATps,[])
  }
  parseTypeArgs(Lc,As,Env) => valof{
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

  parseTypeFields:(cons[ast],tipes,rules,dict) => (tipes,rules).
  parseTypeFields([],Flds,Tps,_) => (Flds,Tps).
  parseTypeFields([A,..L],Flds,Tps,Env) where _ ?= isAnnotation(A) =>
    parseTypeFields(L,Flds,Tps,Env).
  parseTypeFields([A,..L],Flds,Tps,Env) => valof{
    (FF,TT) = parseTypeField(A,Flds,Tps,Env);
    valis parseTypeFields(L,FF,TT,Env)
  }

  parseTypeField:(ast,tipes,rules,dict) => (tipes,rules).
  parseTypeField(F,Flds,Tps,Env) where (_,Lhs,Rhs) ?= isTypeAnnotation(F) => valof{
    if (ILc,Nm) ?= isName(Lhs) then {
      FTp=parseType(Rhs,Env);
      valis ([(Nm,FTp),..Flds],Tps)
    } else{
      reportError("invalid lhs -- $(Lhs) -- of type annotation",locOf(Lhs));
      valis ([],[])
    }
  }
  parseTypeField(F,Flds,Tps,Env) where Rl ?= parseTypeFun(F,Env) =>
    (Flds,[(tpRuleName(Rl),Rl),..Tps]).
  parseTypeField(F,Flds,Tps,Env) => valof{
    reportError("invalid type declaration -- $(F)",locOf(F));
    valis ([],[])
  }.

  parseTypeFun(St,Env) where (Lc,H,B) ?= isTypeLambda(St) => valof{
    if traceCanon! then{
      showMsg("parse type fun $(St)")
    };
    
    (Tp,_) = parseTypeHead(H,Env,id);

    if traceCanon! then{
      showMsg("parse type $(B)")
    };
    
    RTp = parseType(B,Env);
    
    valis .some(.typeLambda(Tp,RTp))
  }
  parseTypeFun(St,Env) where (Lc,V,C,H,B) ?= isTypeExistsStmt(St) => valof{
    if traceCanon! then{
      showMsg("parse type exists $(St)")
    };

    Q = parseBoundTpVars(V);
    QEnv = declareTypeVars(Q,Env);
    (Tp,_) = parseTypeHead(H,QEnv,id);
    Cx = parseConstraints(C,Env);
    Fce = parseType(B,declareType(tpName(Tp),Lc,Tp,.typeExists(Tp,.faceType([],[])),QEnv));
    TpRl = foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeExists(reConstrainType(Cx,Tp),Fce),Q);
    valis .some(TpRl)
  }
  parseTypeFun(St,Env) where (Lc,V,B) ?= isQuantified(St) => valof{
    Q = parseBoundTpVars(V);
    QEnv = declareTypeVars(Q,Env);

    if Tp ?= parseTypeFun(B,QEnv) then{
      TpRl = foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),Tp,Q);
      valis .some(TpRl)
    } else
    valis .none
  }
  
  parseTypeFun(St,_) default => valof{
    reportError("cant parse $(St)",locOf(St));
    valis .none
  }
  
  parseTypeName(_,"_",_) => (.anonType,.none).
  parseTypeName(_,Nm,Env) where (_,T,TpRl,_) ?= findType(Env,Nm) => valof{
    if isLambdaRule(TpRl) then 
      valis (T,.some(TpRl))
    else
    valis (T,.none)
  }
  parseTypeName(Lc,Nm,Env) => valof{
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
    
  parseBoundTpVar(Nm) where (_,Id) ?= isName(Nm) => (Id,.kVar(Id)).
  parseBoundTpVar(FNm) where
      (_,Lhs,Rhs) ?= isBinary(FNm,"/") &&
      (_,Id) ?= isName(Lhs) &&
      (_,Ar) ?= isInt(Rhs) => (Id,.kFun(Id,Ar)).
  parseBoundTpVar(O) default => valof{
    reportError("invalid bound type variable $(O)",locOf(O));
    valis ("",.voidType)
  }

  public parseConstraints:(cons[ast],dict)=>cons[constraint].
  parseConstraints([],_) => [].
  parseConstraints([A,..As],Env) => valof{
    Cn = parseConstraint(A,Env);
    Cx = parseConstraints(As,Env);
    valis [Cn,..Cx]
  }

  parseConstraint(A,Env) where (Lc,Lh,Rh) ?= isBinary(A,"<~") => valof{
    Bnd = parseType(Lh,Env);
    Face = parseType(Rh,Env);
    if .faceType([(Fld,FTp)],_).=deRef(Face) then
      valis .hasField(Bnd,Fld,FTp)
    else{
      reportError("invalid rhs:$(Rh) of field constraint, expecting $(Lh)<~{F:T}",Lc);
      valis .hasField(.voidType,"",.voidType)
    }
  }
  parseConstraint(A,Env) where (Lc,Id,T) ?= isImplicit(A) => valof{
    Tp = parseType(T,Env);
    valis .implicit(Id,Tp)
  }
  parseConstraint(A,Env) => parseContractConstraint(A,Env).
  
  public rebind:(tipes,tipe,dict)=>tipe.
  rebind([],T,_) => T.
  rebind([(Nm,TV),..L],T,Env) where
      Ar ?= isUnboundFVar(TV) && sameType(TV,.kFun(Nm,Ar),Env) =>
    rebind(L,.allType(.kFun(Nm,Ar),T),Env).
  rebind([(Nm,TV),..L],T,Env) where sameType(TV,.kVar(Nm),Env) =>
    rebind(L,.allType(.kVar(Nm),T),Env).

  public wrapConstraints([],Tp)=>Tp.
  wrapConstraints([Cx,..Cs],Tp) => wrapConstraints(Cs,.constrainedType(Tp,Cx)).
    
  public reQ:all t ~~ reQuant[t] |: (tipes,t) => t.
  reQ(QV,X) => reQuant(QV//snd,X).

  public reQX:all t ~~ reQuant[t] |: (tipes,t) => t.
  reQX(QV,X) => reQuantX(QV//snd,X).

  public parseContractConstraint:(ast,dict) => constraint.
  parseContractConstraint(A,Env) where
      (Lc,O,As) ?= isSquareTerm(A) && (OLc,Nm) ?= isName(O) => valof{
	Cn = parseContractName(OLc,Nm,Env);
	if [AAs].=As && (_,L,R) ?= isBinary(AAs,"->>") then{
	  Tps = parseTypes(deComma(L),Env);
	  Dps = parseTypes(deComma(R),Env);
	  valis .conTract(Cn,Tps,Dps)
	} else{
	  Tps = parseTypes(As,Env);
	  valis .conTract(Cn,Tps,[])
	}
      }.
  parseContractConstraint(A,Env) => valof{
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

  parseTypes:(cons[ast],dict) => cons[tipe].
  parseTypes([],_) => [].
  parseTypes([T,..L],Env) => valof{
    Tl = parseType(T,Env);
    Tr = parseTypes(L,Env);
    valis [Tl,..Tr]
  }

  pickTypeTemplate:(tipe) => tipe.
  pickTypeTemplate(.allType(_,Tp)) => pickTypeTemplate(Tp).
  pickTypeTemplate(.existType(_,Tp)) => pickTypeTemplate(Tp).
  pickTypeTemplate(.constrainedType(Hd,_)) => pickTypeTemplate(Hd).
  pickTypeTemplate(.kVar(Nm)) => .kVar(Nm).
  pickTypeTemplate(.nomnal(Hd)) => .nomnal(Hd).
  pickTypeTemplate(.tpFun(Nm,Ar)) => .tpFun(Nm,Ar).
  pickTypeTemplate(.kFun(Nm,Ar)) => .kFun(Nm,Ar).
  pickTypeTemplate(.tpExp(Op,_)) => pickTypeTemplate(Op).

  public parseTypeDef:(string,ast,dict,string) => (cons[canonDef],cons[decl]).
  parseTypeDef(Nm,St,Env,Path) where (Lc,V,C,H,B) ?= isTypeExistsStmt(St) => valof{
    Q = parseBoundTpVars(V);
    QEnv = declareTypeVars(Q,Env);
    (Tp,_) = parseTypeHead(H,QEnv,(Nme)=>qualifiedName(Path,.typeMark,Nme));
    Cx = parseConstraints(C,QEnv);
    Tmplte = pickTypeTemplate(Tp);
    Fce = parseType(B,declareType(Nm,Lc,Tmplte,.typeExists(Tp,.faceType([],[])),QEnv));
    TpRl = foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeExists(reConstrainType(Cx,Tp),Fce),Q);
    valis ([.typeDef(Lc,Nm,Tmplte,TpRl)],[.tpeDec(Lc,Nm,Tmplte,TpRl)])
  }
  parseTypeDef(Nm,St,Env,Path) where (Lc,V,C,H,B) ?= isTypeFunStmt(St) => valof{
    Q = parseBoundTpVars(V);
    QEnv = declareTypeVars(Q,Env);
    (Tp,_) = parseTypeHead(H,QEnv,(Nme)=>qualifiedName(Path,.typeMark,Nme));
    Cx = parseConstraints(C,QEnv);
    RTp = parseType(B,QEnv);
    
    Tmplte = pickTypeTemplate(Tp);
    TpRl = foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeLambda(reConstrainType(Cx,Tp),RTp),Q);

    valis ([.typeDef(Lc,Nm,Tmplte,TpRl)],[.tpeDec(Lc,Nm,Tmplte,TpRl)])
  }
  parseTypeDef(Nm,St,Env,Path) where (Lc,V,C,H,B) ?= isAlgebraicTypeStmt(St) =>
    parseAlgebraicType(Lc,Nm,V,C,H,B,Env,Path).
  parseTypeDef(Nm,St,Env,Path) where (Lc,V,C,H,B) ?= isStructTypeStmt(St) =>
    parseStructType(Lc,Nm,V,C,H,B,Env,Path).
  parseTypeDef(_,St,_,_) => valof{
    reportError("invalid type definition: $(St)",locOf(St));
    valis ([],[])
  }

  public parseTypeCore:(ast,dict,string) => option[(tipe,typeRule)].
  parseTypeCore(St,Env,Path) where (Lc,V,C,H,B) ?= isTypeExistsStmt(St) => valof{
    Q = parseBoundTpVars(V);
    QEnv = declareTypeVars(Q,Env);
    (Tp,TArgs) = parseTypeHead(H,QEnv,(Nme)=>qualifiedName(Path,.typeMark,Nme));
    Cx = parseConstraints(C,QEnv);
    Tmplte = pickTypeTemplate(Tp);
    valis .some((Tmplte,foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeExists(reConstrainType(Cx,Tp),.faceType([],[])),Q)))
  }
  parseTypeCore(St,Env,Path) where (Lc,V,C,H,B) ?= isTypeFunStmt(St) => valof{
    Q = parseBoundTpVars(V);
    QEnv = declareTypeVars(Q,Env);
    (Tp,TArgs) = parseTypeHead(H,QEnv,(Nme)=>qualifiedName(Path,.typeMark,Nme));
    Cx = parseConstraints(C,QEnv);
    Tmplte = pickTypeTemplate(Tp);
    valis .some((Tmplte,foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeExists(reConstrainType(Cx,Tp),.faceType([],[])),Q)))
  }
  parseTypeCore(St,Env,Path) where (Lc,V,C,H,_) ?= isAlgebraicTypeStmt(St) => valof{
    Q = parseBoundTpVars(V);
    QEnv = declareTypeVars(Q,Env);
    (Tp,TArgs) = parseTypeHead(H,QEnv,(Nme)=>qualifiedName(Path,.typeMark,Nme));
    Cx = parseConstraints(C,QEnv);
    Tmplte = pickTypeTemplate(Tp);
    valis .some((Tmplte,foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeExists(reConstrainType(Cx,Tp),.faceType([],[])),Q)))
  }
  parseTypeCore(St,Env,Path) where (Lc,V,C,H,_) ?= isStructTypeStmt(St) => valof{
    Q = parseBoundTpVars(V);
    QEnv = declareTypeVars(Q,Env);
    (Tp,TArgs) = parseTypeHead(H,QEnv,(Nme)=>qualifiedName(Path,.typeMark,Nme));
    Cx = parseConstraints(C,QEnv);
    Tmplte = pickTypeTemplate(Tp);
    valis .some((Tmplte,foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeExists(reConstrainType(Cx,Tp),.faceType([],[])),Q)))
  }
  parseTypeCore(St,Env,Path) default => .none.

  parseTypeHead:(ast,dict,(string)=>string) => (tipe,cons[tipe]).
  parseTypeHead(Tp,Env,GenId) where (Lc,Nm) ?= isName(Tp) => 
    (.nomnal(GenId(Nm)),[]).
  parseTypeHead(Tp,Env,GenId) where
      (Lc,O,Args) ?= isSquareTerm(Tp) && (_,Nm) ?= isName(O) => valof{
	ArgTps = parseHeadArgs(Args,[],Env);
	valis (mkTypeExp(.tpFun(GenId(Nm),size(ArgTps)),ArgTps),ArgTps)
      }.

  parseHeadArgs:(cons[ast],cons[tipe],dict) => cons[tipe].
  parseHeadArgs([],ArgTps,_) => reverse(ArgTps).
  parseHeadArgs([A,..As],Args,Env) where (_,Nm) ?= isName(A) =>
    parseHeadArgs(As,[.kVar(Nm),..Args],Env).
  parseHeadArgs([A,.._],_,_) => valof{
    reportError("invalid argument in type: $(A)",locOf(A));
    valis []
  }.
      
  public parseConstructor:(string,ast,dict,string) => (cons[canonDef],cons[decl]).
  parseConstructor(Nm,St,Env,Path) => valof{
    Tp = parseType(St,Env);
    Lc = locOf(St);
    FullNm = qualifiedName(Path,.conMark,Nm);
    valis ([],[.cnsDec(Lc,Nm,FullNm,Tp)])
  }

  public parseContract:(ast,dict,string) => (cons[canonDef],cons[decl]).
  parseContract(St,Env,Path) where (Lc,Lhs,Els) ?= isContractStmt(St) &&
      (_,Q,C,Id,As,Ds) ?= isContractSpec(Lhs) => valof{
    Qv = parseBoundTpVars(Q);
    QEnv = declareTypeVars(Qv,Env);

    Cx = parseConstraints(C,QEnv);
    ArgTps = parseHeadArgs(As,[],QEnv);
    DepTps = parseHeadArgs(Ds,[],QEnv);
    (Flds,Tps) = parseTypeFields(Els,[],[],QEnv);
    Face = .faceType(sort(Flds,cmpField),Tps);
    FullNm = qualifiedName(Path,.typeMark,Id);
    
    ConRl = foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.contractExists(FullNm,ArgTps,DepTps,Face),Qv);
    ConDec = .conDec(Lc,Id,FullNm,ConRl);
    
    DlId = dlrName(Id);
    DlTp = makeTpExp(FullNm,ArgTps++DepTps);
    
    TypeRl = foldLeft(((_,QV),Rl) => .allRule(QV,Rl),.typeExists(DlTp,Face),Qv);
    Tmplte = .tpFun(FullNm,[|ArgTps|]+[|DepTps|]);
    TpeDec = .tpeDec(Lc,Id,Tmplte,TypeRl);
    TDef = .typeDef(Lc,Id,Tmplte,TypeRl);
    
    ConConTp = reQ(Qv,wrapConstraints(Cx,consType(.faceType(Flds,Tps),DlTp)));
    
    ConFullNm = qualifiedName(Path,.typeMark,DlId);
    ConCns = .cnsDec(Lc,DlId,ConFullNm,ConConTp);
    
    (ConAccs,AccDecs) = buildAccessors(Flds,mkBrTerm(Lc,.nme(Lc,DlId),Els),Qv,Cx,DlTp,Path);
    valis ([TDef,.cnsDef(Lc,ConFullNm,0,ConConTp),..ConAccs], [TpeDec,ConDec,ConCns,..AccDecs])
      }.
  parseContract(St,_,_) => valof{
    reportError("invalid contract definition $(St)",locOf(St));
    valis ([],[])
  }

  cmpField:((string,tipe),(string,tipe))=>boolean.
  cmpField((F1,_),(F2,_)) => F1<F2.

  public parseAlgebraicType:(option[locn],string,cons[ast],cons[ast],ast,ast,dict,string) => (cons[canonDef],cons[decl]).
  parseAlgebraicType(Lc,Nm,V,C,H,B,Env,Path) => valof{
    if traceCanon! then
      showMsg("parse algebraic type defn at $(Lc)");
    
    Q = parseBoundTpVars(V);
    if traceCanon! then
      showMsg("bound vars $(Q)");
    QEnv = declareTypeVars(Q,Env);

    (Tp,_) = parseTypeHead(H,QEnv,(Nme)=>qualifiedName(Path,.typeMark,Nme));
    Cx = parseConstraints(C,QEnv);
    Tmplte = pickTypeTemplate(Tp);

    Cs = collectConstructors(B);

    if traceCanon! then
      showMsg("Cs=$(Cs)");
    
    TpRl = foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeExists(reConstrainType(Cx,Tp),
	.faceType([],[])),Q);

    if traceCanon! then
      showMsg("type rule $(TpRl)");

    Css = sort(Cs,((F1,_),(F2,_))=>F1<F2);
    CMap = foldLeft(((F,_),(M,Ix))=>(M[F->Ix],Ix+1),(([]:map[string,integer]),0),Css).0;

    if traceCanon! then
      showMsg("algebraic CMP $(CMap)");

    (CDefs,CDecs) = buildConstructors(B,CMap,Cx,Tp,QEnv,Path);

    TDef = .typeDef(Lc,Nm,Tmplte,TpRl);
    TDec = .tpeDec(Lc,Nm,Tmplte,TpRl);

    Qs = Q//snd;

    valis ([TDef,..(CDefs//(D)=>reQuant(Qs,D))],
      [TDec,..(CDecs//(D)=>reQuant(Qs,D))])
  }
  parseAlgebraicType(Lc,Nm,_,_,_,_,_,_) => valof{
    reportError("invalid type definition of $(Nm)",Lc);
    valis ([],[])
  }

  public parseStructType:(option[locn],string,cons[ast],cons[ast],ast,ast,dict,string) => (cons[canonDef],cons[decl]).
  parseStructType(Lc,Nm,V,C,H,B,Env,Path) => valof{
    if traceCanon! then
      showMsg("parse struct type defn at $(Lc)");
    
    Q = parseBoundTpVars(V);
    if traceCanon! then
      showMsg("bound vars $(Q)");
    QEnv = declareTypeVars(Q,Env);

    (Tp,_) = parseTypeHead(H,QEnv,(Nme)=>qualifiedName(Path,.typeMark,Nme));
    Cx = parseConstraints(C,QEnv);
    Tmplte = pickTypeTemplate(Tp);

    (Fs,Xs,Ts,Cs) = parseAlgebraicFace(B,QEnv,Path);
    
    TpRl = foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeExists(reConstrainType(Cx,Tp),
	foldLeft(((_,XV),F)=>.existType(XV,F),.faceType(Fs,Ts),Xs)),Q);

    if traceCanon! then
      showMsg("type rule $(TpRl)");

    Css = sort(Cs,((F1,_),(F2,_))=>F1<F2);
    CMap = foldLeft(((F,_),(M,Ix))=>(M[F->Ix],Ix+1),(([]:map[string,integer]),0),Css).0;

    (CDefs,CDecs) = buildConstructors(B,CMap,Cx,Tp,QEnv,Path);
    (ADefs,ADecs) = buildAccessors(Fs,B,Q,Cx,Tp,Path);
    (UDefs,UDecs) = buildUpdaters(Fs,B,Q,Cx,Tp,Path);

    TDef = .typeDef(Lc,Nm,Tmplte,TpRl);
    TDec = .tpeDec(Lc,Nm,Tmplte,TpRl);

    Qs = Q//snd;

    valis ([TDef,..(CDefs//(D)=>reQuant(Qs,D))]++ADefs++UDefs,
      [TDec,..(CDecs//(D)=>reQuant(Qs,D))]++ADecs++UDecs)
  }
  parseStructType(Lc,Nm,_,_,_,_,_,_) => valof{
    reportError("invalid type definition of $(Nm)",Lc);
    valis ([],[])
  }

  collectConstructors:(ast)=>cons[(string,ast)].
  collectConstructors(A) where (Lc,L,R) ?= isBinary(A,"|") =>
    collectConstructors(L) ++ collectConstructors(R).
  collectConstructors(A) where (Lc,R) ?= isUnary(A,"|") => 
    collectConstructors(R).
  collectConstructors(A) where (Lc,Op,_) ?= isRoundTerm(A) && (_,Id)?=isName(Op) =>
    [(Id,A)].
  collectConstructors(A) where (Lc,Id) ?= isEnumSymb(A) => [(Id,A)].
  collectConstructors(A) where (Lc,Op,_) ?= isEnumCon(A) && (_,Id) ?= isName(Op) => [(Id,A)].
  collectConstructors(A) where (_,_,B) ?= isQuantified(A) =>
    collectConstructors(B).
  collectConstructors(A) where (_,_,B) ?= isXQuantified(A) =>
    collectConstructors(B).
  collectConstructors(A) default => valof{
    reportError("invalid case in algebraic type",locOf(A));
    valis []
  }

  parseAlgebraicFace:(ast,dict,string)=>(tipes,tipes,rules,cons[(string,ast)]).
  parseAlgebraicFace(A,Env,Path) => 
    algebraicFace(A,[],[],Env,Path).

  algebraicFace:(ast,tipes,rules,dict,string) => (tipes,tipes,rules,cons[(string,ast)]).
  algebraicFace(A,Fs,Ts,Env,Path) where (Lc,L,R) ?= isBinary(A,"|") => valof{
    (F1,X1,T1,C1) = algebraicFace(L,Fs,Ts,Env,Path);
    (F2,X2,T2,C2) = algebraicFace(R,Fs,Ts,Env,Path);
    valis (combineTypes(F1,F2,Env,Lc),combineTypes(X1,X2,Env,Lc),combineTypeRules(T1,T2,Env,Lc),C1++C2)
  }
  algebraicFace(A,Fs,Ts,Env,Path) where (Lc,R) ?= isUnary(A,"|") => 
    algebraicFace(R,Fs,Ts,Env,Path).
  algebraicFace(A,Fs,Ts,Env,Path) where (Lc,Op,_) ?= isRoundTerm(A) && (_,Id)?=isName(Op) =>
    ([],[],[],[(Id,A)]).
  algebraicFace(A,Fs,Ts,Env,Path) where (Lc,Id) ?= isEnumSymb(A) => ([],[],[],[(Id,A)]).
  algebraicFace(A,Fs,Ts,Env,Path) where (Lc,Op,_) ?= isEnumCon(A) && (_,Id) ?= isName(Op) => ([],[],[],[(Id,A)]).
  algebraicFace(A,Fs,Ts,Env,Path) where (Lc,Op,Els) ?= isBrTerm(A) && (_,Id) ?= isName(Op) => valof{
    (F,T) = parseTypeFields(Els,Fs,Ts,Env);
    valis (F,[],T,[(Id,A)])
  }
  algebraicFace(A,Fs,Ts,Env,Path) where (_,V,B) ?= isQuantified(A) => valof{
    BV = parseBoundTpVars(V);
    QEnv = declareTypeVars(BV,Env);
    (F,Xs,T,Cs) = algebraicFace(B,Fs,Ts,QEnv,Path);
    if [(Id,_)].=Cs then
      valis (F//(((Fld,Ftp))=>(Fld,reQ(BV,Ftp))),Xs,T,[(Id,A)])
    else{
      reportError("invalid case in algebraic type (quantifier)",locOf(A));
      valis ([],[],[],[])
    }      
  }
  algebraicFace(A,Fs,Ts,Env,Path) where (_,V,B) ?= isXQuantified(A) => valof{
    BV = parseBoundTpVars(V);
    QEnv = declareTypeVars(BV,Env);

    (F,_,T,Cs) = algebraicFace(B,Fs,Ts,QEnv,Path);
    if [(Id,_)].=Cs then
      valis (F//(((Fld,Ftp))=>(Fld,reQX(BV,Ftp))),BV,T,[(Id,A)])
    else{
      reportError("invalid case in algebraic type (quantifier)",locOf(A));
      valis ([],[],[],[])
    }      
  }
  algebraicFace(A,Fs,Ts,Env,Path) default => valof{
    reportError("invalid case in algebraic type",locOf(A));
    valis ([],[],[],[])
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

  combineTypeRules([],F2,_,_) => F2.
  combineTypeRules(F1,[],_,_) => F1.
  combineTypeRules([(Nm,Rl),..Fs],Gs,Env,Lc) =>
    [(Nm,Rl),..combineTypeRules(Fs,mergeType(Nm,Rl,Gs,Env,Lc),Env,Lc)].

  mergeType(_,_,[],_,_) => [].
  mergeType(Id,Rl,[(RNm,Rl2),..As],Env,Lc) => valof{
    if RNm == Id then{
      if Rl==Rl2 then
	valis As
      else{
	reportError("typerule $(Rl) not identical to $(Rl2)",Lc);
	valis As
      }
    }
    else
    valis [(RNm,Rl2),..mergeType(Id,Rl,As,Env,Lc)]
  }

  buildConstructors:(ast,map[string,integer],cons[constraint],tipe,dict,string)=> (cons[canonDef],cons[decl]).

  buildConstructors(A,Mp,Cx,Tp,Env,Path) where (Lc,L,R) ?= isBinary(A,"|") => valof{
    (Dfl,Dcl) = buildConstructors(L,Mp,Cx,Tp,Env,Path);
    (Dfr,Dcr) = buildConstructors(R,Mp,Cx,Tp,Env,Path);
    valis (Dfl++Dfr,Dcl++Dcr)
  }
  buildConstructors(A,Mp,Cx,Tp,Env,Path) where (Lc,R) ?= isUnary(A,"|") =>
    buildConstructors(R,Mp,Cx,Tp,Env,Path).
  buildConstructors(A,Mp,Cx,Tp,Env,Path) =>
    buildConstructor(A,Mp,Cx,Tp,Env,Path).

  buildConstructor:(ast,map[string,integer],cons[constraint],tipe,dict,string)=> (cons[canonDef],cons[decl]).
  
  buildConstructor(A,Mp,Cx,Tp,Env,Path) where (Lc,O,Els) ?= isBrTerm(A) &&
      (_,Nm) ?= isName(O) => valof{
	(Flds,Tps) = parseTypeFields(Els,[],[],Env);
	ConNm = qualifiedName(Path,.conMark,Nm);

	ConTp = wrapConstraints(Cx,consType(.faceType(Flds,Tps),Tp));
	if Ix?=Mp[Nm] then
	  valis ([.cnsDef(Lc,ConNm,Ix,ConTp)],[.cnsDec(Lc,Nm,ConNm,ConTp)])
	else{
	  reportError("(internal) cant find #(Nm) in $(Mp)",Lc);
	  valis ([],[])
	}
      }.
  buildConstructor(A,Mp,Cx,Tp,Env,Path)
      where (Lc,O,Args) ?= isEnumCon(A) && (_,Nm) ?= isName(O) && Ix?=Mp[Nm] => valof{
    ConNm = qualifiedName(Path,.conMark,Nm);
    
    ConTp = wrapConstraints(Cx,consType(.tupleType(parseTypes(Args,Env)),Tp));
    valis ([.cnsDef(Lc,ConNm,Ix,ConTp)],[.cnsDec(Lc,Nm,ConNm,ConTp)])
  }.
  buildConstructor(A,Mp,Cx,Tp,Env,Path)
      where (Lc,Nm) ?= isEnumSymb(A) && Ix?=Mp[Nm] => valof{
    ConNm = qualifiedName(Path,.conMark,Nm);
    
    ConTp = wrapConstraints(Cx,enumType(Tp));
    valis ([.cnsDef(Lc,ConNm,Ix,ConTp)],[.cnsDec(Lc,Nm,ConNm,ConTp)])
  }.
  buildConstructor(A,Mp,Cx,Tp,Env,Path) where (Lc,B,C) ?= isQuantified(A) => valof{
    BV = parseBoundTpVars(B);
    QEnv = declareTypeVars(BV,Env);
    (Df,Dc) = buildConstructor(C,Mp,Cx,Tp,QEnv,Path);

    
    if [.cnsDef(LLc,ConNm,Ix,ConTp)] .= Df && [.cnsDec(LLc2,Nm,ConNm,CTp)] .= Dc then{
      valis ([.cnsDef(LLc,ConNm,Ix,reQ(BV,ConTp))],[.cnsDec(LLc2,Nm,ConNm,reQ(BV,CTp))])
    } else{
      reportError("invalid constructor case $(A)",locOf(A));
      valis ([],[])
    }
  }
  buildConstructor(A,Mp,Cx,Tp,Env,Path) where (Lc,B,C) ?= isXQuantified(A) => valof{
    BV = parseBoundTpVars(B);
    XEnv = declareTypeVars(BV,Env);
    (Df,Dc) = buildConstructor(C,Mp,Cx,Tp,XEnv,Path);
    if [.cnsDef(LLc,ConNm,Ix,ConTp)] .= Df && [.cnsDec(LLc2,Nm,ConNm,CTp)] .= Dc then{

    if traceCanon! then
      showMsg("X vars in constructor $(BV)");
      
      CnTp = reQX(BV,ConTp);
      if traceCanon! then
	showMsg("reconstructed $(ConTp) is $(CnTp)");

      valis ([.cnsDef(LLc,ConNm,Ix,CnTp)],[.cnsDec(LLc2,Nm,ConNm,CnTp)])
    } else{
      reportError("invalid constructor case $(A)",locOf(A));
      valis ([],[])
    }
  }
  buildConstructor(A,_,_,_,_,_) => valof{
    reportError("invalid constructor case $(A)",locOf(A));
    valis ([],[])
  }

  buildAccessors:(tipes,ast,tipes,cons[constraint],tipe,string)=>(cons[canonDef],cons[decl]).
  buildAccessors(Fields,B,Q,Cx,RcTp,Path) => let{.
    -- TODO: make result optional
    
    makeAccessor:(string,tipe,ast)=> (cons[canonDef],cons[decl]).
    makeAccessor(Fld,FldTp,B) => valof{
      (XQ,FTp) = deQuantX(FldTp); -- special rule for existentials
      AccFnTp = reQ(Q,reQuant(XQ,wrapConstraints(Cx,funType([RcTp],FTp))));
      Lc = locOf(B);
--      DefltEqn = .rule(Lc,.tple(Lc,[.anon(Lc,RcTp)]),.none,.enm(Lc,"none",optType(FldTp)));
      AcEqs = accessorEqns(B,Fld,FTp,[/*DefltEqn*/]);

      if isEmpty(AcEqs) then
	reportWarning("no accessor for $(Fld)",locOf(B));

      AccFnNm = qualifiedName(tpName(RcTp),.fldMark,Fld);
      
      Acc = .funDef(Lc,AccFnNm,AcEqs,Cx,AccFnTp);
      AccDec = .accDec(Lc,RcTp,Fld,AccFnNm,AccFnTp);
      AccFnDec = .funDec(Lc,AccFnNm,AccFnNm,AccFnTp);
      valis ([Acc],[AccDec,AccFnDec])
    }
    makeAccessor(_,_,_) default => ([],[]).

    accessorEqns:(ast,string,tipe,cons[rule[canon]]) => cons[rule[canon]].
    accessorEqns(TB,Fld,FldTp,SoFar) where (_,L,R)?=isBinary(TB,"|") =>
      accessorEqns(R,Fld,FldTp,accessorEqns(L,Fld,FldTp,SoFar)).
    accessorEqns(TB,Fld,FldTp,SoFar) where (_,R)?=isUnary(TB,"|") =>
      accessorEqns(R,Fld,FldTp,SoFar).
    accessorEqns(TB,Fld,FldTp,SoFar) where (Lc,CN,Els)?=isBrTerm(TB) && (_,CnNm)?=isName(CN) && isFieldOfFc(Els,Fld) => valof{
      Sorted = sort(Els,compEls);
      XX = .vr(Lc,"X",FldTp);
      ConArgs = projectArgTypes(Sorted,0,(FLc,_,ATp) => .anon(FLc,ATp),XX,Fld,Fields);
      
      Eqn = .rule(Lc,.tple(Lc,[
	    .apply(Lc,.enm(Lc,CnNm,consType(.tupleType(ConArgs//typeOf),RcTp)),ConArgs,RcTp)]),
	.none,XX);
      valis [Eqn,..SoFar]
    }.
    accessorEqns(TB,Fld,FldTp,Eqns) where (_,_,I) ?= isXQuantified(TB) =>
      accessorEqns(I,Fld,FldTp,Eqns).
    accessorEqns(_,_,_,Eqns) default => Eqns.
  .} in collapsePairs(Fields//((Fld,FTp))=>makeAccessor(Fld,FTp,B)).


  buildUpdaters:(tipes,ast,tipes,cons[constraint],tipe,string)=>(cons[canonDef],cons[decl]).
  buildUpdaters(Fields,B,Q,Cx,RcTp,Path) => let{.
    makeUpdater:(string,tipe,ast)=> (cons[canonDef],cons[decl]).
    makeUpdater(Fld,FldTp,B) => valof{
      (XQ,ITp) = deQuantX(FldTp); -- special rule for existentials
      UpdFnTp = reQ(Q, reQuant(XQ,wrapConstraints(Cx,funType([RcTp,ITp],RcTp))));
      Lc = locOf(B);
      AcEqs = updaterEqns(B,Fld,FldTp,[]);

      if isEmpty(AcEqs) then
	reportWarning("no updater for $(Fld)",locOf(B));

      UpdFnNm = qualifiedName(tpName(RcTp),.overMark,Fld);

      Upd = .funDef(Lc,UpdFnNm,AcEqs,Cx,UpdFnTp);
      UpdDec = .updDec(Lc,RcTp,Fld,UpdFnNm,UpdFnTp);
      UpdFnDec = .funDec(Lc,UpdFnNm,UpdFnNm,UpdFnTp);
      valis ([Upd],[UpdDec,UpdFnDec])
    }
    makeUpdater(_,_,_) default => ([],[]).

    updaterEqns:(ast,string,tipe,cons[rule[canon]]) => cons[rule[canon]].
    updaterEqns(TB,Fld,FldTp,SoFar) where (Lc,L,R)?=isBinary(TB,"|") =>
      updaterEqns(R,Fld,FldTp,updaterEqns(L,Fld,FldTp,SoFar)).
    updaterEqns(TB,Fld,FldTp,SoFar) where (Lc,R)?=isUnary(TB,"|") =>
      updaterEqns(R,Fld,FldTp,SoFar).
    updaterEqns(TB,Fld,FldTp,SoFar) where (Lc,CN,Els)?=isBrTerm(TB) && (_,CnNm)?=isName(CN) && isFieldOfFc(Els,Fld) => valof{
      Sorted = sort(Els,compEls);
      XX = .vr(Lc,"X",FldTp);
      ConArgs = projectArgTypes(Sorted,0,(FLc,Ix,FTp)=>.vr(FLc,"X$(Ix)",FTp),.anon(Lc,FldTp),Fld,Fields);
      RepArgs = projectArgTypes(Sorted,0,(FLc,Ix,FTp)=>.vr(FLc,"X$(Ix)",FTp),XX,Fld,Fields);
      ConsTp = consType(.tupleType(ConArgs//typeOf),RcTp);
      
      Eqn = .rule(Lc,.tple(Lc,[.apply(Lc,.enm(Lc,CnNm,ConsTp),ConArgs,RcTp),XX]),
	.none,.apply(Lc,.enm(Lc,CnNm,ConsTp),RepArgs,RcTp));
      valis [Eqn,..SoFar]
    }.
    updaterEqns(TB,Fld,FldTp,Eqns) where (_,_,I) ?= isXQuantified(TB) =>
      updaterEqns(I,Fld,FldTp,Eqns).
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

  implementation all t ~~ reQuant[t] |: reQuant[cons[t]] => let{.
    reQ:(cons[tipe],cons[t])=>cons[t].
    reQ(Qs,[]) => [].
    reQ(Qs,[X,..Xs]) => [reQuant(Qs,X),..reQ(Qs,Xs)].
    reX(Qs,[]) => [].
    reX(Qs,[X,..Xs]) => [reQuantX(Qs,X),..reX(Qs,Xs)].
  .} in {
  reQuant(Qs,L) => reQ(Qs,L).
  reQuantX(Qs,L) => reX(Qs,L)
  }
}
