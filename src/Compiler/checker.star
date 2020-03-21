star.compiler.checker{
  import star.

  import star.pkg.
  import star.repo.

  import star.compiler.action.
  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.canondeps.
  import star.compiler.dependencies.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.freshen.
  import star.compiler.impawt.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.resolve.
  import star.compiler.terms.
  import star.compiler.types.
  import star.compiler.typeparse.
  import star.compiler.unify.
  import star.compiler.wff.

  -- package level of type checker

  public checkPkg:all r ~~ repo[r],display[r]|:(r,pkg,ast,dict,reports) => either[reports,(pkgSpec,canonDef)].
  checkPkg(Repo,Pkge,P,Base,Rp) => do{
    if (Lc,Pk,Els) ^= isBrTerm(P) && either(Pkg) .= pkgeName(Pk) then{
      (Imports,Stmts) <- collectImports(Els,[],[],Rp);
      (PkgEnv,AllImports,PkgVars) <- importAll(Imports,Repo,Base,[],[],Rp);
--      logMsg("imports found $(AllImports), package vars = $(PkgVars)");
--      logMsg("pkg env after imports $(PkgEnv)");
      
      PkgNm .= packageName(Pkg);
      -- We treat a package specially, buts its essentially a theta record
      (Vis,Opens,Annots,Gps) <- dependencies(Stmts,Rp);
--      logMsg("Package $(Pkg), groups: $(Gps)");

      TEnv <- checkTypeGroups(Gps,PkgEnv,PkgNm,Rp);
      
      (Defs,ThEnv) <- checkGroups(Gps,[],faceType([],[]),Annots,TEnv,PkgNm,Rp);
      if [Open,.._] .= Opens then
	throw reportError(Rp,"open statement $(Open) not permitted in package",locOf(Open));

--      logMsg("Final Pkg dict $(ThEnv)");
--      logMsg("Public names: $(Vis)");
--      logMsg("Defs: $(Defs)");
      Contracts .= [ D | DD in Defs && D in DD && conDef(_,Nm,_,_).=D &&
	    (conSp(Nm), .pUblic) in Vis];
--      logMsg("exported contracts: $(Contracts)");
      Fields .= exportedFields(Defs,Vis,.pUblic);
--      logMsg("exported fields: $(Fields)");
      Impls .= [ implSpec(some(ILc),INm,FllNm,ITp) |
	  DD in Defs &&
	      implDef(ILc,INm,FllNm,_,_,ITp) in DD &&
	      (implSp(INm),V) in Vis && V>=.pUblic];
--      logMsg("exported implementations $(Impls)");
      Types .= exportedTypes(Defs,Vis,.pUblic);
--      logMsg("exported types: $(Types)");
      RDefs <- overloadEnvironment(Defs,ThEnv,Rp);
      PkgType .= faceType(Fields,Types);
      PkgTheta <- makePkgTheta(Lc,PkgNm,PkgType,ThEnv,RDefs,Rp);
      valis (pkgSpec(Pkge,Imports,PkgType,Contracts,Impls,PkgVars),varDef(Lc,packageVar(Pkg),PkgNm,PkgTheta,[],PkgType))
    } else
    throw reportError(Rp,"invalid package structure",locOf(P))
  }

  makePkgTheta:(locn,string,tipe,dict,list[list[canonDef]],reports)=>either[reports,canon].
  makePkgTheta(Lc,Nm,Tp,Env,Defs,Rp) =>
    mkRecord(Lc,Nm,deRef(Tp),Env,sortDefs(flatten(Defs)),Tp,Rp).

  exportedFields:(list[list[canonDef]],list[(defnSp,visibility)],visibility) => list[(string,tipe)].
  exportedFields(Defs,Vis,DVz) =>
    [ (Nm,Tp) |
	DD in Defs && D in DD &&
	    ((varDef(_,Nm,_,_,_,Tp) .=D && isVisible(varSp(Nm),Vis,DVz)) ||
	      (cnsDef(_,Nm,_,Tp) .=D && isVisible(cnsSp(Nm),Vis,DVz)) ||
	      (implDef(_,N,Nm,_,_,Tp) .=D && isVisible(implSp(N),Vis,DVz)))].

  isVisible:(defnSp,list[(defnSp,visibility)],visibility) => boolean.
  isVisible(Sp,Vis,DVz) => (Sp,V) in Vis && V >= DVz.

  exportedTypes:(list[list[canonDef]],list[(defnSp,visibility)],visibility) => list[(string,tipe)].
  exportedTypes(Defs,Vis,DVz) => [ (Nm,ExTp) |
      DD in Defs && typeDef(_,Nm,_,ExTp) in DD && (tpSp(Nm),V) in Vis && V>=DVz].

  mkRecord:(locn,string,tipe,dict,list[list[canonDef]],tipe,reports) => either[reports,canon].
  mkRecord(Lc,Lbl,faceType(Flds,Tps),Env,Defs,Tp,Rp) => do{
--    logMsg("making record from $(Defs)\:$(faceType(Flds,Tps))");
    Rc <- findDefs(Lc,Flds,[],Defs,Rp);
    valis foldRight((Gp,I)=>letExp(Lc,Gp,I),record(Lc,Lbl,Rc,Tp),Defs)
  }

  findDefs:(locn,list[(string,tipe)],list[(string,canon)],list[list[canonDef]],reports) =>
    either[reports,list[(string,canon)]].
  findDefs(_,[],Flds,_,_) => either(Flds).
  findDefs(Lc,[(Nm,Tp),..Fs],SoF,Defs,Rp) where Val ^= findDefn(Defs,Nm) => do{
    findDefs(Lc,Fs,[SoF..,(Nm,Val)],Defs,Rp)
  }
  findDefs(Lc,[(Nm,Tp),.._],_,_,Rp) =>
    other(reportError(Rp,"cannot locate definition of $(Nm)\:$(Tp)",Lc)).

  findDefn:(list[list[canonDef]],string) => option[canon].
  findDefn([],_) => .none.
  findDefn([Gp,..Gps],Nm) => lookInGroup(Gp,Nm,Gps).

  lookInGroup:(list[canonDef],string,list[list[canonDef]])=>option[canon].
  lookInGroup([],Nm,Gps) => findDefn(Gps,Nm).
  lookInGroup([varDef(Lc,Nm,FullNm,lambda(_,_,_),Cx,Tp),..Gp],Nm,_) => some(vr(Lc,FullNm,Tp)).
  lookInGroup([varDef(Lc,Nm,FullNm,_,Cx,Tp),..Gp],Nm,_) => some(vr(Lc,Nm,Tp)).
  lookInGroup([cnsDef(Lc,Nm,FullNm,Tp),..Gp],Nm,_) => some(enm(Lc,FullNm,Tp)).
  lookInGroup([implDef(Lc,Nm,FullNm,Val,Cx,Tp),..Gp],FullNm,_) => some(vr(Lc,FullNm,Tp)).
  lookInGroup([_,..Gp],Nm,Gps) => lookInGroup(Gp,Nm,Gps).

  thetaEnv:(locn,string,list[ast],tipe,dict,reports,visibility) =>
    either[reports,(list[list[canonDef]],dict,tipe)].
  thetaEnv(Lc,Pth,Els,Face,Env,Rp,DefViz) => do{
    (Vis,Opens,Annots,Gps) <- dependencies(Els,Rp);
--    logMsg("pushing face $(Face)");
    Base .= pushFace(Face,Lc,Env);
--    logMsg("theta groups: $(Gps)");
--    logMsg("base env $(Base)");
    (Defs,ThEnv) <- checkGroups(Gps,[],Face,Annots,Base,Pth,Rp);

--    logMsg("Defs: $(Defs)");
    PubVrTps .= exportedFields(Defs,Vis,DefViz);
    PubTps .= exportedTypes(Defs,Vis,DefViz);
--    logMsg("exported fields $(PubVrTps)");
    valis (Defs,ThEnv,faceType(PubVrTps,PubTps))
  }

  recordEnv:(locn,string,list[ast],tipe,dict,reports,visibility) =>
    either[reports,(list[canonDef],dict,tipe)].
  recordEnv(Lc,Pth,Els,Face,Env,Rp,DefViz) => do{
--    logMsg("check record $(Els)");

    (Vis,Opens,Annots,G) <- recordDefs(Els,Rp);
--    logMsg("annotations: $(Annots)");
    TmpEnv <- parseAnnotations(G,Face,Annots,Env,Rp);
    
    (Defs,Ev) <- checkGroup(G,TmpEnv,Env,Pth,Rp);
--    logMsg("env after record $(Ev)");
--    logMsg("Defs: $(Defs)");
    
    PubVrTps .= exportedFields([Defs],Vis,DefViz);
    PubTps .= exportedTypes([Defs],Vis,DefViz);
--    logMsg("exported fields $(PubVrTps)");
    valis (Defs,Ev,faceType(PubVrTps,PubTps))
  }

  checkTypeGroups:(list[list[defnSpec]],dict,string,reports) =>
    either[reports,dict].
  checkTypeGroups([],Env,_,Rp) => either(Env).
  checkTypeGroups([G,..Gs],Env,Path,Rp) => do{
--    logMsg("check type group $(G)");
    Ev <- checkTypeGroup(G,Env,Path,Rp);
--    logMsg("env after group $(Ev)");
    checkTypeGroups(Gs,Ev,Path,Rp)
  }

  checkTypeGroup([],Env,_,_) => either(Env).
  checkTypeGroup([T,..G],Env,Path,Rp) => do{
    Ev <- checkTypeDefn(T,Env,Path,Rp);
    checkTypeGroup(G,Ev,Path,Rp)
  }

  checkTypeDefn(defnSpec(tpSp(TpNm),Lc,[St]),Env,Path,Rp) => do{
    (_,Ev) <- parseTypeDef(TpNm,St,Env,Path,Rp);
    valis Ev
  }
  checkTypeDefn(defnSpec(cnsSp(CnNm),Lc,[St]),Env,Path,Rp) => do{
    (_,Ev) <- parseConstructor(CnNm,St,Env,Path,Rp);
    valis Ev
  }
  checkTypeDefn(defnSpec(conSp(ConNm),Lc,[St]),Env,Path,Rp) => do{
    Contract <- parseContract(St,Env,Path,Rp);
    valis declareContract(Lc,ConNm,Contract,Env)
  }
  checkTypeDefn(defnSpec(implSp(Nm),Lc,[St]),Env,Path,Rp) => do {
    if (_,Q,C,H,B) ^= isImplementationStmt(St) then{
--      logMsg("record implementation $(Nm) $(Q) $(H)");
      BV <- parseBoundTpVars(Q,Rp);
      Cx <- parseConstraints(C,BV,Env,Rp);
      Cn <- parseContractConstraint(BV,H,Env,Rp);
--      logMsg("implemented contract $(BV) $(Cx) $(Cn)");
      ConName .= localName(tpName(Cn),.typeMark);
      if Con ^= findContract(Env,ConName) then{
	(_,typeExists(ConTp,_)) .= freshen(Con,Env);
--	logMsg("found contract type $(ConTp)");
	if sameType(ConTp,Cn,Env) then {
	  FullNm .= implementationName(ConTp);
--	  logMsg("full name of implementation of $(ConTp) is $(FullNm)");
	  ImplTp .= rebind(BV,reConstrainType(Cx,ConTp),Env);
	  valis declareVr(FullNm,some(Lc),ImplTp,(LL,TT)=>vr(Lc,FullNm,ImplTp),
	    declareImplementation(FullNm,ImplTp,Env))
	}
	else{
	  throw reportError(Rp,"implementation type $(Cn) not consistent with contract type $(ConTp)",Lc)
	}
      } else{
	throw reportError(Rp,"Contract $(ConName) not known",Lc)
      }
    }
    else
    throw reportError(Rp,"not a valid implementation statement",Lc)
  }
  checkTypeDefn(_,Env,_,_) default => either(Env).
  

  checkGroups:(list[list[defnSpec]],list[list[canonDef]],
    tipe,list[(string,ast)],dict,string,reports) =>
    either[reports,(list[list[canonDef]],dict)].
  checkGroups([],Gps,_,_,Env,_,Rp) => either((Gps,Env)).
  checkGroups([G,..Gs],Gx,Face,Annots,Env,Path,Rp) => do{
--    logMsg("check group $(G)");
    TmpEnv <- parseAnnotations(G,Face,Annots,Env,Rp);
--    logMsg("group env $(head(TmpEnv))");
    (Gp,Ev) <- checkGroup(G,TmpEnv,TmpEnv,Path,Rp);
--    logMsg("env after group $(Ev)");
    checkGroups(Gs,[Gx..,Gp],Face,Annots,Ev,Path,Rp)
  }

  parseAnnotations:(list[defnSpec],tipe,list[(string,ast)],dict,reports) => either[reports,dict].
  parseAnnotations([],_,_,Env,_) => either(Env).
  parseAnnotations([defnSpec(varSp(Nm),Lc,Stmts),..Gs],Fields,Annots,Env,Rp) => do{
    Tp <- parseAnnotation(Nm,Lc,Stmts,Fields,Annots,Env,Rp);
--    logMsg("found type of $(Nm)\:$(Tp)");
    parseAnnotations(Gs,Fields,Annots,declareVar(Nm,Nm,some(Lc),Tp,Env),Rp)
  }
  parseAnnotations([_,..Gs],Fields,Annots,Env,Rp) => parseAnnotations(Gs,Fields,Annots,Env,Rp).

  parseAnnotation:(string,locn,list[ast],tipe,list[(string,ast)],dict,reports) =>
    either[reports,tipe].
  parseAnnotation(Nm,_,_,_,Annots,Env,Rp) where (Nm,T) in Annots => do{
--    logMsg("parsing annotation $(Nm)\:$(T) in $(Env)");
    parseType([],T,Env,Rp)
  }.
  parseAnnotation(Nm,_,_,faceType(Vrs,_),_,_,_) where (Nm,Tp) in Vrs => either(Tp).
  parseAnnotation(Nm,_,_,_,_,Env,Rp) where vrEntry(_,_,Tp) ^= isVar(Nm,Env) => either(Tp).
  parseAnnotation(Nm,Lc,Stmts,Fields,Annots,Env,Rp) =>
    guessStmtType(Stmts,Nm,Lc,Rp).

  guessStmtType([],Nm,Lc,Rp) => other(reportError(Rp,"$(Nm) not declared",Lc)).
  guessStmtType([St,.._],Nm,Lc,Rp) => do{
    if (_,H,_) ^= isEquation(St) && (_,Args,_,_) ^= splitHead(H) then {
      valis funType(genArgTps(Args),newTypeVar("_R"))
    } else if (_,_,_) ^= isAssignment(St) then{
      valis refType(newTypeVar("R"))
    }
    else{
      valis newTypeVar("_D")
    }
  }
      
  checkGroup:(list[defnSpec],dict,dict,string,reports) =>
    either[reports,(list[canonDef],dict)].
  checkGroup(Specs,Env,Outer,Path,Rp) => do{
    (Defs,GEnv) <- checkDefs(Specs,[],Env,Outer,Path,Rp);
    valis (Defs,Env)
  }

  checkDefs:(list[defnSpec],list[canonDef],dict,dict,string,reports) =>
    either[reports,(list[canonDef],dict)].
  checkDefs([],Defs,Env,_,_,_) => either((Defs,Env)).
  checkDefs([D,..Ds],Defs,Env,Outer,Path,Rp) => do{
    (Defn,E0) <- checkDefn(D,Env,Outer,Path,Rp);
    checkDefs(Ds,[Defs..,Defn],E0,Outer,Path,Rp)
  }

  checkDefn:(defnSpec,dict,dict,string,reports) => either[reports,(canonDef,dict)].
  checkDefn(defnSpec(varSp(Nm),Lc,Stmts),Env,Outer,Path,Rp) where
      vrEntry(_,_,Tp) ^= isVar(Nm,Env) && areEquations(Stmts) =>
    checkFunction(Nm,Tp,Lc,Stmts,Env,Outer,Path,Rp).
  checkDefn(defnSpec(varSp(Nm),Lc,[Stmt]),Env,Outer,Path,Rp) where
      vrEntry(_,_,Tp) ^= isVar(Nm,Env) => do{
	(Q,ETp) .= evidence(Tp,Env);
	(Cx,VarTp) .= deConstrain(ETp);
	Es .= declareConstraints(Lc,Cx,declareTypeVars(Q,Outer));
	if (_,Lhs,R) ^= isDefn(Stmt) then{
	  Val <- typeOfExp(R,VarTp,Es,Path,Rp);
	  FullNm .= qualifiedName(Path,.valMark,Nm);
	  valis (varDef(Lc,Nm,FullNm,Val,Cx,Tp),
	    declareVar(Nm,FullNm,some(Lc),Tp,Env))
	}
	else{
	  throw reportError(Rp,"bad definition $(Stmt)",Lc)
	}
      }.
  checkDefn(defnSpec(tpSp(TpNm),Lc,[St]),Env,_,Path,Rp) =>
    parseTypeDef(TpNm,St,Env,Path,Rp).
  checkDefn(defnSpec(cnsSp(CnNm),Lc,[St]),Env,_,Path,Rp) =>
    parseConstructor(CnNm,St,Env,Path,Rp).
  checkDefn(defnSpec(conSp(ConNm),Lc,[St]),Env,_,Path,Rp) => do{
    Contract <- parseContract(St,Env,Path,Rp);
    valis (conDef(Lc,ConNm,ConNm,Contract),
      declareContract(Lc,ConNm,Contract,Env))
  }
  checkDefn(defnSpec(implSp(Nm),Lc,[St]),Env,Outer,Path,Rp) => do {
    if (_,Q,C,H,B) ^= isImplementationStmt(St) then
      checkImplementation(Lc,Nm,Q,C,H,B,Env,Outer,Path,Rp)
    else
      throw reportError(Rp,"not a valid implementation statement",Lc)
  }

  checkFunction:(string,tipe,locn,list[ast],dict,dict,string,reports) =>
    either[reports,(canonDef,dict)].
  checkFunction(Nm,Tp,Lc,Stmts,Env,Outer,Path,Rp) => do{
    (Q,ETp) .= evidence(Tp,Env);
--    logMsg("check function $(Nm)\:$(Tp) -- $(Q)~~$(ETp)");
    (Cx,ProgramTp) .= deConstrain(ETp);
    Es .= declareConstraints(Lc,Cx,declareTypeVars(Q,Env));
--    logMsg("env for equations: $(Es)");
    (Rls,Dflt) <- processEqns(Stmts,deRef(ProgramTp),[],[],Es,
      declareConstraints(Lc,Cx,declareTypeVars(Q,Outer)),Path,Rp);
    FullNm .= qualifiedName(Path,.valMark,Nm);
    valis (varDef(Lc,Nm,FullNm,
	lambda(Lc,Rls++Dflt,Tp),Cx,Tp),declareVar(Nm,FullNm,some(Lc),Tp,Env))
  }.

  processEqns:(list[ast],tipe,list[equation],list[equation],dict,dict,string,reports) =>
    either[reports,(list[equation],list[equation])].
  processEqns([],_,Rls,Deflt,_,_,_,_) => either((Rls,Deflt)).
  processEqns([St,..Ss],ProgramType,Rls,Deflt,Env,Outer,Path,Rp) => do{
    (Rl,IsDeflt) <- processEqn(St,ProgramType,Env,Outer,Path,Rp);
    if IsDeflt then{
      if [DRl,.._] .= Deflt then{
	throw reportError(Rp,"cannot have more than one default, other one at $(locOf(DRl))",
	  locOf(Rl))
      } else{
	processEqns(Ss,ProgramType,Rls,[Rl],Env,Outer,Path,Rp)
      }
    }
    else{
      processEqns(Ss,ProgramType,[Rls..,Rl],Deflt,Env,Outer,Path,Rp)
    }    
  }

  processEqn(St,ProgramType,Env,Outer,Path,Rp) where
      (Lc,H,R) ^= isEquation(St) && (_,Arg,Cnd,IsDeflt) ^= splitHead(H) => do{
	Ats .= genArgTps(Arg);
	RTp .= newTypeVar("_R");
	checkType(St,funType(Ats,RTp),ProgramType,Env,Rp);
	(Args,E0) <- typeOfArgPtn(Arg,tupleType(Ats),Outer,Path,Rp);
	if Wh^=Cnd then{
	  (Cond,E1) <- checkCond(Wh,E0,Path,Rp);
	  Rep <- typeOfExp(R,RTp,E1,Path,Rp);
	  
	  valis (eqn(Lc,Args,some(Cond),Rep),IsDeflt)
	} else{
	  Rep <- typeOfExp(R,RTp,E0,Path,Rp);
	  valis (eqn(Lc,Args,.none,Rep),IsDeflt)
	}
      }.

  checkImplementation:(locn,string,list[ast],list[ast],ast,ast,dict,dict,string,reports) =>
    either[reports,(canonDef,dict)].
  checkImplementation(Lc,Nm,Q,C,H,B,Env,Outer,Path,Rp) => do{
--    logMsg("check implementation $(Nm) $(Q) $(H)");
    BV <- parseBoundTpVars(Q,Rp);
    Cx <- parseConstraints(C,BV,Env,Rp);
    Cn <- parseContractConstraint(BV,H,Env,Rp);
--    logMsg("implemented contract $(BV) $(Cx) $(Cn)");
    ConName .= localName(tpName(Cn),.typeMark);
    if Con ^= findContract(Env,ConName) then{
--      logMsg("found contract $(Con)");
      (_,typeExists(ConTp,ConFaceTp)) .= freshen(Con,Env);
--      logMsg("found contract type $(ConTp), implementation type $(ConFaceTp)");
      if sameType(ConTp,Cn,Env) then {
--	logMsg("implementation constraints $(Cx)");
	Es .= declareConstraints(Lc,Cx,declareTypeVars(BV,Outer));
--	logMsg("check implementation body $(B) against $(ConFaceTp)");
	Impl <- typeOfExp(B,ConFaceTp,Es,Path,Rp);
	FullNm .= implementationName(ConTp);
--	logMsg("full name of implementation of $(ConTp) is $(FullNm)");
	ImplTp .= rebind(BV,reConstrainType(Cx,ConTp),Es);
--	logMsg("implementation is $(implDef(Lc,Nm,FullNm,Impl,Cx,ImplTp))");
	valis (implDef(Lc,Nm,FullNm,Impl,Cx,ImplTp),
	  declareImplementation(FullNm,ImplTp,Env))
      }
      else{
	throw reportError(Rp,"implementation type $(Cn) not consistent with contract type $(ConTp)",Lc)
      }
    } else{
      throw reportError(Rp,"Contract $(ConName) not known",Lc)
    }
  }
    
  typeOfPtn:(ast,tipe,dict,string,reports) => either[reports,(canon,dict)].
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,"_") ^= isName(A) =>
    either((vr(Lc,genSym("_"),Tp),Env)).
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) && Spec ^= isVar(Id,Env) =>
    typeOfPtn(mkWhereEquality(A),Tp,Env,Path,Rp).
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) => do{
    Ev .= declareVar(Id,Id,some(Lc),Tp,Env);
    valis (vr(Lc,Id,Tp),Ev)
  }
  typeOfPtn(A,Tp,Env,Path,Rp) where _ ^= isEnum(A) => do{
    Enm <- typeOfExp(A,Tp,Env,Path,Rp);
    valis (Enm,Env)
  }
  typeOfPtn(A,Tp,Env,Path,Rp) where isLit(A) => do{
    Exp <- typeOfExp(A,Tp,Env,Path,Rp);
    valis (Exp,Env)
  }
  typeOfPtn(A,Tp,Env,Path,Rp) where
      (Lc,E,T) ^= isTypeAnnotation(A) => do{
	ETp <- parseType([],T,Env,Rp);
	checkType(E,Tp,ETp,Env,Rp);
	typeOfPtn(E,Tp,Env,Path,Rp)
      }.
  typeOfPtn(A,Tp,Env,Path,Rp) where 
      (Lc,E,C) ^= isWhere(A) => do{
	(Ptn,Ev0) <- typeOfPtn(E,Tp,Env,Path,Rp);
	(Cond,Ev1) <- checkCond(C,Ev0,Path,Rp);
	valis (whr(Lc,Ptn,Cond),Ev1)
      }.
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isSqTuple(A) => do{
    SqPtn .= macroSquarePtn(Lc,Els);
    typeOfPtn(SqPtn,Tp,Env,Path,Rp)
  }.
  typeOfPtn(A,Tp,Env,Path,Rp) where (_,[El]) ^= isTuple(A) && ! _ ^= isTuple(El) =>
    typeOfPtn(El,Tp,Env,Path,Rp).
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs .= genTpVars(Els);
    checkType(A,tupleType(Tvs),Tp,Env,Rp);
    (Ptns,Ev) <- typeOfPtns(Els,Tvs,[],Env,Path,Rp);
    valis (tple(Lc,Ptns),Ev)
  }
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Pt,Ex) ^= isOptionPtn(A) =>
    typeOfPtn(mkWherePtn(Lc,Pt,Ex),Tp,Env,Path,Rp).
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Op,Els) ^= isRoundTerm(A) => do{
    At .= newTypeVar("A");
--    logMsg("checking round patterm $(A) for $(consType(At,Tp))");
    Fun <- typeOfExp(Op,consType(At,Tp),Env,Path,Rp);
    (Args,Ev) <- typeOfArgPtn(rndTuple(Lc,Els),At,Env,Path,Rp);
    valis (apply(Lc,Fun,Args,Tp),Ev)
  }
  typeOfPtn(A,Tp,_,_,Rp) => other(reportError(Rp,"illegal pattern: $(A), expecting a $(Tp)",locOf(A))).

  typeOfArgPtn:(ast,tipe,dict,string,reports) => either[reports,(canon,dict)].
  typeOfArgPtn(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs .= genTpVars(Els);
    checkType(A,tupleType(Tvs),Tp,Env,Rp);
    (Ptns,Ev) <- typeOfPtns(Els,Tvs,[],Env,Path,Rp);
    valis (tple(Lc,Ptns),Ev)
  }

  typeOfPtns:(list[ast],list[tipe],list[canon],dict,string,reports) =>
    either[reports,(list[canon],dict)].
  typeOfPtns([],[],Els,Env,_,_) => either((Els,Env)).
  typeOfPtns([P,..Ps],[T,..Ts],Els,Env,Path,Rp) => do{
    (Pt,E0) <- typeOfPtn(P,T,Env,Path,Rp);
    typeOfPtns(Ps,Ts,[Els..,Pt],E0,Path,Rp)
  }
  
  typeOfExp:(ast,tipe,dict,string,reports) => either[reports,canon].
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) => do{
--    logMsg("check $(Id) has type $(Tp)");
    if vrEntry(_,Mk,VTp) ^= isVar(Id,Env) then{
      (_,VrTp) .= freshen(VTp,Env);
--      logMsg("freshened type of $(Id) is $(VrTp)");
      (MTp,Term) <- manageConstraints(VrTp,[],Lc,Mk(Lc,VrTp),Env,Rp);
--      logMsg("check var $(Id)\:$(MTp) against $(Tp)");
      if sameType(Tp,MTp,Env) then {
	valis Term
      } else
      throw reportError(Rp,"variable $(Id)\:$(VTp) not consistent with expected type: $(Tp)",Lc)
    }
    else
    throw reportError(Rp,"variable $(Id) not defined. Expecting a $(Tp)",locOf(A)).
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Nm) ^= isEnumSymb(A) =>
    typeOfExp(zeroary(Lc,Nm),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Ix) ^= isInt(A) &&
      (_,IntTp,_) ^= findType(Env,"integer") => do{
	checkType(A,IntTp,Tp,Env,Rp);
	valis intr(Lc,Ix)
      }
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Dx) ^= isFlt(A) &&
      (_,FltTp,_) ^= findType(Env,"float") => do{
	checkType(A,FltTp,Tp,Env,Rp);
	valis flt(Lc,Dx)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Sx) ^= isStr(A) &&
      (_,StrTp,_) ^= findType(Env,"string") => do{
	checkType(A,StrTp,Tp,Env,Rp);
	valis strng(Lc,Sx)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,E,T) ^= isTypeAnnotation(A) => do{
	ETp <- parseType([],T,Env,Rp);
	checkType(E,Tp,ETp,Env,Rp);
	typeOfExp(E,Tp,Env,Path,Rp)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,E,T) ^= isCoerce(A) => do{
	typeOfExp(binary(Lc,":",unary(Lc,"_coerce",E),T),Tp,Env,Path,Rp)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,E,C) ^= isWhere(A) => do{
	(Cond,Ev0) <- checkCond(C,Env,Path,Rp);
	Exp <- typeOfExp(E,Tp,Ev0,Path,Rp);
	valis whr(Lc,Exp,Cond)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,R,F) ^= isFieldAcc(A) && (_,Fld) ^= isName(F) => do{
--	logMsg("field access $(Lc)");
	FldTp .= newTypeVar(Fld);
	TV .= newTVFieldConstraint(Fld,FldTp);
	Rc <- typeOfExp(R,TV,Env,Path,Rp);
--	logMsg("record type $(Rc)\:$(showType(TV,.true,0))");

        (_,VrTp) .= freshen(FldTp,Env);
        (MTp,Term) <- manageConstraints(VrTp,[],Lc,dot(Lc,Rc,Fld,VrTp),Env,Rp);
--	logMsg("constraint managed type $(MTp)");
        if sameType(Tp,MTp,Env) then{
--	  logMsg("$(Tp) field type ok");
	  valis Term
	}
	else
  	throw reportError(Rp,"field $(Fld)\:$(FldTp) not consistent with expected type: $(Tp)",Lc)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isConjunct(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkCond(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isDisjunct(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkCond(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,T,L,R) ^= isConditional(A) => do{
--    logMsg("conditional expression: $(A) against $(Tp)");
    (Tst,E0) <- checkCond(T,Env,Path,Rp);
    Thn <- typeOfExp(L,Tp,E0,Path,Rp);
    Els <- typeOfExp(R,Tp,Env,Path,Rp);
    valis cond(Lc,Tst,Thn,Els)
  }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isNegation(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkCond(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isMatch(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkCond(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isOptionMatch(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkCond(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isSearch(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkCond(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,G,Cases) ^= isCaseExp(A) => let{
    ETp = newTypeVar("_e").
    
    checkRle(E,RRp) where (Lc,H,R) ^= isEquation(E) => do{
      (Arg,E0) <- typeOfPtn(H,ETp,Env,Path,RRp);
      Rep <- typeOfExp(R,Tp,E0,Path,RRp);
      valis eqn(Lc,Arg,.none,Rep)
    }
    checkRle(E,RRp) => other(reportError(RRp,"invalid case $(E)",locOf(E))).

    typeOfCases:(list[ast],list[equation],reports) => either[reports,list[equation]].
    typeOfCases([],Els,_) => either(Els).
    typeOfCases([Cs,..Ps],SoFar,RRp) => do{
      Trm <- checkRle(Cs,Rp);
      typeOfCases(Ps,[SoFar..,Trm],RRp)
    }
  } in do{
    Gv <- typeOfExp(G,ETp,Env,Path,Rp);
    Gc <- typeOfCases(Cases,[],Rp);
    valis csexp(Lc,Gv,Gc,Tp)
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,C,Ix) ^= isIndex(A) =>
    ((_,K,V) ^= isBinary(Ix,"->") ?
	typeOfExp(ternary(Lc,"_put",C,K,V),Tp,Env,Path,Rp) ||
	(_,N) ^= isNegation(Ix) ?
	  typeOfExp(binary(Lc,"_remove",C,N),Tp,Env,Path,Rp) ||
	  typeOfExp(binary(Lc,"_index",C,Ix),Tp,Env,Path,Rp)).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,S,F,T) ^= isSlice(A) =>
    typeOfExp(ternary(Lc,"_slice",S,F,T),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Stmts) ^= isActionTerm(A) &&
      (_,ActionTp,_) ^= findType(Env,"action") => do{
	ErTp .= newTypeVar("E");
	XTp .= newTypeVar("X");
	checkType(A,tpExp(tpExp(ActionTp,ErTp),XTp),Tp,Env,Rp);
	checkDo(Stmts,Tp,Env,Path,Rp)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Stmts) ^= isTaskTerm(A) &&
      (_,ActionTp,_) ^= findType(Env,"task") => do{
	ErTp .= newTypeVar("E");
	XTp .= newTypeVar("X");
	checkType(A,tpExp(tpExp(ActionTp,ErTp),XTp),Tp,Env,Rp);
	checkDo(Stmts,Tp,Env,Path,Rp)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Stmts) ^= isDoTerm(A)  => 
    checkDo(Stmts,Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Exp) ^= isValof(A) =>
    typeOfExp(unary(Lc,"_perform",Exp),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isSqTuple(A) && ! _ ^= isAbstraction(A) =>
    typeOfExp(macroSquareExp(Lc,Els),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (_,[El]) ^= isTuple(A) && ! _ ^= isTuple(El) =>
    typeOfExp(El,Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs .= genTpVars(Els);
    checkType(A,tupleType(Tvs),Tp,Env,Rp);
    Ptns <- typeOfExps(Els,Tvs,[],Env,Path,Rp);
    valis tple(Lc,Ptns)
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,B,C) ^= isAbstraction(A) =>
    checkAbstraction(Lc,B,C,Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where hasPromotion(A) =>
    typeOfExp(promoteOption(A),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Ar,R) ^= isEquation(A) => do{
--    logMsg("check lambda $(A)");
    At .= newTypeVar("_A");
    Rt .= newTypeVar("_R");
    (As,E0) <- typeOfArgPtn(Ar,At,Env,Path,Rp);
    Rep <- typeOfExp(R,Rt,E0,Path,Rp);
--    logMsg("lambda return: $(Rep)\:$(typeOf(Rep))");
    checkType(A,fnType(At,Rt),Tp,Env,Rp);
    valis lambda(Lc,[eqn(Lc,As,.none,Rep)],Tp)
  }
  typeOfExp(A,Tp,Env,Pth,Rp) where (Lc,Els) ^= isTheta(A) => do{
    (Q,ETp) .= evidence(Tp,Env);
    FaceTp .= faceOfType(Tp,Env);
--    logMsg("checking theta record, expected type $(Tp), face $(FaceTp)");
    (Cx,Face) .= deConstrain(FaceTp);
    Base .= declareConstraints(Lc,Cx,declareTypeVars(Q,pushScope(Env)));
    Path .= genNewName(Pth,"θ");
    (Defs,ThEnv,ThetaTp) <- thetaEnv(Lc,Path,Els,Face,Base,Rp,.deFault);
    if sameType(ThetaTp,ETp,Env) then{
--      logMsg("building record from theta, $(ThEnv)");
      mkRecord(Lc,Path,deRef(faceOfType(Tp,ThEnv)),ThEnv,sortDefs(flatten(Defs)),
	reConstrainType(Cx,ThetaTp),Rp)
    }
    else
    throw reportError(Rp,"type of theta: $(ThetaTp)\nnot consistent with \n$(Tp)",Lc)
  }
  typeOfExp(A,Tp,Env,Pth,Rp) where (Lc,Els) ^= isRecord(A) => do{
    (Q,ETp) .= evidence(Tp,Env);
    FaceTp .= faceOfType(Tp,Env);
    (Cx,Face) .= deConstrain(FaceTp);
    Base .= declareConstraints(Lc,Cx,declareTypeVars(Q,pushScope(Env)));
    Path .= genNewName(Pth,"θ");
    (Defs,ThEnv,ThetaTp) <- recordEnv(Lc,Path,Els,Face,Base,Rp,.deFault);
    if sameType(ThetaTp,Tp,Env) then{
      mkRecord(Lc,Path,deRef(faceOfType(Tp,ThEnv)),ThEnv,[Defs],reConstrainType(Cx,ThetaTp),Rp)
    }
    else
    throw reportError(Rp,"type of theta: $(ThetaTp)\nnot consistent with \n$(Tp)",Lc)
  }
  typeOfExp(A,Tp,Env,Pth,Rp) where (Lc,Rc,Upd) ^= isRecordUpdate(A) => do{
    Rec <- typeOfExp(Rc,Tp,Env,Pth,Rp);
    UpTp .= newTypeVar("_");
    Update <- typeOfExp(Upd,UpTp,Env,Pth,Rp);
    faceType(RecFlds,_) .= faceOfType(Tp,Env);
    faceType(UpFlds,_) .= faceOfType(UpTp,Env);
    for (F,TU) in UpFlds do{
      if (F,TR) in RecFlds then{
	if !sameType(TU,TR,Env) then
	  throw reportError(Rp,"replacement for field $(F)\:$(TU) not consistent with record field $(TR)",Lc)
      } else
      throw reportError(Rp,"replacement for field $(F)\:$(TU) does not exist in record $(Rec)",Lc)
    };
    valis update(Lc,Rec,Update)
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,I) ^= isUnary(A,"-") =>
    typeOfExp(unary(Lc,"__minus",I),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Els,Bnd) ^= isLetDef(A) => do{
--    logMsg("checking let exp");

    (Defs,ThEnv,ThetaTp)<-thetaEnv(Lc,genNewName(Path,"Γ"),Els,faceType([],[]),Env,Rp,.priVate);
    
    BndEnv .= pushFace(ThetaTp,Lc,Env);
--    logMsg("let env groups $(Defs)");
--    logMsg("sub env is $(BndEnv)");
    El <- typeOfExp(Bnd,Tp,BndEnv,Path,Rp);

    Sorted .= sortDefs(flatten(Defs));
    
    valis foldRight((Gp,I)=>letExp(Lc,Gp,I),El,Sorted)
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Op,Args) ^= isRoundTerm(A) =>
    typeOfRoundTerm(Lc,Op,Args,Tp,Env,Path,Rp).

  typeOfExps:(list[ast],list[tipe],list[canon],dict,string,reports) =>
    either[reports,list[canon]].
  typeOfExps([],[],Els,Env,_,_) => either(Els).
  typeOfExps([P,..Ps],[T,..Ts],Els,Env,Path,Rp) => do{
    Trm <- typeOfExp(P,T,Env,Path,Rp);
    typeOfExps(Ps,Ts,[Els..,Trm],Env,Path,Rp)
  }

  typeOfRoundTerm:(locn,ast,list[ast],tipe,dict,string,reports) => either[reports,canon].
  typeOfRoundTerm(Lc,Op,As,Tp,Env,Path,Rp) => do{
--    logMsg("checking round term $(Op)$(As) against $(Tp)");
    Vrs .= genTpVars(As);
    At .= tupleType(Vrs);
    FFTp .= newTypeFun("_F",2);
    ExTp .= mkTypeExp(FFTp,[At,Tp]);
    Fun <- typeOfExp(Op,ExTp,Env,Path,Rp);
--    logMsg("type of $(Op) |- $(ExTp)");
    Args <- typeOfExps(As,Vrs,[],Env,Path,Rp);
--    logMsg("arg $(Args), FFTp=$(FFTp)");
    if sameType(FFTp,tpFun("=>",2),Env) || sameType(FFTp,tpFun("<=>",2),Env) then{
--      logMsg("we have apply $(apply(Lc,Fun,tple(Lc,Args),Tp))");
      valis apply(Lc,Fun,tple(Lc,Args),Tp)
    } else
      throw reportError(Rp,"type of $(Op)\:$(ExTp) not consistent with $(fnType(At,Tp))",Lc)
  }

  typeOfArgExp:(ast,tipe,dict,string,reports) => either[reports,(canon,dict)].
  typeOfArgExp(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs .= genTpVars(Els);
    checkType(A,tupleType(Tvs),Tp,Env,Rp);
    (Ptns,Ev) <- typeOfPtns(Els,Tvs,[],Env,Path,Rp);
    valis (tple(Lc,Ptns),Ev)
  }

  checkCond:(ast,dict,string,reports) => either[reports,(canon,dict)].
  checkCond(A,Env,Path,Rp) => do{
    (Gl,NE) <- checkGoal(A,Env,Path,Rp);
    if isIterableGoal(Gl) then{
      Cond <- processIterable(Gl,Path,NE,Rp);
      valis (Cond,NE)
    }
    else
    valis (Gl,NE)
  }

  checkGoal:(ast,dict,string,reports) => either[reports,(canon,dict)].
  checkGoal(A,Env,Path,Rp) where (Lc,L,R) ^= isConjunct(A) => do{
    (Lhs,E0) <- checkGoal(L,Env,Path,Rp);
    (Rhs,E1) <- checkGoal(R,E0,Path,Rp);
    valis (conj(Lc,Lhs,Rhs),E1)
  }.
  checkGoal(A,Env,Path,Rp) where (Lc,L,R) ^= isDisjunct(A) => do{
    (Lhs,E0) <- checkGoal(L,Env,Path,Rp);
    (Rhs,E1) <- checkGoal(R,Env,Path,Rp);
    valis (disj(Lc,Lhs,Rhs),mergeDict(E0,E1,Env))
  }.
  checkGoal(A,Env,Path,Rp) where (Lc,R) ^= isNegation(A) => do{
    (Rhs,_) <- checkGoal(R,Env,Path,Rp);
    valis (neg(Lc,Rhs),Env)
  }.
  checkGoal(A,Env,Path,Rp) where (Lc,T,L,R) ^= isConditional(A) => do{
    (Tst,E0) <- checkGoal(T,Env,Path,Rp);
    (Thn,E1) <- checkGoal(L,E0,Path,Rp);
    (Els,E2) <- checkGoal(R,Env,Path,Rp);
    valis (cond(Lc,Tst,Thn,Els),mergeDict(E1,E2,Env))
  }.
  checkGoal(A,Env,Path,Rp) where (Lc,L,R) ^= isMatch(A) => do{
    PtnTp .= newTypeVar("_M");
    Val <- typeOfExp(R,PtnTp,Env,Path,Rp);
    (Ptn,Ev) <- typeOfPtn(L,PtnTp,Env,Path,Rp);
    valis (match(Lc,Ptn,Val),Ev)
  }
  checkGoal(A,Env,Path,Rp) where (Lc,L,R) ^= isOptionMatch(A) => do{
    PtnTp .= newTypeVar("_M");
    Val <- typeOfExp(R,PtnTp,Env,Path,Rp);
    (Ptn,Ev) <- typeOfPtn(unary(Lc,"some",L),PtnTp,Env,Path,Rp);
    valis (match(Lc,Ptn,Val),Ev)
  }
  checkGoal(A,Env,Path,Rp) where (Lc,L,R) ^= isSearch(A) => do{
    MTp .= newTypeFun("M",1);
    StTp .= newTypeVar("X");
    SrTp .= newTypeVar("Sr");
    ElTp .= newTypeVar("El");
    MdTp .= tpExp(MTp,StTp);
    ActionTp .= funType([ElTp,StTp],MdTp);
    IterTp .= funType([SrTp,MdTp,ActionTp],MdTp);
    Iterator <- typeOfExp(nme(Lc,"_iter"),IterTp,Env,Path,Rp);
    (Ptn,Ev) <- typeOfPtn(L,ElTp,Env,Path,Rp);
    Gen <- typeOfExp(R,SrTp,Env,Path,Rp);
    valis (serch(Lc,Ptn,Gen,Iterator),Ev)
  }
  checkGoal(A,Env,Path,Rp) where (_,BoolTp,_) ^= findType(Env,"boolean") => do{
--    logMsg("try bool exp conde $(A)");
    Exp <- typeOfExp(A,BoolTp,Env,Path,Rp);
    valis (Exp,Env)
  }

  checkDo:(ast,tipe,dict,string,reports) => either[reports,canon].
  checkDo(Stmts,Tp,Env,Path,Rp) => do{
--    logMsg("process do $(Stmts)");
    VlTp .= newTypeVar("_e");
    ErTp .= newTypeVar("_e");
    Lc .= locOf(Stmts);
    if Con ^= findContract(Env,"execution") then{
      (_,typeExists(tpExp(Op,StTp),_)) .= freshen(Con,Env);
      if sameType(mkTypeExp(StTp,[ErTp,VlTp]),Tp,Env) then {
	(Action,_) <- checkAction(Stmts,Env,StTp,VlTp,ErTp,Path,Rp);
	genAction(Action,Op,.none,Path,Rp)
      } else
	throw reportError(Rp,"$(tpExp(StTp,ErTp)) not consistent with expected type $(Tp)",Lc)
    } else
      throw reportError(Rp,"cannot find execution contract",Lc)
  }

  checkAction:(ast,dict,tipe,tipe,tipe,string,reports) =>
    either[reports,(canonAction,dict)].
  checkAction(A,Env,_,_,_,_,_) where (Lc,"nothing") ^= isName(A) => either((noDo(Lc),Env)).
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,L,R) ^= isActionSeq(A) => do{
    E .= newTypeVar("_e");
    (Fst,E0) <- checkAction(L,Env,StTp,E,ErTp,Path,Rp);
    (Snd,E1) <- checkAction(R,E0,StTp,ElTp,ErTp,Path,Rp);
    valis (seqnDo(Lc,Fst,Snd),E1)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,L,R) ^= isBind(A) => do{
    E .= newTypeVar("P");
    HT .= mkTypeExp(StTp,[ErTp,E]); -- bound terms must be in the same monad
    Exp <- typeOfExp(R,HT,Env,Path,Rp);
    (Ptn,Ev) <- typeOfPtn(L,E,Env,Path,Rp);
    valis (bindDo(Lc,Ptn,Exp,E,StTp,ErTp),Ev)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,L,R) ^= isDefn(A) => do{
    E .= newTypeVar("P");
    (Ptn,Ev) <- typeOfPtn(L,E,Env,Path,Rp);
    Exp <- typeOfExp(R,E,Env,Path,Rp);

    valis (varDo(Lc,Ptn,Exp),Ev)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,L,R) ^= isMatch(A) => do{
    E .= newTypeVar("P");
    (Ptn,Ev) <- typeOfPtn(L,E,Env,Path,Rp);
    Exp <- typeOfExp(R,E,Env,Path,Rp);

    valis (varDo(Lc,Ptn,Exp),Ev)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,L,R) ^= isOptionMatch(A) =>
    checkAction(binary(Lc,".=",unary(Lc,"some",L),R),Env,StTp,ElTp,ErTp,Path,Rp).
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,L,R) ^= isAssignment(A) => do{
    Et .= newTypeVar("P");
    if (LLc,Coll,[Arg]) ^= isSquareTerm(L) then {
      Lhs <- typeOfExp(Coll,refType(Et),Env,Path,Rp);
      Rhs <- typeOfExp(ternary(LLc,"_put",unary(LLc,"!!",Coll),Arg,R),Et,Env,Path,Rp);
      valis (assignDo(Lc,Lhs,Rhs,StTp,ErTp),Env)
    } else{
      Lhs <- typeOfExp(L,refType(Et),Env,Path,Rp);
      Rhs <- typeOfExp(R,Et,Env,Path,Rp);
      valis (assignDo(Lc,Lhs,Rhs,StTp,ErTp),Env)
    }
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,T,L,R) ^= isIfThenElse(A) => do{
    (Cond,Ev0) <- checkCond(T,Env,Path,Rp);
    (Th,E1) <- checkAction(L,Env,StTp,ElTp,ErTp,Path,Rp);
    (El,E2) <- checkAction(R,Env,StTp,ElTp,ErTp,Path,Rp);
    valis (ifThenElseDo(Lc,Cond,Th,El,StTp,ElTp,ErTp),mergeDict(E1,E2,Env))
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,T,L) ^= isIfThen(A) => do{
    (Cond,Ev0) <- checkCond(T,Env,Path,Rp);
    (Th,E1) <- checkAction(L,Env,StTp,ElTp,ErTp,Path,Rp);
    valis (ifThenElseDo(Lc,Cond,Th,noDo(Lc),StTp,ElTp,ErTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,T,B) ^= isWhileDo(A) => do{
    (Cond,Ev0) <- checkCond(T,Env,Path,Rp);
    (Body,_) <- checkAction(B,Env,StTp,tupleType([]),ErTp,Path,Rp);
    valis (whileDo(Lc,Cond,Body,StTp,ErTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,T,B) ^= isForDo(A) => do{
    (Cond,Ev0) <- checkCond(T,Env,Path,Rp);
    (Body,_) <- checkAction(B,Env,StTp,tupleType([]),ErTp,Path,Rp);
    valis (forDo(Lc,Cond,Body,StTp,ErTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,B,C) ^= isTryCatch(A) => do{
    BErrTp .= newTypeVar("_");
    (Body,_) <- checkAction(B,Env,StTp,ElTp,BErrTp,Path,Rp);
    Hndlr <- checkCatch(C,Env,StTp,ElTp,BErrTp,ErTp,Path,Rp);
    valis (tryCatchDo(Lc,Body,Hndlr,StTp,ElTp,ErTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,E) ^= isThrow(A) => do{
    Err <- typeOfExp(E,ErTp,Env,Path,Rp);
    valis (throwDo(Lc,Err,StTp,ElTp,ErTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,E) ^= isValis(A) => do{
    Rtn <- typeOfExp(E,ElTp,Env,Path,Rp);
    valis (returnDo(Lc,Rtn,StTp,ElTp,ErTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,[S]) ^= isBrTuple(A) => 
    checkAction(S,Env,StTp,ElTp,ErTp,Path,Rp).

  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) => do{
    Exp <- typeOfExp(A,mkTypeExp(StTp,[ErTp,ElTp]),Env,Path,Rp);
    valis (simpleDo(locOf(A),Exp,StTp),Env)
  }

  checkCatch:(ast,dict,tipe,tipe,tipe,tipe,string,reports) => either[reports,canon].
  checkCatch(A,Env,StTp,ElTp,BErrTp,ErTp,Path,Rp) where (Lc,Stmts) ^= isBrTuple(A) => do{
    HT .= funType([BErrTp],mkTypeExp(StTp,[ErTp,ElTp]));
    (H,_) <- checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp);
    valis lambda(Lc,[eqn(Lc,tple(Lc,[vr(Lc,genSym("_"),ErTp)]),.none,act(locOf(A),H))],HT)
  }
  checkCatch(A,Env,StTp,ElTp,BErrTp,ErTp,Path,Rp) => do{
    HT .= funType([ErTp],mkTypeExp(StTp,[ErTp,ElTp]));
    typeOfExp(A,HT,Env,Path,Rp)
  }

  typeOfField:(locn,canon,string,tipe,dict,string,reports) => either[reports,canon].
  typeOfField(Lc,Rc,Fld,Tp,Env,Path,Rp) => do{
    faceType(Flds,_) .= faceOfType(typeOf(Rc),Env);
    if (Fld,FldTp) in Flds then{
      (_,VrTp) .= freshen(FldTp,Env);
      (MTp,Term) <- manageConstraints(VrTp,[],Lc,dot(Lc,Rc,Fld,VrTp),Env,Rp);
      if sameType(Tp,VrTp,Env) then {
	valis Term
      } else
	throw reportError(Rp,"field $(Fld)\:$(FldTp) not consistent with expected type: $(Tp)",Lc)
    } else
    throw reportError(Rp,"field $(Fld) is not present in $(Rc)\:$(typeOf(Rc))",Lc)
  }

  checkType:(ast,tipe,tipe,dict,reports) => either[reports,()].
  checkType(_,Actual,Expected,Env,_) where sameType(Actual,Expected,Env) => either(()).
  checkType(A,ATp,ETp,_,Rp) => other(reportError(
      Rp,"$(A)\:$(ATp) not consistent with expected type $(ETp)",locOf(A))).

  mergeDict:(dict,dict,dict) => dict.
  mergeDict(D1,D2,Env) => let{
    mergeScopes([scope(T1,V1,C1,I1),..Rst],
      [scope(_,V2,_,_),.._]) =>
      [scope(T1,mergeVDefs(V1,V2),C1,I1),..Rst].
    mergeVDefs(V1,V2) => {Nm->E1|Nm->E1 in V1 && E2^=V2[Nm] && sameDesc(E1,E2)}.
    sameDesc(vrEntry(_,_,T1),vrEntry(_,_,T2)) => sameType(T1,T2,Env)
  } in mergeScopes(D1,D2).

  genTpVars:(list[ast]) => list[tipe].
  genTpVars(Els) => (Els//(_)=>newTypeVar("_v")).

  genArgTps:(ast) => list[tipe].
  genArgTps(A) where (_,Ar,_) ^= isWhere(A) =>
    genArgTps(Ar).
  genArgTps(A) where (_,Els) ^= isTuple(A) =>
    genTpVars(Els).

  checkAbstraction:(locn,ast,ast,tipe,dict,string,reports) => either[reports,canon].
  checkAbstraction(Lc,B,C,Tp,Env,Path,Rp) => do{
--    logMsg("checking abstraction $(B) | $(C)");
    (Cond,E0) <- checkGoal(C,Env,Path,Rp);
    (_,StTp,ElTp) <- pickupContract(locOf(Cond),Env,"sequence",Rp);
    checkType(B,Tp,StTp,Env,Rp);
    Bnd <- typeOfExp(B,ElTp,E0,Path,Rp);
    genAbstraction(Lc,Bnd,Cond,Env,Path,Rp)
  }

  processIterable:(canon,string,dict,reports)=>either[reports,canon].
  processIterable(Cond,Path,Env,Rp) where isIterableGoal(Cond) => do {
    (Contract,_,_) <- pickupContract(locOf(Cond),Env,"execution",Rp);
    genIterableGoal(Cond,Contract,Path,Rp)
  }
  processIterable(apply(Lc,Op,Arg,Tp),Path,Env,Rp) => do{
    NOp <- processIterable(Op,Path,Env,Rp);
    NArg <- processIterable(Arg,Path,Env,Rp);
    valis apply(Lc,NOp,NArg,Tp)
  }
  processIterable(dot(Lc,Rec,Fld,Tp),Path,Env,Rp) => do{
    NRec <- processIterable(Rec,Path,Env,Rp);
    valis dot(Lc,NRec,Fld,Tp)
  }
  processIterable(tple(Lc,Els),Path,Env,Rp) => do{
    NEls <- _iter(Els,do{valis []},(El,Ot)=>either([Ot..,valof processIterable(El,Path,Env,Rp)]));
    valis tple(Lc,NEls)
  }
  processIterable(whr(Lc,E,C),Path,Env,Rp) => do{
    NE <- processIterable(E,Path,Env,Rp);
    NC <- processIterable(C,Path,Env,Rp);
    valis whr(Lc,NE,NC)
  }
  processIterable(conj(Lc,E,C),Path,Env,Rp) => do{
    NE <- processIterable(E,Path,Env,Rp);
    NC <- processIterable(C,Path,Env,Rp);
    valis conj(Lc,NE,NC)
  }
  processIterable(disj(Lc,E,C),Path,Env,Rp) => do{
    NE <- processIterable(E,Path,Env,Rp);
    NC <- processIterable(C,Path,Env,Rp);
    valis disj(Lc,NE,NC)
  }
  processIterable(implies(Lc,E,C),Path,Env,Rp) => do{
    NE <- processIterable(E,Path,Env,Rp);
    NC <- processIterable(C,Path,Env,Rp);
    valis implies(Lc,NE,NC)
  }
  processIterable(cond(Lc,Ts,Th,El),Path,Env,Rp) => do{
    NTs <- processIterable(Ts,Path,Env,Rp);
    NTh <- processIterable(Th,Path,Env,Rp);
    NEl <- processIterable(El,Path,Env,Rp);
    valis cond(Lc,NTs,NTh,NEl)
  }
  processIterable(neg(Lc,C),Path,Env,Rp) => do{
    NC <- processIterable(C,Path,Env,Rp);
    valis neg(Lc,NC)
  }
  processIterable(match(Lc,E,C),Path,Env,Rp) => do{
    NE <- processIterable(E,Path,Env,Rp);
    NC <- processIterable(C,Path,Env,Rp);
    valis match(Lc,NE,NC)
  }
  processIterable(E,Path,Env,Rp) default => do{
    valis E
  }



}
