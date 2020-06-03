star.compiler.checker{
  import star.

  import star.pkg.
  import star.repo.

  import star.compiler.ast.
  import star.compiler.ast.display.
  import star.compiler.canon.
  import star.compiler.canondeps.
  import star.compiler.dependencies.
  import star.compiler.dict.
  import star.compiler.dict.mgt.
  import star.compiler.errors.
  import star.compiler.freshen.
  import star.compiler.impawt.
  import star.compiler.location.
  import star.compiler.macro.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.resolve.
  import star.compiler.types.
  import star.compiler.typeparse.
  import star.compiler.unify.
  import star.compiler.wff.

  -- package level of type checker

  public checkPkg:all r ~~ repo[r],display[r]|:(r,pkg,ast,dict,compilerOptions,reports) => either[reports,(pkgSpec,canonDef)].
  checkPkg(Repo,Pkge,PP,Base,Opts,Rp) => do{
    P <- macroPkg(PP,Rp);
    if Opts.showMacro then{
      logMsg("macrod package:\n$(P)")
    };
    if (Lc,Pk,Els) ^= isBrTerm(P) && either(Pkg) .= pkgeName(Pk) then{
      if compatiblePkg(Pkg,Pkge) then{
	(Imports,Stmts) <- collectImports(Els,[],[],Rp);
	(PkgEnv,AllImports,PkgVars) <- importAll(Imports,Repo,Base,[],[],Rp);
	
	PkgNm .= packageName(Pkg);
	-- We treat a package specially, buts its essentially a theta record
	(Vis,Opens,Annots,Gps) <- dependencies(Stmts,Rp);

	TEnv <- checkTypeGroups(Gps,PkgEnv,PkgNm,Rp);
	
	(Defs,ThEnv) <- checkGroups(Gps,[],faceType([],[]),Annots,TEnv,PkgNm,Rp);
	if [Open,.._] .= Opens then
	  throw reportError(Rp,"open statement $(Open) not permitted in package",locOf(Open));

	Contracts .= [ D | DD in Defs && D in DD && conDef(_,Nm,_,_).=D &&
	      (conSp(Nm), .pUblic) in Vis];
	Fields .= exportedFields(Defs,Vis,.pUblic);
	Impls .= [ implSpec(some(ILc),INm,FllNm,ITp) |
	    DD in Defs &&
		implDef(ILc,INm,FllNm,_,_,ITp) in DD &&
		(implSp(INm),V) in Vis && V>=.pUblic];
	Types .= exportedTypes(Defs,Vis,.pUblic);
	RDefs <- overloadEnvironment(Defs,PkgEnv,Rp);
	PkgType .= faceType(Fields,Types);
	PkgTheta <- makePkgTheta(Lc,PkgNm,PkgType,ThEnv,sortDefs(multicat(RDefs)),Rp);
	valis (pkgSpec(Pkge,Imports,PkgType,Contracts,Impls,PkgVars),varDef(Lc,PkgNm,packageVar(Pkg),PkgTheta,[],PkgType))
      }
      else
      throw reportError(Rp,"package name $(Pkg) does not match expected $(Pkge)",locOf(P))
    } else
    throw reportError(Rp,"invalid package structure",locOf(P))
  }

  makePkgTheta:(locn,string,tipe,dict,cons[cons[canonDef]],reports)=>either[reports,canon].
  makePkgTheta(Lc,Nm,Tp,Env,Defs,Rp) =>
    formTheta(Lc,some(Nm),deRef(Tp),Env,Defs,Tp,Rp).

  exportedFields:(cons[cons[canonDef]],cons[(defnSp,visibility)],visibility) => cons[(string,tipe)].
  exportedFields(Defs,Vis,DVz) =>
    [ (Nm,Tp) |
	DD in Defs && D in DD &&
	    ((varDef(_,Nm,_,_,_,Tp) .=D && isVisible(varSp(Nm),Vis,DVz)) ||
	      (cnsDef(_,Nm,_,Tp) .=D && isVisible(cnsSp(Nm),Vis,DVz)) ||
	      (implDef(_,N,Nm,_,_,Tp) .=D && isVisible(implSp(N),Vis,DVz)))].

  isVisible:(defnSp,cons[(defnSp,visibility)],visibility) => boolean.
  isVisible(Sp,Vis,DVz) => (Sp,V) in Vis && V >= DVz.

  exportedTypes:(cons[cons[canonDef]],cons[(defnSp,visibility)],visibility) =>
    cons[(string,tipe)].
  exportedTypes(Defs,Vis,DVz) => [ (Nm,ExTp) |
      DD in Defs && typeDef(_,Nm,_,ExTp) in DD && (tpSp(Nm),V) in Vis && V>=DVz].

  formRecordExp:(locn,option[string],tipe,dict,cons[cons[canonDef]],tipe,reports) => either[reports,canon].
  formRecordExp(Lc,Lbl,faceType(Flds,Tps),Env,Defs,Tp,Rp) => do{
    Rc <- findDefs(Lc,Flds,[],Defs,Rp);
    valis foldRight((Gp,I)=>letExp(Lc,Gp^/keepDef,I),record(Lc,Lbl,Rc,Tp),Defs)
  }

  keepDef(varDef(_,Nm,_,vr(_,Nm,_),_,_)) => .false.
  keepDef(_) default => .true.
  
  formTheta:(locn,option[string],tipe,dict,cons[cons[canonDef]],tipe,reports) =>
    either[reports,canon].
  formTheta(Lc,Lbl,faceType(Flds,Tps),Env,Defs,Tp,Rp) => do{
    Rc <- findDefs(Lc,Flds,[],Defs,Rp);
    valis foldRight((Gp,I)=>letRec(Lc,Gp,I),record(Lc,Lbl,Rc,Tp),Defs)
  }

  findDefs:(locn,cons[(string,tipe)],cons[(string,canon)],cons[cons[canonDef]],reports) =>
    either[reports,cons[(string,canon)]].
  findDefs(_,[],Flds,_,_) => either(Flds).
  findDefs(Lc,[(Nm,Tp),..Fs],SoF,Defs,Rp) where Val ^= findDefn(Defs,Nm) => do{
    findDefs(Lc,Fs,[(Nm,Val),..SoF],Defs,Rp)
  }
  findDefs(Lc,[(Nm,Tp),.._],_,_,Rp) =>
    other(reportError(Rp,"cannot locate definition of $(Nm)\:$(Tp)",Lc)).

  findDefn:(cons[cons[canonDef]],string) => option[canon].
  findDefn([],_) => .none.
  findDefn([Gp,..Gps],Nm) => lookInGroup(Gp,Nm,Gps).

  lookInGroup:(cons[canonDef],string,cons[cons[canonDef]])=>option[canon].
  lookInGroup([],Nm,Gps) => findDefn(Gps,Nm).
  lookInGroup([varDef(Lc,Nm,_,_,Cx,Tp),..Gp],Nm,_) => some(vr(Lc,Nm,Tp)).
  lookInGroup([cnsDef(Lc,Nm,FullNm,Tp),..Gp],Nm,_) => some(enm(Lc,FullNm,Tp)).
  lookInGroup([implDef(Lc,Nm,FullNm,Val,Cx,Tp),..Gp],FullNm,_) => some(vr(Lc,FullNm,Tp)).
  lookInGroup([_,..Gp],Nm,Gps) => lookInGroup(Gp,Nm,Gps).

  thetaEnv:(locn,string,cons[ast],tipe,dict,reports,visibility) =>
    either[reports,(cons[cons[canonDef]],dict,tipe)].
  thetaEnv(Lc,Pth,Stmts,Face,Env,Rp,DefViz) => do{
    (Vis,Opens,Annots,Gps) <- dependencies(Stmts,Rp);
    Base .= pushFace(Face,Lc,Env);
    TEnv <- checkTypeGroups(Gps,Base,Pth,Rp);
    (Defs,ThEnv) <- checkGroups(Gps,[],Face,Annots,TEnv,Pth,Rp);

    PubVrTps .= exportedFields(Defs,Vis,DefViz);
    PubTps .= exportedTypes(Defs,Vis,DefViz);
    valis (Defs,ThEnv,faceType(PubVrTps,PubTps))
  }

  recordEnv:(locn,string,cons[ast],tipe,dict,reports,visibility) =>
    either[reports,(cons[canonDef],dict,tipe)].

  recordEnv(Lc,Pth,Stmts,Face,Env,Rp,DefViz) => do{
    (Vis,Opens,Annots,Gps) <- dependencies(Stmts,Rp);
    Grp .= multicat(Gps);

    (Defs,ThEnv) <- letGroup(Grp,[],Pth,Face,Annots,Env,Rp);

    PubVrTps .= exportedFields([Defs],Vis,DefViz);
    PubTps .= exportedTypes([Defs],Vis,DefViz);
    valis (Defs,ThEnv,faceType(PubVrTps,PubTps))
  }

  letGroup:(cons[defnSpec],cons[canonDef],string,tipe,
    cons[(string,ast)],dict,reports) =>
    either[reports,(cons[canonDef],dict)].
  letGroup([],Defs,_,_,_,Env,_) => either((reverse(Defs),Env)).
  letGroup([Df,..Ds],So,Pth,Face,Annots,Env,Rp) => do{
    TEnv <- parseAnnotations([Df],Face,Annots,Env,Rp);
    (Defn,E0) <- checkDefn(Df,TEnv,Env,Pth,Rp);
    letGroup(Ds,[Defn,..So],Pth,Face,Annots,E0,Rp)
  }

/*
  recordEnv(Lc,Pth,Stmts,Face,Env,Rp,DefViz) => do{
    (Vis,Opens,Annots,Gps) <- dependencies(Stmts,Rp);
    TEnv <- checkTypeGroups(Gps,Env,Pth,Rp);

    G .= multicat(Gps);
    TmpEnv <- parseAnnotations(G,Face,Annots,TEnv,Rp);
    (Defs,ThEnv) <- checkGroup(G,TmpEnv,Env,Pth,Rp);

    PubVrTps .= exportedFields([Defs],Vis,DefViz);
    PubTps .= exportedTypes([Defs],Vis,DefViz);
    valis (Defs,ThEnv,faceType(PubVrTps,PubTps))
  }
*/
  
  checkTypeGroups:(cons[cons[defnSpec]],dict,string,reports) =>
    either[reports,dict].
  checkTypeGroups([],Env,_,Rp) => either(Env).
  checkTypeGroups([G,..Gs],Env,Path,Rp) => do{
    Ev <- checkTypeGroup(G,Env,Path,Rp);
    checkTypeGroups(Gs,Ev,Path,Rp)
  }

  checkTypeGroup:(cons[defnSpec],dict,string,reports) => either[reports,dict].
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
--	logMsg("found contract type $(Con)");
	(_,typeExists(ConTp,_)) .= freshen(Con,Env);
--	logMsg("freshened contract type $(ConTp)");
	if sameType(ConTp,Cn,Env) then {
	  FullNm .= implementationName(ConTp);
--	  logMsg("full name of implementation of $(ConTp) is $(FullNm)");
	  ImplTp .= rebind(BV,reConstrainType(Cx,ConTp),Env);
--	  logMsg("implementation type is $(ImplTp)");
	  valis declareVar(FullNm,some(Lc),ImplTp,.none,
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
  
  checkGroups:(cons[cons[defnSpec]],cons[cons[canonDef]],
    tipe,cons[(string,ast)],dict,string,reports) =>
    either[reports,(cons[cons[canonDef]],dict)].
  checkGroups([],Gps,_,_,Env,_,Rp) => either((reverse(Gps),Env)).
  checkGroups([G,..Gs],Gx,Face,Annots,Env,Path,Rp) => do{
--    logMsg("check group $(G)");
    TmpEnv <- parseAnnotations(G,Face,Annots,Env,Rp);
--    logMsg("group env $(head(TmpEnv))");
    (Gp,Ev) <- checkGroup(G,TmpEnv,TmpEnv,Path,Rp);
--    logMsg("env after group $(Ev)");
    checkGroups(Gs,[Gp,..Gx],Face,Annots,Ev,Path,Rp)
  }

  parseAnnotations:(cons[defnSpec],tipe,cons[(string,ast)],dict,reports) => either[reports,dict].
  parseAnnotations([],_,_,Env,_) => either(Env).
  parseAnnotations([defnSpec(varSp(Nm),Lc,Stmts),..Gs],Fields,Annots,Env,Rp) => do{
    Tp <- parseAnnotation(Nm,Lc,Stmts,Fields,Annots,Env,Rp);
--    logMsg("found type of $(Nm)\:$(Tp)");
    parseAnnotations(Gs,Fields,Annots,declareVar(Nm,some(Lc),Tp,some(faceOfType(Tp,Env)),Env),Rp)
  }
  parseAnnotations([_,..Gs],Fields,Annots,Env,Rp) => parseAnnotations(Gs,Fields,Annots,Env,Rp).

  parseAnnotation:(string,locn,cons[ast],tipe,cons[(string,ast)],dict,reports) =>
    either[reports,tipe].
  parseAnnotation(Nm,_,_,_,Annots,Env,Rp) where (Nm,T) in Annots => do{
--    logMsg("parsing annotation $(Nm)\:$(T)");
    parseType([],T,Env,Rp)
  }.
  parseAnnotation(Nm,_,_,faceType(Vrs,_),_,_,_) where (Nm,Tp) in Vrs => either(Tp).
  parseAnnotation(Nm,_,_,_,_,Env,Rp) where Tp ^= varType(Nm,Env) => either(Tp).
  parseAnnotation(Nm,Lc,Stmts,Fields,Annots,Env,Rp) =>
    guessStmtType(Stmts,Nm,Lc,Rp).

  guessStmtType([],Nm,Lc,Rp) => other(reportError(Rp,"$(Nm) not declared",Lc)).
  guessStmtType([St,.._],Nm,Lc,Rp) => do{
    if (_,_,_,Args,_,_) ^= isEquation(St) then {
      valis funType(genArgTps(Args),newTypeVar("_R"))
    } else if (_,_,_) ^= isAssignment(St) then{
      valis refType(newTypeVar("R"))
    }
    else{
      valis newTypeVar("_D")
    }
  }

  allFunDefs:(cons[canonDef])=>boolean.
  allFunDefs(Dfs) => D in Dfs *> varDef(_,_,_,lambda(_,_,_),_,_).=D.

  checkGroup:(cons[defnSpec],dict,dict,string,reports) =>
    either[reports,(cons[canonDef],dict)].
  checkGroup(Specs,Env,Outer,Path,Rp) => 
    checkDefs(Specs,[],Env,Outer,Path,Rp).

  checkDefs:(cons[defnSpec],cons[canonDef],dict,dict,string,reports) =>
    either[reports,(cons[canonDef],dict)].
  checkDefs([],Defs,Env,_,_,_) => either((reverse(Defs),Env)).
  checkDefs([D,..Ds],Defs,Env,Outer,Path,Rp) => do{
    (Defn,E0) <- checkDefn(D,Env,Outer,Path,Rp);
    checkDefs(Ds,[Defn,..Defs],E0,Outer,Path,Rp)
  }

  checkDefn:(defnSpec,dict,dict,string,reports) => either[reports,(canonDef,dict)].
  checkDefn(defnSpec(varSp(Nm),Lc,Stmts),Env,Outer,Path,Rp) where
      Tp ^= varType(Nm,Env) && areEquations(Stmts) =>
    checkFunction(Nm,Tp,Lc,Stmts,Env,Outer,Path,Rp).
  checkDefn(defnSpec(varSp(Nm),Lc,[Stmt]),Env,Outer,Path,Rp) where Tp ^= varType(Nm,Env) => do{
    (Q,ETp) .= evidence(Tp,Env);
--	logMsg("given type of $(Nm) is $(ETp)");
    (Cx,VarTp) .= deConstrain(ETp);
    Es .= declareConstraints(Lc,Cx,declareTypeVars(Q,Outer));
    if (_,Lhs,R) ^= isDefn(Stmt) then{
      Val <- typeOfExp(R,VarTp,Es,Path,Rp);
      FullNm .= qualifiedName(Path,.valMark,Nm);
      valis (varDef(Lc,Nm,FullNm,Val,Cx,Tp),
	declareVar(Nm,some(Lc),Tp,some(faceOfType(Tp,Env)),Env))
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

  checkFunction:(string,tipe,locn,cons[ast],dict,dict,string,reports) =>
    either[reports,(canonDef,dict)].
  checkFunction(Nm,Tp,Lc,Stmts,Env,Outer,Path,Rp) => do{
    (Q,ETp) .= evidence(Tp,Env);
    (Cx,ProgramTp) .= deConstrain(ETp);
--    logMsg("check function $(Nm) ProgramTp=$(ProgramTp) Tp=$(Tp)");
    Es .= declareConstraints(Lc,Cx,declareTypeVars(Q,Env));
--    logMsg("env for equations: $(head(Es))");
    Rls <- processEqns(Stmts,deRef(ProgramTp),[],.none,Es,
      declareConstraints(Lc,Cx,declareTypeVars(Q,Outer)),Path,Rp);
    FullNm .= qualifiedName(Path,.valMark,Nm);
--    logMsg("checked equations $(Rls), ProgramTp=$(ProgramTp), Tp=$(Tp)");
    valis (varDef(Lc,Nm,FullNm,
	lambda(FullNm,Rls,Tp),Cx,Tp),declareVar(Nm,some(Lc),Tp,.none,Env))
  }.

  processEqns:(cons[ast],tipe,cons[equation],option[equation],dict,dict,string,reports) =>
    either[reports,cons[equation]].
  processEqns([],_,Rls,.none,_,_,_,_) => either(reverse(Rls)).
  processEqns([],_,Rls,some(Dflt),_,_,_,_) => either(reverse([Dflt,..Rls])).
  processEqns([St,..Ss],ProgramType,Rls,Deflt,Env,Outer,Path,Rp) => do{
    (Rl,IsDeflt) <- processEqn(St,ProgramType,Env,Outer,Path,Rp);
    if IsDeflt then{
      if DRl ^= Deflt then{
	throw reportError(Rp,"cannot have more than one default, other one at $(locOf(DRl))",
	  locOf(Rl))
      } else{
	processEqns(Ss,ProgramType,Rls,some(Rl),Env,Outer,Path,Rp)
      }
    }
    else{
      processEqns(Ss,ProgramType,[Rl,..Rls],Deflt,Env,Outer,Path,Rp)
    }    
  }

  processEqn(St,ProgramType,Env,Outer,Path,Rp) where
      (Lc,_,IsDeflt,Arg,Cnd,R) ^= isEquation(St) => do{
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

  checkImplementation:(locn,string,cons[ast],cons[ast],ast,ast,dict,dict,string,reports) =>
    either[reports,(canonDef,dict)].
  checkImplementation(Lc,Nm,Q,C,H,B,Env,Outer,Path,Rp) => do{
--    logMsg("check implementation $(Nm) $(Q) $(H)");
    BV <- parseBoundTpVars(Q,Rp);
    Cx <- parseConstraints(C,BV,Env,Rp);
    Cn <- parseContractConstraint(BV,H,Env,Rp);
--    logMsg("implemented contract $(Cx) |: $(Cn)");
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
--	logMsg("implementation type of $(Nm) is $(ImplTp)");
	valis (implDef(Lc,Nm,FullNm,Impl,Cx,ImplTp),declareImplementation(FullNm,ImplTp,Env))
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
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) &&
      varDefined(Id,Env) =>
    typeOfPtn(mkWhereEquality(A),Tp,Env,Path,Rp).
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) => do{
    Ev .= declareVar(Id,some(Lc),Tp,some(faceOfType(Tp,Env)),Env);
    valis (vr(Lc,Id,Tp),Ev)
  }
  typeOfPtn(A,Tp,Env,Path,Rp) where _ ^= isEnum(A) => do{
    Enm <- typeOfExp(A,Tp,Env,Path,Rp);
    valis (Enm,Env)
  }
  typeOfPtn(A,Tp,Env,Path,Rp) where isLitAst(A) => do{
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
  typeOfPtn(A,Tp,Env,Path,Rp) where (_,[El]) ^= isTuple(A) && ! _ ^= isTuple(El) =>
    typeOfPtn(El,Tp,Env,Path,Rp).
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs .= genTpVars(Els);
    checkType(A,tupleType(Tvs),Tp,Env,Rp);
    (Ptns,Ev) <- typeOfPtns(Els,Tvs,[],Env,Path,Rp);
    valis (tple(Lc,Ptns),Ev)
  }
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Op,Els) ^= isRoundTerm(A) => do{
    At .= newTypeVar("A");
--    logMsg("checking round pattern $(A) for $(consType(At,Tp))");
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

  typeOfPtns:(cons[ast],cons[tipe],cons[canon],dict,string,reports) =>
    either[reports,(cons[canon],dict)].
  typeOfPtns([],[],Els,Env,_,_) => either((reverse(Els),Env)).
  typeOfPtns([P,..Ps],[T,..Ts],Els,Env,Path,Rp) => do{
    (Pt,E0) <- typeOfPtn(P,T,Env,Path,Rp);
    typeOfPtns(Ps,Ts,[Pt,..Els],E0,Path,Rp)
  }
  
  typeOfExp:(ast,tipe,dict,string,reports) => either[reports,canon].
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) => do{
--    logMsg("check $(Id) has type $(Tp)");
    if Var ^= findVar(Lc,Id,Env) then{
--      logMsg("check var $(Var)\:$(typeOf(Var))");
      if sameType(Tp,typeOf(Var),Env) then {
	valis Var
      } else
      throw reportError(Rp,"variable $(Id)\:$(typeOf(Var)) not consistent with expected type: $(Tp)",Lc)
    }
    else
    throw reportError(Rp,"variable $(Id) not defined. Expecting a $(Tp)",locOf(A)).
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Nm) ^= isEnumSymb(A) =>
    typeOfExp(zeroary(Lc,Nm),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Ix) ^= isInt(A)  => do{
    checkType(A,intType,Tp,Env,Rp);
    valis intr(Lc,Ix)
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Dx) ^= isFlt(A) => do{
    checkType(A,fltType,Tp,Env,Rp);
    valis flt(Lc,Dx)
  }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Sx) ^= isStr(A) => do{
    checkType(A,strType,Tp,Env,Rp);
    valis strng(Lc,Sx)
  }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,E,T) ^= isTypeAnnotation(A) => do{
    ETp <- parseType([],T,Env,Rp);
    checkType(E,Tp,ETp,Env,Rp);
    typeOfExp(E,Tp,Env,Path,Rp)
  }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,E,C) ^= isWhere(A) => do{
	(Cond,Ev0) <- checkCond(C,Env,Path,Rp);
	Exp <- typeOfExp(E,Tp,Ev0,Path,Rp);
	valis whr(Lc,Exp,Cond)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,R,F) ^= isFieldAcc(A) && (_,Fld) ^= isName(F) => do{
--	logMsg("field access $(A)");
	FldTp .= newTypeVar(Fld);
	TV .= newTVFieldConstraint(Fld,FldTp);
--	logMsg("check record $(R)");
	Rc <- typeOfExp(R,TV,Env,Path,Rp);
--	logMsg("record type $(Rc)\:$(showType(TV,.true,0))");
	Acc .= refreshVr(Lc,FldTp,Env,(T)=>dot(Lc,Rc,Fld,T));
--	logMsg("refreshed field type $(typeOf(Acc))");
	if sameType(Tp,typeOf(Acc),Env) then{
--	  logMsg("$(Tp) field type ok");
	  valis Acc
	}
	else
	throw reportError(Rp,"field $(Fld)\:$(FldTp) not consistent with expected type: $(Tp)",Lc)
      }

  typeOfExp(A,Tp,Env,Path,Rp) where _ ^= isConjunct(A) => do{
    checkType(A,boolType,Tp,Env,Rp);
    (Gl,_) <- checkCond(A,Env,Path,Rp);
    valis Gl
  }.
  typeOfExp(A,Tp,Env,Path,Rp) where _ ^= isDisjunct(A)  => do{
    checkType(A,boolType,Tp,Env,Rp);
    (Gl,_) <- checkCond(A,Env,Path,Rp);
    valis Gl
  }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,T,L,R) ^= isConditional(A) => do{
    (Tst,E0) <- checkCond(T,Env,Path,Rp);
    Thn <- typeOfExp(L,Tp,E0,Path,Rp);
    Els <- typeOfExp(R,Tp,Env,Path,Rp);
    valis cond(Lc,Tst,Thn,Els)
  }.
  typeOfExp(A,Tp,Env,Path,Rp) where _ ^= isNegation(A)  => do{
    checkType(A,boolType,Tp,Env,Rp);
    (Gl,_) <- checkCond(A,Env,Path,Rp);
    valis Gl
  }.
  typeOfExp(A,Tp,Env,Path,Rp) where _ ^= isImplies(A) => do{
    checkType(A,boolType,Tp,Env,Rp);
    (Gl,_) <- checkCond(A,Env,Path,Rp);
    valis Gl
  }.
  typeOfExp(A,Tp,Env,Path,Rp) where _ ^= isMatch(A) => do{
    checkType(A,boolType,Tp,Env,Rp);
    (Gl,_) <- checkCond(A,Env,Path,Rp);
    valis Gl
  }.
  typeOfExp(A,Tp,Env,Path,Rp) where _ ^= isOptionMatch(A) => do{
    checkType(A,boolType,Tp,Env,Rp);
    (Gl,_) <- checkCond(A,Env,Path,Rp);
    valis Gl
  }.
  typeOfExp(A,Tp,Env,Path,Rp) where _ ^= isSearch(A) => do{
    checkType(A,boolType,Tp,Env,Rp);
    (Gl,_) <- checkCond(A,Env,Path,Rp);
    valis Gl
  }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,G,Cases) ^= isCaseExp(A) => let{
    checkRle(E,ETp,RRp) where (CLc,IsDeflt,H,C,R) ^= isLambda(E) => do{
      (Arg,E0) <- typeOfPtn(H,ETp,Env,Path,RRp);
      if Cnd ^= C then {
	(Cond,CE) <- checkCond(Cnd,Env,Path,RRp);
	Rep <- typeOfExp(R,Tp,CE,Path,RRp);
	(Ags,ACnd) .= pullWhere(Arg,some(Cond));
	valis eqn(Lc,tple(Lc,[Ags]),ACnd,Rep)
      }
      else{
	(Ags,ACnd) .= pullWhere(Arg,.none);
	Rep <- typeOfExp(R,Tp,E0,Path,RRp);
	valis eqn(CLc,tple(Lc,[Ags]),ACnd,Rep)
      }
    }
    checkRle(E,_,RRp) => other(reportError(RRp,"invalid case $(E)",locOf(E))).

    pullWhere:(canon,option[canon]) => (canon,option[canon]).
    pullWhere(whr(WLc,Vl,Cn),Gl) where (Val,G1) .= pullWhere(Vl,Gl) =>
      (Val,mergeGoal(WLc,some(Cn),G1)).
    pullWhere(Exp,Gl) default => (Exp,Gl).

    mergeGoal:(locn,option[canon],option[canon])=>option[canon].
    mergeGoal(_,Gl,.none) => Gl.
    mergeGoal(_,.none,Gl) => Gl.
    mergeGoal(Lc,some(Gl),some(H)) => some(conj(Lc,Gl,H)).

    typeOfCases:(cons[ast],tipe,cons[equation],reports) => either[reports,cons[equation]].
    typeOfCases([],PtnTp,Els,_) => either(reverse(Els)).
    typeOfCases([Cs,..Ps],PtnTp,SoFar,RRp) => do{
--      logMsg("check rule $(Cs)");
      Trm <- checkRle(Cs,PtnTp,Rp);
      typeOfCases(Ps,PtnTp,[Trm,..SoFar],RRp)
    }
  } in do{
--    logMsg("check case gov $(G) against $(ETp)");
    ETp .= newTypeVar("_e");
    
    Gv <- typeOfExp(G,ETp,Env,Path,Rp);
    Gc <- typeOfCases(Cases,ETp,[],Rp);
    valis csexp(Lc,Gv,Gc,Tp)
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,S,F,T) ^= isSlice(A) =>
    typeOfExp(ternary(Lc,"_slice",S,F,T),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,I) ^= isCellRef(A) => do{
    RTp .= refType(Tp);
    Acc <- typeOfExp(nme(Lc,"!!"),funType([RTp],Tp),Env,Path,Rp);
    Cell <- typeOfExp(I,RTp,Env,Path,Rp);
    valis apply(Lc,Acc,tple(Lc,[Cell]),Tp)
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,I) ^= isRef(A) =>
    typeOfExp(roundTerm(Lc,nme(Lc,"_cell"),[I]),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (_,[El]) ^= isTuple(A) && ! _ ^= isTuple(El) =>
    typeOfExp(El,Tp,Env,Path,Rp).

  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs .= genTpVars(Els);
    checkType(A,tupleType(Tvs),Tp,Env,Rp);
    Ptns <- typeOfExps(Els,Tvs,[],Env,Path,Rp);
    valis tple(Lc,Ptns)
  }
  typeOfExp(A,Tp,Env,Path,Rp) where hasPromotion(A) =>
    typeOfExp(promoteOption(A),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,_,Ar,C,R) ^= isLambda(A) => do{
--    logMsg("check lambda $(A)");
    At .= newTypeVar("_A");
    Rt .= newTypeVar("_R");
--    logMsg("check lambda arg $(Ar)");
    (As,E0) <- typeOfArgPtn(Ar,At,Env,Path,Rp);
    LName .= genSym(Path++"λ");
    if Cnd ^= C then {
--      logMsg("check lambda cond $(Cnd)");
      (Cond,E1) <- checkCond(Cnd,E0,Path,Rp);
      Rep <- typeOfExp(R,Rt,E1,Path,Rp);
      checkType(A,fnType(At,Rt),Tp,Env,Rp);
      valis lambda(LName,[eqn(Lc,As,some(Cond),Rep)],Tp)
    } else{
      Rep <- typeOfExp(R,Rt,E0,Path,Rp);
      checkType(A,fnType(At,Rt),Tp,Env,Rp);
--    logMsg("lambda return: $(Rep)\:$(typeOf(Rep))");
      valis lambda(LName,[eqn(Lc,As,.none,Rep)],Tp)
    }
  }
  typeOfExp(A,Tp,Env,Pth,Rp) where (Lc,Els) ^= isTheta(A) => do{
    (Q,ETp) .= evidence(Tp,Env);
    FaceTp .= faceOfType(ETp,Env);
--    logMsg("checking theta record, expected type $(Tp), face $(FaceTp)");
    (Cx,Face) .= deConstrain(FaceTp);
    Base .= declareConstraints(Lc,Cx,declareTypeVars(Q,pushScope(Env)));
    Path .= genNewName(Pth,"θ");
    (Defs,ThEnv,ThetaTp) <- thetaEnv(Lc,Path,Els,Face,Base,Rp,.deFault);
    if sameType(ThetaTp,ETp,Env) then{
--      logMsg("building record from theta, $(ThEnv)");
      formTheta(Lc,.none,deRef(faceOfType(Tp,ThEnv)),ThEnv,sortDefs(multicat(Defs)),
	reConstrainType(Cx,ThetaTp),Rp)
    }
    else
    throw reportError(Rp,"type of theta: $(ThetaTp)\nnot consistent with \n$(Tp)",Lc)
  }
  typeOfExp(A,Tp,Env,Pth,Rp) where (Lc,Els) ^= isQTheta(A) => do{
    (Q,ETp) .= evidence(Tp,Env);
    FaceTp .= faceOfType(ETp,Env);
    (Cx,Face) .= deConstrain(FaceTp);
    Base .= declareConstraints(Lc,Cx,declareTypeVars(Q,pushScope(Env)));
    Path .= genNewName(Pth,"θ");
    (Defs,ThEnv,ThetaTp) <- recordEnv(Lc,Path,Els,Face,Base,Rp,.deFault);
    if sameType(ThetaTp,Tp,Env) then{
      formRecordExp(Lc,.none,deRef(faceOfType(Tp,ThEnv)),ThEnv,[Defs],reConstrainType(Cx,ThetaTp),Rp)
    }
    else
    throw reportError(Rp,"type of qtheta: $(ThetaTp)\nnot consistent with \n$(Tp)",Lc)
  }
  typeOfExp(A,Tp,Env,Pth,Rp) where (Lc,Op,Els) ^= isLabeledTheta(A) && (_,Nm)^=isName(Op) => do{
--    logMsg("checking theta term $(Op){$(Els)} against $(Tp)");
 
    FceTp .= newTypeVar("_");
    ConTp .= consType(FceTp,Tp);
    Fun <- typeOfExp(Op,ConTp,Env,Pth,Rp);
--    logMsg("type of $(Op) |- $(ConTp)");

    (Q,ETp) .= evidence(FceTp,Env);
    FaceTp .= faceOfType(ETp,Env);
    (Cx,Face) .= deConstrain(FaceTp);
    Base .= declareConstraints(Lc,Cx,declareTypeVars(Q,pushScope(Env)));
    
--    logMsg("checking labeled theta, expected type $(Face)");
    (Defs,ThEnv,ThetaTp) <- thetaEnv(Lc,genNewName(Pth,"θ"),Els,Face,Base,Rp,.deFault);
--    logMsg("resulting theta type $(ThetaTp)");
--    logMsg("check against expected face $(Face)");
    if sameType(ThetaTp,Face,Env) then{
--      logMsg("building record from theta, $(ThEnv)");
      formTheta(Lc,some(Nm),Face,ThEnv,sortDefs(multicat(Defs)),Tp,Rp)
    }
    else
    throw reportError(Rp,"type of theta: $(ThetaTp)\nnot consistent with \n$(Op)\:$(ConTp) ",Lc)
  }
  typeOfExp(A,Tp,Env,Pth,Rp) where (Lc,Op,Els) ^= isLabeledRecord(A) && (_,Nm)^=isName(Op) => do{
--    logMsg("checking record term $(Op)\{.$(Els).} against $(Tp)");
 
    FceTp .= newTypeVar("_");
    ConTp .= consType(FceTp,Tp);
    Fun <- typeOfExp(Op,ConTp,Env,Pth,Rp);
--    logMsg("type of $(Op) |- $(ConTp)");

    (Q,ETp) .= evidence(FceTp,Env);
    FaceTp .= faceOfType(ETp,Env);
    (Cx,Face) .= deConstrain(FaceTp);
    Base .= declareConstraints(Lc,Cx,declareTypeVars(Q,pushScope(Env)));
    
--    logMsg("checking theta record, expected type $(Tp), face $(Face)");
    (Defs,ThEnv,ThetaTp) <- recordEnv(Lc,genNewName(Pth,"θ"),Els,Face,Base,Rp,.deFault);
    if sameType(ThetaTp,Face,Env) then{
--      logMsg("building record from theta, $(ThEnv)");
      formRecordExp(Lc,some(Nm),Face,ThEnv,sortDefs(Defs),Tp,Rp)
    }
    else
    throw reportError(Rp,"type of theta: $(ThetaTp)\nnot consistent with \n$(Op)\:$(ConTp) ",Lc)
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
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Els,Bnd) ^= isLetDef(A) => do{
--    logMsg("checking let exp");

    (Defs,ThEnv,ThetaTp)<-thetaEnv(Lc,genNewName(Path,"Γ"),Els,faceType([],[]),Env,Rp,.priVate);
    
    BndEnv .= pushFace(ThetaTp,Lc,Env);
--    logMsg("let env groups $(Defs)");
--    logMsg("sub env is $(BndEnv)");
    El <- typeOfExp(Bnd,Tp,BndEnv,Path,Rp);

    Sorted .= sortDefs(multicat(Defs));
    
    valis foldRight((Gp,I)=>letRec(Lc,Gp,I),El,Sorted)
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Els,Bnd) ^= isQLetDef(A) => do{
    (Defs,ThEnv,ThetaTp)<-recordEnv(Lc,genNewName(Path,"Γ"),Els,faceType([],[]),Env,Rp,.priVate);
    
    BndEnv .= pushFace(ThetaTp,Lc,Env);
    El <- typeOfExp(Bnd,Tp,BndEnv,Path,Rp);

    Sorted .= sortDefs(Defs);
    
    valis foldRight((Gp,I)=>letExp(Lc,Gp,I),El,Sorted)
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Op,Args) ^= isRoundTerm(A) =>
    typeOfRoundTerm(Lc,Op,Args,Tp,Env,Path,Rp).
  typeOfExp(A,_,_,_,Rp) => other(reportError(Rp,"cannot type check $(A)",locOf(A))).

  typeOfExps:(cons[ast],cons[tipe],cons[canon],dict,string,reports) =>
    either[reports,cons[canon]].
  typeOfExps([],[],Els,Env,_,_) => either(reverse(Els)).
  typeOfExps([P,..Ps],[T,..Ts],Els,Env,Path,Rp) => do{
    Trm <- typeOfExp(P,T,Env,Path,Rp);
    typeOfExps(Ps,Ts,[Trm,..Els],Env,Path,Rp)
  }

  typeOfRoundTerm:(locn,ast,cons[ast],tipe,dict,string,reports) => either[reports,canon].
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
    if isIterable(A) then {
      Itr <- makeIterableGoal(A,Rp);
      checkGoal(Itr,Env,Path,Rp)
    } else{
      checkGoal(A,Env,Path,Rp)
    }
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
  checkGoal(A,Env,Path,Rp) where (Lc,L,R) ^= isImplies(A) => do{
    (Lhs,E0) <- checkGoal(L,Env,Path,Rp);
    (Rhs,E1) <- checkGoal(R,E0,Path,Rp);
    valis (implies(Lc,Lhs,Rhs),Env)
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
  checkGoal(A,Env,Path,Rp) where (_,[Inner]) ^= isTuple(A) =>
    checkGoal(Inner,Env,Path,Rp).
  checkGoal(A,Env,Path,Rp) => do{
    Exp <- typeOfExp(A,boolType,Env,Path,Rp);
    valis (Exp,Env)
  }

  checkDo:(ast,tipe,dict,string,reports) => either[reports,canon].
  checkDo(Actn,Tp,Env,Path,Rp) => do{
    if isSimpleAction(Actn) then{
--      logMsg("using macro process to simplify $(Actn)");
      Simple <- makeAction(Actn,.none,Rp);
--      logMsg("macroed action is $(Simple)");
      typeOfExp(Simple,Tp,Env,Path,Rp)
    } else {
      throw reportError(Rp,"cannot process action $(Actn)",locOf(Actn))
    }
  }

  checkType:(ast,tipe,tipe,dict,reports) => either[reports,()].
  checkType(_,Actual,Expected,Env,_) where sameType(Actual,Expected,Env) => either(()).
  checkType(A,ATp,ETp,_,Rp) => other(reportError(
      Rp,"$(A)\:$(ATp) not consistent with expected type $(ETp)",locOf(A))).

  genTpVars:(cons[ast]) => cons[tipe].
  genTpVars(Els) => (Els//(_)=>newTypeVar("_v")).

  genArgTps:(ast) => cons[tipe].
  genArgTps(A) where (_,Ar,_) ^= isWhere(A) =>
    genArgTps(Ar).
  genArgTps(A) where (_,Els) ^= isTuple(A) =>
      genTpVars(Els).

  checkAbstraction:(locn,ast,ast,tipe,dict,string,reports) => either[reports,canon].
  checkAbstraction(Lc,B,C,Tp,Env,Path,Rp) => do{
--    logMsg("checking abstraction [ $(B) | $(C) ] expected type $(Tp)");
    Test <- makeAbstraction(Lc,B,C,Rp);
--    logMsg("macrod abstraction $(Test)");
    typeOfExp(Test,Tp,Env,Path,Rp)
  }
}
