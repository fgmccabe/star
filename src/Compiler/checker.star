star.compiler.checker{
  import star.

  import star.pkg.
  import star.repo.

  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.canondeps.
  import star.compiler.dependencies.
  import star.compiler.dict.
  import star.compiler.dict.mgt.
  import star.compiler.errors.
  import star.compiler.freshen.
  import star.compiler.impawt.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.resolve.
  import star.compiler.types.
  import star.compiler.typeparse.
  import star.compiler.unify.
  import star.compiler.wff.

  -- package level of type checker

  public checkPkg:all r ~~ repo[r],display[r]|:(r,pkg,ast,compilerOptions,reports) => either[reports,(pkgSpec,cons[canonDef])].
  checkPkg(Repo,Pkge,P,Opts,Rp) => do{
    Base .= stdDict;
    if (Lc,Pk,Els) ^= isQBrTerm(P) && Pkg .= pkgeName(Pk) then{
      if compatiblePkg(Pkg,Pkge) then{
	(Imports,Stmts) .= collectImports(Els,[],[]);
	(AllImports,Decls) <- importAll(Imports,Repo,[],[],Rp);
	PkgEnv <- declareDecls(Decls,some(Lc),Base,Rp);
	
	PkgNm .= packageName(Pkg);

	(Vis,Opens,Annots,Gps) <- dependencies(Stmts,Rp);

	if ~isEmpty(Opens) then
	  raise reportError(Rp,"open statements $(Opens) not supported",Lc);
    
	(Defs,ThEnv) <- checkGroups(Gps,[],faceType([],[]),Annots,PkgEnv,PkgNm,Rp);
	RDefs <- overloadEnvironment(Defs,ThEnv,Rp);
	
	(ExDecls,LclDecl,PkgDefs).=genDecls(RDefs*,completePublic(Vis,PkgNm));

	valis (pkgSpec(Pkge,Imports,ExDecls),PkgDefs)
      }
      else
      raise reportError(Rp,"package name $(Pkg) does not match expected $(Pkge)",locOf(P))
    } else
    raise reportError(Rp,"invalid package structure",locOf(P))
  }

  completePublic:(cons[(defnSp,visibility)],string) =>
    cons[(defnSp,visibility)].
  completePublic([],_) => [].
  completePublic([(conSp(Nm),.pUblic),..Ds],Pth) =>
    [(tpSp(qualifiedName(Pth,.typeMark,Nm)),.pUblic),
      (varSp(qualifiedName(Pth,.valMark,Nm)),.pUblic),
      (conSp(Nm),.pUblic),..completePublic(Ds,Pth)].
  completePublic([S,..Ds],Pth) => [S,..completePublic(Ds,Pth)].

  genDecls:(cons[canonDef],cons[(defnSp,visibility)]) => (cons[decl],cons[decl],cons[canonDef]).
  genDecls([],_) => ([],[],[]).

  exportedFields:(cons[cons[canonDef]],cons[(defnSp,visibility)],visibility) => cons[(string,tipe)].
  exportedFields(Defs,Vis,DVz) =>
    { (Nm,Tp) |
	DD in Defs && D in DD &&
	    ((varDef(_,Nm,_,_,_,Tp) .=D && isVisible(varSp(Nm),Vis,DVz)) ||
	      (cnsDef(_,Nm,_,Tp) .=D && isVisible(cnsSp(Nm),Vis,DVz)) ||
	      (implDef(_,N,Nm,_,_,Tp) .=D && isVisible(implSp(N),Vis,DVz)))}.

  isVisible:(defnSp,cons[(defnSp,visibility)],visibility) => boolean.
  isVisible(Sp,Vis,DVz) => {? (Sp,V) in Vis && V >= DVz ?}.

  exportedTypes:(cons[cons[canonDef]],cons[(defnSp,visibility)],visibility) =>
    cons[(string,tipe)].
  exportedTypes(Defs,Vis,DVz) => { (Nm,ExTp) |
      DD in Defs && typeDef(_,Nm,_,ExTp) in DD && (tpSp(Nm),V) in Vis && V>=DVz}.

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
  lookInGroup([varDef(Lc,Nm,_,_,Cx,Tp),..Gp],Nm,_) => some(vr(some(Lc),Nm,Tp)).
  lookInGroup([cnsDef(Lc,Nm,FullNm,Tp),..Gp],Nm,_) => some(enm(Lc,FullNm,Tp)).
  lookInGroup([implDef(Lc,Nm,FullNm,Val,Cx,Tp),..Gp],FullNm,_) =>
    some(vr(some(Lc),FullNm,Tp)).
  lookInGroup([_,..Gp],Nm,Gps) => lookInGroup(Gp,Nm,Gps).

  thetaEnv:(locn,string,cons[ast],tipe,dict,reports,visibility) =>
    either[reports,(cons[cons[canonDef]],dict)].
  thetaEnv(Lc,Pth,Stmts,Face,Env,Rp,DefViz) => do{
    (Vis,Opens,Annots,Gps) <- dependencies(Stmts,Rp);

    if ~isEmpty(Opens) then
      raise reportError(Rp,"open statements $(Opens) not supported",Lc);
    
    Base .= pushFace(Face,Lc,Env);
    checkGroups(Gps,[],Face,Annots,Base,Pth,Rp)
  }

  recordEnv:(locn,string,cons[ast],tipe,dict,reports,visibility) =>
    either[reports,(cons[canonDef],dict)].

  recordEnv(Lc,Pth,Stmts,Face,Env,Rp,DefViz) => do{
    (Vis,Opens,Annots,Gps) <- dependencies(Stmts,Rp);
    letGroup(Gps*,[],Pth,Face,Annots,Env,Rp);
  }

  letGroup:(cons[defnSpec],cons[canonDef],string,tipe,
    map[string,ast],dict,reports) =>
    either[reports,(cons[canonDef],dict)].
  letGroup([],Defs,_,_,_,Env,_) => either((reverse(Defs),Env)).
  letGroup([Df,..Ds],So,Pth,Face,Annots,Env,Rp) => do{
    TEnv <- parseAnnotations([Df],Face,Annots,Env,Rp);
    (Defn,E0) <- checkDefn(Df,TEnv,Env,Pth,Rp);
    letGroup(Ds,[Defn,..So],Pth,Face,Annots,E0,Rp)
  }

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
    valis declareContract(some(Lc),ConNm,Contract,Env)
  }
  checkTypeDefn(defnSpec(implSp(Nm),Lc,[St]),Env,Path,Rp) => do {
    if (_,Q,C,H,B) ^= isImplementationStmt(St) then{
      BV <- parseBoundTpVars(Q,Rp);
      Cx <- parseConstraints(C,BV,Env,Rp);
      Cn <- parseContractConstraint(BV,H,Env,Rp);
      ConName .= localName(tpName(Cn),.typeMark);
      if Con ^= findContract(Env,ConName) then{
	(_,typeExists(ConTp,_)) .= freshen(Con,Env);
	if sameType(ConTp,Cn,Env) then {
	  FullNm .= implementationName(ConTp);
	  ImplTp .= rebind(BV,reConstrainType(Cx,ConTp),Env);
	  valis declareVar(FullNm,some(Lc),ImplTp,.none,
	    declareImplementation(some(Lc),FullNm,FullNm,ImplTp,Env))
	}
	else{
	  raise reportError(Rp,"implementation type $(Cn) not consistent with contract type $(ConTp)",Lc)
	}
      } else{
	raise reportError(Rp,"Contract $(ConName) not known",Lc)
      }
    }
    else
    raise reportError(Rp,"not a valid implementation statement",Lc)
  }
  checkTypeDefn(_,Env,_,_) default => either(Env).
  
  checkGroups:(cons[cons[defnSpec]],cons[cons[canonDef]],
    tipe,map[string,ast],dict,string,reports) =>
    either[reports,(cons[cons[canonDef]],dict)].
  checkGroups([],Gps,_,_,Env,_,Rp) => either((reverse(Gps),Env)).
  checkGroups([G,..Gs],Gx,Face,Annots,Env,Path,Rp) => do{
    TmpEnv <- parseAnnotations(G,Face,Annots,Env,Rp);
    (Gp,Ev) <- checkGroup(G,TmpEnv,TmpEnv,Path,Rp);
    checkGroups(Gs,[Gp,..Gx],Face,Annots,Ev,Path,Rp)
  }

  parseAnnotations:(cons[defnSpec],tipe,map[string,ast],dict,reports) =>
    either[reports,dict].
  parseAnnotations([],_,_,Env,_) => either(Env).
  parseAnnotations([defnSpec(varSp(Nm),Lc,Stmts),..Gs],Fields,Annots,Env,Rp) => do{
    Tp <- parseAnnotation(Nm,Lc,Stmts,Fields,Annots,Env,Rp);
    parseAnnotations(Gs,Fields,Annots,declareVar(Nm,some(Lc),Tp,faceOfType(Tp,Env),Env),Rp)
  }
  parseAnnotations([_,..Gs],Fields,Annots,Env,Rp) => parseAnnotations(Gs,Fields,Annots,Env,Rp).

  parseAnnotation:(string,locn,cons[ast],tipe,map[string,ast],dict,reports) =>
    either[reports,tipe].
  parseAnnotation(Nm,_,_,_,Annots,Env,Rp) where T ^= Annots[Nm] => do{
    parseType([],T,Env,Rp)
  }.
  parseAnnotation(Nm,_,_,faceType(Vrs,_),_,_,_) where Tp^={!Tp|(Nm,Tp) in Vrs!} => either(Tp).
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
  allFunDefs(Dfs) => {? D in Dfs *> varDef(_,_,_,lambda(_,_,_),_,_).=D ?}.

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
    (Cx,VarTp) .= deConstrain(ETp);
    Es .= declareConstraints(some(Lc),Cx,declareTypeVars(Q,Outer));
    if (_,Lhs,R) ^= isDefn(Stmt) then{
      Val <- typeOfExp(R,VarTp,Es,Path,Rp);
      FullNm .= qualifiedName(Path,.valMark,Nm);
      valis (varDef(Lc,Nm,FullNm,Val,Cx,Tp),
	declareVar(Nm,some(Lc),Tp,faceOfType(Tp,Env),Env))
    }
    else{
      raise reportError(Rp,"bad definition $(Stmt)",Lc)
    }
  }.
  checkDefn(defnSpec(tpSp(TpNm),Lc,[St]),Env,_,Path,Rp) =>
    parseTypeDef(TpNm,St,Env,Path,Rp).
  checkDefn(defnSpec(cnsSp(CnNm),Lc,[St]),Env,_,Path,Rp) =>
    parseConstructor(CnNm,St,Env,Path,Rp).
  checkDefn(defnSpec(conSp(ConNm),Lc,[St]),Env,_,Path,Rp) => do{
    Contract <- parseContract(St,Env,Path,Rp);
    valis (conDef(Lc,ConNm,ConNm,Contract),
      declareContract(some(Lc),ConNm,Contract,Env))
  }
  checkDefn(defnSpec(implSp(Nm),Lc,[St]),Env,Outer,Path,Rp) => do {
    if (_,Q,C,H,B) ^= isImplementationStmt(St) then
      checkImplementation(Lc,Nm,Q,C,H,B,Env,Outer,Path,Rp)
    else
      raise reportError(Rp,"not a valid implementation statement",Lc)
  }

  checkFunction:(string,tipe,locn,cons[ast],dict,dict,string,reports) =>
    either[reports,(canonDef,dict)].
  checkFunction(Nm,Tp,Lc,Stmts,Env,Outer,Path,Rp) => do{
    (Q,ETp) .= evidence(Tp,Env);
    (Cx,ProgramTp) .= deConstrain(ETp);
    Es .= declareConstraints(some(Lc),Cx,declareTypeVars(Q,Env));
    Rls <- processEqns(Stmts,deRef(ProgramTp),[],.none,Es,
      declareConstraints(some(Lc),Cx,declareTypeVars(Q,Outer)),Path,Rp);
    FullNm .= qualifiedName(Path,.valMark,Nm);
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
	raise reportError(Rp,"cannot have more than one default, other one at $(locOf(DRl))",
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
    BV <- parseBoundTpVars(Q,Rp);
    Cx <- parseConstraints(C,BV,Env,Rp);
    Cn <- parseContractConstraint(BV,H,Env,Rp);
    ConName .= localName(tpName(Cn),.typeMark);
    
    if Con ^= findContract(Env,ConName) then{
      (_,typeExists(ConTp,ConFaceTp)) .= freshen(Con,Env);
      if sameType(ConTp,Cn,Env) then {
	Es .= declareConstraints(some(Lc),Cx,declareTypeVars(BV,Outer));
	Impl <- typeOfExp(B,ConFaceTp,Es,Path,Rp);
	ImplVrNm .= qualifiedName(Path,.valMark,implementationName(ConTp));
	ImplTp .= rebind(BV,reConstrainType(Cx,ConTp),Es);
	valis (implDef(Lc,Nm,ImplVrNm,Impl,Cx,ImplTp),
	  declareImplementation(some(Lc),implementationName(ConTp),
	    ImplVrNm,ImplTp,Env))
      }
      else{
	raise reportError(Rp,"implementation type $(Cn) not consistent with contract type $(ConTp)",Lc)
      }
    } else{
      raise reportError(Rp,"Contract $(ConName) not known",Lc)
    }
  }
    
  typeOfPtn:(ast,tipe,dict,string,reports) => either[reports,(canon,dict)].
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,"_") ^= isName(A) =>
    either((vr(some(Lc),genSym("_"),Tp),Env)).
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) &&
      varDefined(Id,Env) =>
    typeOfPtn(mkWhereEquality(A),Tp,Env,Path,Rp).
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) => do{
    Ev .= declareVar(Id,some(Lc),Tp,faceOfType(Tp,Env),Env);
    valis (vr(some(Lc),Id,Tp),Ev)
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
  typeOfPtn(A,Tp,Env,Path,Rp) where (_,[El]) ^= isTuple(A) && ~ _ ^= isTuple(El) =>
    typeOfPtn(El,Tp,Env,Path,Rp).
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs .= genTpVars(Els);
    checkType(A,tupleType(Tvs),Tp,Env,Rp);
    (Ptns,Ev) <- typeOfPtns(Els,Tvs,[],Env,Path,Rp);
    valis (tple(Lc,Ptns),Ev)
  }
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Op,Els) ^= isRoundTerm(A) => do{
    At .= newTypeVar("A");
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
    if Var ^= findVar(Lc,Id,Env) then{
      if sameType(Tp,typeOf(Var),Env) then {
	valis Var
      } else
      raise reportError(Rp,"variable $(Id)\:$(typeOf(Var)) not consistent with expected type: $(Tp)",Lc)
    }
    else
    raise reportError(Rp,"variable $(Id) not defined. Expecting a $(Tp)",locOf(A)).
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
	RTp .= newTypeVar("R");
	Rc <- typeOfExp(R,RTp,Env,Path,Rp);
	valis dot(Lc,Rc,Fld,Tp)
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
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,G,Cases) ^= isCase(A) &&
    ETp .= newTypeVar("_e") => let{.
    checkRle(E,RRp) where (CLc,IsDeflt,H,C,R) ^= isLambda(E) => do{
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
    checkRle(E,RRp) => other(reportError(RRp,"invalid case $(E)",locOf(E))).

    pullWhere:(canon,option[canon]) => (canon,option[canon]).
    pullWhere(whr(WLc,Vl,Cn),Gl) where (Val,G1) .= pullWhere(Vl,Gl) =>
      (Val,mergeGoal(WLc,some(Cn),G1)).
    pullWhere(Exp,Gl) default => (Exp,Gl).

    mergeGoal:(locn,option[canon],option[canon])=>option[canon].
    mergeGoal(_,Gl,.none) => Gl.
    mergeGoal(_,.none,Gl) => Gl.
    mergeGoal(Lc,some(Gl),some(H)) => some(conj(Lc,Gl,H)).

    typeOfCases:(cons[ast],cons[equation],reports) => either[reports,cons[equation]].
    typeOfCases([],Els,_) => either(reverse(Els)).
    typeOfCases([Cs,..Ps],SoFar,RRp) => do{
      Trm <- checkRle(Cs,Rp);
      typeOfCases(Ps,[Trm,..SoFar],RRp)
    }
  .} in do{
    Gv <- typeOfExp(G,ETp,Env,Path,Rp);
    Gc <- typeOfCases(Cases,[],Rp);
    valis csexp(Lc,Gv,Gc,Tp)
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,S,F,T) ^= isSlice(A) =>
    typeOfExp(ternary(Lc,"_slice",S,F,T),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,I) ^= isCellRef(A) => do{
    RTp .= refType(Tp);
    Acc <- typeOfExp(nme(Lc,"!"),funType([RTp],Tp),Env,Path,Rp);
    Cell <- typeOfExp(I,RTp,Env,Path,Rp);
    valis apply(Lc,Acc,tple(Lc,[Cell]),Tp)
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,I) ^= isRef(A) =>
    typeOfExp(roundTerm(Lc,nme(Lc,"_cell"),[I]),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (_,[El]) ^= isTuple(A) && ~ _ ^= isTuple(El) =>
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
    At .= newTypeVar("_A");
    Rt .= newTypeVar("_R");
    (As,E0) <- typeOfArgPtn(Ar,At,Env,Path,Rp);
    LName .= genSym(Path++"λ");
    if Cnd ^= C then {
      (Cond,E1) <- checkCond(Cnd,E0,Path,Rp);
      Rep <- typeOfExp(R,Rt,E1,Path,Rp);
      checkType(A,fnType(At,Rt),Tp,Env,Rp);
      valis lambda(LName,[eqn(Lc,As,some(Cond),Rep)],Tp)
    } else{
      Rep <- typeOfExp(R,Rt,E0,Path,Rp);
      checkType(A,fnType(At,Rt),Tp,Env,Rp);
      valis lambda(LName,[eqn(Lc,As,.none,Rep)],Tp)
    }
  }
/*  typeOfExp(A,Tp,Env,Pth,Rp) where (Lc,Els) ^= isTheta(A) => do{
    (Q,ETp) .= evidence(Tp,Env);
    FaceTp .= faceOfType(ETp,Env);
    (Cx,Face) .= deConstrain(FaceTp);
  Base .= declareConstraints(some(Lc),Cx,declareTypeVars(Q,pushScope(Env)));
    Path .= genNewName(Pth,"θ");
    (Defs,ThEnv) <- thetaEnv(Lc,Path,Els,Face,Base,Rp,.deFault);
    if sameType(ThetaTp,ETp,Env) then{
      formTheta(Lc,.none,deRef(faceOfType(Tp,ThEnv)),ThEnv,sortDefs(Defs*),
	reConstrainType(Cx,ThetaTp),Rp)
    }
    else
    raise reportError(Rp,"type of theta: $(ThetaTp)\nnot consistent with \n$(Tp)",Lc)
  }
  typeOfExp(A,Tp,Env,Pth,Rp) where (Lc,Els) ^= isQTheta(A) => do{
    (Q,ETp) .= evidence(Tp,Env);
    FaceTp .= faceOfType(ETp,Env);
    (Cx,Face) .= deConstrain(FaceTp);
  Base .= declareConstraints(some(Lc),Cx,declareTypeVars(Q,pushScope(Env)));
    Path .= genNewName(Pth,"θ");
    (Defs,ThEnv,ThetaTp) <- recordEnv(Lc,Path,Els,Face,Base,Rp,.deFault);
    if sameType(ThetaTp,Tp,Env) then{
      formRecordExp(Lc,.none,deRef(faceOfType(Tp,ThEnv)),ThEnv,[Defs],reConstrainType(Cx,ThetaTp),Rp)
    }
    else
    raise reportError(Rp,"type of qtheta: $(ThetaTp)\nnot consistent with \n$(Tp)",Lc)
  }

  typeOfExp(A,Tp,Env,Pth,Rp) where (Lc,Op,Els) ^= isLabeledTheta(A) && (_,Nm)^=isName(Op) => do{
    FceTp .= newTypeVar("_");
    ConTp .= consType(FceTp,Tp);
    Fun <- typeOfExp(Op,ConTp,Env,Pth,Rp);

    (Q,ETp) .= evidence(FceTp,Env);
    FaceTp .= faceOfType(ETp,Env);
    (Cx,Face) .= deConstrain(FaceTp);
  Base .= declareConstraints(some(Lc),Cx,declareTypeVars(Q,pushScope(Env)));
    
    (Defs,ThEnv,ThetaTp) <- thetaEnv(Lc,genNewName(Pth,"θ"),Els,Face,Base,Rp,.deFault);
    if sameType(ThetaTp,Face,Env) then{
      formTheta(Lc,some(Nm),Face,ThEnv,sortDefs(Defs*),Tp,Rp)
    }
    else
    raise reportError(Rp,"type of theta: $(ThetaTp)\nnot consistent with \n$(Op)\:$(ConTp) ",Lc)
  }
  typeOfExp(A,Tp,Env,Pth,Rp) where (Lc,Op,Els) ^= isLabeledRecord(A) && (_,Nm)^=isName(Op) => do{
    FceTp .= newTypeVar("_");
    ConTp .= consType(FceTp,Tp);
    Fun <- typeOfExp(Op,ConTp,Env,Pth,Rp);
    (Q,ETp) .= evidence(FceTp,Env);
    FaceTp .= faceOfType(ETp,Env);
    (Cx,Face) .= deConstrain(FaceTp);
  Base .= declareConstraints(some(Lc),Cx,declareTypeVars(Q,pushScope(Env)));
    
    (Defs,ThEnv,ThetaTp) <- recordEnv(Lc,genNewName(Pth,"θ"),Els,Face,Base,Rp,.deFault);
    if sameType(ThetaTp,Face,Env) then{
      formRecordExp(Lc,some(Nm),Face,ThEnv,sortDefs(Defs),Tp,Rp)
    }
    else
    raise reportError(Rp,"type of theta: $(ThetaTp)\nnot consistent with \n$(Op)\:$(ConTp) ",Lc)
  }

  typeOfExp(A,Tp,Env,Pth,Rp) where (Lc,Rc,Upd) ^= isRecordUpdate(A) => do{
    Rec <- typeOfExp(Rc,Tp,Env,Pth,Rp);
    UpTp .= newTypeVar("_");
    Update <- typeOfExp(Upd,UpTp,Env,Pth,Rp);
    faceType(RecFlds,_) ^= faceOfType(Tp,Env);
    faceType(UpFlds,_) ^= faceOfType(UpTp,Env);
    for (F,TU) in UpFlds do{
      if (F,TR) in RecFlds then{
	if ~sameType(TU,TR,Env) then
	  raise reportError(Rp,"replacement for field $(F)\:$(TU) not consistent with record field $(TR)",Lc)
      } else
      raise reportError(Rp,"replacement for field $(F)\:$(TU) does not exist in record $(Rec)",Lc)
    };
    valis update(Lc,Rec,Update)
  }
*/
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Els,Bnd) ^= isLetRecDef(A) => do{
    (Defs,ThEnv)<-thetaEnv(Lc,genNewName(Path,"Γ"),Els,faceType([],[]),Env,Rp,.priVate);
    
    El <- typeOfExp(Bnd,Tp,ThEnv,Path,Rp);

    Sorted .= sortDefs(Defs*);
    
    valis foldRight((Gp,I)=>letRec(Lc,Gp,I),El,Sorted)
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Els,Bnd) ^= isLetDef(A) => do{
    (Defs,ThEnv)<-recordEnv(Lc,genNewName(Path,"Γ"),Els,faceType([],[]),Env,Rp,.priVate);
    
    El <- typeOfExp(Bnd,Tp,ThEnv,Path,Rp);

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
    Vrs .= genTpVars(As);
    At .= tupleType(Vrs);
    FFTp .= newTypeFun("_F",2);
    ExTp .= mkTypeExp(FFTp,[At,Tp]);
    Fun <- typeOfExp(Op,ExTp,Env,Path,Rp);
    Args <- typeOfExps(As,Vrs,[],Env,Path,Rp);
    if sameType(FFTp,tpFun("=>",2),Env) || sameType(FFTp,tpFun("<=>",2),Env) then{
      valis apply(Lc,Fun,tple(Lc,Args),Tp)
    } else
      raise reportError(Rp,"type of $(Op)\:$(ExTp) not consistent with $(fnType(At,Tp))",Lc)
  }

  typeOfArgExp:(ast,tipe,dict,string,reports) => either[reports,(canon,dict)].
  typeOfArgExp(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs .= genTpVars(Els);
    checkType(A,tupleType(Tvs),Tp,Env,Rp);
    (Ptns,Ev) <- typeOfPtns(Els,Tvs,[],Env,Path,Rp);
    valis (tple(Lc,Ptns),Ev)
  }

  checkCond:(ast,dict,string,reports) => either[reports,(canon,dict)].
  checkCond(A,Env,Path,Rp) => checkGoal(A,Env,Path,Rp).

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
}
