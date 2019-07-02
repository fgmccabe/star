star.compiler.checker{
  import star.

  import star.pkg.
  import star.repo.

  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.dependencies.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.impawt.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.resolve.
  import star.compiler.types.
  import star.compiler.typeparse.
  import star.compiler.freshen.
  import star.compiler.unify.
  import star.compiler.wff.

  -- package level of type checker

  public checkPkg:all r ~~ repo[r]|:(r,ast,dict,reports) => either[reports,(pkgSpec,list[list[canonDef]],list[canon])].
  checkPkg(Repo,P,Base,Rp) => do{
--    logMsg("processing package $(P)");
    if (Lc,Pk,Els) ^= isBrTerm(P) && Pkg ^= pkgName(Pk) then{
      (Imports,Stmts) <- collectImports(Els,[],[],Rp);
      (PkgEnv,AllImports) <- importAll(Imports,Repo,Base,[],[],Rp);
--      logMsg("imports found $(AllImports)");
--      logMsg("pkg env after imports $(PkgEnv)");
      
      Path = packageName(Pkg);
      -- We treat a package specially, buts its essentially a theta record
      (Vis,Opens,Ots,Annots,Gps) <- dependencies(Stmts,Rp);
      
--      logMsg("Package $(Pkg), groups: $(Gps)");
      (Defs,ThEnv) <- checkGroups(Gps,[],faceType([],[]),Annots,PkgEnv,Path,Rp);
      Others <- checkOthers(Ots,[],ThEnv,Path,Rp);
      logMsg("Final Pkg dict $(ThEnv)");
      logMsg("Public names: $(Vis)");
      logMsg("Defs: $(Defs)");
      Contracts = [ D | DD in Defs && D in DD && conDef(_,Nm,_,_).=D && (conSp(Nm),pUblic) in Vis];
      logMsg("exported contracts: $(Contracts)");
      Fields = exportedFields(Defs,Vis,pUblic);
      logMsg("exported fields: $(Fields)");
      Impls = [ implSpec(some(ILc),INm,FllNm,ITp) |
	  DD in Defs &&
	      implDef(ILc,INm,FllNm,_,ITp) in DD &&
	      (implSp(INm),V) in Vis && V>=pUblic];
      logMsg("exported implementations $(Impls)");
      Types = exportedTypes(Defs,Vis,pUblic);
      logMsg("exported types: $(Types)");
      (RDefs,ROthers) <- overloadEnvironment(Defs,Others,ThEnv,Rp);
      valis (pkgSpec(Pkg,Imports,faceType(Fields,Types),Contracts,Impls),
	RDefs,ROthers)
    } else
    throw reportError(Rp,"invalid palckage structure",locOf(P))
  }

  exportedFields:(list[list[canonDef]],list[(defnSp,visibility)],visibility) => list[(string,tipe)].
  exportedFields(Defs,Vis,DVz) =>
    [ (Nm,Tp) |
	DD in Defs && D in DD &&
	    ((varDef(_,Nm,_,_,_,Tp) .=D && isVisible(varSp(Nm),Vis,DVz)) ||
	      (cnsDef(_,Nm,_,Tp) .=D && isVisible(cnsSp(Nm),Vis,DVz)))].

  isVisible:(defnSp,list[(defnSp,visibility)],visibility) => boolean.
  isVisible(Sp,Vis,DVz) => (Sp,V) in Vis && V >= DVz.

  exportedTypes:(list[list[canonDef]],list[(defnSp,visibility)],visibility) => list[(string,tipe)].
  exportedTypes(Defs,Vis,DVz) => [ (Nm,ExTp) |
      DD in Defs && typeDef(_,Nm,_,ExTp) in DD && (tpSp(Nm),V) in Vis && V>=DVz].

  typeOfTheta:(locn,list[ast],tipe,dict,string,reports) => either[reports,canon].
  typeOfTheta(Lc,Els,Tp,Env,Path,Rp) => do{
    (Q,ETp) = evidence(Tp,[],Env);
    FaceTp = faceOfType(Tp,Env);
    (Cx,Face) = deConstrain(FaceTp);
    Base = declareConstraints(Cx,declareTypeVars(Q,pushScope(Env)));
    (Defs,Others,ThEnv,ThetaTp) <- thetaEnv(Lc,Path,Els,Face,Base,Rp);
    if sameType(ThetaTp,Tp,Env) then
      valis theta(Lc,Path,true,Defs,Others,reConstrain(Cx,ThetaTp))
    else
    throw reportError(Rp,"type of theta: $(ThetaTp) not consistent with $(Tp)",Lc)
  }

  thetaEnv:(locn,string,list[ast],tipe,dict,reports) =>
    either[reports,(list[list[canonDef]],list[canon],dict,tipe)].
  thetaEnv(Lc,Pth,Els,Face,Env,Rp) => do{
    (Vis,Imports,Ots,Annots,Gps) <- dependencies(Els,Rp);
    logMsg("visible: $(Vis)");
    Base = pushFace(Face,Lc,Env);
    (Defs,ThEnv) <- checkGroups(Gps,[],Face,Annots,Base,Pth,Rp);
    Others <- checkOthers(Ots,[],ThEnv,Pth,Rp);

    logMsg("Defs: $(Defs)");
    PubVrTps = exportedFields(Defs,Vis,deFault);
    PubTps = exportedTypes(Defs,Vis,deFault);
    logMsg("exported fields $(PubVrTps)");
    valis (Defs,Others,ThEnv,faceType(PubVrTps,PubTps))
  }

  checkGroups:(list[list[defnSpec]],list[list[canonDef]],
    tipe,list[(string,ast)],dict,string,reports) =>
    either[reports,(list[list[canonDef]],dict)].
  checkGroups([],Gps,_,_,Env,_,Rp) => either((Gps,Env)).
  checkGroups([G,..Gs],Gx,Face,Annots,Env,Path,Rp) => do{
--    logMsg("check group $(G)");
    TmpEnv <- parseAnnotations(G,Face,Annots,Env,Rp);
    (Gp,Ev) <- checkGroup(G,[],TmpEnv,Path,Rp);
    checkGroups(Gs,[Gx..,Gp],Face,Annots,Ev,Path,Rp)
  }

  parseAnnotations:(list[defnSpec],tipe,list[(string,ast)],dict,reports) => either[reports,dict].
  parseAnnotations([],_,_,Env,_) => either(Env).
  parseAnnotations([defnSpec(varSp(Nm),Lc,Stmts),..Gs],Fields,Annots,Env,Rp) => do{
    Tp <- parseAnnotation(Nm,Lc,Stmts,Fields,Annots,Env,Rp);
--    logMsg("found type of $(Nm)\:$(Tp)");
    parseAnnotations(Gs,Fields,Annots,declareVar(Nm,some(Lc),Tp,Env),Rp)
  }
  parseAnnotations([_,..Gs],Fields,Annots,Env,Rp) => parseAnnotations(Gs,Fields,Annots,Env,Rp).

  parseAnnotation:(string,locn,list[ast],tipe,list[(string,ast)],dict,reports) =>
    either[reports,tipe].
  parseAnnotation(Nm,_,_,_,Annots,Env,Rp) where (Nm,T) in Annots =>
    parseType([],T,Env,Rp).
  parseAnnotation(Nm,_,_,faceType(Vrs,_),_,_,_) where (Nm,Tp) in Vrs => either(Tp).
  parseAnnotation(Nm,_,_,_,_,Env,Rp) where vrEntry(_,_,Tp) ^= isVar(Nm,Env) => either(Tp).
  parseAnnotation(Nm,Lc,Stmts,Fields,Annots,Env,Rp) =>
    guessStmtType(Stmts,Nm,Lc,Rp).

  guessStmtType([],Nm,Lc,Rp) => other(reportError(Rp,"$(Nm) not declared",Lc)).
  guessStmtType([St,.._],Nm,Lc,Rp) => do{
    if (_,H,_) ^= isEquation(St) && (_,Args,_) ^= splitHead(H) then {
      valis funType(genArgTps(Args),newTypeVar("_R"))
    } else if (_,_,_) ^= isAssignment(St) then{
      valis refType(newTypeVar("R"))
    }
    else{
      valis newTypeVar("_D")
    }
  }
      
  checkGroup:(list[defnSpec],list[canonDef],dict,string,reports) =>
    either[reports,(list[canonDef],dict)].
  checkGroup([],Defs,Env,_,_) => either((Defs,Env)).
  checkGroup([D,..Ds],Defs,Env,Path,Rp) => do{
    (Defn,E0) <- checkDefn(D,Env,Path,Rp);
    checkGroup(Ds,[Defs..,Defn],E0,Path,Rp)
  }

  checkDefn:(defnSpec,dict,string,reports) => either[reports,(canonDef,dict)].
  checkDefn(defnSpec(varSp(Nm),Lc,Stmts),Env,Path,Rp) where
      vrEntry(_,_,Tp) ^= isVar(Nm,Env) && areEquations(Stmts) =>
    checkFunction(Nm,Tp,Lc,Stmts,Env,Path,Rp).
  checkDefn(defnSpec(varSp(Nm),Lc,[Stmt]),Env,Path,Rp) where
      vrEntry(_,_,Tp) ^= isVar(Nm,Env) => do{
	(Q,ETp) = evidence(Tp,[],Env);
	(Cx,VarTp) = deConstrain(ETp);
	Es = declareConstraints(Cx,declareTypeVars(Q,Env));
	if (_,Lhs,R) ^= isDefn(Stmt) then{
	  Val <- typeOfExp(R,VarTp,Es,Path,Rp);
	  LclNm = qualifiedName(Path,markerString(pkgMark),Nm);
	  valis (varDef(Lc,Nm,LclNm,Val,Cx,Tp),declareVar(Nm,some(Lc),Tp,Env))
	}
	else{
	  throw reportError(Rp,"bad definition $(Stmt)",Lc)
	}
      }.
  checkDefn(defnSpec(tpSp(TpNm),Lc,[St]),Env,Path,Rp) =>
    parseTypeDef(TpNm,St,Env,Path,Rp).
  checkDefn(defnSpec(cnsSp(CnNm),Lc,[St]),Env,Path,Rp) =>
    parseConstructor(CnNm,St,Env,Path,Rp).
  checkDefn(defnSpec(conSp(ConNm),Lc,[St]),Env,Path,Rp) => do{
    Contract <- parseContract(St,Env,Path,Rp);
    valis (conDef(Lc,ConNm,ConNm,Contract),
      declareContract(Lc,ConNm,Contract,Env))
  }
  checkDefn(defnSpec(implSp(Nm),Lc,[St]),Env,Path,Rp) => do {
    if (_,Q,C,H,B) ^= isImplementationStmt(St) then
      checkImplementation(Lc,Nm,Q,C,H,B,Env,Path,Rp)
    else
      throw reportError(Rp,"not a valid implementation statement",Lc)
  }

  checkFunction:(string,tipe,locn,list[ast],dict,string,reports) =>
    either[reports,(canonDef,dict)].
  checkFunction(Nm,Tp,Lc,Stmts,Env,Path,Rp) => do{
    (Q,ETp) = evidence(Tp,[],Env);
    (Cx,ProgramTp) = deConstrain(ETp);
    Es = declareConstraints(Cx,declareTypeVars(Q,Env));
    (Rls,Dflt) <- processEqns(Stmts,deRef(ProgramTp),[],[],Es,Path,Rp);
    LclNm = qualifiedName(Path,markerString(pkgMark),Nm);
    valis (varDef(Lc,Nm,LclNm,lambda(Rls++Dflt,Tp),Cx,Tp),declareVar(Nm,some(Lc),Tp,Env))
  }.

  processEqns:(list[ast],tipe,list[equation],list[equation],dict,string,reports) =>
    either[reports,(list[equation],list[equation])].
  processEqns([],_,Rls,Deflt,_,_,_) => either((Rls,Deflt)).
  processEqns([St,..Ss],ProgramType,Rls,Deflt,Env,Path,Rp) => do{
    (Rl,IsDeflt) <- processEqn(St,ProgramType,Env,Path,Rp);
    if IsDeflt then{
      if [DRl,.._] .= Deflt then{
	throw reportError(Rp,"cannot have more than one default, other one at $(locOf(DRl))",
	  locOf(Rl))
      } else{
	processEqns(Ss,ProgramType,Rls,[Rl],Env,Path,Rp)
      }
    }
    else{
      processEqns(Ss,ProgramType,[Rls..,Rl],Deflt,Env,Path,Rp)
    }    
  }

  processEqn(St,ProgramType,Env,Path,Rp) where
      (Lc,H,R) ^= isEquation(St) && (_,Arg,IsDeflt) ^= splitHead(H) => do{
	Ats = genArgTps(Arg);
	RTp = newTypeVar("_R");
	checkType(St,funType(Ats,RTp),ProgramType,Env,Rp);
	(Args,Ev) <- typeOfArgPtn(Arg,Ats,Env,Path,Rp);
	Rep <- typeOfExp(R,RTp,Ev,Path,Rp);
	valis (eqn(Lc,Args,Rep),IsDeflt)
      }.

  checkImplementation:(locn,string,list[ast],list[ast],ast,ast,dict,string,reports) =>
    either[reports,(canonDef,dict)].
  checkImplementation(Lc,Nm,Q,C,H,B,Env,Path,Rp) => do{
    logMsg("check implementation $(Nm)");
    BV <- parseBoundTpVars(Q,Rp);
    Cx <- parseConstraints(C,BV,Env,Rp);
    Cn <- parseContractConstraint(BV,H,Env,Rp);
--    logMsg("implemented contract type $(Cn)");
    ConName = localName(typeName(Cn),typeMark);
    if Con ^= findContract(Env,ConName) then{
      (_,typeExists(ConTp,ConFaceTp)) =
	freshen(Con,[],Env);
      logMsg("found contract type $(ConTp), implementation type $(ConFaceTp)");
      if sameType(ConTp,Cn,Env) then {
	Es = declareConstraints(Cx,declareTypeVars(BV,Env));
	logMsg("check implementation body $(B) against $(ConFaceTp)");
	Impl <- typeOfExp(B,ConFaceTp,Es,Path,Rp);
	FullNm = implementationName(ConTp);
--	logMsg("full name of implementation of $(ConTp) is $(FullNm)");
	ImplTp = rebind(BV,reConstrain(Cx,ConTp),Es);
--	logMsg("implementation type $(ImplTp)");
	valis (implDef(Lc,Nm,FullNm,Impl,ImplTp),
	  declareImplementation(FullNm,ImplTp,Env))
      }
      else{
	throw reportError(Rp,"implementation type $(Cn) not consistent with contract type $(ConTp)",Lc)
      }
    } else{
      throw reportError(Rp,"Contract $(ConName) not known",Lc)
    }
  }
    
  checkOthers:(list[ast],list[canon],dict,string,reports) => either[reports,list[canon]].
  checkOthers([],Oth,_,_,_) => either(Oth).
  checkOthers([O,..Ots],Oth,Env,Path,Rp) where (Lc,C) ^= isIntegrity(O) => do{
    (Cond,_) <- checkGoal(C,Env,Path,Rp);
    checkOthers(Ots,[Oth..,Cond],Env,Path,Rp)
  }
  
  typeOfPtn:(ast,tipe,dict,string,reports) => either[reports,(canon,dict)].
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,"_") ^= isName(A) =>
    either((vr(Lc,genSym("_"),Tp),Env)).
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) && Spec ^= isVar(Id,Env) => do{
    if isConstructorType(vrType(Spec)) then{
      Exp <- typeOfVar(Lc,Id,Tp,Spec,Env,Rp);
      valis (Exp,Env)
    }
    else{
      WhExp <- typeOfExp(mkWhereEquality(A),Tp,Env,Path,Rp);
      valis (WhExp,Env)
    }
  }
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) => do{
    Ev = declareVar(Id,some(Lc),Tp,Env);
    valis (vr(Lc,Id,Tp),Ev)
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
	(Cond,Ev1) <- checkGoal(C,Ev0,Path,Rp);
	valis (whr(Lc,Ptn,Cond),Ev1)
      }.
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isSqTuple(A) => do{
    SqPtn = macroSquarePtn(Lc,Els);
    typeOfPtn(SqPtn,Tp,Env,Path,Rp)
  }.
  typeOfPtn(A,Tp,Env,Path,Rp) where (_,[El]) ^= isTuple(A) && \+ _ ^= isTuple(El) =>
    typeOfPtn(El,Tp,Env,Path,Rp).
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs = genTpVars(Els);
    checkType(A,tupleType(Tvs),Tp,Env,Rp);
    (Ptns,Ev) <- typeOfPtns(Els,Tvs,[],Env,Path,Rp);
    valis (tple(Lc,Ptns),Ev)
  }
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Pt,Ex) ^= isOptionPtn(A) =>
    typeOfPtn(mkWherePtn(Lc,Pt,Ex),Tp,Env,Path,Rp).
  typeOfPtn(A,Tp,Env,Path,Rp) where (Lc,Op,Els) ^= isRoundTerm(A) => do{
    At = newTypeVar("A");
--    logMsg("checking round patterm $(A) for $(consType(At,Tp))");
    Fun <- typeOfExp(Op,consType(At,Tp),Env,Path,Rp);
    (Args,Ev) <- typeOfArgPtn(rndTuple(Lc,Els),At,Env,Path,Rp);
    valis (apply(Lc,Fun,Args,Tp),Ev)
  }
  typeOfPtn(A,Tp,_,_,Rp) => other(reportError(Rp,"illegal pattern: $(A), expecting a $(Tp)",locOf(A))).

  typeOfArgPtn:(ast,tipe,dict,string,reports) => either[reports,(canon,dict)].
  typeOfArgPtn(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs = genTpVars(Els);
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
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) =>
    (Spec ^= isVar(Id,Env) ?
	typeOfVar(Lc,Id,Tp,Spec,Env,Rp) ||
	other(reportError(Rp,"variable $(Id) not defined. Expecting a $(Tp)",Lc))).
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Ix) ^= isInt(A) &&
      (_,IntTp,_) ^= findType(Env,"integer") => do{
	checkType(A,IntTp,Tp,Env,Rp);
	valis intLit(Lc,Ix)
      }
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Dx) ^= isFlt(A) &&
      (_,FltTp,_) ^= findType(Env,"float") => do{
	checkType(A,FltTp,Tp,Env,Rp);
	valis floatLit(Lc,Dx)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Sx) ^= isStr(A) &&
      (_,StrTp,_) ^= findType(Env,"string") => do{
	checkType(A,StrTp,Tp,Env,Rp);
	valis stringLit(Lc,Sx)
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
	(Cond,Ev0) <- checkGoal(C,Env,Path,Rp);
	Exp <- typeOfExp(E,Tp,Ev0,Path,Rp);
	valis whr(Lc,Exp,Cond)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isConjunct(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkGoal(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isDisjunct(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkGoal(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,T,L,R) ^= isConditional(A) => do{
    (Tst,E0) <- checkGoal(T,Env,Path,Rp);
    Thn <- typeOfExp(L,Tp,E0,Path,Rp);
    Els <- typeOfExp(R,Tp,Env,Path,Rp);
    valis cond(Lc,Tst,Thn,Els)
  }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isNegation(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkGoal(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isMatch(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkGoal(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isSearch(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkGoal(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,R,F) ^= isFieldAcc(A) && (_,Fld) ^= isName(F) => do{
	Rc <- typeOfExp(R,newTypeVar("_r"),Env,Path,Rp);
	typeOfField(Lc,Rc,Fld,Tp,Env,Path,Rp)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,C,Ix) ^= isIndex(A) =>
    ((_,K,V) ^= isBinary(Ix,"->") ?
	typeOfExp(ternary(Lc,"_put",C,K,V),Tp,Env,Path,Rp) ||
	(_,N) ^= isUnary(Ix,"\\+") ?
	  typeOfExp(binary(Lc,"_remove",C,N),Tp,Env,Path,Rp) ||
	  typeOfExp(binary(Lc,"_index",C,Ix),Tp,Env,Path,Rp)).
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Stmts) ^= isActionTerm(A) &&
      (_,ActionTp,_) ^= findType(Env,"action") => do{
	ErTp = newTypeVar("E");
	XTp = newTypeVar("X");
	checkType(A,tpExp(tpExp(ActionTp,ErTp),XTp),Tp,Env,Rp);
	checkDo(Stmts,Tp,Env,Path,Rp)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Stmts) ^= isTaskTerm(A) &&
      (_,ActionTp,_) ^= findType(Env,"task") => do{
	ErTp = newTypeVar("E");
	XTp = newTypeVar("X");
	checkType(A,tpExp(tpExp(ActionTp,ErTp),XTp),Tp,Env,Rp);
	checkDo(Stmts,Tp,Env,Path,Rp)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Stmts) ^= isDoTerm(A)  => 
    checkDo(Stmts,Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Exp) ^= isValof(A) =>
    typeOfExp(unary(Lc,"_perform",Exp),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isSqTuple(A) =>
    typeOfExp(macroSquareExp(Lc,Els),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (_,[El]) ^= isTuple(A) && \+ _ ^= isTuple(El) =>
    typeOfExp(El,Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs = genTpVars(Els);
    checkType(A,tupleType(Tvs),Tp,Env,Rp);
    Ptns <- typeOfExps(Els,Tvs,[],Env,Path,Rp);
    valis tple(Lc,Ptns)
  }
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,B,C) ^= isAbstraction(A) =>
    checkAbstraction(Lc,B,C,Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where hasPromotion(A) =>
    typeOfExp(promoteOption(A),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Ar,R) ^= isEquation(A) =>
    typeOfLambda(Lc,Ar,R,Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTheta(A) =>
    typeOfTheta(Lc,Els,Tp,Env,genNewName(Path,"θ"),Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,I) ^= isUnary(A,"-") =>
    typeOfExp(binary(Lc,"-",lit(Lc,intgr(0)),I),Tp,Env,Path,Rp).
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Body,Bnd) ^= isLetDef(A) => do{
    logMsg("checking let exp");
    Btp = newTypeVar("_");
    
    Th <- typeOfExp(Body,Btp,Env,genNewName(Path,"Γ"),Rp);
    logMsg("theta: $(Th), its type is $(Btp), face = $(faceOfType(Btp,Env))");
    BndEnv = pushFace(faceOfType(Btp,Env),Lc,Env);
    logMsg("sub env is $(BndEnv)");
    El <- typeOfExp(Bnd,Tp,BndEnv,Path,Rp);
    valis letExp(Lc,Th,El)
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
    Vrs = genTpVars(As);
    At = tupleType(Vrs);
    FFTp = newTypeFun("_F",2);
    ExTp = mkTypeExp(FFTp,[At,Tp]);
    Fun <- typeOfExp(Op,ExTp,Env,Path,Rp);
--    logMsg("type of $(Op) |- $(ExTp)");
    Args <- typeOfExps(As,Vrs,[],Env,Path,Rp);
    if sameType(FFTp,tpFun("=>",2),Env) || sameType(FFTp,tpFun("<=>",2),Env) then{
      valis apply(Lc,Fun,tple(Lc,Args),Tp)
    } else
      throw reportError(Rp,"type of $(Op)\:$(ExTp) not consistent with $(funType(At,Tp))",Lc)
  }

  typeOfArgExp:(ast,tipe,dict,string,reports) => either[reports,(canon,dict)].
  typeOfArgExp(A,Tp,Env,Path,Rp) where (Lc,Els) ^= isTuple(A) => do{
    Tvs = genTpVars(Els);
    checkType(A,tupleType(Tvs),Tp,Env,Rp);
    (Ptns,Ev) <- typeOfPtns(Els,Tvs,[],Env,Path,Rp);
    valis (tple(Lc,Ptns),Ev)
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
    PtnTp = newTypeVar("_M");
    Val <- typeOfExp(R,PtnTp,Env,Path,Rp);
    (Ptn,Ev) <- typeOfPtn(L,PtnTp,Env,Path,Rp);
    valis (match(Lc,Ptn,Val),Ev)
  }
  checkGoal(A,Env,Path,Rp) where (Lc,L,R) ^= isSearch(A) => do{
    MTp = newTypeFun("M",1);
    StTp = newTypeVar("X");
    SrTp = newTypeVar("Sr");
    ElTp = newTypeVar("El");
    MdTp = tpExp(MTp,StTp);
    ActionTp = funType(tupleType([ElTp,StTp]),MdTp);
    IterTp = funType(tupleType([SrTp,MdTp,ActionTp]),MdTp);
    Iterator <- typeOfExp(nme(Lc,"_iter"),IterTp,Env,Path,Rp);
    (Ptn,Ev) <- typeOfPtn(L,ElTp,Env,Path,Rp);
    Gen <- typeOfExp(R,SrTp,Env,Path,Rp);
    valis (serch(Lc,Ptn,Gen,Iterator),Ev)
  }
  checkGoal(A,Env,Path,Rp) where (_,BoolTp,_) ^= findType(Env,"boolean") => do{
    Exp <- typeOfExp(A,BoolTp,Env,Path,Rp);
    valis (Exp,Env)
  }

  checkDo:(ast,tipe,dict,string,reports) => either[reports,canon].
  checkDo(Stmts,Tp,Env,Path,Rp) => do{
    ElTp = newTypeVar("_e");
    Lc = locOf(Stmts);
    if Con ^= findContract(Env,"execution") then{
      (_,typeExists(funDeps(tpExp(Op,StTp),[ErTp]),_)) = freshen(Con,[],Env);
      if sameType(tpExp(StTp,ElTp),Tp,Env) then {
	(Action,_) <- checkAction(Stmts,Env,StTp,ElTp,ErTp,Path,Rp);
	valis act(Lc,Action)
      } else
	throw reportError(Rp,"$(tpExp(StTp,ErTp)) not consistent with expected type $(Tp)",Lc)
    } else
      throw reportError(Rp,"cannot find execution contract",Lc)
  }

  checkAction:(ast,dict,tipe,tipe,tipe,string,reports) =>
    either[reports,(canonAction,dict)].
  checkAction(A,Env,_,_,_,_,_) where (Lc,"nothing") ^= isName(A) => either((noDo(Lc),Env)).
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,L,R) ^= isActionSeq(A) => do{
    E = newTypeVar("_e");
    (Fst,E0) <- checkAction(L,Env,StTp,E,ErTp,Path,Rp);
    (Snd,E1) <- checkAction(R,E0,StTp,ElTp,ErTp,Path,Rp);
    valis (seqnDo(Lc,Fst,Snd),E1)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,L,R) ^= isBind(A) => do{
    E = newTypeVar("P");
    HT = tpExp(StTp,E); -- bound terms must be in the same monad
    Exp <- typeOfExp(R,HT,Env,Path,Rp);
    (Ptn,Ev) <- typeOfPtn(L,E,Env,Path,Rp);
    valis (bindDo(Lc,Ptn,Exp,E,StTp,ErTp),Ev)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,L,R) ^= isDefn(A) => do{
    E = newTypeVar("P");
    (Ptn,Ev) <- typeOfPtn(L,E,Env,Path,Rp);
    Exp <- typeOfExp(R,E,Env,Path,Rp);

    valis (varDo(Lc,Ptn,Exp),Ev)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,L,R) ^= isAssignment(A) => do{
    Et = newTypeVar("P");
    if (LLc,Coll,[Arg]) ^= isSquareTerm(L) then {
      Lhs <- typeOfExp(Coll,refType(Et),Env,Path,Rp);
      Rhs <- typeOfExp(ternary(LLc,"_put",unary(LLc,"!",Coll),Arg,R),Et,Env,Path,Rp);
      valis (assignDo(Lc,Lhs,Rhs,StTp,ErTp),Env)
    } else{
      Lhs <- typeOfExp(L,refType(Et),Env,Path,Rp);
      Rhs <- typeOfExp(R,Et,Env,Path,Rp);
      valis (assignDo(Lc,Lhs,Rhs,StTp,ErTp),Env)
    }
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,T,L,R) ^= isIfThenElse(A) => do{
    (Cond,Ev0) <- checkGoal(T,Env,Path,Rp);
    (Th,E1) <- checkAction(L,Env,StTp,ElTp,ErTp,Path,Rp);
    (El,E2) <- checkAction(R,Env,StTp,ElTp,ErTp,Path,Rp);
    valis (ifThenDo(Lc,Cond,Th,El,StTp,ElTp),mergeDict(E1,E2,Env))
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,T,L) ^= isIfThen(A) => do{
    (Cond,Ev0) <- checkGoal(T,Env,Path,Rp);
    (Th,E1) <- checkAction(L,Env,StTp,ElTp,ErTp,Path,Rp);
    valis (ifThenDo(Lc,Cond,Th,noDo(Lc),StTp,ElTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,T,B) ^= isWhileDo(A) => do{
    (Cond,Ev0) <- checkGoal(T,Env,Path,Rp);
    (Body,_) <- checkAction(B,Env,StTp,tupleType([]),ErTp,Path,Rp);
    valis (whileDo(Lc,Cond,Body,StTp,ErTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,T,B) ^= isForDo(A) => do{
    (Cond,Ev0) <- checkGoal(T,Env,Path,Rp);
    (Body,_) <- checkAction(B,Env,StTp,tupleType([]),ErTp,Path,Rp);
    valis (forDo(Lc,Cond,Body,StTp,ErTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,B,C) ^= isTryCatch(A) => do{
    (Body,_) <- checkAction(B,Env,StTp,ElTp,ErTp,Path,Rp);
    Hndlr <- checkCatch(C,Env,StTp,ElTp,ErTp,Path,Rp);
    valis (tryCatchDo(Lc,Body,Hndlr,StTp,ElTp,ErTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,E) ^= isThrow(A) => do{
    Err <- typeOfExp(E,ErTp,Env,Path,Rp);
    valis (throwDo(Lc,Err,StTp,ErTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,E) ^= isValis(A) => do{
    Rtn <- typeOfExp(E,ElTp,Env,Path,Rp);
    valis (returnDo(Lc,Rtn,StTp,ElTp),Env)
  }
  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,[S]) ^= isBrTuple(A) => 
    checkAction(S,Env,StTp,ElTp,ErTp,Path,Rp).

  checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp) => do{
    Exp <- typeOfExp(A,tpExp(StTp,ElTp),Env,Path,Rp);
    valis (simpleDo(locOf(A),Exp,StTp,ErTp),Env)
  }

  checkCatch:(ast,dict,tipe,tipe,tipe,string,reports) => either[reports,canon].
  checkCatch(A,Env,StTp,ElTp,ErTp,Path,Rp) where (Lc,Stmts) ^= isBrTuple(A) => do{
    HT = funType(tupleType([ErTp]),tpExp(StTp,ElTp));
    (H,_) <- checkAction(A,Env,StTp,ElTp,ErTp,Path,Rp);
    valis lambda([eqn(Lc,tple(Lc,[vr(Lc,genSym("_"),ErTp)]),act(locOf(A),H))],HT)
  }
  checkCatch(A,Env,StTp,ElTp,ErTp,Path,Rp) => do{
    HT = funType(tupleType([ErTp]),tpExp(StTp,ElTp));
    typeOfExp(A,HT,Env,Path,Rp)
  }
      
  typeOfVar:(locn,string,tipe,vrEntry,dict,reports) => either[reports,canon].
  typeOfVar(Lc,Nm,Tp,vrEntry(_,Mk,VTp),Env,Rp) => do{
    (_,VrTp) = freshen(VTp,[],Env);
    (MTp,Term) <- manageConstraints(VrTp,[],Lc,Mk(Lc,VrTp),Env,Rp);
--    logMsg("check var $(Nm)\:$(MTp) against $(Tp)");
    if sameType(Tp,MTp,Env) || sameType(enumType(Tp),MTp,Env) then {
      valis Term
    } else
      throw reportError(Rp,"type of $(Nm)\:$(VTp) not consistent with expected type: $(Tp)",Lc)
  }

  typeOfField:(locn,canon,string,tipe,dict,string,reports) => either[reports,canon].
  typeOfField(Lc,Rc,Fld,Tp,Env,Path,Rp) => do{
    faceType(Flds,_) = faceOfType(typeOf(Rc),Env);
    if (Fld,FldTp) in Flds then{
      (_,VrTp) = freshen(FldTp,[],Env);
      (MTp,Term) <- manageConstraints(VrTp,[],Lc,dot(Lc,Rc,Fld,VrTp),Env,Rp);
      if sameType(Tp,VrTp,Env) then {
	valis Term
      } else
	throw reportError(Rp,"$(Fld)\:$(FldTp) not consistent with expected type: $(Tp)",Lc)
    } else
      throw reportError(Rp,"field $(Fld) is not present in $(Rc)",Lc)
  }

  typeOfLambda:(locn,ast,ast,tipe,dict,string,reports) => either[reports,canon].
  typeOfLambda(Lc,A,R,Tp,Env,Path,Rp) => do{
    At = newTypeVar("_A");
    Rt = newTypeVar("_R");
    (As,E0) <- typeOfArgPtn(A,At,Env,Path,Rp);
    Rep <- typeOfExp(R,Rt,E0,Path,Rp);
    checkType(A,funType(At,Rt),Tp,Env,Rp);
    valis lambda([eqn(Lc,As,Rep)],Tp)
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
    mergeVDefs(V1,V2) => {Nm->E1|Nm->E1 in V1 && Nm->E2 in V2 &&
	  sameDesc(E1,E2)}.
    sameDesc(vrEntry(_,_,T1),vrEntry(_,_,T2)) => sameType(T1,T2,Env)
  } in mergeScopes(D1,D2).

  genTpVars:(list[ast]) => list[tipe].
  genTpVars(Els) => (Els//(_)=>newTypeVar("_v")).

  genArgTps:(ast) => tipe.
  genArgTps(A) where (_,Ar,_) ^= isWhere(A) =>
    genArgTps(Ar).
  genArgTps(A) where (_,Els) ^= isTuple(A) =>
    tupleType(genTpVars(Els)).

  checkAbstraction:(locn,ast,ast,tipe,dict,string,reports) => either[reports,canon].
  checkAbstraction(Lc,B,C,Tp,Env,Path,Rp) => do{
    (Cond,E0) <- checkGoal(C,Env,Path,Rp);
    (StTp,BndTp) <- pickupContract(Lc,Env,"sequence",Rp);
    checkType(B,Tp,StTp,Env,Rp);
    Bnd <- typeOfExp(B,BndTp,E0,Path,Rp);
    valis abstraction(Lc,Bnd,Cond,Tp)
  }

  pickupContract:(locn,dict,string,reports) => either[reports,(tipe,tipe)].
  pickupContract(Lc,Env,Nm,Rp) => do{
    if Con ^= findContract(Env,Nm) then{
      (_,typeExists(funDeps(tpExp(Op,StTp),[ErTp]),_)) =
	freshen(Con,[],Env);
      valis (StTp,ErTp)
    } else
      throw reportError(Rp,"$(Nm) contract not defined",Lc)
  }

  pickupIxContract:(locn,dict,string,reports) => either[reports,(tipe,tipe,tipe,tipe)].
  pickupIxContract(Lc,Env,Nm,Rp) => do{
    if Con ^= findContract(Env,Nm) then{
      (_,typeExists(funDeps(tpExp(Op,IxTp),[KyTp,VlTp]),_)) =
	freshen(Con,[],Env);
      valis (Op,IxTp,KyTp,VlTp)
    } else
      throw reportError(Rp,"$(Nm) contract not defined",Lc)
  }
}
