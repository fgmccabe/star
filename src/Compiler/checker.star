star.compiler.checker{
  import star.

  import star.pkg.
  import star.repo.

  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.canondeps.
  import star.compiler.coverage.
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

  public checkPkg:all r ~~ repo[r],display[r]|:(r,pkg,ast) =>
    (pkgSpec,cons[canonDef],cons[decl],cons[decl]).
  checkPkg(Repo,Pkge,P) => valof{
    Base = stdDict;
    if (Lc,Pk,Els) ?= isQBrTerm(P) && Pkg .= pkgeName(Pk) then{
      if compatiblePkg(Pkg,Pkge) then{
	(Imports,Stmts) = collectImports(Els,[],[]);
	(AllImports,IDecls) = importAll(Imports,Repo,[],[]);

	PkgEnv = declareDecls(IDecls,Base);
	PkgPth = packageName(Pkg);

	(Vis,Opens,Annots,Gps) = dependencies(Stmts);

	if ~isEmpty(Opens) then
	  reportError("open statements $(Opens) not currently supported",Lc);
    
	(Defs,ExDecls,ThDecls,ThEnv) = checkGroups(Gps,Vis,.faceType([],[]),Annots,PkgEnv,PkgPth);

	AllDecls = ThDecls*;

	RDefs = overloadProgram(Defs,declareDecls(AllDecls,PkgEnv))*;

	if traceCanon! then
	  logMsg("exported declarations $(ExDecls)");

	valis (pkgSpec{pkg=Pkge. imports=Imports. exports=ExDecls},RDefs,IDecls,AllDecls)
      }
      else
      reportError("package name $(Pkg) does not match expected $(Pkge)",locOf(P))
    } else
    reportError("invalid package structure",locOf(P));
    valis (pkgSpec{pkg=Pkge. imports=[]. exports=[]},[],[],[])
  }

  pickDefltVis(.deFault,V) => V.
  pickDefltVis(V,_) => V.

  isVisible:(visMap,visibility,defnSp)=>boolean.
  isVisible([],_,_) => .false.
  isVisible([(e,V),.._],D,e) => pickDefltVis(V,D)>=.transItive.
  isVisible([_,..l],D,e) => isVisible(l,D,e).

  formRecordExp:(option[locn],string,tipe,cons[canonDef],cons[decl],tipe) => canon.
  formRecordExp(Lc,Lbl,.faceType(Flds,Tps),Defs,Decls,Tp) => valof{
    sortedFlds = sortFieldTypes(Flds);
    valis letExp(Lc,Defs,Decls,apply(Lc,vr(.none,Lbl,consType(.tupleType(sortedFlds//snd),Tp)),
	(sortedFlds//((FNm,FTp))=>vr(.none,FNm,FTp)),Tp))
  }

  genLetRec:all e,g,d ~~ (cons[g],cons[d],(g,d,e)=>e,e) => e.
  genLetRec([],[],_,E) => E.
  genLetRec([G,..Gps],[D,..Ds],F,E) => F(G,D,genLetRec(Gps,Ds,F,E)).

  formTheta:(option[locn],string,tipe,cons[cons[canonDef]],cons[cons[decl]],tipe) =>
    canon.
  formTheta(Lc,Lbl,.faceType(Flds,Tps),Defs,Decls,Tp) => valof{
    sortedFlds = sortFieldTypes(Flds);
    valis genLetRec(Defs,Decls,(G,D,E) => letRec(Lc,G,D,E),
      apply(Lc,vr(Lc,Lbl,consType(.tupleType(sortedFlds//snd),Tp)),
	(sortedFlds//((FNm,FTp))=>vr(Lc,FNm,FTp)),Tp))
  }

  thetaEnv:(option[locn],string,cons[ast],tipe,dict) =>
    (cons[cons[canonDef]],cons[decl],cons[cons[decl]],dict).
  thetaEnv(Lc,Pth,Stmts,Face,Env) => valof{
    (Vis,Opens,Annots,Gps) = dependencies(Stmts);

    if ~isEmpty(Opens) then
      reportError("open statements $(Opens) not supported",Lc);
    
    valis checkGroups(Gps,Vis,Face,Annots,pushFace(Face,Lc,Env),Pth)
  }

  recordEnv:(option[locn],string,cons[ast],tipe,dict,dict) =>
    (cons[canonDef],cons[decl]).
  recordEnv(Lc,Path,Stmts,Face,Env,Outer) => valof{
    -- We sort for dependencies to get types right
    (Vis,Opens,Annots,Gps) = dependencies(Stmts);

    if ~isEmpty(Opens) then{
      reportError("open statements not implemented",Lc)
    };

    G = Gps*; -- Flatten the result

    TmpEnv = parseAnnotations(G,Face,Annots,Env);
    (Gp,_,Ds) = checkGroup(G,Vis,TmpEnv,Outer,Path);

    valis (Gp,Ds)
  }

  checkGroups:(cons[cons[defnSpec]],visMap,tipe,map[string,ast],dict,string) =>
    (cons[cons[canonDef]],cons[decl],cons[cons[decl]],dict).
  checkGroups(AGps,Vis,Face,Annots,Env,Path) => let{.
    checkGps([],Ev,Gps,ExDcs,Dcs) => (reverse(Gps),reverse(ExDcs),reverse(Dcs),Ev).
    checkGps([G,..Gs],Ev,Gx,ExDcs,Dx) => valof{
      TmpEnv = parseAnnotations(G,Face,Annots,Ev);
      (Gp,Xps,Ds) = checkGroup(G,Vis,TmpEnv,TmpEnv,Path);
      valis checkGps(Gs,declareDecls(Ds,Ev),[Gp,..Gx],Xps++ExDcs,[Ds,..Dx])
    }
  .} in checkGps(AGps,Env,[],[],[]).

  parseAnnotations:(cons[defnSpec],tipe,map[string,ast],dict) => dict.
  parseAnnotations([],_,_,Env) => Env.
  parseAnnotations([.defnSpec(.varSp(Nm),Lc,Stmts),..Gs],Fields,Annots,Env) => valof{
    Tp = parseAnnotation(Nm,Lc,Stmts,Fields,Annots,Env);
    valis parseAnnotations(Gs,Fields,Annots,declareVar(Nm,Lc,Tp,faceOfType(Tp,Env),Env))
  }
  parseAnnotations([_,..Gs],Fields,Annots,Env) => parseAnnotations(Gs,Fields,Annots,Env).

  parseAnnotation:(string,option[locn],cons[ast],tipe,map[string,ast],dict) => tipe.
  parseAnnotation(Nm,_,_,_,Annots,Env) where T ?= Annots[Nm] => 
    parseType([],T,Env).
  parseAnnotation(Nm,_,_,.faceType(Vrs,_),_,_) where Tp?={!Tp|(Nm,Tp) in Vrs!} => Tp.
  parseAnnotation(Nm,_,_,_,_,Env) where Tp ?= varType(Nm,Env) => Tp.
  parseAnnotation(Nm,Lc,Stmts,Fields,Annots,Env) =>
    guessStmtType(Stmts,Nm,Lc).

  guessStmtType([],Nm,Lc) => valof{
    reportError("$(Nm) not declared",Lc);
    valis .voidType
  }
  guessStmtType([St,.._],Nm,Lc) => valof{
    if (_,_,_,Args,_,_) ?= isEquation(St) then {
      valis funType(genArgTps(Args),newTypeVar("_R"))
    } else if (_,_,_) ?= isAssignment(St) then{
      valis refType(newTypeVar("R"))
    }
    else{
      valis newTypeVar("_D")
    }
  }

  allFunDefs:(cons[canonDef])=>boolean.
  allFunDefs(Dfs) => {? D in Dfs *> .varDef(_,_,_,.lambda(_,_,_,_),_,_).=D ?}.

  checkGroup:(cons[defnSpec],visMap,dict,dict,string) =>
    (cons[canonDef],cons[decl],cons[decl]).
  checkGroup(Specs,Vis,Env,Outer,Path) => let{.
    checkDefs([],Dfs,XptDcs,Dcs) => (reverse(Dfs),XptDcs,Dcs).
    checkDefs([D,..Ds],Defs,XDcs,Decls) => valof{
      (Dfs,Xpts,Dcs) = checkDefn(D,Vis,Env,Outer,Path);
      valis checkDefs(Ds,Dfs++Defs,XDcs++Xpts,Decls++Dcs)
    }
  .} in checkDefs(Specs,[],[],[]).

  checkDefn:(defnSpec,visMap,dict,dict,string) => (cons[canonDef],cons[decl],cons[decl]).
  checkDefn(Defn,Vis,Env,Outer,Path) => case Defn in {
    .defnSpec(.varSp(Nm),Lc,Stmts) where Tp ?= varType(Nm,Env) && areEquations(Stmts) => valof{
      (Defs,Decls) = checkFunction(Nm,Tp,Lc,Stmts,Env,Outer,Path);
      valis (Defs,(isVisible(Vis,.priVate,.varSp(Nm))??Decls||[]),Decls)
    }.
    .defnSpec(.varSp(Nm),Lc,[Stmt]) where Tp ?= varType(Nm,Env) => valof{
      (Defs,Decls) = checkVar(Nm,Tp,Lc,Stmt,Env,Outer,Path);
      valis (Defs,(isVisible(Vis,.priVate,.varSp(Nm))??Decls||[]),Decls)
    }.
    .defnSpec(.tpSp(TpNm),Lc,[St]) => valof{
      (Df,Dc) = parseTypeDef(TpNm,St,Env,Path);
      valis (Df,(isVisible(Vis,.priVate,.tpSp(TpNm))??Dc||[]),Dc)
    }.
    .defnSpec(.cnsSp(CnNm),Lc,[St]) => valof{
      (Defs,Decls) = parseConstructor(CnNm,St,Env,Path);
      valis (Defs,(isVisible(Vis,.priVate,.cnsSp(CnNm))??Decls||[]),Decls)
    }.
    .defnSpec(.conSp(ConNm),Lc,[St]) => valof{
      (Defs,Decls) = parseContract(St,Env,Path);
      valis (Defs,(isVisible(Vis,.priVate,.conSp(ConNm))??Decls||[]),Decls)
    }.
    .defnSpec(.implSp(Nm),Lc,[St]) => valof {
      if (_,Q,C,H,B) ?= isImplementationStmt(St) then{
	(Defs,Decls) = checkImplementation(Lc,Q,C,H,B,Env,Outer,Path);
	valis (Defs,(isVisible(Vis,.priVate,.implSp(Nm))??Decls||[]),Decls)
      }
      else{
	reportError("not a valid implementation statement",Lc);
	valis ([],[],[])
      }
    }.
    .defnSpec(.accSp(Nm,Fld),Lc,[St]) => valof {
      if (_,Q,C,T,B) ?= isAccessorStmt(St) then{
	(Defs,Decls) = checkAccessor(Lc,Nm,Q,C,T,B,Env,Outer,Path);
	valis (Defs,(isVisible(Vis,.priVate,.accSp(Nm,Fld))??Decls||[]),Decls)
      }
      else{
	reportError("not a valid accessor statement",Lc);
	valis ([],[],[])
      }
    }.
    .defnSpec(.updSp(Nm,Fld),Lc,[St]) => valof {
      if (_,Q,C,H,B) ?= isUpdaterStmt(St) then{
	(Defs,Decls) = checkUpdater(Lc,Nm,Q,C,H,B,Env,Outer,Path);
	valis (Defs,(isVisible(Vis,.priVate,.updSp(Nm,Fld))??Decls||[]),Decls)
      }
      else{
	reportError("not a valid updater statement",Lc);
	valis ([],[],[])
      }
    }
  }

  checkFunction:(string,tipe,option[locn],cons[ast],dict,dict,string) =>
    (cons[canonDef],cons[decl]).
  checkFunction(Nm,Tp,Lc,Stmts,Env,Outer,Path) => valof{
    if traceCanon! then
      logMsg("check function $(Stmts)\:$(Tp), existing #(showVar(Nm,Outer))");

    (Q,ETp) = evidence(Tp,Env);
    (Cx,ProgTp) = deConstrain(ETp);
    Es = declareConstraints(Lc,Cx,declareTypeVars(Q,Env));
    (ProgramType,ErTp) = isThrowsType(ProgTp);
    Rls = processEqns(Stmts,ProgramType,ErTp,[],.none,Es,
      declareConstraints(Lc,Cx,declareTypeVars(Q,Outer)),Path);
    FullNm = qualifiedName(Path,.valMark,Nm);

    if traceCanon! then
      logMsg("function $(Nm) is bound to $(lambda(Lc,FullNm,Rls,Tp))\:$(Tp)");
    
    valis ([varDef(Lc,Nm,FullNm,lambda(Lc,lambdaLbl(Lc),Rls,Tp),Cx,Tp)],
      [funDec(Lc,Nm,FullNm,Tp)])
  }

  checkVar:(string,tipe,option[locn],ast,dict,dict,string) => (cons[canonDef],cons[decl]).
  checkVar(Nm,Tp,Lc,Stmt,Env,Outer,Path) => valof{
    if traceCanon! then
      logMsg("check definition $(Stmt)\:$(Tp)");
      
    (Q,ETp) = evidence(Tp,Env);
    (Cx,VarTp) = deConstrain(ETp);
    Es = declareConstraints(Lc,Cx,declareTypeVars(Q,Outer));
    if (_,Lhs,R) ?= isDefn(Stmt) then{
      Val = typeOfExp(R,VarTp,.none,Es,Path);
      FullNm = qualifiedName(Path,.valMark,Nm);
      valis ([.varDef(Lc,Nm,FullNm,Val,Cx,Tp)],[.varDec(Lc,Nm,FullNm,Tp)])
    }
    else{
      reportError("bad definition $(Stmt)",Lc);
      valis ([],[])
    }
  }

  isThrowsType(Tp) => (.throwsType(PrTp,ErTp) .= deRef(Tp) ??
    (deRef(PrTp),.some(ErTp)) || (deRef(Tp),.none)).

  processEqns:(cons[ast],tipe,option[tipe],cons[rule[canon]],option[rule[canon]],dict,dict,string) =>
    cons[rule[canon]].
  processEqns([],_,_,Rls,.none,_,_,_) => reverse(Rls).
  processEqns([],_,_,Rls,.some(Dflt),_,_,_) => reverse([Dflt,..Rls]).
  processEqns([St,..Ss],ProgramType,ErTp,Rls,Deflt,Env,Outer,Path) => valof{
    (Rl,IsDeflt) = processEqn(St,ProgramType,ErTp,Env,Outer,Path);
    if IsDeflt then{
      if DRl ?= Deflt then{
	reportError("cannot have more than one default, other one at $(locOf(DRl))",
	  locOf(St));
	valis []
      } else{
	valis processEqns(Ss,ProgramType,ErTp,Rls,.some(Rl),Env,Outer,Path)
      }
    }
    else{
      valis processEqns(Ss,ProgramType,ErTp,[Rl,..Rls],Deflt,Env,Outer,Path)
    }
  }

  processEqn(St,ProgramType,ErTp,Env,Outer,Path) where (Lc,_,IsDeflt,Arg,Cnd,R) ?= isEquation(St) => valof{
    if traceCanon! then
      logMsg("check equation $(St)");
    Ats = genArgTps(Arg);
    RTp = newTypeVar("_R");
    checkType(St,funType(Ats,RTp),ProgramType,Env);
    (Args,E0) = typeOfArgPtn(Arg,.tupleType(Ats),ErTp,Outer,Path);
	
    if Wh?=Cnd then{
      (Cond,E1) = checkCond(Wh,ErTp,E0,Path);
      Rep = typeOfExp(R,RTp,ErTp,E1,Path);
	  
      valis (rule(Lc,Args,.some(Cond),Rep),IsDeflt)
    } else{
      valis (rule(Lc,Args,.none,typeOfExp(R,RTp,ErTp,E0,Path)),IsDeflt)
    }
  }

  checkImplementation:(option[locn],cons[ast],cons[ast],ast,ast,dict,dict,string) =>
    (cons[canonDef],cons[decl]).
  checkImplementation(Lc,Q,C,H,B,Env,Outer,Path) => valof{
    if traceCanon! then
      logMsg("checking implementation for $(H) = $(B) at $(Lc)");
    
    BV = parseBoundTpVars(Q);
    Cx = parseConstraints(C,BV,Env);
    Cn = parseContractConstraint(BV,H,Env);
    ConName = localName(conTractName(Cn),.pkgMark);
    if traceCanon! then
      logMsg("Implemented contract name $(implementedContractName(H))");
    
    if Con ?= findContract(Env,ConName) then{
      (_,.contractExists(CnNm,CnTps,CnDps,ConFaceTp)) = freshen(Con,Env);
      ConTp = mkConType(CnNm,CnTps,CnDps);
      if traceCanon! then
	logMsg("contract exists: $(ConTp) ~ $(Cn)");
      if sameType(ConTp,typeOf(Cn),Env) then {
	Es = declareConstraints(Lc,Cx,declareTypeVars(BV,Outer));
	Impl = typeOfExp(B,ConTp,.none,Es,Path);
	if traceCanon! then
	  logMsg("implementation expression $(Impl)");
	ImplNm = implementationName(conTract(CnNm,CnTps,CnDps));
	ImplVrNm = qualifiedName(Path,.valMark,ImplNm);
	ImplTp = rebind(BV,reConstrainType(Cx,ConTp),Es);
	if traceCanon! then
	  logMsg("implementation definition $(implDef(Lc,ImplNm,ImplVrNm,Impl,Cx,ImplTp))");
	
	valis ([.implDef(Lc,ImplNm,ImplVrNm,Impl,Cx,ImplTp)],
	  [.implDec(Lc,ImplNm,ImplVrNm,ImplTp),
	    (~isEmpty(Cx) ??
	      .funDec(Lc,ImplVrNm,ImplVrNm,ImplTp) ||
	      .varDec(Lc,ImplVrNm,ImplVrNm,ImplTp))])
      }
      else{
	reportError("implementation type $(Cn) not consistent with contract type $(ConTp)",Lc);
	valis ([],[])
      }
    } else{
      reportError("Contract $(ConName) not known",Lc);
      valis ([],[])
    }
  }

  checkAccessor:(option[locn],string,cons[ast],cons[ast],ast,ast,dict,dict,string) =>
    (cons[canonDef],cons[decl]).
  checkAccessor(Lc,Nm,Q,C,T,B,Env,Outer,Path) => valof{
    if traceCanon! then
      logMsg("check accessor $(Nm) B=$(B)");
    QV = parseBoundTpVars(Q);
    Cx = parseConstraints(C,QV,Env);
    (_,Fn,[TA]) = ^isSquareTerm(T);
    (_,[L],[R]) = ^isDepends(TA);
    (_,Fld) = ^isName(Fn);
    RcTp = parseType(QV,L,Env);
    FldTp = parseType(QV,R,Env);
    AT = funType([RcTp],FldTp);
    AccTp = rebind(QV,reConstrainType(Cx,AT),Env);

    (Qs,ETp) = evidence(AccTp,Env);
    (CCx,VarTp) = deConstrain(ETp);
    Es = declareConstraints(Lc,CCx,declareTypeVars(Qs,Env));

    if traceCanon! then
      logMsg("accessor type $(AccTp)");
    
    AccFn = typeOfExp(B,VarTp,.none,Es,Path);

    if traceCanon! then
      logMsg("accessor exp $(AccFn)");

    AccVrNm = qualifiedName(Path,.valMark,qualifiedName(tpName(RcTp),.typeMark,"^"++Fld));

    if traceCanon! then
      logMsg("accessor var $(AccVrNm)");
    Defn = .varDef(Lc,AccVrNm,AccVrNm,AccFn,Cx,AccTp);
    Decl = .accDec(Lc,rebind(QV,reConstrainType(Cx,RcTp),Env),Fld,AccVrNm,AccTp);
    FDecl = .funDec(Lc,AccVrNm,AccVrNm,AccTp);
    
    if traceCanon! then
      logMsg("accessor $(Decl)");
    valis ([Defn],[Decl,FDecl])
  }

  checkUpdater:(option[locn],string,cons[ast],cons[ast],ast,ast,dict,dict,string) => (cons[canonDef],cons[decl]).
  checkUpdater(Lc,Nm,Q,C,T,B,Env,Outer,Path) => valof{
    if traceCanon! then
      logMsg("Check updater: $(Nm) Head:$(T), Body:$(B)");
    QV = parseBoundTpVars(Q);
    Cx = parseConstraints(C,QV,Env);
    (_,Fn,[TA]) = ^isSquareTerm(T);
    (_,[L],[R]) = ^isDepends(TA);
    (_,Fld) = ^isName(Fn);
    RcTp = parseType(QV,L,Env);
    FldTp = parseType(QV,R,Env);

    AT = funType([RcTp,FldTp],RcTp);
    AccTp = rebind(QV,reConstrainType(Cx,AT),Env);

    (Qs,ETp) = evidence(AccTp,Env);
    (CCx,VarTp) = deConstrain(ETp);
    Es = declareConstraints(Lc,CCx,declareTypeVars(Qs,Env));

    AccFn = typeOfExp(B,VarTp,.none,Es,Path);

    AccVrNm = qualifiedName(Path,.valMark,qualifiedName(tpName(RcTp),.typeMark,"!"++Fld));

    Defn = .varDef(Lc,AccVrNm,AccVrNm,AccFn,Cx,AccTp);
    Decl = .updDec(Lc,rebind(QV,reConstrainType(Cx,RcTp),Env),Fld,AccVrNm,AccTp);
    FDecl = .funDec(Lc,AccVrNm,AccVrNm,AccTp);
    if traceCanon! then
      logMsg("updater $(Decl)");
    valis ([Defn],[Decl,FDecl])
  }
    
  typeOfPtn:(ast,tipe,option[tipe],dict,string) => (canon,dict).
  typeOfPtn(A,Tp,_,Env,_) where Lc?=isAnon(A) => (anon(Lc,Tp),Env).
  typeOfPtn(A,Tp,ErTp,Env,Path) where (Lc,Id) ?= isName(A) &&
      varDefined(Id,Env) =>
    typeOfPtn(mkWhereEquality(A),Tp,ErTp,Env,Path).
  typeOfPtn(A,Tp,_,Env,Path) where (Lc,Id) ?= isName(A) => valof{
    Ev = declareVar(Id,Lc,Tp,faceOfType(Tp,Env),Env);
    valis (vr(Lc,Id,Tp),Ev)
  }
  typeOfPtn(A,Tp,ErTp,Env,Path) where _ ?= isEnumSymb(A) => valof{
    Enm = typeOfExp(A,Tp,ErTp,Env,Path);
    valis (Enm,Env)
  }
  typeOfPtn(A,Tp,ErTp,Env,Path) where isLitAst(A) => valof{
    Exp = typeOfExp(A,Tp,ErTp,Env,Path);
    valis (Exp,Env)
  }
  typeOfPtn(A,Tp,ErTp,Env,Path) where (Lc,E,T) ?= isTypeAnnotation(A) => valof{
    ETp = parseType([],T,Env);

    checkType(E,Tp,ETp,Env);
    valis typeOfPtn(E,snd(evidence(ETp,Env)),ErTp,Env,Path)
  }.
  typeOfPtn(A,Tp,ErTp,Env,Path) where 
      (Lc,E,C) ?= isWhere(A) => valof{
	(Ptn,Ev0) = typeOfPtn(E,Tp,ErTp,Env,Path);
	(Cond,Ev1) = checkCond(C,ErTp,Ev0,Path);
	valis (whr(Lc,Ptn,Cond),Ev1)
      }.
  typeOfPtn(A,Tp,ErTp,Env,Path) where (_,[El]) ?= isTuple(A) && ~ _ ?= isTuple(El) =>
    typeOfPtn(El,Tp,ErTp,Env,Path).
  typeOfPtn(A,Tp,ErTp,Env,Path) where (Lc,Els) ?= isTuple(A) => valof{
    Tvs = genTpVars(Els);
    checkType(A,.tupleType(Tvs),Tp,Env);
    (Ptns,Ev) = typeOfPtns(Els,Tvs,ErTp,[],Env,Path);
    valis (tple(Lc,Ptns),Ev)
  }
  typeOfPtn(A,Tp,ErTp,Env,Path) where (Lc,Op,Els) ?= isEnumCon(A) => valof{
    At = newTypeVar("A");
    Fun = typeOfExp(Op,consType(At,Tp),ErTp,Env,Path);
    Tps = genTpVars(Els);
    checkType(A,.tupleType(Tps),At,Env);
    (Args,Ev) = typeOfArgsPtn(Els,Tps,ErTp,Env,Path);
    valis (apply(Lc,Fun,Args,Tp),Ev)
  }
  typeOfPtn(A,Tp,ErTp,Env,Path) where (Lc,Op,Ss) ?= isLabeledRecord(A) &&
      (OLc,Nm)?=isName(Op) => valof{
    if traceCanon! then
      logMsg("labeled record ptn: $(A)");
    At = newTypeVar("A");
    Fun = typeOfExp(Op,consType(At,Tp),ErTp,Env,Path);

    (Q,ETp) = evidence(deRef(At),Env);
    FaceTp = ^faceOfType(ETp,Env);
    (Cx,Face) = deConstrain(FaceTp);
    Base = declareConstraints(Lc,Cx,declareTypeVars(Q,pushScope(Env)));
    (Els,Ev) = typeOfElementPtns(Ss,Face,ErTp,Base,Path,[]);
    Args = fillinElementPtns(Lc,Els,Face);
    valis (apply(Lc,vr(OLc,dlrName(Nm),typeOf(Fun)),Args,Tp),Ev)
  }
  typeOfPtn(A,Tp,ErTp,Env,Path) where (Lc,Op,Ss) ?= isLabeledTheta(A) &&
      (OLc,Nm)?=isName(Op) => valof{
    if traceCanon! then
      logMsg("labeled theta ptn: $(A)");
    At = newTypeVar("A");
    Fun = typeOfExp(Op,consType(At,Tp),ErTp,Env,Path);

    (Q,ETp) = evidence(deRef(At),Env);
    FaceTp = ^faceOfType(ETp,Env);
    (Cx,Face) = deConstrain(FaceTp);
    Base = declareConstraints(Lc,Cx,declareTypeVars(Q,pushScope(Env)));
    (Els,Ev) = typeOfElementPtns(Ss,Face,ErTp,Base,Path,[]);
    Args = fillinElementPtns(Lc,Els,Face);
    valis (apply(Lc,vr(OLc,dlrName(Nm),typeOf(Fun)),Args,Tp),Ev)
  }
  typeOfPtn(A,Tp,_,Env,_) => valof{
    Lc = locOf(A);
    reportError("illegal pattern: $(A), expecting a $(Tp)",Lc);
    valis (anon(Lc,Tp),Env)    
  }
    
  typeOfArgPtn:(ast,tipe,option[tipe],dict,string) => (canon,dict).
  typeOfArgPtn(A,Tp,ErTp,Env,Path) where (Lc,Els) ?= isTuple(A) => valof{
    Tvs = genTpVars(Els);
    checkType(A,.tupleType(Tvs),Tp,Env);
    (Ptns,Ev) = typeOfPtns(Els,Tvs,ErTp,[],Env,Path);
    valis (tple(Lc,Ptns),Ev)
  }
  typeOfArgPtn(A,Tp,ErTp,Env,Path) => typeOfPtn(A,Tp,ErTp,Env,Path).

  typeOfArgsPtn:(cons[ast],cons[tipe],option[tipe],dict,string) => (cons[canon],dict).
  typeOfArgsPtn(Els,Tps,ErTp,Env,Path) => typeOfPtns(Els,Tps,ErTp,[],Env,Path).

  typeOfElementPtns:(cons[ast],tipe,option[tipe],dict,string,cons[(string,canon)]) =>
    (cons[(string,canon)],dict).
  typeOfElementPtns([],_,_,Env,_,Prs) => (Prs,Env).
  typeOfElementPtns([D,..Ds],Tp,ErTp,Env,Pth,SoFr) => valof{
    if (Lc,Lhs,R) ?= isDefn(D) &&
	(_,Nm) ?= isName(Lhs) &&
	FTp ?= fieldInFace(Tp,Nm) then{
	  (Ptn,Ev) = typeOfPtn(R,FTp,ErTp,Env,Pth);
	  valis typeOfElementPtns(Ds,Tp,ErTp,Ev,Pth,[(Nm,Ptn),..SoFr])
	} else{
	  reportError("$(D) is not a legal element pattern",locOf(D));
	  valis ([],Env)
	}
  }

  fillinElementPtns:(option[locn],cons[(string,canon)],tipe) => cons[canon].
  fillinElementPtns(Lc,Ptns,Tp) =>
    project1(foldRight(((Nm,Pt),Ps)=>fillinElementPtn(Lc,Nm,Pt,Ps),
	_optval(fieldTypes(Tp))//((N,T))=>(N,vr(Lc,genSym("_"),T)),
	Ptns)).

  fillinElementPtn(Lc,Nm,Pt,Ps) => replace(Ps,((N,_))=>N==Nm,(Nm,Pt)).

  project1(L) => (L//snd).

  typeOfPtns:(cons[ast],cons[tipe],option[tipe],cons[canon],dict,string) => (cons[canon],dict).
  typeOfPtns([],[],_,Els,Env,_) => (reverse(Els),Env).
  typeOfPtns([P,..Ps],[T,..Ts],ErTp,Els,Env,Path) => valof{
    (Pt,E0) = typeOfPtn(P,snd(freshen(T,Env)),ErTp,Env,Path);
    valis typeOfPtns(Ps,Ts,ErTp,[Pt,..Els],E0,Path)
  }
  
  typeOfExp:(ast,tipe,option[tipe],dict,string) => canon.
  typeOfExp(A,Tp,_,Env,Path) where Lc ?= isAnon(A) => valof{
    reportError("anonymous variable not permitted in expression",Lc);
    valis anon(Lc,Tp)
  }
  typeOfExp(A,Tp,_,Env,Path) where (Lc,Id) ?= isName(A) => valof{
    if Var ?= findVar(Lc,Id,Env) then{
      if sameType(Tp,typeOf(Var),Env) then {
	valis Var
      } else{
	reportError("variable $(Id)\:$(typeOf(Var)) not consistent with expected type: $(Tp)",Lc);
	valis anon(Lc,Tp)
      }
    }
    else{
      reportError("variable $(Id) not defined. Expecting a $(Tp)",locOf(A));
      valis anon(locOf(A),Tp)
    }
  }
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,Nm) ?= isEnumSymb(A) => valof{
    Fun = typeOfExp(.nme(Lc,Nm),consType(tupleType([]),Tp),ErTp,Env,Path);
    valis apply(Lc,Fun,[],Tp)
  }
  typeOfExp(A,Tp,_,Env,Path) where (Lc,Ix) ?= isInt(A)  => valof{
    checkType(A,intType,Tp,Env);
    valis intr(Lc,Ix)
  }
  typeOfExp(A,Tp,_,Env,Path) where (Lc,Ix) ?= isBig(A)  => valof{
    checkType(A,bigintType,Tp,Env);
    valis bintr(Lc,Ix)
  }
  typeOfExp(A,Tp,_,Env,Path) where (Lc,Dx) ?= isFlt(A) => valof{
    checkType(A,fltType,Tp,Env);
    valis flt(Lc,Dx)
  }.
  typeOfExp(A,Tp,_,Env,Path) where (Lc,Cx) ?= isChr(A)  => valof{
    checkType(A,chrType,Tp,Env);
    valis kar(Lc,Cx)
  }
  typeOfExp(A,Tp,_,Env,Path) where (Lc,Sx) ?= isStr(A) => valof{
    checkType(A,strType,Tp,Env);
    valis strng(Lc,Sx)
  }.
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,E,T) ?= isTypeAnnotation(A) => valof{
    ETp = parseType([],T,Env);
    checkType(E,Tp,ETp,Env);
    valis typeOfExp(E,snd(evidence(ETp,Env)),ErTp,Env,Path)
  }.
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,E,C) ?= isWhere(A) => valof{
    (Cond,Ev0) = checkCond(C,ErTp,Env,Path);
    Exp = typeOfExp(E,Tp,ErTp,Ev0,Path);
    valis whr(Lc,Exp,Cond)
  }.
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,R,F) ?= isFieldAcc(A) && (_,Fld) ?= isName(F) => valof{
    RTp = newTypeVar("R");
    Rc = typeOfExp(R,RTp,ErTp,Env,Path);
    valis dot(Lc,Rc,Fld,Tp)
  }
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,R,Fld,V) ?= isRecordUpdate(A) => valof{
    Rc = typeOfExp(R,Tp,ErTp,Env,Path);
    VTp = newTypeVar("_V");
    Vl = typeOfExp(V,VTp,ErTp,Env,Path);
    valis update(Lc,Rc,Fld,Vl)
  }
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,T,L,R) ?= isConditional(A) => valof{
    (Tst,E0) = checkCond(T,ErTp,Env,Path);
    Thn = typeOfExp(L,Tp,ErTp,E0,Path);
    Els = typeOfExp(R,Tp,ErTp,Env,Path);
    valis cond(Lc,Tst,Thn,Els)
  }.
  typeOfExp(A,Tp,ErTp,Env,Path) where _ ?= isConjunct(A) => valof{
    (_,boolTp,_,_) = ^findType(Env,"boolean");
    checkType(A,boolTp,Tp,Env);
    (Gl,_) = checkCond(A,ErTp,Env,Path);
    valis Gl
  }.
  typeOfExp(A,Tp,ErTp,Env,Path) where _ ?= isDisjunct(A)  => valof{
    checkType(A,boolType,Tp,Env);
    (Gl,_) = checkCond(A,ErTp,Env,Path);
    valis Gl
  }.
  typeOfExp(A,Tp,ErTp,Env,Path) where _ ?= isNegation(A)  => valof{
    checkType(A,boolType,Tp,Env);
    (Gl,_) = checkCond(A,ErTp,Env,Path);
    valis Gl
  }.
  typeOfExp(A,Tp,ErTp,Env,Path) where _ ?= isMatch(A) => valof{
    checkType(A,boolType,Tp,Env);
    (Gl,_) = checkCond(A,ErTp,Env,Path);
    valis Gl
  }
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,G,Cases) ?= isCase(A) => valof{
    ETp = newTypeVar("_e");
    Gv = typeOfExp(G,ETp,ErTp,Env,Path);
    
    Rules = checkRules(Cases,ETp,Tp,ErTp,Env,Path,typeOfExp,[],.none);
    checkPtnCoverage(Rules//((.rule(_,.tple(_,[Ptn]),_,_))=>Ptn),Env,ETp);
    valis csexp(Lc,Gv,Rules,Tp)
  }
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,C) ?= isCellRef(A) => valof{
    Cl = typeOfExp(C,refType(Tp),ErTp,Env,Path);
    valis apply(Lc,vr(Lc,"_get",funType([refType(Tp)],Tp)),[Cl],Tp)
  }
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,V) ?= isRef(A) => valof{
    VTp = newTypeVar("_C");
    Vl = typeOfExp(V,VTp,ErTp,Env,Path);
    checkType(V,refType(VTp),Tp,Env);
    valis apply(Lc,vr(Lc,"_cell",funType([VTp],Tp)),[Vl],Tp)
  }
  typeOfExp(A,Tp,ErTp,Env,Path) where (_,[El]) ?= isTuple(A) && ~ _ ?= isTuple(El) =>
    typeOfExp(El,Tp,ErTp,Env,Path).
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,Els) ?= isTuple(A) => valof{
    Tvs = genTpVars(Els);
    checkType(A,.tupleType(Tvs),Tp,Env);
    Ptns = typeOfExps(Els,Tvs,ErTp,[],Env,Path);
    valis tple(Lc,Ptns)
  }
  typeOfExp(A,Tp,_,Env,Path) where (Lc,_,Ar,C,R) ?= isLambda(A) => valof{
    At = newTypeVar("_A");
    Rt = newTypeVar("_R");

    (Q,ETp) = evidence(Tp,Env);
    (Cx,ProgTp) = deConstrain(ETp);
    Es = declareConstraints(Lc,Cx,declareTypeVars(Q,Env));

    checkType(A,fnType(At,Rt),ProgTp,Es);
    
    (As,E0) = typeOfArgPtn(Ar,At,.none,Es,Path);
    LName = genSym(Path++"λ");

    if Cnd ?= C then {
      (Cond,E1) = checkCond(Cnd,.none,E0,Path);
      Rep = typeOfExp(R,Rt,.none,E1,Path);
      valis lambda(Lc,LName,[rule(Lc,As,?Cond,Rep)],Tp)
    } else{
      Rep = typeOfExp(R,Rt,.none,E0,Path);
      valis lambda(Lc,LName,[rule(Lc,As,.none,Rep)],Tp)
    }
  }
  typeOfExp(A,Tp,ErTp,Env,Pth) where (Lc,Op,Els) ?= isLabeledTheta(A) && (_,Nm)?=isName(Op) => valof{
    FceTp = newTypeVar("_");
    ConTp = consType(FceTp,Tp);
    Fun = typeOfExp(Op,ConTp,ErTp,Env,Pth);

    (Q,ETp) = evidence(FceTp,Env);
    FaceTp = ^faceOfType(ETp,Env);
    (Cx,Face) = deConstrain(FaceTp);
    Base = declareConstraints(Lc,Cx,declareTypeVars(Q,pushScope(Env)));
    
    (Defs,ExDecls,Decls,ThEnv) = thetaEnv(Lc,genNewName(Pth,"θ"),Els,Face,Base);

    valis formTheta(Lc,dlrName(Nm),Face,Defs,Decls,Tp)
  }
  typeOfExp(A,Tp,ErTp,Env,Pth) where (Lc,Op,Els) ?= isLabeledRecord(A) && (_,Nm)?=isName(Op) => valof{
--    logMsg("labeled record expression $(A) should be $(Tp)");
    FceTp = newTypeVar("_");
    ConTp = consType(FceTp,Tp);
--    logMsg("checking type of $(Op) against $(ConTp)");
    Fun = typeOfExp(Op,ConTp,ErTp,Env,Pth);
--    logMsg("$(Op) |: $(ConTp)");
    (Q,ETp) = evidence(FceTp,Env);
--    logMsg("face of type $(ETp)\:$(faceOfType(FceTp,Env))");
    FaceTp = ^faceOfType(ETp,Env);
    (Cx,Face) = deConstrain(FaceTp);
    Base = declareConstraints(Lc,Cx,declareTypeVars(Q,pushScope(Env)));
    
    (Defs,Decls) = recordEnv(Lc,genNewName(Pth,"θ"),Els,Face,Base,Env);
    
    valis formRecordExp(Lc,dlrName(Nm),Face,Defs,Decls,Tp)
  }
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,Els,Bnd) ?= isLetRecDef(A) => valof{
    (Defs,_,Decls,ThEnv)=thetaEnv(Lc,genNewName(Path,"Γ"),Els,.faceType([],[]),Env);
    
    El = typeOfExp(Bnd,Tp,ErTp,ThEnv,Path);

    valis genLetRec(Defs,Decls,(G,D,E) => letRec(Lc,G,D,E),El)
  }
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,Els,Bnd) ?= isLetDef(A) => valof{
--    logMsg("let exp $(A)");
    (Defs,Decls)=recordEnv(Lc,genNewName(Path,"Γ"),Els,.faceType([],[]),Env,Env);

    El = typeOfExp(Bnd,Tp,ErTp,declareDecls(Decls,Env),Path);
--    logMsg("bound exp $(El)");

    Sorted = sortDefs(Defs);

    valis foldRight((Gp,I)=>letExp(Lc,Gp,Decls,I),El,Sorted)
  }
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,Op,Args) ?= isEnumCon(A) => valof{
    Vrs = genTpVars(Args);
    At = .tupleType(Vrs);
    FFTp = newTypeFun("_F",2);
    Fun = typeOfExp(Op,consType(At,Tp),ErTp,Env,Path);
    Args = typeOfExps(Args,Vrs,ErTp,[],Env,Path);
    valis apply(Lc,Fun,Args,Tp)
  }
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,Op,Args) ?= isRoundTerm(A) =>
    typeOfRoundTerm(Lc,Op,Args,Tp,ErTp,Env,Path).
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,Ac) ?= isValof(A) => valof{
    (Act,_) = checkAction(Ac,Tp,ErTp,Env,Path);
    valis .vlof(Lc,Act,Tp)
  }
  typeOfExp(A,Tp,ErTp,Env,Path) where (Lc,Body,Rls) ?= isTryCatch(A) => valof{
    NErTp = newTypeVar("_E");
    NB = typeOfExp(Body,Tp,.some(NErTp),Env,Path);
    HRls = checkRules(Rls,NErTp,Tp,ErTp,Env,Path,typeOfExp,[],.none);
    
    valis .trycatch(Lc,NB,NErTp,HRls,Tp)
  }
  typeOfExp(A,Tp,_,_,_) => valof{
    reportError("cannot type check expression $(A)",locOf(A));
    valis anon(locOf(A),Tp)
  }.

  typeOfExps:(cons[ast],cons[tipe],option[tipe],cons[canon],dict,string) => cons[canon].
  typeOfExps([],[],_,Els,Env,_) =>reverse(Els).
  typeOfExps([P,..Ps],[T,..Ts],ErTp,Els,Env,Path) =>
    typeOfExps(Ps,Ts,ErTp,[typeOfExp(P,snd(evidence(T,Env)),ErTp,Env,Path),..Els],Env,Path).

  typeOfRoundTerm:(option[locn],ast,cons[ast],tipe,option[tipe],dict,string) => canon.
  typeOfRoundTerm(Lc,Op,As,Tp,ErTp,Env,Path) => valof{
--    logMsg("check round term $(Op)$(As) expected type $(Tp)");
    Vrs = genTpVars(As);
    At = .tupleType(Vrs);
    ExTp = newTypeVar("F");
    Fun = typeOfExp(Op,ExTp,ErTp,Env,Path);
    logMsg("Fun $(Fun)\:$(ExTp)");
    FnTp = fnType(At,Tp);

    if sameType(ExTp,FnTp,Env) then{
      Args = typeOfExps(As,Vrs,ErTp,[],Env,Path);      
      valis apply(Lc,Fun,Args,Tp)
    } else if ETp?=ErTp && sameType(ExTp,.throwsType(FnTp,ETp),Env) then{
      Args = typeOfExps(As,Vrs,ErTp,[],Env,Path);      
      valis apply(Lc,Fun,Args,Tp)
    } else{
      reportError("type of $(Op)\:$(ExTp) not consistent with $(FnTp)",Lc);
      valis vr(Lc,"_",Tp)
    }
  }

  checkAction:(ast,tipe,option[tipe],dict,string) => (canonAction,dict).
  checkAction(A,Tp,ErTp,Env,Path) where (Lc,[St]) ?= isBrTuple(A) =>
    checkAction(St,Tp,ErTp,Env,Path).
  checkAction(A,_Tp,_ErTp,Env,Path) where (Lc,[]) ?= isBrTuple(A) =>
    (doNop(Lc),Env).
  checkAction(A,Tp,ErTp,Env,Path) where (Lc,L,R) ?= isActionSeq(A) => valof{
    (LL,E0) = checkAction(L,Tp,ErTp,Env,Path);
    (RR,E1) = checkAction(R,Tp,ErTp,E0,Path);
    valis (doSeq(Lc,LL,RR),E1)
  }
  checkAction(A,Tp,ErTp,Env,Path) where (_,L) ?= isSoloSeq(A) =>
    checkAction(L,Tp,ErTp,Env,Path).
  checkAction(A,Tp,ErTp,Env,Path) where (Lc,Lb,Ac) ?= isLbldAction(A) => valof{
    (RR,E1) = checkAction(Ac,Tp,ErTp,Env,Path);
    valis (doLbld(Lc,Lb,RR),E1)
  }
  checkAction(A,_Tp,_ErTp,Env,_Path) where (Lc,Lb) ?= isBreak(A) =>
    (doBrk(Lc,Lb),Env).
  checkAction(A,Tp,ErTp,Env,Path) where (Lc,E) ?= isValis(A) => valof{
    V = typeOfExp(E,Tp,ErTp,Env,Path);
    valis (doValis(Lc,V),Env)
  }
  checkAction(A,_Tp,.some(ErTp),Env,Path) where (Lc,E) ?= isThrow(A) => valof{
    V = typeOfExp(E,ErTp,.none,Env,Path);
    valis (doThrow(Lc,V),Env)
  }
  checkAction(A,Tp,.none,Env,Path) where (Lc,E) ?= isThrow(A) => valof{
    reportError("Not permitted to throw",Lc);
    valis (doNop(Lc),Env)
  }
  checkAction(A,_,ErTp,Env,Path) where (Lc,Lhs,Rhs) ?= isDefn(A) => valof{
    if (ILc,Id) ?= isName(Lhs) then{
      Tp = newTypeVar("_V");
      Val = typeOfExp(Rhs,Tp,ErTp,Env,Path);
      Ev = declareVar(Id,Lc,Tp,faceOfType(Tp,Env),Env);
      valis (doDefn(Lc,vr(ILc,Id,Tp),Val),Ev)
    } else if (ILc,Els) ?= isTuple(Lhs) then {
      TTp = newTypeVar("_T");
      (Ptn,Ev) = typeOfPtn(Lhs,TTp,ErTp,Env,Path);
      Val = typeOfExp(Rhs,TTp,ErTp,Env,Path);
      valis (doMatch(Lc,Ptn,Val),Ev)
    } else{
      reportError("invalid lhs $(Lhs) of definition",locOf(Lhs));
      valis (doNop(Lc),Env)
    }
  }
  checkAction(A,_,ErTp,Env,Path) where (Lc,Lhs,Rhs) ?= isAssignment(A) =>
    checkAssignment(Lc,Lhs,Rhs,ErTp,Env,Path).
  checkAction(A,Tp,ErTp,Env,Path) where (Lc,Body,Rls) ?= isTryCatch(A) => valof{
    NErTp = newTypeVar("_E");
    (NB,_) = checkAction(Body,Tp,.some(NErTp),Env,Path);
    Hs = checkRules(Rls,NErTp,Tp,ErTp,Env,Path,
      (AA,_,_,Ev,_)=>valof{
	(HA,_)=checkAction(AA,Tp,ErTp,Ev,Path);
	valis HA
      },[],.none);
    valis (.doTryCatch(Lc,NB,NErTp,Hs),Env)
  }
  checkAction(A,Tp,ErTp,Env,Path) where (Lc,C,T,E) ?= isIfThenElse(A) => valof{
    (CC,E0) = checkGoal(C,ErTp,Env,Path);
    (TT,_) = checkAction(T,Tp,ErTp,E0,Path);
    (EE,_) = checkAction(E,Tp,ErTp,Env,Path);
    valis (doIfThen(Lc,CC,TT,EE),Env)
  }
  checkAction(A,Tp,ErTp,Env,Path) where (Lc,C,T) ?= isIfThen(A) => valof{
    (CC,E0) = checkGoal(C,ErTp,Env,Path);
    (TT,_) = checkAction(T,Tp,ErTp,E0,Path);
    valis (doIfThen(Lc,CC,TT,doNop(Lc)),Env)
  }
  checkAction(A,Tp,ErTp,Env,Path) where (Lc,C,B) ?= isWhileDo(A) => valof{
    (CC,E0) = checkGoal(C,ErTp,Env,Path);
    (BB,_) = checkAction(B,Tp,ErTp,E0,Path);
    valis (doWhile(Lc,CC,BB),Env)
  }
  checkAction(A,Tp,ErTp,Env,Path) where (Lc,Ds,B) ?= isLetDef(A) => valof{
    (Defs,Decls)=recordEnv(Lc,genNewName(Path,"Γ"),Ds,.faceType([],[]),Env,Env);

    (Ac,_) = checkAction(B,Tp,ErTp,declareDecls(Decls,Env),Path);
    Sorted = sortDefs(Defs);

    valis (foldRight((Gp,I)=>doLet(Lc,Gp,Decls,I),Ac,Sorted),Env)
  }
  checkAction(A,Tp,ErTp,Env,Path) where (Lc,Ds,B) ?= isLetRecDef(A) => valof{
    (Defs,_,Decls,ThEnv)=thetaEnv(Lc,genNewName(Path,"Γ"),Ds,.faceType([],[]),Env);
    (Ac,_) = checkAction(B,Tp,ErTp,ThEnv,Path);

    valis (genLetRec(Defs,Decls,(G,D,E)=>doLetRec(Lc,G,D,E),Ac),Env)
  }
  checkAction(A,Tp,ErTp,Env,Path) where (Lc,G,Cases) ?= isCase(A) => valof{
    ETp = newTypeVar("_e");
    Gv = typeOfExp(G,ETp,ErTp,Env,Path);
    Rules = checkRules(Cases,ETp,Tp,ErTp,Env,Path,
      (Ac,_,_,E,Path) => valof{
	(Act,_) = checkAction(Ac,Tp,ErTp,E,Path);
	valis Act
      },[],.none);
    valis (doCase(Lc,Gv,Rules),Env)
  }
  checkAction(A,Tp,ErTp,Env,Path) where (Lc,T,E,Cases) ?= isSuspend(A) => valof{
    STp = newTypeVar("_s");
    RTp = newTypeVar("_r");
    TT = typeOfExp(T,fiberType(RTp,STp),ErTp,Env,Path);
    EE = typeOfExp(E,STp,ErTp,Env,Path);
    
    Rules = checkRules(Cases,RTp,Tp,ErTp,Env,Path,
      (Ac,_,_,Ev,Path) => valof{
	(Act,_) = checkAction(Ac,Tp,ErTp,Ev,Path);
	valis Act
      },[],.none);
    valis (.doSuspend(Lc,TT,EE,RTp,Rules),Env)
  }
  checkAction(A,Tp,ErTp,Env,Path) where (Lc,T,E,Cases) ?= isResume(A) => valof{
    STp = newTypeVar("_s");
    RTp = newTypeVar("_r");
    TT = typeOfExp(T,fiberType(RTp,STp),ErTp,Env,Path);
    EE = typeOfExp(E,RTp,ErTp,Env,Path);

    Rules = checkRules(Cases,STp,Tp,ErTp,Env,Path,
      (Ac,_,_,Ev,Path) => valof{
	(Act,_) = checkAction(Ac,Tp,ErTp,Ev,Path);
	valis Act
      },[],.none);
    valis (.doResume(Lc,TT,EE,STp,Rules),Env)
  }
  checkAction(A,Tp,ErTp,Env,Path) where (Lc,T,E) ?= isRetire(A) => valof{
    STp = newTypeVar("_s");
    RTp = newTypeVar("_r");
    TT = typeOfExp(T,fiberType(RTp,STp),ErTp,Env,Path);
    EE = typeOfExp(E,STp,ErTp,Env,Path);

    valis (doRetire(Lc,TT,EE),Env)
  }
  
  checkAction(A,Tp,ErTp,Env,Path) where (Lc,Op,Args) ?= isRoundTerm(A) => valof{
    Call = typeOfRoundTerm(Lc,Op,Args,unitTp,ErTp,Env,Path);
    valis (doCall(Lc,Call),Env)
  }
  checkAction(A,_Tp,_ErTp,Env,_Path) default => valof{
    Lc = locOf(A);
    reportError("invalid action $(A)",Lc);
    valis (doNop(Lc),Env)
  }

  checkAssignment(Lc,Lhs,Rhs,ErTp,Env,Path) => valof{
    Tp = newTypeVar("_V");
    if (ILc,Id) ?= isName(Lhs) && ~ varDefined(Id,Env) then {
      RfTp = refType(Tp);
      Val = typeOfExp(Rhs,Tp,ErTp,Env,Path);
      Ev = declareVar(Id,ILc,refType(Tp),.none,Env);
      valis (doDefn(Lc,vr(ILc,Id,RfTp),apply(Lc,vr(Lc,"_cell",funType([Tp],RfTp)),[Val],RfTp)),Ev)
    } else{
      Val = typeOfExp(Rhs,Tp,ErTp,Env,Path);
      Var = typeOfExp(Lhs,refType(Tp),ErTp,Env,Path);
      valis (doAssign(Lc,Var,Val),Env)
    }
  }

  checkRules:all t ~~ (cons[ast],tipe,tipe,option[tipe],dict,string,
    (ast,tipe,option[tipe],dict,string)=>t,cons[rule[t]],option[rule[t]]) => cons[rule[t]].
  checkRules([],_,_,_,_,_,_,Els,.none) => reverse(Els).
  checkRules([],_,_,_,_,_,_,Els,.some(Dflt)) => reverse([Dflt,..Els]).
  checkRules([Cs,..Ps],ATp,Tp,ErTp,Env,Path,Chk,SoFar,Deflt) => valof{
    if (Rl,Dflt) ?= checkRule(Cs,ATp,Tp,ErTp,Env,Chk,Path) then{
      if Dflt then{
	if DRl?=Deflt then{
	  reportError("cannot have more than one default, other one at $(locOf(DRl))",
	    locOf(Cs));
	}
	else
	valis checkRules(Ps,ATp,Tp,ErTp,Env,Path,Chk,SoFar,.some(Rl))
      }
      else
      valis checkRules(Ps,ATp,Tp,ErTp,Env,Path,Chk,[Rl,..SoFar],Deflt)
    } else
    valis checkRules(Ps,ATp,Tp,ErTp,Env,Path,Chk,SoFar,Deflt)
  }

  checkRule(St,ATp,RTp,ErTp,Env,Chk,Path) where (Lc,IsDeflt,Lhs,Cnd,R) ?= isLambda(St) => valof{
    (Arg,E0) = typeOfPtn(Lhs,ATp,ErTp,Env,Path);
	
    if Wh?=Cnd then{
      (Cond,E1) = checkCond(Wh,ErTp,E0,Path);
      Rep = Chk(R,RTp,ErTp,E1,Path);
      (Ags,ACnd) = pullWhere(Arg,.some(Cond));
      valis .some((rule(Lc,tple(Lc,[Ags]),ACnd,Rep),IsDeflt))
    }
    else{
      (Ags,ACnd) = pullWhere(Arg,.none);
      Rep = Chk(R,RTp,ErTp,E0,Path);
      valis .some((rule(Lc,tple(Lc,[Ags]),ACnd,Rep),IsDeflt))
    }
  }
  checkRule(St,_,_,_,_,_,_) default => valof{
    reportError("expecting a rule, not $(St)",locOf(St));
    valis .none
  }

  pullWhere:(canon,option[canon]) => (canon,option[canon]).
  pullWhere(.whr(WLc,Vl,Cn),Gl) where (Val,G1) .= pullWhere(Vl,Gl) =>
    (Val,mergeGoal(WLc,.some(Cn),G1)).
  pullWhere(Exp,Gl) default => (Exp,Gl).

  mergeGoal:(option[locn],option[canon],option[canon])=>option[canon].
  mergeGoal(_,Gl,.none) => Gl.
  mergeGoal(_,.none,Gl) => Gl.
  mergeGoal(Lc,.some(Gl),.some(H)) => .some(conj(Lc,Gl,H)).
  
  checkCond:(ast,option[tipe],dict,string) => (canon,dict).
  checkCond(A,ErTp,Env,Path) => checkGoal(A,ErTp,Env,Path).

  checkGoal:(ast,option[tipe],dict,string) => (canon,dict).
  checkGoal(A,ErTp,Env,Path) where (Lc,L,R) ?= isConjunct(A) => valof{
    (Lhs,E0) = checkGoal(L,ErTp,Env,Path);
    (Rhs,E1) = checkGoal(R,ErTp,E0,Path);
    valis (conj(Lc,Lhs,Rhs),E1)
  }.
  checkGoal(A,ErTp,Env,Path) where (Lc,L,R) ?= isDisjunct(A) => valof{
    (Lhs,E0) = checkGoal(L,ErTp,Env,Path);
    (Rhs,E1) = checkGoal(R,ErTp,Env,Path);
    valis (disj(Lc,Lhs,Rhs),mergeDict(E0,E1,Env))
  }.
  checkGoal(A,ErTp,Env,Path) where (Lc,R) ?= isNegation(A) => valof{
    (Rhs,_) = checkGoal(R,ErTp,Env,Path);
    valis (neg(Lc,Rhs),Env)
  }.
  checkGoal(A,ErTp,Env,Path) where (Lc,T,L,R) ?= isConditional(A) => valof{
    (Tst,E0) = checkGoal(T,ErTp,Env,Path);
    (Thn,E1) = checkGoal(L,ErTp,E0,Path);
    (Els,E2) = checkGoal(R,ErTp,Env,Path);
    valis (cond(Lc,Tst,Thn,Els),mergeDict(E1,E2,Env))
  }.
  checkGoal(A,ErTp,Env,Path) where (Lc,L,R) ?= isMatch(A) => valof{
    PtnTp = newTypeVar("_M");
    Val = typeOfExp(R,PtnTp,ErTp,Env,Path);
    (Ptn,Ev) = typeOfPtn(L,PtnTp,ErTp,Env,Path);
    valis (match(Lc,Ptn,Val),Ev)
  }
  checkGoal(A,ErTp,Env,Path) where (_,[Inner]) ?= isTuple(A) =>
    checkGoal(Inner,ErTp,Env,Path).
  checkGoal(A,ErTp,Env,Path) => valof{
    (_,boolTp,_,_) = ^findType(Env,"boolean");
    Exp = typeOfExp(A,boolTp,ErTp,Env,Path);
    valis (Exp,Env)
  }

  checkType:(ast,tipe,tipe,dict) => ().
  checkType(_,Actual,Expected,Env) where sameType(Actual,Expected,Env) => ().
  checkType(A,ATp,ETp,_) => valof{
    reportError("$(A)\:$(ATp) not consistent with expected type $(ETp)",locOf(A));
    valis ()
  }.

  genTpVars:(cons[ast]) => cons[tipe].
  genTpVars(Els) => (Els//(_)=>newTypeVar("_v")).

  genArgTps:(ast) => cons[tipe].
  genArgTps(A) where (_,Ar,_) ?= isWhere(A) =>
    genArgTps(Ar).
  genArgTps(A) where (_,Els) ?= isTuple(A) =>
      genTpVars(Els).
}
