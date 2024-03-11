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
    if (Lc,Pk,Els) ?= isQBrTerm(P) && Pkg .= pkgeName(Pk) then{
      if compatiblePkg(Pkg,Pkge) then{
	(Imports,Stmts) = collectImports(Els,[],[]);
	(AllImports,IDecls) = importAll(Imports,Repo,[],[]);

	-- if traceCanon! then
	--   logMsg("Import declarations $(IDecls)");

	PkgEnv = declareDecls(IDecls,stdDict);
	PkgPth = packageName(Pkg);

	(Vis,Opens,Annots,Gps) = dependencies(Stmts);

	if ~isEmpty(Opens) then
	  reportError("open statements $(Opens) not currently supported",Lc);
    
	(Defs,ThDecls,ThEnv) = checkGroups(Gps,pkgExport(Vis),emptyFace,Annots,PkgEnv,PkgPth);
	
	(BrDfs,BrDcs) = pullBrDefs(ThEnv);
	(AllX,All) = squashDecls(ThDecls);

	RDefs = (errorFree() ??
	  overloadProgram([BrDfs,..Defs],declareDecls(All++BrDcs,PkgEnv))* || []);

	if traceCanon! then{
	  logMsg("pkg definitions: $(RDefs),\nexports: $(AllX)");
	};

	valis (pkgSpec{pkg=Pkge. imports=Imports. exports=AllX},
	  RDefs,IDecls,All++BrDcs)
      }
      else
      reportError("package name $(Pkg) does not match expected $(Pkge)",locOf(P))
    } else
    reportError("invalid package structure",locOf(P));
    valis (pkgSpec{pkg=Pkge. imports=[]. exports=[]},[],[],[])
  }

  squashDecls:(cons[(cons[decl],cons[decl])])=>(cons[decl],cons[decl]).
  squashDecls(Th) => foldLeft(((A,B),(C,D))=>(A++C,B++D),([],[]),Th).

  publishFn ~> (defnSp,cons[decl])=>cons[decl].

  pkgExport:(visMap) => publishFn.
  pkgExport(Viz) => (Sp,Dcs) => (isVisible(Viz,.pUblic,Sp) ?? Dcs || []).

  letExport:(visMap) => publishFn.
  letExport(Viz) => (Sp,Dcs) => (isVisible(Viz,.deFault,Sp) ?? Dcs||[]).

  pickDefltVis(.deFault,V) => V.
  pickDefltVis(V,_) => V.

  isVisible:(visMap,visibility,defnSp)=>boolean.
  isVisible([],D,_) => D>=.deFault.
  isVisible([(e,V),.._],D,e) => V>=D.
  isVisible([_,..l],D,e) => isVisible(l,D,e).

  formRecordExp:(option[locn],canon,tipe,cons[canonDef],cons[decl],cons[decl],tipe) => canon.
  formRecordExp(Lc,Lbl,.faceType(Flds,Tps),Defs,XDcs,Decls,Tp) => valof{
    sortedFlds = sortFields(Flds);
    Dcls = getFullNms([(XDcs,Decls)]);
    
    valis .letExp(Lc,Defs,Decls,.apply(Lc,Lbl,
	(sortedFlds//((FNm,FTp)) => FullNm ?=Dcls[FNm] ?? .vr(Lc,FullNm,FTp) || .vr(Lc,FNm,FTp)),Tp))
  }

  genLetRec:all e,g,d ~~ (cons[g],cons[(cons[d],cons[d])],(g,cons[d],e)=>e,e) => e.
  genLetRec([],[],_,E) => E.
  genLetRec([G,..Gps],[(_,A),..Ds],F,E) => F(G,A,genLetRec(Gps,Ds,F,E)).

  formTheta:(option[locn],canon,tipe,cons[cons[canonDef]],cons[(cons[decl],cons[decl])],tipe) =>
    canon.
  formTheta(Lc,Lbl,.faceType(Flds,Tps),Defs,Decls,Tp) => valof{
    sortedFlds = sortFields(Flds);
    Dcls = getFullNms(Decls);
    
    valis genLetRec(Defs,Decls,(G,D,E) => .letRec(Lc,G,D,E),
      .apply(Lc,Lbl,(sortedFlds//(((Nm,FTp)) => (FullNm ?=Dcls[Nm] ?? .vr(Lc,FullNm,FTp) || .vr(Lc,Nm,FTp)))),Tp))
  }

  getFullNms:(cons[(cons[decl],cons[decl])]) => map[string,string].
  getFullNms(Decs) => let{.
    addDecs:(cons[decl],map[string,string])=>map[string,string].
    addDecs(Dcs,M) => foldLeft(addDec,M,Dcs).

    addDec(.varDec(_,Nm,FullNm,_),M) => M[Nm->FullNm].
    addDec(.funDec(_,Nm,FullNm,_),M) => M[Nm->FullNm].
    addDec(_,M) => M.
  .} in foldLeft(((X,A),M)=>addDecs(A,M),[],Decs).

  thetaEnv:(option[locn],string,cons[ast],tipe,dict) =>
    (cons[cons[canonDef]],cons[(cons[decl],cons[decl])],dict).
  thetaEnv(Lc,Pth,Stmts,Face,Env) => valof{
    (Vis,Opens,Annots,Gps) = dependencies(Stmts);

    if ~isEmpty(Opens) then
      reportError("open statements $(Opens) not supported",Lc);

    if traceCanon! then{
      logMsg("visibility of $(Stmts) is $(Vis)")
    };
    
    valis checkGroups(Gps,letExport(Vis),Face,Annots,pushFace(Face,Lc,Env,Pth),Pth)
  }

  recordEnv:(option[locn],string,cons[ast],tipe,dict,dict) =>
    (cons[canonDef],cons[decl],cons[decl]).
  recordEnv(Lc,Path,Stmts,Face,Env,Outer) => valof{
    -- We sort for dependencies to get types right
    (Vis,Opens,Annots,Gps) = dependencies(Stmts);

    if ~isEmpty(Opens) then{
      reportError("open statements not implemented",Lc)
    };

    if traceCanon! then{
      logMsg("visibility of $(Stmts) is $(Vis)")
    };
    
    G = Gps*; -- Flatten the result

    TmpEnv = parseAnnotations(G,Face,Annots,Env,Path);
    valis checkGroup(G,letExport(Vis),TmpEnv,Outer,Path);
  }

  checkGroups:(cons[cons[defnSpec]],publishFn,tipe,map[string,ast],dict,string) =>
    (cons[cons[canonDef]],cons[(cons[decl],cons[decl])],dict).
  checkGroups(AGps,Publish,Face,Annots,Env,Path) => let{.
    checkGps([],Ev,Gps,Dcs) => (reverse(Gps),reverse(Dcs),Ev).
    checkGps([G,..Gs],Ev,Gx,Dx) => valof{
      TmpEnv = parseAnnotations(G,Face,Annots,Ev,Path);
      (Gp,XDs,Ds) = checkGroup(G,Publish,TmpEnv,TmpEnv,Path);
      valis checkGps(Gs,declareDecls(Ds,Ev),[Gp,..Gx],[(XDs,Ds),..Dx])
    }
  .} in checkGps(AGps,Env,[],[]).

  parseAnnotations:(cons[defnSpec],tipe,map[string,ast],dict,string) => dict.
  parseAnnotations([],_,_,Env,_) => Env.
  parseAnnotations([.defnSpec(.varSp(Nm),Lc,Stmts),..Gs],Fields,Annots,Env,Path) => valof{
    Tp = parseAnnotation(Nm,Lc,Stmts,Fields,Annots,Env);
    valis parseAnnotations(Gs,Fields,Annots,
      declareVar(Nm,qualifiedName(Path,.valMark,Nm),Lc,Tp,faceOfType(Tp,Env),Env),Path)
  }
  parseAnnotations([.defnSpec(.tpSp(Nm),Lc,[Stmt]),..Gs],Fields,Annots,Env,Path) =>
    parseAnnotations(Gs,Fields,Annots,defineType(Nm,Lc,Stmt,Env,Path),Path).
  parseAnnotations([_,..Gs],Fields,Annots,Env,Path) => parseAnnotations(Gs,Fields,Annots,Env,Path).

  -- Prescan a type definition, before actually parsing it properly later
  defineType:(string,option[locn],ast,dict,string) => dict.
  defineType(Nm,Lc,Stmt,Env,Path) => valof{
    if (Tmpl,TpRl) ?= parseTypeCore(Stmt,Env,Path) then
      valis declareType(Nm,Lc,Tmpl,TpRl,Env)
    else{
      reportError("Cannot parse type statement $(Stmt)",Lc);
      valis Env
    }
  }

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

  checkGroup:(cons[defnSpec],publishFn,dict,dict,string) =>
    (cons[canonDef],cons[decl],cons[decl]).
  checkGroup(Specs,Publish,Env,Outer,Path) => let{.
    checkDefs([],Dfs,XptDcs,Dcs,Ev) => (reverse(Dfs),XptDcs,Dcs).
    checkDefs([D,..Ds],Defs,XDcs,Decls,Ev) => valof{
      (Dfs,Xpts,Dcs) = checkDefn(D,Publish,Ev,Outer,Path);

      valis checkDefs(Ds,Dfs++Defs,XDcs++Xpts,Decls++Dcs,declareDecls(Dcs,Ev))
    }.

    redoTypeLambdas = valof{
      for S in Specs do{
	if .defnSpec(.tpSp(_),_,[St]) .= S && _ ?= isTypeFunStmt(St) then{
	  valis [|Specs|]>1
	}
      };
      valis .false
    }
  .} in valof{
    if redoTypeLambdas then{
      (_,Xpts,Dcs) = checkDefs(Specs,[],[],[],Env);
      valis checkDefs(Specs,[],[],[],declareDecls(Xpts,declareDecls(Dcs,Env)))
    } else{
      valis checkDefs(Specs,[],[],[],Env)
    }
  }

  checkDefn:(defnSpec,publishFn,dict,dict,string) => (cons[canonDef],cons[decl],cons[decl]).
  checkDefn(Defn,Publish,Env,Outer,Path) => case Defn in {
    .defnSpec(.varSp(Nm),Lc,Stmts) where Tp ?= varType(Nm,Env) && areEquations(Stmts) => valof{
      (Defs,Decls) = checkFunction(Nm,Tp,Lc,Stmts,Env,Outer,Path);
      valis (Defs,Publish(.varSp(Nm),Decls),Decls)
    }.
    .defnSpec(.varSp(Nm),Lc,[Stmt]) where Tp ?= varType(Nm,Env) => valof{
      (Defs,Decls) = checkVar(Nm,Tp,Lc,Stmt,Env,Outer,Path);
      valis (Defs,Publish(.varSp(Nm),Decls),Decls)
    }.
    .defnSpec(.tpSp(Nm),Lc,[St]) => valof{
      if traceCanon! then
	logMsg("parse type defn $(St)");

      (Df,Dcs) = parseTypeDef(Nm,St,Env,Path);
      if traceCanon! then
	logMsg("declarations from $(St)\:$(Dcs)");
      valis (Df,Publish(.tpSp(Nm),Dcs),Dcs)
    }.
    .defnSpec(.cnsSp(_),_,_) => ([],[],[]).
    .defnSpec(.conSp(ConNm),Lc,[St]) => valof{
      (Defs,Decls) = parseContract(St,Env,Path);
      valis (Defs,Publish(.conSp(ConNm),Decls),Decls)
    }.
    .defnSpec(.implSp(Nm),Lc,[St]) => valof {
      if (_,Q,C,H,B) ?= isImplementationStmt(St) then{
	(Defs,Decls) = checkImplementation(Lc,Q,C,H,B,Env,Outer,Path);
	valis (Defs,Publish(.implSp(Nm),Decls),Decls)
      }
      else{
	reportError("not a valid implementation statement",Lc);
	valis ([],[],[])
      }
    }.
  }

  checkFunction:(string,tipe,option[locn],cons[ast],dict,dict,string) =>
    (cons[canonDef],cons[decl]).
  checkFunction(Nm,Tp,Lc,Stmts,Env,Outer,Path) => valof{
    if traceCanon! then
      logMsg("check function $(Stmts)\:$(Tp)");

    (Q,ETp) = evidence(Tp,Env);
    (Cx,ProgramType) = deConstrain(ETp);
    Es = declareConstraints(Lc,Cx,declareTypeVars(Q,Env));
    Rls = processEqns(Stmts,ProgramType,[],.none,Es,
      declareConstraints(Lc,Cx,declareTypeVars(Q,Outer)),Path);
    FullNm = qualifiedName(Path,.valMark,Nm);

    if traceCanon! then
      logMsg("function $(.funDef(Lc,FullNm,Rls,Cx,Tp))");
    
    valis ([.funDef(Lc,FullNm,Rls,Cx,Tp)],[.funDec(Lc,Nm,FullNm,Tp)])
  }

  checkVar:(string,tipe,option[locn],ast,dict,dict,string) => (cons[canonDef],cons[decl]).
  checkVar(Nm,Tp,Lc,Stmt,Env,Outer,Path) => valof{
    if traceCanon! then
      logMsg("check definition $(Stmt)\:$(Tp)@$(Lc)");

    (Q,ETp) = evidence(Tp,Env);
    (Cx,VarTp) = deConstrain(ETp);
    Es = declareConstraints(Lc,Cx,declareTypeVars(Q,Outer));
    if (_,Lhs,R) ?= isDefn(Stmt) then{
      Val = typeOfExp(R,VarTp,Es,Path);
      FullNm = qualifiedName(Path,.valMark,Nm);
      if traceCanon! then
	logMsg("definition $(.varDef(Lc,FullNm,Val,Cx,Tp))");

      if .lambda(_,_,Rl,_,_).=Val then
	valis ([.funDef(Lc,FullNm,[Rl],Cx,Tp)],[.funDec(Lc,Nm,FullNm,Tp)])
      else if ~isEmpty(Cx) then
	valis ([.varDef(Lc,FullNm,Val,Cx,Tp)],[.funDec(Lc,Nm,FullNm,Tp)])
      else
      valis ([.varDef(Lc,FullNm,Val,Cx,Tp)],[.varDec(Lc,Nm,FullNm,Tp)])
    }
    else{
      reportError("bad definition $(Stmt)",Lc);
      valis ([],[])
    }
  }

  processEqns:(cons[ast],tipe,cons[rule[canon]],option[rule[canon]],dict,dict,string) =>
    cons[rule[canon]].
  processEqns([],_,Rls,.none,_,_,_) => reverse(Rls).
  processEqns([],_,Rls,.some(Dflt),_,_,_) => reverse([Dflt,..Rls]).
  processEqns([St,..Ss],ProgramType,Rls,Deflt,Env,Outer,Path) => valof{
    (Rl,IsDeflt) = processEqn(St,ProgramType,Env,Outer,Path);
    if IsDeflt then{
      if DRl ?= Deflt then{
	reportError("cannot have more than one default, other one at $(locOf(DRl))",
	  locOf(St));
	valis []
      } else{
	valis processEqns(Ss,ProgramType,Rls,.some(Rl),Env,Outer,Path)
      }
    }
    else{
      valis processEqns(Ss,ProgramType,[Rl,..Rls],Deflt,Env,Outer,Path)
    }
  }

  processEqn(St,ProgramType,Env,Outer,Path) where (Lc,_,IsDeflt,Arg,Cnd,R) ?= isEquation(St) => valof{
    Ats = genArgTps(Arg);
    RTp = newTypeVar("_R");
    checkType(St,funType(Ats,RTp),ProgramType,Env);
    (Args,ACnd,E0) = typeOfArgPtn(Arg,.tupleType(Ats),Outer,Path);

    if Wh?=Cnd then{
      (Cond,E1) = checkCond(Wh,E0,Path);
      Rep = typeOfExp(R,RTp,E1,Path);

      valis (.rule(Lc,Args,mergeGoal(Lc,ACnd,.some(Cond)),Rep),IsDeflt)
    } else{
      valis (.rule(Lc,Args,ACnd,typeOfExp(R,RTp,E0,Path)),IsDeflt)
    }
  }

  checkImplementation:(option[locn],cons[ast],cons[ast],ast,ast,dict,dict,string) =>
    (cons[canonDef],cons[decl]).
  checkImplementation(Lc,Q,C,H,B,Env,Outer,Path) => valof{
    if traceCanon! then{
      logMsg("checking implementation for $(H) = $(B) at $(Lc)");
    };
    
    BV = parseBoundTpVars(Q);
    Cx = parseConstraints(C,BV,Env);
    Cn = parseContractConstraint(BV,H,Env);
    ConName = localName(conTractName(Cn),.typeMark);
    
    if Con ?= findContract(Env,ConName) && (_,CTp,_,_) ?= findType(Env,ConName)then{
      (_,.contractExists(CnNm,CnTps,CnDps,ConFaceTp)) = freshen(Con,Env);
      ConTp = mkConType(CnNm,CnTps,CnDps);
      if sameType(ConTp,typeOf(Cn),Env) then {
	Es = declareConstraints(Lc,Cx,declareTypeVars(BV,Outer));
	Impl = typeOfExp(B,ConTp,Es,Path);
	ImplNm = implementationName(.conTract(CnNm,CnTps,CnDps));
	ImplVrNm = qualifiedName(Path,.valMark,ImplNm);
	ImplTp = rebind(BV,reConstrainType(Cx,ConTp),Es);
	if traceCanon! then
	  logMsg("implementation definition $(.implDef(Lc,ImplNm,ImplVrNm,Impl,Cx,ImplTp))");
	
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

  typeOfPtn:(ast,tipe,dict,string) => (canon,option[canon],dict).
  typeOfPtn(A,Tp,Env,_) where Lc?=isAnon(A) => (.anon(Lc,Tp),.none,Env).
  typeOfPtn(A,Tp,Env,Path) where (Lc,Id) ?= isName(A) && varDefined(Id,Env) => valof{
    V = genName(Lc,Id);
    (Ptn,PCond,Ev0) = typeOfPtn(V,Tp,Env,Path);
    (Cond,Ev1) = checkCond(binary(Lc,"==",V,A),Ev0,Path);
    valis (Ptn,mergeGoal(Lc,PCond,.some(Cond)),Ev1)
  }.
  typeOfPtn(A,Tp,Env,Path) where (Lc,Id) ?= isName(A) => valof{
    Ev = declareVar(Id,Id,Lc,Tp,faceOfType(Tp,Env),Env);

    valis (.vr(Lc,Id,Tp),.none,Ev)
  }
  typeOfPtn(A,Tp,Env,Path) where _ ?= isEnumSymb(A) => valof{
    Enm = typeOfExp(A,Tp,Env,Path);
    valis (Enm,.none,Env)
  }
  typeOfPtn(A,Tp,Env,Path) where isLitAst(A) => valof{
    Exp = typeOfExp(A,Tp,Env,Path);
    valis (Exp,.none,Env)
  }

  typeOfPtn(A,Tp,Env,Path) where (Lc,E,T) ?= isTypeAnnotation(A) && (_,Id) ?= isName(E) => valof{
    if traceCanon! then
      logMsg("type annotated var $(Id)\:$(T)");

    ETp = parseType([],T,Env);

    if traceCanon! then
      logMsg("type  $(ETp), expected type $(Tp)");

    checkType(E,Tp,ETp,Env);
    Ev = declareVr(Id,Lc,Tp,(OLc,_,D) => .vr(OLc,Id,Tp),.none,Env);
    valis (.vr(Lc,Id,Tp),.none,Ev)
  }
  typeOfPtn(A,Tp,Env,Path) where (Lc,E,T) ?= isTypeAnnotation(A) => valof{
    if traceCanon! then
      logMsg("type annotated pattern $(E)\:$(T)");

    ETp = parseType([],T,Env);

    if traceCanon! then
      logMsg("type  $(ETp), expected type $(Tp)");

    checkType(E,Tp,ETp,Env);
    if traceCanon! then
      logMsg("type annotated ptn $(E), check against $(ETp)");
    valis typeOfPtn(E,ETp,Env,Path)
  }.
  typeOfPtn(A,Tp,Env,Path) where 
      (Lc,E,C) ?= isWhere(A) => valof{
	(Ptn,PCond,Ev0) = typeOfPtn(E,Tp,Env,Path);
	(Cond,Ev1) = checkCond(C,Ev0,Path);
	valis (Ptn,mergeGoal(Lc,PCond,.some(Cond)),Ev1)
      }.
  typeOfPtn(A,Tp,Env,Path) where (_,[El]) ?= isTuple(A) && ~ _ ?= isTuple(El) =>
    typeOfPtn(El,Tp,Env,Path).
  typeOfPtn(A,Tp,Env,Path) where (Lc,Els) ?= isTuple(A) => valof{
    Tvs = genTpVars(Els);
    checkType(A,.tupleType(Tvs),Tp,Env);
    (Ptns,Cond,Ev) = typeOfPtns(Els,Tvs,Lc,.none,[],Env,Path);
    valis (.tple(Lc,Ptns),Cond,Ev)
  }
  typeOfPtn(A,Tp,Env,Path) where (Lc,Op,Els) ?= isEnumCon(A) => valof{
    At = newTypeVar("A");
    Fun = typeOfExp(Op,consType(At,Tp),Env,Path);

    (Q,ETp) = evidence(deRef(At),Env);
    (Cx,ArgTp) = deConstrain(ETp);
    Base = declareConstraints(Lc,Cx,declareTypeVars(Q,pushScope(Env)));

    if .tupleType(Tps) .= deRef(ArgTp) then{
      (Args,Cond,Ev) = typeOfArgsPtn(Els,Tps,Lc,Base,Path);

      valis (.apply(Lc,Fun,Args,Tp),Cond,Ev)
    } else{
      reportError("expecting argument type $(At) to be a tuple type",Lc);
      valis (.anon(Lc,Tp),.none,Env)
    }
  }
  typeOfPtn(A,Tp,Env,Path) where (Lc,Op,Ss) ?= isBrTerm(A) && (OLc,Nm)?=isName(Op) => valof{
    if traceCanon! then
      logMsg("labeled record ptn: $(A)");
    At = newTypeVar("A");
    Fun = typeOfExp(Op,consType(At,Tp),Env,Path);

    (Q,ETp) = evidence(deRef(At),Env);
    FaceTp = _optval(faceOfType(ETp,Env));
    (Cx,Face) = deConstrain(FaceTp);
    Base = declareConstraints(Lc,Cx,declareTypeVars(Q,pushScope(Env)));
    (Els,Cond,Ev) = typeOfElementPtns(Ss,Face,Base,Path,.none,[]);
    Args = fillinElementPtns(Lc,Els,Face);

    valis (.apply(Lc,Fun,Args,Tp),Cond,Ev)
  }
  typeOfPtn(A,Tp,Env,_) => valof{
    Lc = locOf(A);
    reportError("illegal pattern: $(A), expecting a $(Tp)",Lc);
    valis (.anon(Lc,Tp),.none,Env)    
  }

  typeOfArgPtn:(ast,tipe,dict,string) => (canon,option[canon],dict).
  typeOfArgPtn(A,Tp,Env,Path) where (Lc,Els) ?= isTuple(A) => valof{
    Tvs = genTpVars(Els);
    checkType(A,.tupleType(Tvs),Tp,Env);
    (Ptns,Cond,Ev) = typeOfPtns(Els,Tvs,Lc,.none,[],Env,Path);
    valis (.tple(Lc,Ptns),Cond,Ev)
  }
  typeOfArgPtn(A,Tp,Env,Path) => typeOfPtn(A,Tp,Env,Path).

  typeOfArgsPtn:(cons[ast],cons[tipe],option[locn],dict,string) =>
    (cons[canon],option[canon],dict).
  typeOfArgsPtn(Els,Tps,Lc,Env,Path) => typeOfPtns(Els,Tps,Lc,.none,[],Env,Path).

  typeOfElementPtns:(cons[ast],tipe,dict,string,option[canon],cons[(string,canon)]) =>
    (cons[(string,canon)],option[canon],dict).
  typeOfElementPtns([],_,Env,_,Cnd,Prs) => (Prs,Cnd,Env).
  typeOfElementPtns([D,..Ds],Tp,Env,Pth,Cnd,SoFr) => valof{
    if (Lc,Lhs,R) ?= isDefn(D) && (_,Nm) ?= isName(Lhs) then{
      if FTp ?= fieldInFace(Tp,Nm) then{
	(Ptn,C1,Ev) = typeOfPtn(R,FTp,Env,Pth);
	valis typeOfElementPtns(Ds,Tp,Ev,Pth,mergeGoal(Lc,Cnd,C1),[(Nm,Ptn),..SoFr])
      } else{
	reportError("Field $(Nm) is not a legal element of $(Tp)",locOf(D));
	valis ([],.none,Env)
      }
    } else {
      reportError("$(D) not a legal form of element pattern",locOf(D));
      valis ([],.none,Env)
    }
  }

  fillinElementPtns:(option[locn],cons[(string,canon)],tipe) => cons[canon].
  fillinElementPtns(Lc,Ptns,Tp) =>
    project1(foldRight(((Nm,Pt),Ps)=>fillinElementPtn(Lc,Nm,Pt,Ps),
	_optval(fieldTypes(Tp))//((N,T))=>(N,.vr(Lc,genId("_"),T)),
	Ptns)).

  fillinElementPtn(Lc,Nm,Pt,Ps) => replace(Ps,((N,_))=>N==Nm,(Nm,Pt)).

  project1(L) => (L//snd).

  typeOfPtns:(cons[ast],cons[tipe],option[locn],option[canon],cons[canon],dict,string) =>
    (cons[canon],option[canon],dict).
  typeOfPtns([],[],_,Cond,Els,Env,_) => (reverse(Els),Cond,Env).
  typeOfPtns([P,..Ps],[T,..Ts],_,Cnd,Els,Env,Path) => valof{
    (Pt,C,E0) = typeOfPtn(P,T,Env,Path);
    valis typeOfPtns(Ps,Ts,locOf(P),mergeGoal(locOf(P),Cnd,C),[Pt,..Els],E0,Path)
  }
  typeOfPtns([],[T,.._],Lc,Cnd,Els,Env,_) => valof{
    reportError("insufficient arguments, expecting a $(T)",Lc);
    valis (reverse(Els),Cnd,Env)
  }
  typeOfPtns([P,.._],[],_,Cnd,Els,Env,_) => valof{
    reportError("too many arguments: $(P)",locOf(P));
    valis (reverse(Els),Cnd,Env)
  }
  
  typeOfExp:(ast,tipe,dict,string) => canon.
  typeOfExp(A,Tp,Env,Path) where Lc ?= isAnon(A) => valof{
    reportError("anonymous variable not permitted in expression",Lc);
    valis .anon(Lc,Tp)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,Id) ?= isName(A) =>
    typeOfVar(Lc,Id,Tp,.true,Env,Path).
  typeOfExp(A,Tp,Env,Path) where (Lc,Nm) ?= isEnumSymb(A) => valof{
    Fun = typeOfExp(.nme(Lc,Nm),consType(.tupleType([]),Tp),Env,Path);
    valis .apply(Lc,Fun,[],Tp)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,Ix) ?= isInt(A)  => valof{
    checkType(A,intType,Tp,Env);
    valis .intr(Lc,Ix)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,Ix) ?= isBig(A)  => valof{
    checkType(A,bigintType,Tp,Env);
    valis .bintr(Lc,Ix)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,Dx) ?= isFlt(A) => valof{
    checkType(A,fltType,Tp,Env);
    valis .flt(Lc,Dx)
  }.
  typeOfExp(A,Tp,Env,Path) where (Lc,Cx) ?= isChr(A)  => valof{
    checkType(A,chrType,Tp,Env);
    valis .kar(Lc,Cx)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,Sx) ?= isStr(A) => valof{
    checkType(A,strType,Tp,Env);
    valis .strng(Lc,Sx)
  }.
  typeOfExp(A,Tp,Env,Path) where (Lc,E,T) ?= isTypeAnnotation(A) => valof{
    if traceCanon! then
      logMsg("type annotated expression $(E)\:$(T)");
    ETp = parseType([],T,Env);

    if traceCanon! then
      logMsg("type  $(ETp)");

    checkType(E,Tp,ETp,Env);

    valis typeOfExp(E,ETp,Env,Path)
  }.
  typeOfExp(A,Tp,Env,Path) where (Lc,E) ?= isOpen(A) => valof{
    XTp = newTypeVar("X");
    EE = typeOfExp(E,XTp,Env,Path);
    (Q,VE) = evidence(XTp,Env);
    checkType(A,VE,Tp,Env);
    valis EE
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,R,F) ?= isFieldAcc(A) => valof{
    RTp = newTypeVar("R");
    Rc = typeOfExp(R,RTp,Env,Path);
    valis .dot(Lc,Rc,F,Tp)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,R,Ix) ?= isTupleAcc(A) => valof{
    RTp = newTypeVar("R");
    Rc = typeOfExp(R,RTp,Env,Path);
    valis .tdot(Lc,Rc,Ix,Tp)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,R,Fld,V) ?= isRecordUpdate(A) => valof{
    Rc = typeOfExp(R,Tp,Env,Path);
    VTp = newTypeVar("_V");
    Vl = typeOfExp(V,VTp,Env,Path);
    valis .update(Lc,Rc,Fld,Vl)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,T,L,R) ?= isConditional(A) => valof{
    (Tst,E0) = checkCond(T,Env,Path);
    Thn = typeOfExp(L,Tp,E0,Path);
    Els = typeOfExp(R,Tp,Env,Path);
    valis .cond(Lc,Tst,Thn,Els)
  }.
  typeOfExp(A,Tp,Env,Path) where _ ?= isConjunct(A) => valof{
    (_,boolTp,_,_) = _optval(findType(Env,"boolean"));
    checkType(A,boolTp,Tp,Env);
    (Gl,_) = checkCond(A,Env,Path);
    valis Gl
  }.
  typeOfExp(A,Tp,Env,Path) where _ ?= isDisjunct(A)  => valof{
    checkType(A,boolType,Tp,Env);
    (Gl,_) = checkCond(A,Env,Path);
    valis Gl
  }.
  typeOfExp(A,Tp,Env,Path) where _ ?= isNegation(A)  => valof{
    checkType(A,boolType,Tp,Env);
    (Gl,_) = checkCond(A,Env,Path);
    valis Gl
  }.
  typeOfExp(A,Tp,Env,Path) where _ ?= isMatch(A) => valof{
    (Gl,_) = checkCond(A,Env,Path);
    valis Gl
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,G,Cases) ?= isCase(A) => valof{
    GTp = newTypeVar("_e");
    Gv = typeOfExp(G,GTp,Env,Path);

    Rules = checkRules(Cases,GTp,Tp,Env,Path,typeOfExp,[],.none);
    checkPtnCoverage(Rules//((.rule(_,.tple(_,[Ptn]),_,_))=>Ptn),Env,GTp);
    valis .csexp(Lc,Gv,Rules,Tp)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,C) ?= isCellRef(A) => valof{
    Cl = typeOfExp(C,refType(Tp),Env,Path);
    valis .apply(Lc,.vr(Lc,"_get",funType([refType(Tp)],Tp)),[Cl],Tp)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,V) ?= isRef(A) => valof{
    VTp = newTypeVar("_C");
    Vl = typeOfExp(V,VTp,Env,Path);
    checkType(V,refType(VTp),Tp,Env);
    valis .apply(Lc,.vr(Lc,"_cell",funType([VTp],Tp)),[Vl],Tp)
  }
  typeOfExp(A,Tp,Env,Path) where (_,[El]) ?= isTuple(A) && ~ _ ?= isTuple(El) =>
    typeOfExp(El,Tp,Env,Path).
  typeOfExp(A,Tp,Env,Path) where (Lc,Els) ?= isTuple(A) => valof{
    Tvs = genTpVars(Els);
    checkType(A,.tupleType(Tvs),Tp,Env);
    Ptns = typeOfExps(Els,Tvs,Lc,[],Env,Path);
    valis .tple(Lc,Ptns)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,_,Ar,C,R) ?= isLambda(A) => valof{
    At = newTypeVar("_A");
    Rt = newTypeVar("_R");

    if traceCanon! then
      logMsg("check lambda $(A), expected type $(Tp)");

    (Q,ETp) = evidence(Tp,Env);
    (Cx,ProgTp) = deConstrain(ETp);
    Es = declareConstraints(Lc,Cx,declareTypeVars(Q,Env));

    checkType(A,fnType(At,Rt),ProgTp,Es);
    
    (As,ACnd,E0) = typeOfArgPtn(Ar,At,Es,Path);

    LName = genId(Path++"λ");

    if Cnd ?= C then {
      (Cond,E1) = checkCond(Cnd,E0,Path);
      Rep = typeOfExp(R,Rt,E1,Path);

      Lam = .lambda(Lc,LName,.rule(Lc,As,mergeGoal(Lc,ACnd,.some(Cond)),Rep),Cx,Tp);

      if traceCanon! then
	logMsg("lambda: $(Lam)\:$(Tp)");

      valis Lam
    } else{
      Rep = typeOfExp(R,Rt,E0,Path);
      Lam = .lambda(Lc,LName,.rule(Lc,As,ACnd,Rep),Cx,Tp);
      if traceCanon! then
	logMsg("lambda: $(Lam) computed type $(Tp)");

      valis Lam
    }
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,E) ?= isThunk(A) => valof{
    Et = newTypeVar("_E");

    checkType(A,thunkType(Et),Tp,Env);

    ThExp = typeOfExp(E,Et,Env,Path);

    if traceCanon! then
      logMsg("thunk $(E)\:$(Et)");

    LName = genId(Path++"λ");
    valis .thunk(Lc,.lambda(Lc,LName,.rule(Lc,.tple(Lc,[]),.none,ThExp),[],funType([],Et)),Tp)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,E) ?= isThunkRef(A) => valof{
    ThExp = typeOfExp(E,thunkType(Tp),Env,Path);

    if traceCanon! then
      logMsg("thunk ref $(ThExp)\:$(Tp)");

    valis .thRef(Lc,ThExp,Tp)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,E) ?= isSuppress(A) => valof{
    if (ILc,Id)?=isName(E) then {
      valis typeOfVar(ILc,Id,Tp,.false,Env,Path)
    }
    else{
      reportError("expecting an identifier, not $(E)",Lc);
      valis typeOfExp(E,Tp,Env,Path)
    }
  }
  typeOfExp(A,Tp,Env,Pth) where (Lc,Op,Els) ?= isLabeledTheta(A) && (_,Nm)?=isName(Op) => valof{
    FceTp = newTypeVar("_");
    ConTp = consType(FceTp,Tp);
    Fun = typeOfExp(Op,ConTp,Env,Pth);
    (Q,ETp) = evidence(FceTp,Env);
    FaceTp = _optval(faceOfType(ETp,Env));
    (Cx,Face) = deConstrain(FaceTp);
    Base = declareConstraints(Lc,Cx,declareTypeVars(Q,pushScope(Env)));
    
    (Defs,Decls,ThEnv) = thetaEnv(Lc,genNewName(Pth,"θ"),Els,Face,Base);

    valis formTheta(Lc,Fun,Face,Defs,Decls,Tp)
  }
  typeOfExp(A,Tp,Env,Pth) where (Lc,Op,Els) ?= isBrTerm(A) && (_,Nm)?=isName(Op) => valof{
    FceTp = newTypeVar("_");
    ConTp = consType(FceTp,Tp);

    Fun = typeOfExp(Op,ConTp,Env,Pth);
    (Q,ETp) = evidence(FceTp,Env);
    if FaceTp ?= faceOfType(ETp,Env) then{
      (Cx,Face) = deConstrain(FaceTp);
      Base = declareConstraints(Lc,Cx,declareTypeVars(Q,pushScope(Env)));
      
      (Defs,XDecls,All) = recordEnv(Lc,genNewName(Pth,"θ"),Els,Face,Base,Env);
      
      valis formRecordExp(Lc,Fun,Face,Defs,XDecls,All,Tp)
    } else {
      reportError("can't compute face of $(FceTp)",Lc);
      valis .anon(Lc,Tp)
    }      
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,Els) ?= isBrTuple(A) => valof{
    (Defs,XDecls,Decls)=recordEnv(Lc,genNewName(Path,"Γ"),Els,Tp,Env,Env);

    sortedFlds = let{
      RcDcl(.varDec(_,Nm,FlNm,FTp),Flds) => [(Nm,(FlNm,FTp)),..Flds].
      RcDcl(.funDec(_,Nm,FlNm,FTp),Flds) => [(Nm,(FlNm,FTp)),..Flds].
      RcDcl(_,Flds) default => Flds.
    } in sortFields(foldLeft(RcDcl,[],XDecls));

    face = .faceType(sortedFlds//((Nm,(_,FTp)))=>(Nm,FTp),[]);

    if traceCanon! then{
      logMsg("anon record type: $(face)");
    };

    if sameType(face,Tp,Env) then{
      brTplNm = "{#(interleave(sortedFlds//fst,".")*)}";

      Tps = sortedFlds//((Nm,_))=>(Nm,.nomnal(Nm));

      if traceCanon! then
	logMsg("types $(Tps)");

      Tmplte = .tpFun(brTplNm,size(Tps));
      AnTp = mkTypeExp(Tmplte,Tps//snd);
      ArTp = .faceType(Tps,[]);
      AnRl = foldLeft(((_,QV),Rl)=>.allRule(QV,Rl),.typeExists(AnTp,ArTp),Tps);

      if traceCanon! then
	logMsg("generated rule $(AnRl)");

      CnsTp = reQ(Tps,consType(ArTp,AnTp));
      CnsDf = .cnsDef(Lc,brTplNm,0,CnsTp);
      CnsDc = .cnsDec(Lc,brTplNm,brTplNm,CnsTp);

      AnTpDef = .typeDef(Lc,brTplNm,Tmplte,AnRl);
      AnTpDec = .tpeDec(Lc,brTplNm,Tmplte,AnRl);

      if traceCanon! then
	logMsg("generated type $(AnTpDef), constructor $(CnsDf)");

      Accessors = foldLeft( ((Fld,(VNm,FTp)),(Ix,ADfs,ADcs)) => valof{
	  AccNm = qualifiedName(brTplNm,.fldMark,Fld);
	  Rc = .vr(Lc,"XX",Tp);
	  AccTp = funType([Tp],FTp);
	  Acc = .varDef(Lc,AccNm,.lambda(Lc,lambdaLbl(Lc),
	      .rule(Lc,.tple(Lc,[Rc]),.none,.tdot(Lc,Rc,Ix,FTp)),[],AccTp),
	    [],AccTp);
	  Dec = .funDec(Lc,AccNm,AccNm,AccTp);
	  ADec = .accDec(Lc,Tp,Fld,AccNm,AccTp);
	  valis (Ix+1,[Acc,..ADfs],[Dec,ADec,..ADcs])},
	(0,.nil,.nil),
	sortedFlds);

      if traceCanon! then
	logMsg("Generated accessors $(Accessors)");

      declareBrType(brTplNm,[AnTpDef,CnsDf,..Accessors.1],[AnTpDec,CnsDc,..Accessors.2],Env);

      if traceCanon! then
	logMsg("generated type $(AnTp), actual type $(Tp)");

      BrArgs = (sortedFlds//((_,(FNm,FTp))) =>.vr(.none,FNm,FTp));
      BrArgTps = (sortedFlds//((_,(FNm,FTp))) =>FTp);
      valis .letExp(Lc,Defs,Decls,
	.apply(Lc,.enm(Lc,brTplNm,consType(.tupleType(BrArgTps),Tp)),BrArgs,Tp))
    } else{
      reportError("expecting $(Tp), not record type $(face)",Lc);
      valis .anon(locOf(A),Tp)
    }
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,Els,Bnd) ?= isLetRecDef(A) => valof{
    (Defs,Decls,ThEnv)=thetaEnv(Lc,genNewName(Path,"Γ"),Els,.faceType([],[]),Env);

    if traceCanon! then
      logMsg("theta decls: $(Decls)");
    
    El = typeOfExp(Bnd,Tp,ThEnv,Path);

    valis genLetRec(Defs,Decls,(G,D,E) => .letRec(Lc,G,D,E),El)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,Els,Bnd) ?= isLetDef(A) => valof{
    (Defs,XDecls,Decls)=recordEnv(Lc,genNewName(Path,"Γ"),Els,.faceType([],[]),Env,Env);

    if traceCanon! then{
      logMsg("record decls: $(Decls)");
      logMsg("exported decls: $(XDecls)");
    };


    El = typeOfExp(Bnd,Tp,declareDecls(XDecls,Env),Path);

    valis foldRight((Gp,I)=>.letExp(Lc,Gp,XDecls++Decls,I),El,sortDefs(Defs))
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,Op,Args) ?= isEnumCon(A) => valof{
    At = newTypeVar("A");
    Fun = typeOfExp(Op,consType(At,Tp),Env,Path);

    (Q,ETp) = evidence(deRef(At),Env);
    (Cx,ArgTp) = deConstrain(ETp);
    Base = declareConstraints(Lc,Cx,declareTypeVars(Q,pushScope(Env)));

    if .tupleType(Tps) .= deRef(ArgTp) then{
      Args = typeOfExps(Args,Tps,Lc,[],Base,Path);
      valis .apply(Lc,Fun,Args,Tp)
    } else{
      reportError("expecting argument type $(At) to be a tuple type",Lc);
      valis .anon(Lc,Tp)
    }
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,Op,Args) ?= isRoundTerm(A) =>
    typeOfRoundTerm(Lc,Op,Args,Tp,Env,Path).
  typeOfExp(A,Tp,Env,Path) where (Lc,Ac) ?= isValof(A) => valof{
    (Act,_) = checkAction(Ac,Tp,Env,Path);
    valis .vlof(Lc,Act,Tp)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,Body,E,Rls) ?= isTryCatch(A) => valof{
    ErTp = parseType([],E,Env);
    ErNm = qualifiedName(Path,.tractMark,tpName(deRef(ErTp)));
    Ev0 = declareTryScope(Lc,ErTp,ErNm,Env);
    NB = typeOfExp(Body,Tp,Ev0,Path);
    HRls = checkRules(Rls,ErTp,Tp,Env,Path,typeOfExp,[],.none);
    valis .trycatch(Lc,NB,.vr(Lc,ErNm,ErTp),HRls,Tp)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,E) ?= isRaise(A) => valof{
    ErTp = newTypeVar("_E");
    V = typeOfExp(E,ErTp,Env,Path);
    valis .over(Lc,.rais(Lc,.anon(Lc,ErTp),V,Tp),.raisEs(ErTp))
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,L,R) ?= isSpawn(A) => valof{
    CTp = newTypeVar("_C");
    F = typeOfExp(equation(Lc,rndTuple(Lc,[L]),R),
      funType([continType(CTp,Tp)],Tp),Env,Path);
    valis .spwn(Lc,F,Tp)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,T,F,E) ?= isPaused(A) => valof{
    RTp = newTypeVar("_R");
    STp = newTypeVar("_S");
    
    checkType(A,continType(RTp,STp),Tp,Env);
    
    Fs = typeOfExp(equation(Lc,rndTuple(Lc,[T,F]),E),funType([Tp,RTp],STp),Env,Path);
    valis .paus(Lc,Fs,Tp)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,F,E) ?= isSuspend(A) => valof{
    CTp = newTypeVar("_C");
    Fb = typeOfExp(F,continType(Tp,CTp),Env,Path);
    Ms = typeOfExp(E,CTp,Env,Path);
    valis .susp(Lc,Fb,Ms,Tp)
  }
  typeOfExp(A,Tp,Env,Path) where (Lc,F,E) ?= isResume(A) => valof{
    CTp = newTypeVar("_C");
    Fb = typeOfExp(F,continType(CTp,Tp),Env,Path);
    Ms = typeOfExp(E,CTp,Env,Path);
    valis .rsme(Lc,Fb,Ms,Tp)
  }
  typeOfExp(A,_Tp,Env,Path) where (Lc,F,E) ?= isRetire(A) => valof{
    CTp = newTypeVar("_C");
    Fb = typeOfExp(F,continType(newTypeVar("_"),CTp),Env,Path);
    Ms = typeOfExp(E,CTp,Env,Path);
    valis .rtire(Lc,Fb,Ms)
  }
  typeOfExp(A,Tp,_,_) => valof{
    reportError("cannot type check expression $(A)",locOf(A));
    valis .anon(locOf(A),Tp)
  }.

  typeOfVar(Lc,Id,Tp,Refresh,Env,Path) => valof{
    if Var ?= findVar(Lc,Id,Refresh,Env) then{
      if sameType(Tp,typeOf(Var),Env) then {
	valis Var
      } else{
	reportError("variable $(Id)\:$(typeOf(Var)) not consistent with expected type: $(Tp)",Lc);
	valis .anon(Lc,Tp)
      }
    }
    else{
      reportError("variable $(Id) not defined.#(isUnbound(Tp)??""||" Expecting a $(Tp)")",Lc);
      valis .anon(Lc,Tp)
    }
  }

  typeOfExps:(cons[ast],cons[tipe],option[locn],cons[canon],dict,string) =>
    cons[canon].
  typeOfExps([],[],_,Els,Env,_) =>reverse(Els).
  typeOfExps([P,..Ps],[T,..Ts],_,Els,Env,Path) =>
    typeOfExps(Ps,Ts,locOf(P),[typeOfExp(P,T,Env,Path),..Els],Env,Path).
  typeOfExps([],[T,.._],Lc,Els,Env,_) => valof{
    reportError("insufficient arguments, expecting a $(T)",Lc);
    valis reverse(Els)
  }
  typeOfExps([P,.._],[],_,Els,Env,_) => valof{
    reportError("too many arguments: $(P)",locOf(P));
    valis reverse(Els)
  }

  typeOfRoundTerm:(option[locn],ast,cons[ast],tipe,dict,string) => canon.
  typeOfRoundTerm(Lc,Op,As,Tp,Env,Path) => valof{
    Vrs = genTpVars(As);
    At = .tupleType(Vrs);
    ExTp = newTypeVar("F");
    Fun = typeOfExp(Op,ExTp,Env,Path);
    FnTp = fnType(At,Tp);

    if sameType(ExTp,FnTp,Env) then{
      Args = typeOfExps(As,Vrs,Lc,[],Env,Path);      
      valis .apply(Lc,Fun,Args,Tp)
    } else{
      reportError("type of $(Op)\:$(ExTp) not consistent with $(FnTp)",Lc);
      valis .vr(Lc,"_",Tp)
    }
  }

  checkAction:(ast,tipe,dict,string) => (canonAction,dict).
  checkAction(A,Tp,Env,Path) where (Lc,[St]) ?= isBrTuple(A) =>
    checkAction(St,Tp,Env,Path).
  checkAction(A,_Tp,Env,Path) where (Lc,[]) ?= isBrTuple(A) =>
    (.doNop(Lc),Env).
  checkAction(A,Tp,Env,Path) where (Lc,L,R) ?= isActionSeq(A) => valof{
    (LL,E0) = checkAction(L,Tp,Env,Path);
    (RR,E1) = checkAction(R,Tp,E0,Path);
    valis (.doSeq(Lc,LL,RR),E1)
  }
  checkAction(A,Tp,Env,Path) where (_,L) ?= isSoloSeq(A) =>
    checkAction(L,Tp,Env,Path).
  checkAction(A,Tp,Env,Path) where (Lc,Lb,Ac) ?= isLbldAction(A) => valof{
    (RR,E1) = checkAction(Ac,Tp,Env,Path);
    valis (.doLbld(Lc,Lb,RR),E1)
  }
  checkAction(A,_Tp,Env,_Path) where (Lc,Lb) ?= isBreak(A) =>
    (.doBrk(Lc,Lb),Env).
  checkAction(A,Tp,Env,Path) where (Lc,E) ?= isValis(A) => valof{
    V = typeOfExp(E,Tp,Env,Path);
    valis (.doValis(Lc,V),Env)
  }
  checkAction(A,Tp,Env,Path) where (Lc,E) ?= isRaise(A) => valof{
    Thrw = typeOfExp(A,newTypeVar("E"),Env,Path);
    valis (.doExp(Lc,Thrw),Env)
  }
  checkAction(A,_Tp,Env,Path) where (Lc,L,R) ?= isSpawn(A) => valof{
    CTp = newTypeVar("_C");
    RTp = newTypeVar("R");
    Tp = funType([continType(CTp,RTp)],RTp);
    F = typeOfExp(equation(Lc,rndTuple(Lc,[L]),R),Tp,Env,Path);
    valis (.doExp(Lc,.spwn(Lc,F,RTp)),Env)
  }
  checkAction(A,_Tp,Env,Path) where (Lc,F,E) ?= isRetire(A) => valof{
    CTp = newTypeVar("_C");
    Fb = typeOfExp(F,continType(newTypeVar("_"),CTp),Env,Path);
    Ms = typeOfExp(E,CTp,Env,Path);
    valis (.doRetire(Lc,Fb,Ms),Env)
  }
  checkAction(A,_Tp,Env,Path) where (Lc,F,E) ?= isSuspend(A) => valof{
    SS = typeOfExp(A,newTypeVar("_"),Env,Path);
    valis (.doExp(Lc,SS),Env)
  }
  checkAction(A,_Tp,Env,Path) where (Lc,F,E) ?= isResume(A) => valof{
    SS = typeOfExp(A,newTypeVar("_"),Env,Path);
    valis (.doExp(Lc,SS),Env)
  }
  checkAction(A,_,Env,Path) where (Lc,Lhs,Rhs) ?= isDefn(A) => valof{
    if (ILc,Id) ?= isName(Lhs) then{
      Tp = newTypeVar("_V");
      Val = typeOfExp(Rhs,Tp,Env,Path);
      Ev = declareVar(Id,Id,Lc,Tp,faceOfType(Tp,Env),Env);
      valis (.doDefn(Lc,.vr(ILc,Id,Tp),Val),Ev)
    } else if (ILc,Els) ?= isTuple(Lhs) then {
      TTp = newTypeVar("_T");
      (Ptn,PCond,Ev) = typeOfPtn(Lhs,TTp,Env,Path);
      if C?=PCond then
	reportError("Not permitted to have where clauses in action $(A)",Lc);
      Val = typeOfExp(Rhs,TTp,Env,Path);
      valis (.doMatch(Lc,Ptn,Val),Ev)
    } else{
      reportError("invalid lhs $(Lhs) of definition",locOf(Lhs));
      valis (.doNop(Lc),Env)
    }
  }
  checkAction(A,_,Env,Path) where (Lc,Lhs,Rhs) ?= isAssignment(A) =>
    checkAssignment(Lc,Lhs,Rhs,Env,Path).
  checkAction(A,Tp,Env,Path) where (Lc,Body,E,Rls) ?= isTryCatch(A) => valof{
    ErTp = parseType([],E,Env);
    ErNm = qualifiedName(Path,.tractMark,tpName(deRef(ErTp)));
    Ev0 = declareTryScope(Lc,ErTp,ErNm,Env);
    (NB,_) = checkAction(Body,Tp,Ev0,Path);
    Hs = checkRules(Rls,ErTp,Tp,Env,Path,
      (AA,_,Eva,_)=>valof{
	(HA,_)=checkAction(AA,Tp,Eva,Path);
	valis HA
      },[],.none);
    valis (.doTryCatch(Lc,NB,.vr(Lc,ErNm,ErTp),Hs),Env)
  }
  checkAction(A,Tp,Env,Path) where (Lc,C,T,E) ?= isIfThenElse(A) => valof{
    (CC,E0) = checkGoal(C,Env,Path);
    (TT,_) = checkAction(T,Tp,E0,Path);
    (EE,_) = checkAction(E,Tp,Env,Path);
    valis (.doIfThen(Lc,CC,TT,EE),Env)
  }
  checkAction(A,Tp,Env,Path) where (Lc,C,T) ?= isIfThen(A) => valof{
    (CC,E0) = checkGoal(C,Env,Path);
    (TT,_) = checkAction(T,Tp,E0,Path);
    valis (.doIfThen(Lc,CC,TT,.doNop(Lc)),Env)
  }
  checkAction(A,Tp,Env,Path) where (Lc,C,B) ?= isWhileDo(A) => valof{
    (CC,E0) = checkGoal(C,Env,Path);
    (BB,_) = checkAction(B,Tp,E0,Path);
    valis (.doWhile(Lc,CC,BB),Env)
  }
  checkAction(A,Tp,Env,Path) where (Lc,Ds,B) ?= isLetDef(A) => valof{
    (Defs,XDecls,Decls)=recordEnv(Lc,genNewName(Path,"Γ"),Ds,.faceType([],[]),Env,Env);

    if traceCanon! then{
      logMsg("record decls: $(Decls)");
      logMsg("exported decls: $(XDecls)");
    };

    (Ac,_) = checkAction(B,Tp,declareDecls(XDecls,Env),Path);
    Sorted = sortDefs(Defs);

    valis (foldRight((Gp,I)=>.doLet(Lc,Gp,Decls,I),Ac,Sorted),Env)
  }
  checkAction(A,Tp,Env,Path) where (Lc,Ds,B) ?= isLetRecDef(A) => valof{
    (Defs,Decls,ThEnv)=thetaEnv(Lc,genNewName(Path,"Γ"),Ds,.faceType([],[]),Env);
    (Ac,_) = checkAction(B,Tp,ThEnv,Path);

    valis (genLetRec(Defs,Decls,(G,D,E)=>.doLetRec(Lc,G,D,E),Ac),Env)
  }
  checkAction(A,Tp,Env,Path) where (Lc,G,Cases) ?= isCase(A) => valof{
    ETp = newTypeVar("_e");
    Gv = typeOfExp(G,ETp,Env,Path);

    Rules = checkRules(Cases,ETp,Tp,Env,Path,
      (Ac,_,E,Path) => valof{
	(Act,_) = checkAction(Ac,Tp,E,Path);
	valis Act
      },[],.none);
    valis (.doCase(Lc,Gv,Rules),Env)
  }
  checkAction(A,Tp,Env,Path) where (Lc,Op,Args) ?= isRoundTerm(A) => valof{
    Call = typeOfRoundTerm(Lc,Op,Args,newTypeVar("_r"),Env,Path);
    valis (.doExp(Lc,Call),Env)
  }
  checkAction(A,_Tp,Env,_Path) default => valof{
    Lc = locOf(A);
    reportError("invalid action $(A)",Lc);
    valis (.doNop(Lc),Env)
  }

  checkAssignment(Lc,Lhs,Rhs,Env,Path) => valof{
    Tp = newTypeVar("_V");
    if (ILc,Id) ?= isName(Lhs) && ~ varDefined(Id,Env) then {
      RfTp = refType(Tp);
      Val = typeOfExp(Rhs,Tp,Env,Path);
      Ev = declareVar(Id,Id,ILc,refType(Tp),.none,Env);
      valis (.doDefn(Lc,.vr(ILc,Id,RfTp),.apply(Lc,.vr(Lc,"_cell",funType([Tp],RfTp)),[Val],RfTp)),Ev)
    } else{
      Val = typeOfExp(Rhs,Tp,Env,Path);
      Var = typeOfExp(Lhs,refType(Tp),Env,Path);
      valis (.doAssign(Lc,Var,Val),Env)
    }
  }

  checkRules:all t ~~ (cons[ast],tipe,tipe,dict,string,
    (ast,tipe,dict,string)=>t,cons[rule[t]],option[rule[t]]) => cons[rule[t]].
  checkRules([],_,_,_,_,_,Els,.none) => reverse(Els).
  checkRules([],_,_,_,_,_,Els,.some(Dflt)) => reverse([Dflt,..Els]).
  checkRules([Cs,..Ps],ATp,Tp,Env,Path,Chk,SoFar,Deflt) => valof{
    if (Rl,Dflt) ?= checkRule(Cs,ATp,Tp,Env,Chk,Path) then{
      if Dflt then{
	if DRl?=Deflt then{
	  reportError("cannot have more than one default, other one at $(locOf(DRl))",
	    locOf(Cs));
	}
	else
	valis checkRules(Ps,ATp,Tp,Env,Path,Chk,SoFar,.some(Rl))
      }
      else
      valis checkRules(Ps,ATp,Tp,Env,Path,Chk,[Rl,..SoFar],Deflt)
    } else
    valis checkRules(Ps,ATp,Tp,Env,Path,Chk,SoFar,Deflt)
  }

  checkRule(St,ATp,RTp,Env,Chk,Path) where (Lc,IsDeflt,Lhs,Cnd,R) ?= isLambda(St) => valof{
    (Q,EATp) = evidence(ATp,Env);
    (Cx,PtnTp) = deConstrain(EATp);
    Es = declareConstraints(Lc,Cx,declareTypeVars(Q,Env));

    (Arg,ACnd,E0) = typeOfPtn(Lhs,PtnTp,Es,Path);

    if Wh?=Cnd then{
      (Cond,E1) = checkCond(Wh,E0,Path);
      Rep = Chk(R,RTp,E1,Path);
      valis .some((.rule(Lc,.tple(Lc,[Arg]),mergeGoal(Lc,ACnd,.some(Cond)),Rep),IsDeflt))
    }
    else{
      Rep = Chk(R,RTp,E0,Path);
      valis .some((.rule(Lc,.tple(Lc,[Arg]),ACnd,Rep),IsDeflt))
    }
  }
  checkRule(St,_,_,_,_,_) default => valof{
    reportError("expecting a rule, not $(St)",locOf(St));
    valis .none
  }

  mergeGoal:(option[locn],option[canon],option[canon])=>option[canon].
  mergeGoal(_,Gl,.none) => Gl.
  mergeGoal(_,.none,Gl) => Gl.
  mergeGoal(Lc,.some(Gl),.some(H)) => .some(.conj(Lc,Gl,H)).

  mergeCond:(option[locn],canon,option[canon]) => canon.
  mergeCond(_,Gl,.none) => Gl.
  mergeCond(Lc,Gl,.some(G2)) => .conj(Lc,Gl,G2).
  
  checkCond:(ast,dict,string) => (canon,dict).
  checkCond(A,Env,Path) => checkGoal(A,Env,Path).

  checkGoal:(ast,dict,string) => (canon,dict).
  checkGoal(A,Env,Path) where (Lc,L,R) ?= isConjunct(A) => valof{
    (Lhs,E0) = checkGoal(L,Env,Path);
    (Rhs,E1) = checkGoal(R,E0,Path);
    valis (.conj(Lc,Lhs,Rhs),E1)
  }.
  checkGoal(A,Env,Path) where (Lc,L,R) ?= isDisjunct(A) => valof{
    (Lhs,E0) = checkGoal(L,Env,Path);
    (Rhs,E1) = checkGoal(R,Env,Path);
    valis (.disj(Lc,Lhs,Rhs),mergeDict(E0,E1,Env))
  }.
  checkGoal(A,Env,Path) where (Lc,R) ?= isNegation(A) => valof{
    (Rhs,_) = checkGoal(R,Env,Path);
    valis (.neg(Lc,Rhs),Env)
  }.
  checkGoal(A,Env,Path) where (Lc,T,L,R) ?= isConditional(A) => valof{
    (Tst,E0) = checkGoal(T,Env,Path);
    (Thn,E1) = checkGoal(L,E0,Path);
    (Els,E2) = checkGoal(R,Env,Path);
    valis (.cond(Lc,Tst,Thn,Els),mergeDict(E1,E2,Env))
  }.
  checkGoal(A,Env,Path) where (Lc,L,R) ?= isMatch(A) => valof{
    PtnTp = newTypeVar("_M");
    Val = typeOfExp(R,PtnTp,Env,Path);
    (Ptn,PCond,Ev) = typeOfPtn(L,PtnTp,Env,Path);
    valis (mergeCond(Lc,.match(Lc,Ptn,Val),PCond),Ev)
  }
  checkGoal(A,Env,Path) where (_,[Inner]) ?= isTuple(A) =>
    checkGoal(Inner,Env,Path).
  checkGoal(A,Env,Path) => valof{
    (_,boolTp,_,_) = _optval(findType(Env,"boolean"));
    Exp = typeOfExp(A,boolTp,Env,Path);
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
