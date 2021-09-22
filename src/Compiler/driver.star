star.compiler{
  import star.
  import star.cmdOpts.
  import star.pkg.
  import star.resources.
  import star.uri.

  import star.repo.

  import star.compiler.assem.
  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.catalog.
  import star.compiler.checker.
  import star.compiler.core.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.gencode.
  import star.compiler.grapher.
  import star.compiler.impawt.
  import star.compiler.inline.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.parser.
  import star.compiler.location.
  import star.compiler.normalize.
  import star.compiler.term.repo.
  import star.compiler.terms.
  import star.compiler.types.

  implementation all e,k ~~ coercion[either[e,k],action[e,k]] => {
    _coerce(either(X)) => some(done(X)).
    _coerce(other(Y)) => some(err(Y))
  }

  repoOption:optionsProcessor[compilerOptions].
  repoOption = {
    shortForm = "-R".
    alternatives = [].
    usage = "-R dir -- directory of repository".
    validator = some(isDir).
    setOption(R,Opts) where RU ^= parseUri(R) && NR^=resolveUri(Opts.cwd,RU) =>
      compilerOptions{repo=NR.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=Opts.showMacro.
	showCanon=Opts.showCanon.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin
      }.
  }

  wdOption:optionsProcessor[compilerOptions].
  wdOption = {
    shortForm = "-W".
    alternatives = [].
    usage = "-W dir -- working directory".
    validator = some(isDir).
    setOption(W,Opts) where RW ^= parseUri(W) && NW^=resolveUri(Opts.cwd,RW)=>
      compilerOptions{.repo=Opts.repo.
	cwd=NW.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=Opts.showMacro.
	showCanon=Opts.showCanon.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin
      .}.
  }

  typeCheckOnlyOption:optionsProcessor[compilerOptions].
  typeCheckOnlyOption = {
    shortForm = "-c".
    alternatives = ["--compile-only"].
    usage = "-c -- type check".
    validator = .none.
    setOption(_,Opts) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=Opts.showMacro.
	showCanon=Opts.showCanon.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	typeCheckOnly=.true.
	doStdin=Opts.doStdin}.
  }

  stdinOption:optionsProcessor[compilerOptions].
  stdinOption = {
    shortForm = "".
    alternatives = ["--stdin"].
    usage = "--stdin -- compile standard input".
    validator = .none.
    setOption(_,Opts) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=Opts.showMacro.
	showCanon=Opts.showCanon.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=.true.
      }.
  }


  traceAstOption:optionsProcessor[compilerOptions].
  traceAstOption = {
    shortForm = "-dA".
    alternatives = [].
    usage = "-dA -- show ast".
    validator = .none.
    setOption(_,Opts) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = .true.
	showMacro=Opts.showMacro.
	showCanon=Opts.showCanon.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin
      }.
  }
  traceMacroOption:optionsProcessor[compilerOptions].
  traceMacroOption = {
    shortForm = "-dM".
    alternatives = [].
    usage = "-dM -- show macro".
    validator = .none.
    setOption(_,Opts) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=.true.
	showCanon=Opts.showCanon.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin
      }.
  }
  traceCodeOption:optionsProcessor[compilerOptions].
  traceCodeOption = {
    shortForm = "-di".
    alternatives = [].
    usage = "-di -- show generated instructions".
    validator = .none.
    setOption(_,Opts) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=Opts.showMacro.
	showCanon=Opts.showCanon.
	showCore=Opts.showCore.
	showCode=.true.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin}.
  }
  traceNormOption:optionsProcessor[compilerOptions].
  traceNormOption = {
    shortForm = "-dT".
    alternatives = [].
    usage = "-dT -- show normalized code".
    validator = .none.
    setOption(_,Opts) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=Opts.showMacro.
	showCanon=Opts.showCanon.
	showCore=.true.
	showCode=Opts.showCode.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin}.
  }
  traceCheckOption:optionsProcessor[compilerOptions].
  traceCheckOption = {
    shortForm = "-dt".
    alternatives = [].
    usage = "-dt -- show type checkedcode".
    validator = .none.
    setOption(_,Opts) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=Opts.showMacro.
	showCanon=.true.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin}.
  }
  optimizeLevel:optionsProcessor[compilerOptions].
  optimizeLevel = {
    shortForm = "-O".
    alternatives = ["--optimize"].
    usage = "-O <Lvl> -- set optimization".
    validator = some((O)=> _ ^= O:?optimizationLvl).
    setOption(L,Opts) where Lvl^=L:?optimizationLvl =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization = Lvl.
	showAst = Opts.showAst.
	showMacro=Opts.showMacro.
	showCanon=Opts.showCanon.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin}.
  }
  showPkgGraphOption:optionsProcessor[compilerOptions].
  showPkgGraphOption = {
    shortForm = "-I".
    alternatives = ["--graph"].
    usage = "-I file -- show package import dot graph".
    validator = some((_)=>.true).
    setOption(U,Opts) where RW ^= parseUri(U) && NW^=resolveUri(Opts.cwd,RW)=>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=some(NW).
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=Opts.showMacro.
	showCanon=.true.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin}.
  }

  public _main:(cons[string])=>().
  _main(Args) => valof action{
    WI^=parseUri("file:"++_cwd());
    RI^=parseUri("file:"++_repo());
    handleCmds(processOptions(Args,[repoOption,wdOption,
	  optimizeLevel,stdinOption,
	  typeCheckOnlyOption,
	  traceAstOption,showPkgGraphOption,
	  traceCodeOption,traceMacroOption,
	  traceNormOption,traceCheckOption],
	defltOptions(WI,RI)
      ))
  }.

  openupRepo:(uri,uri) => action[(), termRepo].
  openupRepo(RU,CU) where CRU ^= resolveUri(CU,RU) => do{
    Repo .= openRepository(CRU);
    valis Repo
  }

  private ignore(F) => action{
    _ .= F();
    valis ()
  }

  handleCmds:(either[string,(compilerOptions,cons[string])])=>action[(),()].
  handleCmds(either((Opts,Args))) => do{
    Repo <- openupRepo(Opts.repo,Opts.cwd);
    if CatUri ^= parseUri("catalog") && CatU ^= resolveUri(Opts.cwd,CatUri) &&
	Cat ^= loadCatalog(CatU) then{
	  for P in Args do{
	    ErRp .= reports([]);	
	    try{
	      Sorted <- makeGraph(extractPkgSpec(P),Repo,Cat,ErRp)
	      ::action[reports,cons[(importSpec,cons[importSpec])]];
	      if Grph ^= Opts.graph then {
		ignore(()=>putResource(Grph,makeDotGraph(P,Sorted)))
	      };
	      
	      processPkgs(Sorted,Repo,Cat,Opts,ErRp)
	    } catch (Er) => action{
	      logMsg("$(Er)");
	      valis _exit(9)
	    }
	  }
	}
    else{
      logMsg("could not access catalog")
    }
  }
  handleCmds(other(Msg)) => do{
    logMsg(Msg)
  }

  extractPkgSpec(P) where Lc .= strFind(P,":",0) && Lc>0 => let{
    Pkg = pkg(P[0:Lc],P[Lc+1:size(P)]::version).
  } in pkgImp(pkgLoc(Pkg),.priVate,Pkg).
  extractPkgSpec(P) default =>
    pkgImp(pkgLoc(pkg(P,.defltVersion)),.priVate,pkg(P,.defltVersion)).

  addSpec:(pkgSpec,termRepo) => termRepo.
  addSpec(Spec,R) where pkgSpec(Pkg,_,_,_,_,_) .= Spec => addSigToRepo(R,Pkg,(Spec::term)::string).

  importVars(pkgSpec(_,_,_,_,_,Vars))=>Vars.

  processPkgs:(cons[(importSpec,cons[importSpec])],termRepo,catalog,compilerOptions,reports) => action[reports,()].
  processPkgs(Pks,Repo,Cat,Opts,Rp) => do{
    Repp .= ref Repo;
    try{
      for (pkgImp(Lc,_,P),Imps) in Pks do{
--	logMsg("$(P) ok? $(~testPkg(P,Repo,Imps))");
	if ~ (pkgOk(Repo,P) && pkgImp(_,_,I) in Imps *> pkgOk(Repo,I)) then{
	  logMsg("Compiling $(P)");
	  if (SrcUri,CPkg) ^= resolveInCatalog(Cat,pkgName(P)) then{
	    Ast <- parseSrc(SrcUri,CPkg,Rp)::action[reports,ast];
	    if Opts.showAst then{
	      logMsg("Ast of $(P) is $(Ast)")
	    };
	    (PkgSpec,PkgFun) <- checkPkg(Repp!,CPkg,Ast,stdDict,Opts,Rp) :: action[reports,(pkgSpec,canonDef)];
	    if Opts.showCanon then {
	      logMsg("type checked $(PkgFun)")
	    };

	    if ~Opts.typeCheckOnly then {
	      NormDefs <- normalize(PkgSpec,PkgFun,Rp)::action[reports,cons[crDefn]];
	      Inlined .= ( Opts.optimization==.inlining ? simplifyDefs(NormDefs) || NormDefs);

	      Repp := addSpec(PkgSpec,Repp!);
	      if Opts.showCore then {
		logMsg("Normalized package $(P)");
		logMsg(dispCrProg(Inlined)::string)
	      };

	      Ins <- compCrProg(P,Inlined,importVars(PkgSpec),Opts,Rp) :: action[reports,cons[codeSegment]];
	      if Opts.showCode then
		logMsg("Generated instructions $(Ins)");
	      Code .= mkTpl([pkgTerm(CPkg),strg(encodeSignature(typeOf(PkgSpec))),
		  mkTpl(pkgImports(PkgSpec)//(pkgImp(_,_,IPkg))=>pkgTerm(IPkg)),
		  mkTpl(Ins//assem)]);
	      Bytes .= (strg(Code::string)::string);
	      Repp := addSource(addPackage(Repp!,CPkg,Bytes),CPkg,SrcUri::string)
	    }
	  }
	  else
	  throw reportError(Rp,"cannot locate source of $(P)",Lc)
	}
      };
      valis flushRepo(Repp!)
    }catch (Erp) => do{
      logMsg("Errors $(Erp)");
      _ .= flushRepo(Repp!);
      throw Erp
    }
  }

  testPkg:(pkg,termRepo,cons[importSpec])=>boolean.
  testPkg(P,Repo,Imps) => valof action{
--    logMsg("checking $(P)");
    if pkgImp(_,_,I) in Imps *> pkgOk(Repo,I) then
      valis .true
    else
    valis .false
  }.
}
