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
  import star.compiler.meta.
  import star.compiler.parser.
  import star.compiler.location.
  import star.compiler.normalize.
  import star.compiler.term.repo.
  import star.compiler.terms.
  import star.compiler.types.

  implementation all e,k ~~ coercion[either[e,k],action[e,k]] => {
    _coerce(either(X)) => done(X).
    _coerce(other(Y)) => err(Y)
  }

  repoOption:optionsProcessor[compilerOptions].
  repoOption = {
    shortForm = "-R".
    alternatives = [].
    usage = "-R dir -- directory of repository".
    validator = some(isDir).
    setOption(R,Opts) where RU ^= parseUri(R) && NR^=resolveUri(Opts.cwd,RU) => compilerOptions{repo=NR. cwd=Opts.cwd}.
  }

  wdOption:optionsProcessor[compilerOptions].
  wdOption = {
    shortForm = "-W".
    alternatives = [].
    usage = "-W dir -- working directory".
    validator = some(isDir).
    setOption(W,Opts) where RW ^= parseUri(W) && NW^=resolveUri(Opts.cwd,RW)=> compilerOptions{repo=Opts.repo. cwd=NW}.
  }

  public _main:(list[string])=>().
  _main(Args) where RI^=parseUri("file:"++_repo()) && WI^=parseUri("file:"++_cwd())=>
    valof handleCmds(processOptions(Args,[repoOption,wdOption],compilerOptions{repo=RI. cwd=WI})).

  openupRepo:(uri,uri) => action[(), termRepo].
  openupRepo(RU,CU) where CRU ^= resolveUri(CU,RU) => do{
    Repo .= openRepository(CRU);
--    logMsg("repo: $(Repo)");
    valis Repo
  }

  handleCmds:(either[string,(compilerOptions,list[string])])=>action[(),()].
  handleCmds(either((Opts,Args))) => do{
    logMsg("Arg=$(Args)");
    Repo <- openupRepo(Opts.repo,Opts.cwd);
    if CatUri ^= parseUri("catalog") && CatU ^= resolveUri(Opts.cwd,CatUri) &&
	Cat ^= loadCatalog(CatU) then{
	  for P in Args do{
--	    logMsg("look up $(P) in catalog $(Cat)");
	    ErRp .= reports([]);	
	    try{
	      Sorted <- makeGraph(extractPkgSpec(P),Repo,Cat,ErRp)
	      ::action[reports,list[(importSpec,list[importSpec])]];
--	      logMsg("package groups $(Sorted)");
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

  extractPkgSpec(P) where Lc .= _str_find(P,":",0) && Lc>0 => let{
    Pkg = pkg(P[0:Lc],P[Lc+1:size(P)]::version).
  } in pkgImp(pkgLoc(Pkg),priVate,Pkg).
  extractPkgSpec(P) default =>
    pkgImp(pkgLoc(pkg(P,defltVersion)),priVate,pkg(P,defltVersion)).

  addSpec:(pkgSpec,termRepo) => termRepo.
  addSpec(Spec,R) where pkgSpec(Pkg,_,_,_,_,_) .= Spec => addSigToRepo(R,Pkg,(Spec::term)::string).

  importVars(pkgSpec(_,_,_,_,_,Vars))=>Vars.

  processPkgs:(list[(importSpec,list[importSpec])],termRepo,catalog,compilerOptions,reports) => action[reports,()].
  processPkgs(Pks,Repo,Cat,Opts,Rp) => do{
    Repp := Repo;
    try{
--      logMsg("repo is $(Repo)");
      for (pkgImp(Lc,_,P),Imps) in Pks do{
--	logMsg("$(P) ok? $(pkgOk(Repo,P))");
	if \+ (pkgOk(Repo,P) && pkgImp(_,_,I) in Imps *> pkgOk(Repo,I)) then{
	  logMsg("Process package $(P)");
	  if (SrcUri,CPkg) ^= resolveInCatalog(Cat,pkgName(P)) then{
--	  logMsg("source uri: $(SrcUri), CPkg=$(CPkg)");
	    Ast <- parseSrc(SrcUri,CPkg,Rp)::action[reports,ast];
--	  logMsg("Ast of $(P) is $(Ast)");
	    (PkgSpec,PkgFun) <- checkPkg(Repp!,CPkg,Ast,stdDict,Rp) :: action[reports,(pkgSpec,canonDef)];
	    logMsg("normalizing $(PkgFun), pkgSpec = $(PkgSpec)");
	    NormDefs <- normalize(PkgSpec,PkgFun,Rp)::action[reports,list[crDefn]];
	    Repp := addSpec(PkgSpec,Repp!);
	    logMsg("Normalized package $(P)");
	    logMsg(dispCrProg(NormDefs)::string);
	    Ins <- compDefs(NormDefs,importVars(PkgSpec),Opts,[],Rp) :: action[reports,list[codeSegment]];
	    logMsg("Generated instructions $(Ins)");
	    Code .= mkTpl([pkgTerm(CPkg),strg(encodeSignature(typeOf(PkgSpec))),
		mkTpl(pkgImports(PkgSpec)//(pkgImp(_,_,IPkg))=>pkgTerm(IPkg)),
		mkTpl(Ins//assem)]);
	    logMsg("generated code $(encodeTerm(Code)::string)");
	    Repp := addSource(addPackage(Repp!,P,encodeTerm(strg(encodeTerm(Code)::string))::string),P,
	      SrcUri::string)
	  }
	  else
	  throw reportError(Rp,"cannot locate source of $(P)",Lc)
	}
      }
    }catch (Erp) => do{
      logMsg("Errors $(Erp)");
      throw Erp
    }
  }
}
