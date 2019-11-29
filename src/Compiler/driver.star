star.compiler{
  import star.
  import star.cmdOpts.
  import star.pkg.
  import star.resources.
  import star.uri.

  import star.repo.
  import star.repo.file.

  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.catalog.
  import star.compiler.checker.
  import star.compiler.core.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.grapher.
  import star.compiler.impawt.
  import star.compiler.meta.
  import star.compiler.parser.
  import star.compiler.location.
  import star.compiler.normalize.
  import star.compiler.terms.

  implementation all e,k ~~ coercion[either[e,k],action[e,k]] => {
    _coerce(either(X)) => done(X).
    _coerce(other(Y)) => err(Y)
  }

  compilerOptions ::= compilerOptions(uri,uri).

  repoOption:optionsProcessor[compilerOptions].
  repoOption = {
    shortForm = "-R".
    alternatives = [].
    usage = "-R dir -- directory of repository".
    validator = some(isDir).
    setOption(R,compilerOptions(_,W)) where RU ^= parseUri(R) && NR^=resolveUri(W,RU) =>
      compilerOptions(NR,W).
  }

  wdOption:optionsProcessor[compilerOptions].
  wdOption = {
    shortForm = "-W".
    alternatives = [].
    usage = "-W dir -- working directory".
    validator = some(isDir).
    setOption(W,compilerOptions(R,OW)) where RW ^= parseUri(W) && NW^=resolveUri(OW,RW)=>
      compilerOptions(R,NW).
  }

  public _main:(list[string])=>().
  _main(Args) where RI^=parseUri("file:"++_repo()) && WI^=parseUri("file:"++_cwd())=>
    valof handleCmds(processOptions(Args,[repoOption,wdOption],compilerOptions(RI,WI))).

  openupRepo:(uri,uri) => action[(), fileRepo].
  openupRepo(RU,CU) where CRU ^= resolveUri(CU,RU) => do{
    valis openRepository(CRU)
  }

  handleCmds:(either[string,(compilerOptions,list[string])])=>action[(),()].
  handleCmds(either((compilerOptions(RU,CU),Args))) => do{
--    logMsg("CU=$(CU), RU=$(RU)");
    Repo <- openupRepo(RU,CU);
    if CatUri ^= parseUri("catalog") && CatU ^= resolveUri(CU,CatUri) &&
	Cat ^= loadCatalog(CatU) then{
	  for P in Args do{
	    ErRp = reports([]);	
	    try{
	      Sorted <- makeGraph(extractPkgSpec(P),Repo,Cat,ErRp)
	      ::action[reports,list[(importSpec,list[importSpec])]];
	      Pkgs = Sorted//((Pk,_))=>Pk;
	      processPkgs(Pkgs,Repo,Cat,ErRp)
	    } catch (Er) => action{
	      logMsg("$(Er)")
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

  addSpec:(pkgSpec,fileRepo) => fileRepo.
  addSpec(Spec,R) where pkgSpec(Pkg,_,_,_,_,_) .= Spec => addSigToRepo(R,Pkg,(Spec::data)::string).

  processPkgs:(list[importSpec],fileRepo,catalog,reports) => action[reports,()].
  processPkgs(Pks,Repo,Cat,Rp) => do{
    Repp := Repo;
    try{
      for pkgImp(Lc,_,P) in Pks do{
	logMsg("Process $(P)");
	if (SrcUri,CPkg) ^= resolveInCatalog(Cat,pkgName(P)) then{
	  Ast <- parseSrc(SrcUri,CPkg,Rp)::action[reports,ast];
--	  logMsg("Ast of $(P) is $(Ast)");
	  (PkgSpec,PkgFun) <- checkPkg(Repp!,Ast,stdDict,Rp) :: action[reports,(pkgSpec,canon)];
	  logMsg("Checked spec $(PkgSpec)");
	  (PrgVal,NormDefs) <- normalize(PkgSpec,PkgFun,Rp)::action[reports,(crExp,list[crDefn])];
	  Repp := addSpec(PkgSpec,Repp!);
	  logMsg("Normalized program: $(NormDefs) with $(PrgVal)")
	}
	else
	throw reportError(Rp,"cannot locate source of $(P)",Lc)
      }
    }catch (Erp) => do{
      logMsg("Errors $(Erp)");
      throw Erp
    }
  }
}
