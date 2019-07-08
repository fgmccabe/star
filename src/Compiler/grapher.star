star.compiler.grapher{
  import star.

  import star.resources.
  import star.topsort.
  import star.pkg.
  import star.uri.

  import star.repo.
  import star.repo.file.

  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.catalog.
  import star.compiler.errors.
  import star.compiler.impawt.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.parser.
  import star.compiler.wff.

  public makeGraph:(importSpec,fileRepo,catalog,reports) => either[reports,list[(importSpec,list[importSpec])]].
  makeGraph(Imp,Repo,Cat,Rp) => do{
    Defs <- scanPkgs([Imp],Repo,Cat,[],Rp);
    Gps = topsort(Defs);
    valis flatten(Gps)
  }

  implementation depends[(importSpec,list[importSpec])->>importSpec] => {
    defined((pkgImp(_,_,P),_),pkgImp(_,_,Q)) => compatiblePkg(P,Q).
    references((_,R)) => R
  }

  scanPkgs:(list[importSpec],fileRepo,catalog,list[(importSpec,list[importSpec])],reports)=>either[reports,list[(importSpec,list[importSpec])]].
  scanPkgs([P,..Pkgs],Repo,Cat,SoFar,Rp) where pkgImp(Lc,Vz,Pkg) .=P => do{
    if (pkgImp(_,_,Pk),_) in SoFar && compatiblePkg(Pk,Pkg) then{
      scanPkgs(Pkgs,Repo,Cat,SoFar,Rp)
    } else if (SrcUri,CodeUri) ^= packageCode(Repo,Pkg) then {
      if newerFile(CodeUri,SrcUri) && pkgSpec(_,Imps,_,_,_) ^= importPkg(Pkg,Lc,Repo) then {
	scanPkgs(Pkgs++Imps,Repo,Cat,[SoFar..,(P,Imps)],Rp)
      } else
      scanCat(Lc,Vz,Pkg,Pkgs,Repo,Cat,SoFar,Rp)
    } else
    scanCat(Lc,Vz,Pkg,Pkgs,Repo,Cat,SoFar,Rp)
  }

  scanCat:(locn,visibility,pkg,list[importSpec],fileRepo,catalog,list[(importSpec,list[importSpec])],reports) =>
    either[reports,list[(importSpec,list[importSpec])]].
  scanCat(Lc,Vz,Pkg,Pkgs,Repo,Cat,SoFar,Rp) => do{
    if (SrcUri,CPkg) ^= resolveInCatalog(Cat,pkgName(Pkg)) then{
      if compatiblePkg(CPkg,Pkg) then{
	Ast <- parseSrc(SrcUri,CPkg,Rp);
	SubImps <- scanForImports(Ast,Rp);
	scanPkgs(Pkgs++SubImps,Repo,Cat,[SoFar..,(pkgImp(Lc,Vz,CPkg),SubImps)],Rp)
      } else
      throw reportError(Rp,"package in catalog $(CPkg) not compatible with requested package $(Pkg)",Lc)
    } else
    throw reportError(Rp,"package $(Pkg) not in catalog",Lc)
  }

  scanForImports:(ast,reports) => either[reports,list[importSpec]].
  scanForImports(Term,Rp) => do{
    if (Lc,_,Els) ^= isBrTerm(Term) then {
      Imps := ([]:list[importSpec]);
      for St in Els do{
	if Imp ^= isImport(St) then{
	  Imps := [Imps! ..,Imp]
	}
      };
      valis Imps!
    } else
    throw reportError(Rp,"not a valid package $(Term)",locOf(Term))
  }

  consistentVersion(defltVersion,_) => true.
  consistentVersion(_,defltVersion) => true.
  consistentVersion(vers(V1),vers(V2)) => V1==V2.  
}
