star.compiler.package.merge{
  import star.
  import star.pkg.
  import star.repo.

  import star.compiler.data.
  import star.compiler.errors.
  import star.compiler.impawt.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.term.
  import star.compiler.term.repo.
  import star.compiler.types.

  -- Merge packages into a single package
  public mergePkgs:(cons[pkg],option[locn],termRepo,set[pkg],cons[cDefn]) =>
    (set[pkg],cons[cDefn]).
  mergePkgs([],_,_,Imported,Defs) => (Imported,Defs).
  mergePkgs([Pkg,..Pkgs],Lc,Repo,Imported,Defs) => valof{
    if Pkg .<. Imported then
      valis mergePkgs(Pkgs,Lc,Repo,Imported,Defs)
    else {
      if Spec ?= importPkg(Pkg,Lc,Repo) then{
	if Xs ?= importLowered(Pkg,Repo) then
	  valis mergePkgs(Pkgs++(Spec.imports//((.pkgImp(_,_,S))=>S)),
	  Lc,Repo,Imported\+Pkg,Xs++Defs)
	else{
	  reportWarning("nothing to import from $(Pkg)",Lc);
	  valis mergePkgs(Pkgs++(Spec.imports//((.pkgImp(_,_,S))=>S)),
	    Lc,Repo,Imported\+Pkg,Defs)
	}
      }
      else{
	reportError("cannot import $(Pkg)",Lc);
	valis mergePkgs(Pkgs,Lc,Repo,Imported,Defs)
      }
    }
  }
}
