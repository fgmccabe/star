star.compiler.grapher{
  import star.

  import star.resources.
  import star.topsort.
  import star.pkg.
  import star.uri.

  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.catalog.
  import star.compiler.errors.
  import star.compiler.impawt.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.parser.
  import star.compiler.token.
  import star.compiler.term.repo.
  import star.compiler.wff.

  public makeGraph:(pkg,termRepo,catalog,reports) => result[reports,cons[(pkg,cons[pkg])]].
  makeGraph(Pkg,Repo,Cat,Rp) => do{
    Defs <- scanPkgs([Pkg],Repo,Cat,[],Rp);
    Gps .= topsort(Defs);
--    logMsg("pkg graph $(Gps)");
    valis Gps*
  }

  implementation depends[(pkg,cons[pkg])->>pkg] => {
    defined((P,_),Q) => compatiblePkg(P,Q).
    references((_,R)) => R
  }

  scanPkgs:(cons[pkg],termRepo,catalog,cons[(pkg,cons[pkg])],reports)=>
    result[reports,cons[(pkg,cons[pkg])]].
  scanPkgs([],_,_,SoFar,_) => do{ valis SoFar}.
  scanPkgs([Pkg,..Pkgs],Repo,Cat,SoFar,Rp) => do{
    if {? (Pk,_) in SoFar && compatiblePkg(Pk,Pkg) ?} then{
      scanPkgs(Pkgs,Repo,Cat,SoFar,Rp)
    } else if (SrcUri,CodeUri) ^= packageCode(Repo,Pkg) then {
      if newerRsrc(CodeUri,SrcUri) then {
	try{
	  pkgSpec(_,Imps,_) <- importPkg(Pkg,.none,Repo,Rp);
	  try{
	    ImpPks .= (Imps//(pkgImp(_,_,Pk))=>Pk);
	    scanPkgs(Pkgs++ImpPks,Repo,Cat,[(Pkg,ImpPks),..SoFar],Rp)
	  } catch {
	    raise reportError(Rp,"cannot graph $(Pkg)",.none) -- This is kind of ugly.
	  }
	} catch{
	  raise reportError(Rp,"cannot import $(Pkg)",.none)
	}
      } else
      scanCat(Pkg,Pkgs,Repo,Cat,SoFar,Rp)
    } else
    scanCat(Pkg,Pkgs,Repo,Cat,SoFar,Rp)
  }

  scanCat:(pkg,cons[pkg],termRepo,catalog,cons[(pkg,cons[pkg])],reports) =>
    result[reports,cons[(pkg,cons[pkg])]].
  scanCat(Pkg,Pkgs,Repo,Cat,SoFar,Rp) => do{
    if (SrcUri,CPkg) ^= resolveInCatalog(Cat,pkgName(Pkg)) then{
      if compatiblePkg(CPkg,Pkg) then{
	(some(Ast),Rp1) .= parseSrc(SrcUri,CPkg,Rp);
	SubImps <- scanForImports(Ast,Rp1);
	scanPkgs(Pkgs++SubImps,Repo,Cat,[(CPkg,SubImps),..SoFar],Rp)
      } else
      raise reportError(Rp,"package in catalog $(CPkg) not compatible with requested package $(Pkg)",.none)
    } else
    raise reportError(Rp,"package $(Pkg) not in catalog",.none)
  }

  scanForImports:(ast,reports) => result[reports,cons[pkg]].
  scanForImports(Term,Rp) => do{
    if (Lc,_,Els) ^= isBrTerm(Term) then {
      Imps .= ref ([]:cons[pkg]);
      for St in Els do{
	if pkgImp(_,_,Imp) ^= isImport(St) then{
	  Imps := [Imp,..Imps!]
	}
      };
      valis Imps!
    } else
    raise reportError(Rp,"not a valid package $(Term)",locOf(Term))
  }

  scanToks:(cons[token])=>cons[string].
  scanToks([]) => [].
  scanToks([tok(_,idTok("import")),..Tks]) => scanPkg(Tks,[]).
  scanToks([_,..Tks]) => scanToks(Tks).

  scanPkg:(cons[token],cons[string])=>cons[string].
  scanPkg([],[]) => [].
  scanPkg([],So) => [pkgNme(reverse(So))].
  scanPkg([tok(_,idTok(". ")),..Tks],So) => [pkgNme(reverse(So)),..scanToks(Tks)].
  scanPkg([tok(_,idTok(Seg)),..Tks],So) => scanPkg(Tks,[Seg,..So]).

  pkgNme(Strs) => _str_multicat(Strs).

  public makeDotGraph:(string,cons[(pkg,cons[pkg])])=>string.
  makeDotGraph(Nm,Defs) => "digraph $(Nm) {\n#(interleave(Defs//((P,I))=>makePkgGraph(P,I),"\n")*)\n}".

  makePkgGraph(Pkg,Imports) =>
    interleave(Imports//(P)=>showImport(Pkg,P),"\n")*.

  showImport(P,I) => "$(pkgName(P)) -> $(pkgName(I));".
}
