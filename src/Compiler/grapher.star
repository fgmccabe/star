star.compiler.grapher{
  import star.

  import star.resources.
  import star.topsort.
  import star.pkg.
  import star.uri.

  import star.compiler.ast.
  import star.compiler.ast.display.
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

  public makeGraph:(importSpec,termRepo,catalog,reports) => either[reports,cons[(importSpec,cons[importSpec])]].
  makeGraph(Imp,Repo,Cat,Rp) => do{
    Defs <- scanPkgs([Imp],Repo,Cat,[],Rp);
    Gps .= topsort(Defs);
--    logMsg("pkg graph $(Gps)");
    valis multicat(Gps)
  }

  implementation depends[(importSpec,cons[importSpec])->>importSpec] => {
    defined((pkgImp(_,_,P),_),pkgImp(_,_,Q)) => compatiblePkg(P,Q).
    references((_,R)) => R
  }

  scanPkgs:(cons[importSpec],termRepo,catalog,cons[(importSpec,cons[importSpec])],reports)=>either[reports,cons[(importSpec,cons[importSpec])]].
  scanPkgs([],_,_,SoFar,_) => either(SoFar).
  scanPkgs([P,..Pkgs],Repo,Cat,SoFar,Rp) where pkgImp(Lc,Vz,Pkg) .=P => do{
--    logMsg("scanning $(P)");
    if (pkgImp(_,_,Pk),_) in SoFar && compatiblePkg(Pk,Pkg) then{
--      logMsg("$(P) already ok");
      scanPkgs(Pkgs,Repo,Cat,SoFar,Rp)
    } else if (SrcUri,CodeUri) ^= packageCode(Repo,Pkg) then {
--      logMsg("$(P) in repo at $(CodeUri)");
      if newerFile(CodeUri,SrcUri) then {
	try{
	  pkgSpec(_,Imps,_,_,_,_) <- importPkg(Pkg,Lc,Repo,Rp);
	  try{
	    scanPkgs(Pkgs++Imps,Repo,Cat,[(P,Imps),..SoFar],Rp)
	  } catch {
	    throw reportError(Rp,"cannot graph $(Pkg)",Lc) -- This is kind of ugly.
	  }
	} catch{
	  throw reportError(Rp,"cannot import $(Pkg)",Lc)
	}
      } else
      scanCat(Lc,Vz,Pkg,Pkgs,Repo,Cat,SoFar,Rp)
    } else
    scanCat(Lc,Vz,Pkg,Pkgs,Repo,Cat,SoFar,Rp)
  }

  scanCat:(locn,visibility,pkg,cons[importSpec],termRepo,catalog,cons[(importSpec,cons[importSpec])],reports) =>
    either[reports,cons[(importSpec,cons[importSpec])]].
  scanCat(Lc,Vz,Pkg,Pkgs,Repo,Cat,SoFar,Rp) => do{
    if (SrcUri,CPkg) ^= resolveInCatalog(Cat,pkgName(Pkg)) then{
      if compatiblePkg(CPkg,Pkg) then{
--	logMsg("parse $(Pkg)");
	Ast <- parseSrc(SrcUri,CPkg,Rp);
--	logMsg("parsed $(Pkg)");
	SubImps <- scanForImports(Ast,Rp);
	scanPkgs(Pkgs++SubImps,Repo,Cat,[(pkgImp(Lc,Vz,CPkg),SubImps),..SoFar],Rp)
      } else
      throw reportError(Rp,"package in catalog $(CPkg) not compatible with requested package $(Pkg)",Lc)
    } else
    throw reportError(Rp,"package $(Pkg) not in catalog $(Cat)",Lc)
  }

  scanForImports:(ast,reports) => either[reports,cons[importSpec]].
  scanForImports(Term,Rp) => do{
    if (Lc,_,Els) ^= isBrTerm(Term) then {
      Imps .= ref ([]:cons[importSpec]);
      for St in Els do{
	if Imp ^= isImport(St) then{
	  Imps := [Imp,..Imps!]
	}
      };
      valis Imps!
    } else
    throw reportError(Rp,"not a valid package $(Term)",locOf(Term))
  }

  consistentVersion(.defltVersion,_) => .true.
  consistentVersion(_,.defltVersion) => .true.
  consistentVersion(vers(V1),vers(V2)) => V1==V2.

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

  public makeDotGraph:(string,cons[(importSpec,cons[importSpec])])=>string.
  makeDotGraph(Nm,Defs) =>
    ssSeq([ss("digraph "),disp(Nm),ss(" {\n"),
	ssSeq(interleave(Defs//((P,I))=>makePkgGraph(P,I),ss("\n"))),
	ss("\n}")])::string.

  makePkgGraph(pkgImp(Lc,Vz,Pkg),Imports) =>
    ssSeq(interleave(Imports//(P)=>showImport(Pkg,P),ss("\n"))).

  showImport(P,pkgImp(Lc,.pUblic,I)) => ssSeq([disp(pkgName(P)),
      ss(" -> "),disp(pkgName(I)),ss(" [color=\"red\"]"),ss(";")]).
  showImport(P,pkgImp(Lc,.transItive,I)) => ssSeq([disp(pkgName(P)),
      ss(" -> "),disp(pkgName(I)),ss(" [style=dotted]"),ss(";")]).
  showImport(P,pkgImp(Lc,Vz,I)) => ssSeq([disp(pkgName(P)),ss(" -> "),disp(pkgName(I)),ss(";")]).
}
