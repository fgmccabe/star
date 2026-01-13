star.compiler.grapher{
  import star.

  import star.resources.
  import star.topsort.
  import star.pkg.
  import star.uri.

  import star.compiler.ast.
  import star.compiler.catalog.
  import star.compiler.errors.
  import star.compiler.impawt.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.parser.
  import star.compiler.token.
  import star.compiler.term.repo.
  import star.compiler.wff.

  public makeGraph:(pkg,termRepo,catalog) => cons[(pkg,cons[pkg])].
  makeGraph(Pkg,Repo,Cat) => valof{
    Defs = scanPkgs([Pkg],Repo,Cat,[]);
    Gps = topsort(Defs);
    valis Gps*
  }

  implementation depends[(pkg,cons[pkg])->>pkg] => {
    defined((P,_),Q) => compatiblePkg(P,Q).
    references((_,R)) => R
  }

  scanPkgs:(cons[pkg],termRepo,catalog,cons[(pkg,cons[pkg])])=> cons[(pkg,cons[pkg])].
  scanPkgs([],_,_,SoFar) => SoFar.
  scanPkgs([Pkg,..Pkgs],Repo,Cat,SoFar) => valof{
    if {? (Pk,_) in SoFar && compatiblePkg(Pk,Pkg) ?} then{
      valis scanPkgs(Pkgs,Repo,Cat,SoFar)
    } else if (SrcUri,CodeUri) ?= packageCode(Repo,Pkg) then {
      if newerRsrc(CodeUri,SrcUri) then {
	if ImpPkg ?= importPkg(Pkg,.none,Repo) then{
	  ImpPks = (ImpPkg.imports//(.pkgImp(_,_,Pk))=>Pk);
	  valis scanPkgs(Pkgs++ImpPks,Repo,Cat,[(Pkg,ImpPks),..SoFar])
	}
      };
      valis scanCat(Pkg,Pkgs,Repo,Cat,SoFar)
    } else
    valis scanCat(Pkg,Pkgs,Repo,Cat,SoFar)
  }

  scanCat:(pkg,cons[pkg],termRepo,catalog,cons[(pkg,cons[pkg])]) => cons[(pkg,cons[pkg])].
  scanCat(Pkg,Pkgs,Repo,Cat,SoFar) => valof{
    if (SrcUri,CPkg) ?= resolveInCatalog(Cat,pkgName(Pkg)) then{
      if compatiblePkg(CPkg,Pkg) then{
	if Ast ?=parseSrc(SrcUri,CPkg) then{
	  SubImps = scanForImports(Ast);
	  valis scanPkgs(Pkgs++SubImps,Repo,Cat,[(CPkg,SubImps),..SoFar])
	} else{
	  reportError("cannot parse source $(SrcUri)",.none);
	  valis []
	}	  
      } else{
	reportError("package in catalog $(CPkg) not compatible with requested package $(Pkg)",.none);
	valis []
      }
    } else{
      reportError("package $(Pkg) not in catalog",.none);
      valis []
    }
  }

  scanForImports:(ast) => cons[pkg].
  scanForImports(Term) => valof{
    if (Lc,_,Els) ?= isBrTerm(Term) then {
      Imps = ref ([]:cons[pkg]);
      for St in Els do{
	if .pkgImp(_,_,Imp) ?= isImport(St) then{
	  Imps := [Imp,..Imps!]
	}
      };
      valis Imps!
    } else{
      reportError("not a valid package $(Term)",locOf(Term));
      valis []
    }
  }

  scanToks:(cons[token])=>cons[string].
  scanToks([]) => [].
  scanToks([.tok(_,.idTok("import")),..Tks]) => scanPkg(Tks,[]).
  scanToks([_,..Tks]) => scanToks(Tks).

  scanPkg:(cons[token],cons[string])=>cons[string].
  scanPkg([],[]) => [].
  scanPkg([],So) => [pkgNme(reverse(So))].
  scanPkg([.tok(_,.idTok(". ")),..Tks],So) => [pkgNme(reverse(So)),..scanToks(Tks)].
  scanPkg([.tok(_,.idTok(Seg)),..Tks],So) => scanPkg(Tks,[Seg,..So]).

  pkgNme(Strs) => _str_multicat(Strs).

  public makeDotGraph:(string,cons[(pkg,cons[pkg])])=>string.
  makeDotGraph(Nm,Defs) => "digraph $(Nm) {\n#(interleave(Defs//((P,I))=>makePkgGraph(P,I),"\n")*)\n}".

  makePkgGraph(Pkg,Imports) =>
    interleave(Imports//(P)=>showImport(Pkg,P),"\n")*.

  showImport(P,I) => "$(pkgName(P)) -> $(pkgName(I));".
}
