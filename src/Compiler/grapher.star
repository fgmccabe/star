star.compiler.grapher{
  import star.

  import star.resources.
  import star.topsort.
  import star.pkg.
  import star.uri.

  import star.repo.

  import star.compiler.catalog.
  import star.compiler.errors.
  import star.compiler.lexer.
  import star.compiler.location.

  scanPkgs:all r ~~ repo[r] |:
    (list[string],r,catalog,uri,list[pkg],reports)=>either[reports,list[pkg]].
  scanPkgs([P,..Pkgs],Repo,Cat,Base,SoFar,Rp) => do{
    Pkg = extractPkgName(P);
    if Pk in SoFar && compatiblePkg(Pk,Pkg) then{
      scanPkgs(Pkgs,Repo,Cat,Base,SoFar,Rp)
    };
    valis SoFar
  }

  extractPkgName(P) where Lc .= _str_find(P,":",0) && Lc>0 =>
    pkg(_sub_str(P,0,Lc),_sub_str(P,Lc+1,_str_len(P))::version).
  extractPkgName(P) default => pkg(P,defltVersion).

  consistentVersion(defltVersion,_) => true.
  consistentVersion(_,defltVersion) => true.
  consistentVersion(vers(V1),vers(V2)) => V1==V2.
}
