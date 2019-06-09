star.repo.file{
  import star.
  import star.json.
  import star.parse.
  import star.pkg.
  import star.repo.
  import star.repo.manifest.
  import star.uri.
  import star.resources.

  public fileRepo ::= repo(uri,manifest).

  public openRepository:(uri) => fileRepo.
  openRepository(Root) where
      ManUri ^= parseUri("manifest") &&
      MU ^= resolveUri(Root,ManUri) &&
      Man ^= readManifest() => repo(Root,Man).
  openRepository(Root) => repo(Root,man([])).

  public implementation repo[fileRepo] => {
    hasSignature(repo(_,Man),Pkg) => locateInManifest(Man,Pkg,"signature").

    hasCode(repo(Root,Man),Pkg) where
      U ^= locateInManifest(Man,Pkg,Kind) &&
	RU ^= resolveUri(Root,Uri) &&
	Uri ^= parseUri(U) => getResource(RU).
    hasCode(_,_) default => none.
  }

  public addToRepo:(fileRepo,pkg,string,string) => fileRepo.
  addToRepo(repo(Root,Man),pkg(Pk,Vr),Kind,Text) where
      Ext .= extensionMapping(Kind) &&
      Fn .= Pk++(hash(Pk)::string)++Ext &&
      FUri ^= parseUri(Fn) &&
      FU ^= resolveUri(Root,FUri) &&
      () .= putResource(FU,Text) &&
      NM .= addToManifest(Man,pkg(Pk,Vr),Kind,Fn)&&
      MU ^= parseUri("manifest") &&
      RepoUri ^= resolveUri(Root,MU) &&
      () .= flushManifest(RepoUri,NM) => repo(Root,NM).

  public packageCodeOk:(fileRepo,pkg) => boolean.
  packageCodeOk(Repo,Pkg) => packageOk(Repo,Pkg,"code").

  packageOk:(fileRepo,pkg,string) => boolean.
  packageOk(repo(Root,Man),Pkg,Kind) where
    U ^= locateInManifest(Man,Pkg,Kind) &&
    S ^= locateInManifest(Man,Pkg,"source") &&
    CU ^= parseUri(U) &&
    CodeFile ^= resolveUri(Root,CU) &&
    SU ^= parseUri(S) &&
    SrcFile ^= resolveUri(Root,SU) &&
    resourcePresent(CodeFile) &&
    resourcePresent(SrcFile) =>
        newerFile(CodeFile,SrcFile).
  packageOk(_,_,_) default => false.

  public addPackage:(fileRepo,pkg,string) => fileRepo.
  addPackage(Repo,Pkg,Text) => addToRepo(Repo,Pkg,"code",Text).

  public addSource:(fileRepo,pkg,string) => fileRepo.
  addSource(repo(Root,Man),Pkg,Nm) => repo(Root,addToManifest(Man,Pkg,"source",Nm)).

  extensionMapping:(string) => string.
  extensionMapping("source") => ".star".
  extensionMapping("term") => ".term".
  extensionMapping("code") => ".cafe".

  public implementation display[fileRepo] => {.
    disp(repo(Root,Man)) => ssSeq([ss("file repo rooted at "),disp(Root),ss("\nmanifest:"),disp(Man)]).
  .}

  flushManifest(Url,Man) => putResource(Url,(Man::json)::string).

  readManifest(Url) where
      Txt ^= getResource(Url) &&
      J.=Txt::json => some(J::manifest).
  readManifest(_) default => none.
}
