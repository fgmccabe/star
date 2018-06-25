star.repo.file{
  import star.
  import star.repo.
  import star.repo.manifest.
  import star.uri.
  import star.resources.

  public fileRepo ::= repo(uri,manifest).

  public openRepository:(uri) => fileRepo.
  openRepository(Root) => repo(Root,readManifest(RepoUri)) :-
    RepoUri = resolveUri(Root,parseUri("manifest")),
    resourcePresent(RepoUri).
  openRepository(Root) => repo(Root,manifest([])).

  public implementation repo[fileRepo] => {
    packagePresent(repo(Root,Man),Pkg,Kind) :-
      locateInManifest(Man,Pkg,"source",S),
      locateInManifest(Man,Pkg,"code",U),
      CodeFile = resolveUri(Root,parseUri(U)),
      SrcFile = resolveUri(Root,parseUri(S)),
      resourcePresent(CodeFile),
      (resourcePresent(SrcFile) ?
          newerFile(getUriPath(CodeFile),getUriPath(SrcFile)) | true).

    loadFromRepo(repo(Root,Man),Pkg,Kind,getResource(resolveUri(Root,parseUri(U)))) :-
      locateInManifest(Man,Pkg,Kind,U).
  }

  public addToRepo:(fileRepo,pkg,string,string) => fileRepo.
  addToRepo(repo(Root,Man),Pkg,Kind,Text) => repo(Root,NM) :-
    extensionMapping(Kind,Ext),
    Fn = Pkg+(hash(Pkg)::string)+Ext,
    putResource(resolveUri(Root,parseUri(Fn)),Text),
    NM = addToManifest(Man,Pkg,Kind,Fn),
    RepoUri = resolveUri(Root,parseUri("manifest")),
    flushManifest(RepoUri,NM).

  public locateCode:(fileRepo,pkg,string,string){}.
  locateCode(repo(Root,Man),Pkg,U,getResource(resolveUri(Root,parseUri(U)))) :-
    locateInManifest(Man,Pkg,"code",U).

  public locateProlog:(fileRepo,pkg,string,string){}.
  locateProlog(repo(Root,Man),Pkg,U,getResource(resolveUri(Root,parseUri(U)))) :-
    locateInManifest(Man,Pkg,"prolog",U).

  public packageCodeOk:(fileRepo,pkg){}.
  packageCodeOk(Repo,Pkg) :- packageOk(Repo,Pkg,"code").

  public packagePrologOk:(fileRepo,pkg){}.
  packagePrologOk(Repo,Pkg) :- packageOk(Repo,Pkg,"prolog").

  packageOk:(fileRepo,pkg,string){}.
  packageOk(repo(Root,Man),Pkg,Kind) :-
    locateInManifest(Man,Pkg,Kind,U),
    locateInManifest(Man,Pkg,"source",S),
    CodeFile = resolveUri(Root,parseUri(U)),
    SrcFile = resolveUri(Root,parseUri(S)),
    resourcePresent(CodeFile),
    (resourcePresent(SrcFile) ?
        newerFile(getUriPath(CodeFile),getUriPath(SrcFile)) | true).

  public addPackage:(fileRepo,pkg,string) => fileRepo.
  addPackage(Repo,Pkg,Text) => addToRepo(Repo,Pkg,"code",Text).

  public addSource:(fileRepo,pkg,string) => fileRepo.
  addSource(repo(Root,Man),Pkg,Nm) => repo(Root,addToManifest(Man,Pkg,"source",Nm)).

  packageHash:(string,version) => integer.
  packageHash(Pkg,defltVersion) => hash(Pkg).
  packageHash(Pkg,vers(V)) => ((37*hash(Pkg))+hash(V)).

  extensionMapping:(string,string){}.
  extensionMapping("source",".lo").
  extensionMapping("prolog",".pl").
  extensionMapping("term",".term").
  extensionMapping("code","").

  public implementation display[fileRepo] => {
    disp(F) => dispRepo(F).
  }

  dispRepo:(fileRepo) => ss.
  dispRepo(repo(Root,Man)) => ssSeq([ss("file repo rooted at "),disp(Root),ss("\nmanifest:"),disp(Man)]).
}
