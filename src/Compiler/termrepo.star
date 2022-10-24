star.compiler.term.repo{
  import star.
  import star.file.
  import star.pkg.
  import star.repo.
  import star.repo.manifest.
  import star.uri.
  import star.resources.
  import star.compiler.misc.
  import star.compiler.data.
  
  public termRepo ::= .repo(uri,manifest).

  public openRepository:(uri) => termRepo.
  openRepository(Root) where
      ManUri ?= parseUri("manifest") &&
      MU ?= resolveUri(Root,ManUri) &&
      Man ?= readManifest(MU) => repo(Root,Man).
  openRepository(Root) => .repo(Root,man([])).

  public implementation repo[termRepo] => {
    pkgSignature(.repo(_,Man),Pkg) => locateInManifest(Man,Pkg,"signature").

    hasCode(.repo(Root,Man),Pkg) where
	U ?= locateInManifest(Man,Pkg,"code") &&
	Uri ?= parseUri(U) &&
	RU ?= resolveUri(Root,Uri)  => getResource(RU).
    hasCode(_,_) default => .none.
  }

  public flushRepo:(termRepo)=>termRepo.
  flushRepo(.repo(Root,Man)) => valof{
    if MU ?= parseUri("manifest") && RepoUri ?= resolveUri(Root,MU) then{
      flushManifest(RepoUri,Man)
    };
    valis .repo(Root,Man);
  }
  
  public addToRepo:(termRepo,pkg,string,string) => termRepo.
  addToRepo(.repo(Root,Man),.pkg(Pk,Vr),Kind,Text) => valof{
    Ext = extensionMapping(Kind);
    Fn = "#(Pk).$(Vr)#(Ext)";
--    Fn = Pk++(hash(Pk)::string)++Ext;
    FUri = ^parseUri(Fn);
    FU = ^resolveUri(Root,FUri);
    logMsg("dest uri $(FU)");
    putResource(FU,Text);
    logMsg("written");
    NM = addToManifest(Man,pkg(Pk,Vr),Kind,Fn);
    logMsg("added to manifest");
    MU = ^parseUri("manifest");
    RepoUri = ^resolveUri(Root,MU);
    flushManifest(RepoUri,NM);
    logMsg("manifest flushed");
    valis .repo(Root,NM)
  }


  public addSigToRepo:(termRepo,pkg,string) => termRepo.
  addSigToRepo(.repo(Root,Man),Pk,Sig) =>
    .repo(Root,addToManifest(Man,Pk,"signature",Sig)).

  public packageCodeOk:(termRepo,pkg) => boolean.
  packageCodeOk(Repo,Pkg) where
      (SrcFile,CodeFile) ?= packageCode(Repo,Pkg) =>
    resourcePresent(CodeFile) &&
    resourcePresent(SrcFile) &&
        newerRsrc(CodeFile,SrcFile).
  packageCodeOk(_,_) default => .false.

  public pkgOk:(termRepo,pkg)=>boolean.
  pkgOk(Repo,Pkg) => (SrcUri,CodeUri) ?= packageCode(Repo,Pkg) ?
    newerRsrc(CodeUri,SrcUri) || .false.

  public packageCode:(termRepo,pkg) => option[(uri,uri)].
  packageCode(.repo(Root,Man),Pkg) where
      U ?= locateInManifest(Man,Pkg,"code") &&
      S ?= locateInManifest(Man,Pkg,"source") &&
      CU ?= parseUri(U) &&
      CodeFile ?= resolveUri(Root,CU) &&
      SU ?= parseUri(S) &&
      SrcFile ?= resolveUri(Root,SU) => some((SrcFile,CodeFile)).
  packageCode(_,_) default => .none.
    
  public addPackage:(termRepo,pkg,string) => termRepo.
  addPackage(Repo,Pkg,Text) => addToRepo(Repo,Pkg,"code",Text).

  public addSource:(termRepo,pkg,string) => termRepo.
  addSource(.repo(Root,Man),Pkg,Nm) => .repo(Root,addToManifest(Man,Pkg,"source",Nm)).

  extensionMapping:(string) => string.
  extensionMapping("source") => ".star".
  extensionMapping("term") => ".term".
  extensionMapping("code") => ".co".

  public implementation display[termRepo] => {
    disp(.repo(Root,Man)) => "file repo rooted at $(Root)\nmanifest:$(Man)".
  }

  termManifest:(data)=>manifest.
  termManifest(.term(_,Els)) => .man(foldRight(termEntry,[],Els)).

  termEntry:(data,map[string,pEntry]) => map[string,pEntry].
  termEntry(.term(_,[.strg(P),.term(_,Els)]),Map) => 
    Map[P->.pEntry(P,foldRight(termVersion,[],Els))].

  termVersion:(data,cons[(version,mInfo)])=>cons[(version,mInfo)].
  termVersion(.term(_,[.strg(V),.term(_,Es)]),Vs) where Vr.=V::version =>
    [(Vr,.mInfo(Vr,foldRight(termInfo,[],Es))),..Vs].

  termInfo:(data,map[string,string])=>map[string,string].
  termInfo(.term(.tLbl(Ky,1),[.strg(V)]),Is) => Is[Ky->V].

  implementation coercion[data,manifest] => {
    _coerce(T) => some(termManifest(T)).
  }

  infoTerm:(mInfo)=>data.
  infoTerm(.mInfo(_,Els)) => mkLst(ixRight((Ky,Vl,Mp)=>[mkCons(Ky,[.strg(Vl)]),..Mp],[],Els)).

  versionTerm:(pEntry)=>data.
  versionTerm(.pEntry(Pk,Vs)) => mkLst(
	foldRight(((V,Is),Es)=>
	[mkTpl([.strg(V::string),infoTerm(Is)]),..Es],[],Vs)).

  manTerm:(manifest)=>data.
  manTerm(.man(Ps))=>mkLst(ixRight((Pk,Vr,Ms)=>
	[mkTpl([.strg(Pk),versionTerm(Vr)]),..Ms],[],Ps)).

  implementation coercion[manifest,data] => {
    _coerce(M)=>.some(manTerm(M)).
  }

  flushManifest(Url,Man) => putResource(Url,(Man::data)::string).

  readManifest(Url) where
      Txt ?= getResource(Url) &&
      J.=Txt::data => some(J::manifest).
  readManifest(_) default => .none.
}
