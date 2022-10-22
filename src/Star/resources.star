star.resources{
  import star.
  import star.file.
  import star.uri.

  public
  getResource:(uri) => option[string].
  getResource(U) where Fn .= getUriPath(U) => getFile(Fn).
  getResource(_) default => .none.

  public putResource:(uri,string)=>().
  putResource(U,Content) => putFile(getUriPath(U),Content).

  public resourcePresent:(uri)=>boolean.
  resourcePresent(U) => filePresent(getUriPath(U)).

  public newerRsrc:(uri,uri)=>boolean.
  newerRsrc(U1,U2) => newerFile(getUriPath(U1),getUriPath(U2)).

  public cwd:()=>uri.
  cwd() where U?=parseUri(_cwd()) => U.

  public searchForRsRc:(uri,string)=>option[uri].
  searchForRsRc(U,Pth) where P?=parseUri("../") && R ?= parseUri(Pth) =>
    let{.
      searchFor(C) where
	  RU ?= resolveUri(C,R) &&
	  P1 .= getUriPath(RU) &&
	  filePresent(P1) => some(RU).
      searchFor(C) where
	  PU ?= resolveUri(C,P) &&
	  P1 .= getUriPath(PU) && P1~="/" =>
	searchFor(PU).
      searchFor(_) default => .none
   .} in searchFor(U).
}
