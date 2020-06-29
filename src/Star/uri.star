star.uri{
  -- Utilities to help parse and manipulate URIs
  -- This code is an attempt to directly implement the specification in RFC 2396

  import star.
  import star.parse.

  public uri ::= absUri(string,rsrcName,query) | relUri(rsrcName,query).
  public rsrcName ::= netRsrc(authority,resourcePath) | localRsrc(resourcePath).
  public resourcePath ::= absPath(cons[string]) | relPath(cons[string]).

  public authority ::= server(option[userInfo],host).

  public userInfo ::= user(string).

  public host ::= hostPort(string,string) | host(string).

  public query ::= qry(string) | .noQ.

  public parseUri:(string) => option[uri].
  parseUri(S) => first(parse(uriParse,S::cons[integer])).

  first([])=>.none.
  first([(E,_),.._])=>some(E).

  public uriParse:parser[cons[integer],uri].
  uriParse = absoluteUri +++ relativeUri.

  absoluteUri:parser[cons[integer],uri].
  absoluteUri = scheme >>= (Scheme) =>
    hierPart >>= (Hier) =>
      query >>= (Query) => return absUri(Scheme,Hier,Query).
  
  scheme:parser[cons[integer],string].
  scheme = _sat(isAlphaNum) >>= (A) => _star(alphaStar) >>= (Rest) => _tk(0c:) >>= (_) => return ([A,..Rest]::string).

  hierPart:parser[cons[integer],rsrcName].
  hierPart = netPath +++ absoluteRsrc.

  netPath:parser[cons[integer],rsrcName].
  netPath = _str("//") >>= (_) =>
    authority >>= (A) =>
      (absolutePath +++ relativePath) >>= (P) => return netRsrc(A,P).

  authority:parser[cons[integer],authority].
  authority = (userInfo >>= (U) => _tk(0c@) >>= (_) => hostNamePort >>= (H) => return server(some(U),H)) +++
  (hostNamePort >>= (H) => return server(.none,H)).

  userInfo:parser[cons[integer],userInfo].
  userInfo = _star(userStar) >>= (U) => return user(U::string).

  relativeUri:parser[cons[integer],uri].
  relativeUri = (netPath+++absoluteRsrc+++relativeRsrc) >>= (P) => query >>= (Q)=>return relUri(P,Q).

  absoluteRsrc:parser[cons[integer],rsrcName].
  absoluteRsrc = absolutePath >>= (P)=> return localRsrc(P).

  absolutePath:parser[cons[integer],resourcePath].
  absolutePath = _tk(0c/) >>= (_) => sepby(segment,_tk(0c/)) >>= (S) => return absPath(S).

  relativeRsrc:parser[cons[integer],rsrcName].
  relativeRsrc = relativePath >>= (P) => return localRsrc(P).

  relativePath:parser[cons[integer],resourcePath].
  relativePath = sepby(segment,_tk(0c/)) >>= (S) => return relPath(S).

  segment:parser[cons[integer],string].
  segment=_star(segChr) >>= (Chrs) => return (Chrs::string).

  segChr:parser[cons[integer],integer].
  segChr = _sat(isSegChr).

  isSegChr:(integer)=>boolean.
  isSegChr(0c:) => .true.
  isSegChr(0c@) => .true.
  isSegChr(0c&) => .true.
  isSegChr(0c=) => .true.
  isSegChr(0c+) => .true.
  isSegChr(0c$) => .true.
  isSegChr(0c<) => .true.
  isSegChr(0c:) => .true.
  isSegChr(0c;) => .true. -- This is a hack to merge parameters with the segment
  isSegChr(Ch) => isUnreserved(Ch).

  query:parser[cons[integer],query].
  query = (_tk(0c?) >>= (_) => _star(_sat(isUric)) >>= (QQ)=> return qry(QQ::string)) +++ (return .noQ).

  userStar:parser[cons[integer],integer].
  userStar = _sat(userCh).

  userCh:(integer) => boolean.
  userCh(0c$) => .true.
  userCh(0c,) => .true.
  userCh(0c;) => .true.
  userCh(0c:) => .true.
  userCh(0c&) => .true.
  userCh(0c=) => .true.
  userCh(0c+) => .true.
  userCh(Ch) => isUnreserved(Ch).

  hostNamePort:parser[cons[integer],host].
  hostNamePort = hostName >>= (H) =>
    ((_tk(0c:) >>= (_) => port >>= (P) => return hostPort(H,P)) +++ (return host(H))).

  hostName:parser[cons[integer],string].
  hostName = _star(alphaDash) >>= (H)=> return (H::string).

  alphaStar:parser[cons[integer],integer].
  alphaStar = _sat(isAlphaStar).

  isAlphaStar:(integer)=>boolean.
  isAlphaStar(Ch) => (isAlphaNum(Ch) || isPlus(Ch) || isMinus(Ch) || isDot(Ch)).

  alphaDash:parser[cons[integer],integer].
  alphaDash = _sat(isAlphaDash).

  isAlphaDash:(integer)=>boolean.
  isAlphaDash(Ch) => (isAlphaNum(Ch) || isMinus(Ch) || isDot(Ch)).

  port:parser[cons[integer],string].
  port = _plus(_sat(isDigit)) >>= (P)=>return (P::string).

  isMinus:(integer)=>boolean.
  isMinus(Ch) => Ch==0c-.

  isPlus:(integer)=>boolean.
  isPlus(Ch)=>Ch==0c+.

  isDot:(integer)=>boolean.
  isDot(Ch)=>Ch==0c..

  isUric:(integer)=>boolean.
  isUric(Ch) => (isReserved(Ch) || isAlphaNum(Ch) || isMark(Ch)).

  isReserved:(integer)=>boolean.
  isReserved(0c;) => .true.
  isReserved(0c/) => .true.
  isReserved(0c?) => .true.
  isReserved(0c:) => .true.
  isReserved(0c@) => .true.
  isReserved(0c&) => .true.
  isReserved(0c=) => .true.
  isReserved(0c+) => .true.
  isReserved(0c$) => .true.
  isReserved(0c,) => .true.
  isReserved(_) => .false.

  isMark:(integer)=>boolean.
  isMark(0c-) => .true.
  isMark(0c_) => .true.
  isMark(0c.) => .true.
  isMark(0c!) => .true.
  isMark(0c~) => .true.
  isMark(0c*) => .true.
  isMark(0c\') => .true.
  isMark(0c\() => .true.
  isMark(0c\)) => .true.
  isMark(_) => .false.

  isUnreserved:(integer) => boolean.
  isUnreserved(Ch) => (isAlphaNum(Ch) || isMark(Ch)).

  isDelim:(integer)=>boolean.
  isDelim(0c<) => .true.
  isDelim(0c>) => .true.
  isDelim(0c#) => .true.
  isDelim(0c%) => .true.
  isDelim(0c\") => .true.
  isDelim(_) => .false.

  -- Implement equality for URIs
  public implementation equality[uri] => {
    U1 == U2 => sameUri(U1,U2).
  }

  sameUri(absUri(S1,R1,Q1),absUri(S2,R2,Q2)) => S1==S2 && sameRsrc(R1,R2) && sameQuery(Q1,Q2).
  sameUri(relUri(R1,Q1),relUri(R2,Q2)) => sameRsrc(R1,R2) && sameQuery(Q1,Q2).
  sameUri(_,_) => .false.

  sameRsrc(netRsrc(A1,P1),netRsrc(A2,P2)) => sameAuth(A1,A2) && samePath(P1,P2).
  sameRsrc(localRsrc(P1),localRsrc(P2)) => samePath(P1,P2).
  sameRsrc(_,_) => .false.

  samePath(absPath(P1),absPath(P2)) => P1==P2.
  samePath(relPath(P1),relPath(P2)) => P1==P2.
  samePath(_,_) => .false.

  sameAuth(server(U1,H1),server(U2,H2)) => sameUser(U1,U2) && sameHost(H1,H2).

  sameUser(some(user(U1)),some(user(U2))) => U1==U2.
  sameUser(.none,.none) => .true.
  sameUser(_,_) => .false.

  sameHost(hostPort(H1,P1),hostPort(H2,P2)) => H1==H2 && P1==P2.
  sameHost(host(H1),host(H2)) => H1==H2.
  sameHost(_,_) => .false.

  sameQuery(qry(S1),qry(S2)) => S1==S2.
  sameQuery(.noQ,.noQ) => .true.
  sameQuery(_,_) => .false.

  -- Resolve a url against a base. The base must be an absolute URI, either net or local.
  public resolveUri:(uri,uri) => option[uri].
  resolveUri(_,U) where absUri(_,_,_).=U => some(U).
  resolveUri(absUri(Scheme,Base,_),relUri(Path,Query)) where
    Pth ^= resolvePath(Base,Path) => some(absUri(Scheme,Pth,Query)).

  resolvePath:(rsrcName,rsrcName)=>option[rsrcName].
  resolvePath(_,netRsrc(A,P)) => some(netRsrc(A,P)).
  resolvePath(netRsrc(A,_),localRsrc(absPath(P))) => some(netRsrc(A,absPath(P))).
  resolvePath(netRsrc(A,absPath(B)),localRsrc(relPath(P))) where Dr ^= drop(reverse(B)) =>
    some(netRsrc(A,absPath(edit(P,Dr)))).
  resolvePath(localRsrc(_),localRsrc(absPath(P))) => some(localRsrc(absPath(P))).
  resolvePath(localRsrc(absPath(B)),localRsrc(relPath(P))) where Dr ^= drop(reverse(B)) => some(localRsrc(absPath(edit(P,Dr)))).

  edit: (cons[string],cons[string]) => cons[string].
  edit([".",..Segs],R) => edit(Segs,R).
  edit(["..",..Segs],[_,..R]) => edit(Segs,R).
  edit(Segs,R) => reverse(R)++Segs.

  drop:all t ~~ (cons[t])=>option[cons[t]].
  drop([_,..L])=>some(L).
  drop(_) default => .none.

  public implementation display[uri] => {
    disp(absUri(Scheme,Rsrc,Query)) => ssSeq([ss(Scheme),ss(":"),dispRsrc(Rsrc),dispQuery(Query)]).
    disp(relUri(Rsrc,Query)) => ssSeq([dispRsrc(Rsrc),dispQuery(Query)]).

    private dispQuery:(query)=>ss.
    dispQuery(.noQ) => ssSeq([]).
    dispQuery(qry(Q)) => ssSeq([ss("?"),ss(Q)]).
  }

  dispRsrc:(rsrcName)=>ss.
  dispRsrc(netRsrc(H,P)) => ssSeq([dispAuthority(H),dispPath(P)]).
  dispRsrc(localRsrc(P)) => dispPath(P).

  dispAuthority:(authority)=>ss.
  dispAuthority(server(.none,H)) => dispHost(H).
  dispAuthority(server(some(U),H)) => ssSeq([dispUser(U),ss("@"),dispHost(H)]).

  dispUser:(userInfo)=>ss.
  dispUser(user(U)) => ss(U).

  dispHost:(host) => ss.
  dispHost(hostPort(H,P)) => ssSeq([ss(H),ss(":"),ss(P)]).
  dispHost(host(H)) => ss(H).

  dispPath:(resourcePath)=>ss.
  dispPath(absPath(Segs)) => ssSeq([ss("/"),..dispSegs(Segs)]).
  dispPath(relPath(Segs)) => ssSeq(dispSegs(Segs)).

  dispSegs:(cons[string]) => cons[ss].
  dispSegs([]) => [].
  dispSegs([S]) => [ss(S)].
  dispSegs([S,..M]) => [ss(S),ss("/"),..dispSegs(M)].

  public
  getUriPath:(uri)=>string.
  getUriPath(absUri(_,Pth,_)) => dispRsrc(Pth)::string.
  getUriPath(relUri(Pth,_)) => dispRsrc(Pth)::string.

  public implementation coercion[uri,string] => {.
    _coerce(U) => disp(U):?string.
  .}

  public implementation coercion[string,uri] => {.
    _coerce(S) => parseUri(S).
  .}

  public editUriPath:(uri,(cons[string])=>option[cons[string]])=>option[uri].
  editUriPath(absUri(Scheme,ResNam,Qury),F) where NRes^=editUriResource(ResNam,F) => some(absUri(Scheme,NRes,Qury)).
  editUriPath(relUri(ResNam,Qury),F) where NRes^=editUriResource(ResNam,F) => some(relUri(NRes,Qury)).
  editUriPath(_,_) default => .none.

  editUriResource:(rsrcName,(cons[string])=>option[cons[string]])=>option[rsrcName].
  editUriResource(netRsrc(Auth,Path),F) where NPath^=editPath(Path,F) => some(netRsrc(Auth,NPath)).
  editUriResource(localRsrc(Path),F) where NPath^=editPath(Path,F) => some(localRsrc(NPath)).
  editUriResource(_,_) default => .none.

  editPath:(resourcePath,(cons[string])=>option[cons[string]])=>option[resourcePath].
  editPath(absPath(Els),F) where NEls^=F(Els) => some(absPath(NEls)).
  editPath(relPath(Els),F) where NEls^=F(Els) => some(relPath(NEls)).
  editPath(_,_) default => .none.
}
