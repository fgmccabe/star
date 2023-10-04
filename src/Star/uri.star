star.uri{
  -- Utilities to help parse and manipulate URIs
  -- This code is an attempt to directly implement the specification in RFC 2396

  import star.

  public uri ::= .absUri(string,rsrcName,query) | .relUri(rsrcName,query).
  public rsrcName ::= .netRsrc(authority,resourcePath) | .localRsrc(resourcePath).
  public resourcePath ::= .absPath(cons[string]) | .relPath(cons[string]).

  public authority ::= .server(option[userInfo],host).

  public userInfo ::= .user(string).

  public host ::= .hostPort(string,string) | .host(string).

  public query ::= .qry(string) | .noQ.

  public parseUri:(string) => option[uri].
  parseUri(S) where (U,[]) ?= parseU(S::cons[char]) => .some(U).
  parseUri(_) default => .none.

  public parseU:() >> uri --> cons[char].
  parseU >> U --> absoluteUri >> U.
  parseU >> U --> relativeUri >> U.

  absoluteUri >> .absUri(Scheme,Hier,Q) --> scheme >> Scheme, hierPart >> Hier, query >> Q.

  scheme >> [F,..R]::string --> alphaNum >> F, alphaStar* >> R, [`:`].

  relativeUri >> .relUri(Pth,Q) --> (netPath | absoluteRsrc | relativeRsrc) >> Pth, query >> Q.

  hierPart >> P --> netPath >> P.
  hierPart >> P --> absoluteRsrc >> P.

  netPath >> .netRsrc(A,P) --> [`/`, `/`], authority>>A, path>>P.

  absoluteRsrc >> .localRsrc(P) --> absolutePath >> P.
  relativeRsrc >> .localRsrc(P) --> relativePath >> P.

  path >> P --> absolutePath >> P.
  path >> P --> relativePath >> P.

  absolutePath >> .absPath(P) --> [`/`], segment * [`/`] >> P.

  relativePath >> .relPath(P) --> segment * [`/`] >> P.

  authority >> .server(.some(U),H) --> userInfo>>U, [`@`], hostNamePort >> H.
  authority >> .server(.none,H) --> hostNamePort >> H.

  userInfo >> .user(U::string) --> userChar* >>U.

  userChar >> C --> [C], {userCh(C)}.

  hostNamePort >> .hostPort(H,P) --> hostName>>H, [`:`], port>>P.
  hostNamePort >> .host(H) --> hostName>>H.

  hostName >> H::string --> alphaDash* >> H.

  port >> P::string --> digit* >> P.

  segment >> S::string --> segChar * >> S.

  query >> .qry(Q::string) --> [`?`], uriChar* >> Q.
  query default >> .noQ --> [].

  segChar >> C --> [C], {isSegChr(C)}.

  alphaNum >> A --> [A], {isAlphaNum(A)}.

  alphaStar >> A --> [A], {isAlphaStar(A)}.

  alphaDash >> A --> [A], {isAlphaDash(A)}.

  uriChar >> Q --> [Q], {isUric(Q)}.

  digit >> D --> [D], {isDigit(D)}.
  
  isAlphaStar:(char)=>boolean.
  isAlphaStar(Ch) => (isAlphaNum(Ch) || isPlus(Ch) || isMinus(Ch) || isDot(Ch)).

  isAlphaDash:(char)=>boolean.
  isAlphaDash(Ch) => (isAlphaNum(Ch) || isMinus(Ch) || isDot(Ch)).

  isMinus:(char)=>boolean.
  isMinus(Ch) => Ch==`-`.

  isPlus:(char)=>boolean.
  isPlus(Ch)=>Ch==`+`.

  isDot:(char)=>boolean.
  isDot(Ch)=>Ch==`.`.

  isUric:(char)=>boolean.
  isUric(Ch) => (isReserved(Ch) || isAlphaNum(Ch) || isMark(Ch)).

  isReserved:(char)=>boolean.
  isReserved(Ch) => case Ch in {
    `;` => .true.
    `/` => .true.
    `?` => .true.
    `:` => .true.
    `@` => .true.
    `&` => .true.
    `=` => .true.
    `+` => .true.
    `$` => .true.
    `,` => .true.
    _ default => .false.
  }
  
  isMark:(char)=>boolean.
  isMark(Ch) => case Ch in {
    `-` => .true.
    `_` => .true.
    `.` => .true.
    `!` => .true.
    `~` => .true.
    `*` => .true.
    `\'` => .true.
    `\(` => .true.
    `\)` => .true.
    _ default => .false.
  }

  isUnreserved:(char) => boolean.
  isUnreserved(Ch) => (isAlphaNum(Ch) || isMark(Ch)).

  isDelim:(char)=>boolean.
  isDelim(Ch) => case Ch in {
    `<` => .true.
    `>` => .true.
    `#` => .true.
    `%` => .true.
    `\"` => .true.
    _ default => .false.
  }

  isSegChr:(char)=>boolean.
  isSegChr(Ch) => case Ch in {
    `:` => .true.
    `@` => .true.
    `&` => .true.
    `=` => .true.
    `+` => .true.
    `$` => .true.
    `<` => .true.
    `;` => .true. -- This is a hack to merge parameters with the segment
    _ default => isUnreserved(Ch).
  }

  userCh:(char) => boolean.
  userCh(Ch) => case Ch in {
    `$` => .true.
    `,` => .true.
    `;` => .true.
    `:` => .true.
    `&` => .true.
    `=` => .true.
    `+` => .true.
    _ default => isUnreserved(Ch).
  }

  -- Implement equality for URIs
  public implementation equality[uri] => {
    U1 == U2 => sameUri(U1,U2).
  }

  sameUri(.absUri(S1,R1,Q1),.absUri(S2,R2,Q2)) => S1==S2 && sameRsrc(R1,R2) && sameQuery(Q1,Q2).
  sameUri(.relUri(R1,Q1),.relUri(R2,Q2)) => sameRsrc(R1,R2) && sameQuery(Q1,Q2).
  sameUri(_,_) => .false.

  sameRsrc(.netRsrc(A1,P1),.netRsrc(A2,P2)) => sameAuth(A1,A2) && samePath(P1,P2).
  sameRsrc(.localRsrc(P1),.localRsrc(P2)) => samePath(P1,P2).
  sameRsrc(_,_) => .false.

  samePath(.absPath(P1),.absPath(P2)) => P1==P2.
  samePath(.relPath(P1),.relPath(P2)) => P1==P2.
  samePath(_,_) => .false.

  sameAuth(.server(U1,H1),.server(U2,H2)) => sameUser(U1,U2) && sameHost(H1,H2).

  sameUser(.some(.user(U1)),.some(.user(U2))) => U1==U2.
  sameUser(.none,.none) => .true.
  sameUser(_,_) => .false.

  sameHost(.hostPort(H1,P1),.hostPort(H2,P2)) => H1==H2 && P1==P2.
  sameHost(.host(H1),.host(H2)) => H1==H2.
  sameHost(_,_) => .false.

  sameQuery(.qry(S1),.qry(S2)) => S1==S2.
  sameQuery(.noQ,.noQ) => .true.
  sameQuery(_,_) => .false.


  public implementation hashable[uri] => {
    hash(.absUri(S1,R1,Q1)) => (hash(S1)*37+hash(R1))*37+hash(Q1).
    hash(.relUri(R1,Q1)) => hash(R1)*37+hash(Q1).
  }

  implementation hashable[rsrcName] => {
    hash(.netRsrc(A,R)) => hash(A)*37+hash(R).
    hash(.localRsrc(P)) => hash(P)
  }

  implementation hashable[resourcePath] => {
    hash(.absPath(Els)) => (hash(Els)*37+hash("abs")).
    hash(.relPath(Els)) => (hash(Els)*37+hash("rel")).
  }

  implementation hashable[authority] => {
    hash(.server(S,H)) => hash(S)*37+hash(H)
  }

  implementation hashable[userInfo] => {
    hash(.user(Txt)) => hash(Txt)
  }

  implementation hashable[host] => {
    hash(.hostPort(H,P)) => hash(H)*37+hash(P).
    hash(.host(H)) => hash(H)
  }

  implementation hashable[query] => {
    hash(.qry(Txt)) => hash(Txt).
    hash(.noQ) => 0.
  }

  -- Resolve a url against a base. The base must be an absolute URI, either net or local.
  public resolveUri:(uri,uri) => option[uri].
  resolveUri(_,U) where .absUri(_,_,_).=U => .some(U).
  resolveUri(.absUri(Scheme,Base,_),.relUri(Path,Query)) where
    Pth ?= resolvePath(Base,Path) => .some(.absUri(Scheme,Pth,Query)).

  resolvePath:(rsrcName,rsrcName)=>option[rsrcName].
  resolvePath(_,.netRsrc(A,P)) => .some(.netRsrc(A,P)).
  resolvePath(.netRsrc(A,_),.localRsrc(.absPath(P))) => .some(.netRsrc(A,.absPath(P))).
  resolvePath(.netRsrc(A,.absPath(B)),.localRsrc(.relPath(P))) where Dr ?= drop(reverse(B)) =>
    .some(.netRsrc(A,.absPath(edit(P,Dr)))).
  resolvePath(.localRsrc(_),.localRsrc(.absPath(P))) => .some(.localRsrc(.absPath(P))).
  resolvePath(.localRsrc(.absPath(B)),.localRsrc(.relPath(P))) where Dr ?= drop(reverse(B)) => .some(.localRsrc(.absPath(edit(P,Dr)))).

  edit: (cons[string],cons[string]) => cons[string].
  edit([".",..Segs],R) => edit(Segs,R).
  edit(["..",..Segs],[_,..R]) => edit(Segs,R).
  edit(Segs,R) => reverse(R)++Segs.

  drop:all t ~~ (cons[t])=>option[cons[t]].
  drop([_,..L])=>.some(L).
  drop(_) default => .none.

  public implementation display[uri] => {
  disp(.absUri(Scheme,Rsrc,Query)) => "#(Scheme)\:#(dispRsrc(Rsrc))#(dispQuery(Query))".
  disp(.relUri(Rsrc,Query)) => "#(dispRsrc(Rsrc))#(dispQuery(Query))".
  }

  private dispQuery:(query)=>string.
  dispQuery(.noQ) => "".
  dispQuery(.qry(Q)) => "?#(Q)".

  dispRsrc:(rsrcName)=>string.
  dispRsrc(.netRsrc(H,P)) => "#(dispAuthority(H))#(dispPath(P))".
  dispRsrc(.localRsrc(P)) => dispPath(P).

  dispAuthority:(authority)=>string.
  dispAuthority(.server(.none,H)) => dispHost(H).
  dispAuthority(.server(.some(U),H)) => "#(dispUser(U))@#(dispHost(H))".

  dispUser:(userInfo)=>string.
  dispUser(.user(U)) => U.

  dispHost:(host) => string.
  dispHost(.hostPort(H,P)) => "#(H)\:#(P)".
  dispHost(.host(H)) => H.

  dispPath:(resourcePath)=>string.
  dispPath(.absPath(Segs)) => "/#(dispSegs(Segs))".
  dispPath(.relPath(Segs)) => dispSegs(Segs).

  dispSegs:(cons[string]) => string.
  dispSegs([]) => "".
  dispSegs([S]) => S.
  dispSegs([S,..M]) => "#(S)/#(dispSegs(M))".

  public getUriPath:(uri)=>string.
  getUriPath(.absUri(_,Pth,_)) => dispRsrc(Pth)::string.
  getUriPath(.relUri(Pth,_)) => dispRsrc(Pth)::string.

  public implementation coercion[uri,string] => {
    _coerce(U) => .some(disp(U)).
  }

  public implementation coercion[string,uri] => {
    _coerce(S) => parseUri(S).
  }

  public editUriPath:(uri,(cons[string])=>option[cons[string]])=>option[uri].
  editUriPath(.absUri(Scheme,ResNam,Qury),F) where NRes?=editUriResource(ResNam,F) =>
    .some(.absUri(Scheme,NRes,Qury)).
  editUriPath(.relUri(ResNam,Qury),F) where NRes?=editUriResource(ResNam,F) =>
    .some(.relUri(NRes,Qury)).
  editUriPath(_,_) default => .none.

  editUriResource:(rsrcName,(cons[string])=>option[cons[string]])=>option[rsrcName].
  editUriResource(.netRsrc(Auth,Path),F) where NPath?=editPath(Path,F) => .some(.netRsrc(Auth,NPath)).
  editUriResource(.localRsrc(Path),F) where NPath?=editPath(Path,F) => .some(.localRsrc(NPath)).
  editUriResource(_,_) default => .none.

  editPath:(resourcePath,(cons[string])=>option[cons[string]])=>option[resourcePath].
  editPath(.absPath(Els),F) where NEls?=F(Els) => .some(.absPath(NEls)).
  editPath(.relPath(Els),F) where NEls?=F(Els) => .some(.relPath(NEls)).
  editPath(_,_) default => .none.
}
