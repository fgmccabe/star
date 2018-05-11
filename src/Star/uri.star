star.uri{

  -- Utilities to help parse and manipulate URIs
  -- This code is an attempt to directly implement the specification in RFC 2396

  import star.
  import star.combo.

  public uri ::= absUri(string,rsrcName,query) | relUri(rsrcName,query).
  public rsrcName ::= netRsrc(authority,resourcePath) | localRsrc(resourcePath).
  public resourcePath ::= absPath(list[string]) | relPath(list[string]).

  public authority ::= server(option[userInfo],host).

  public userInfo ::= user(string).

  public host ::= hostPort(string,string) | host(string).

  public query ::= qry(string) | noQ.

  public parseUri:(string) => option[uri].
  parseUri(S) => uriParse(S::list[integer]).

  public uriParse:(list[integer]) => option[uri].
  uriParse(S) => alt(absoluteUri,relativeUri)(S).

  absoluteUri:(list[integer])=>option[uri].
  absoluteUri(S) =>
    scheme(S) >>= ((S0,Scheme))=>hierPart(S0)
              >>= (([],(Hier,Query)))=>some(absUri(Scheme,Hier,Query)).

  scheme:(list[integer]) => option[(list[integer],string)].
  scheme(S) => alpha(S)
                >>= ((S0,A)) => iter(S0,alphaStar,((Ch,Nm))=>[Nm..,Ch],[A]:list[integer])
                >>= ((S1, Schm)) => term(isK(0c:))(S1)
                >>= ((Sx,_)) => some((Sx,Schm::string)).

  hierPart:(list[integer]) => option[(list[integer],rsrcName)].
  hierPart(S) =>
    alt(netPath,absoluteRsrc)(S)
            >>= ((S0,N)) => query(S0)
            >>= ((Sx,Q)) => some((Sx,(N,Q))).

  relativeUri:(list[integer])=>option[uri].
  relativeUri(S) => alt(netPath,alt(absoluteRsrc,relativeRsrc))(S)
      >>= ((S0,P)) => query(S0)
      >>= ((Sx,Q)) => some((Sx,relUri(P,Q))).

  absoluteRsrc:(list[integer]) => option[(list[integer],resourcePath)].
  absoluteRsrc([0c/,..S]) =>
    pathSegments(S) >>=
      ((Sx,Segs)) => some((Sx,absPath(Segs))).
  absoluteRsrc(_) => none.

  relativeRsrc:(list[integer]) => option[(list[integer],resourcePath)].
  relativeRsrc(S) =>
    pathSegments(S) >>=
      ((Sx,Segs)) => some((Sx,relPath(Segs))).

  pathSegments:(list[integer]) => option[(list[integer],list[string])].
  pathSegments(S) => segment(S)
      >>= ((S0,Seg)) => iter(S0,
          (SS)=> term(isK(0c/)) >>= ((SS1,_))=>segment(SS1),
          (Sg,Sgs)=>[Sgs..,Sg],
          [Seg]).

  segment:(list[integer]) => option[(list[integer],string)].
  segment(S) =>
    pChars(S) >>= ((S1,Chrs)) => parameters(S1,Chrs)
              >>= ((Sx,P)) => some((Sx,P::string)).


  pChars:(list[integer]) => option[(list[integer],list[integer])].
  pChars(S) => iter(S,pChar,(Ch,St)=>[St..,Ch],[]).

  pChar:(list[integer])=>option[(list[integer],integer)].
  pChar([0c:,..S]) => some((S,0c:)).
  pChar([0c@,..S]) => some((S,0c@)).
  pChar([0c&,..S]) => some((S,0c&)).
  pChar([0c=,..S]) => some((S,0c=)).
  pChar([0c+,..S]) => some((S,0c+)).
  pChar([0c$,..S]) => some((S,0c$)).
  pChar([0c<,..S]) => some((S,0c<)).
  pChar([0c:,..S]) => some((S,0c:)).
  pChar([0c:,..S]) => some((S,0c:)).


  parameters:all s ~~ stream[s->>integer] |: (list[integer]) --> s.
  parameters(P) --> parameter(P,S), parameters(S).
  parameters([]) --> [].

  parameter:all s ~~ stream[s->>integer] |: (list[integer],list[integer]) --> s.
  parameter([0c;,..P],M) --> ";", pChars(P,M).

  query:(list[integer]) => option[(list[integer],query)].
  query([0c?,..S]) => iter(S,uric,(Ch,Q)=>[Q..,Ch],[]:list[integer])
      >>= ((Sx,QQ)) => some((Sx,qry(QQ::string))).
  query(S) => some((S,noQ)).

  netPath:(list[integer]) => option[(list[integer],rsrcName)].
  netPath([0c/,0c/,..S]) => authority(S)
        >>= ((S0,A)) => optAbsolutePath(S0)
        >>= ((S1,P)) => some((S1,netRsrc(A,P))).
  netPath(_) => none.

  authority:(list[integer]) => option[(list[integer],authority)].
  authority(server(some(user(implode(U))),H)) --> userInfo(U), "@", hostNamePort(H).
  authority(server(none,H)) --> hostNamePort(H).

  userInfo:all s ~~ stream[s->>integer] |: (list[integer]) --> s.
  userInfo(U) --> userStar(U).

  userStar:all s ~~ stream[s->>integer] |: (list[integer]) --> s.
  userStar([C,..S]) --> unreserved(C), userStar(S).
  userStar([0c$,..S]) --> "$", userStar(S).
  userStar([0c,,..S]) --> ",", userStar(S).
  userStar([0c;,..S]) --> ";", userStar(S).
  userStar([0c:,..S]) --> ":", userStar(S).
  userStar([0c&,..S]) --> "&", userStar(S).
  userStar([0c=,..S]) --> "=", userStar(S).
  userStar([0c+,..S]) --> "+", userStar(S).
  userStar([]) --> "@"+.

  hostNamePort:all s ~~ stream[s->>integer] |: (host) --> s.
  hostNamePort(hostPort(H,P)) --> hostName(H), ":", port(P).
  hostNamePort(host(H)) --> hostName(H), \+":".

  hostName:all s ~~ stream[s->>integer] |: (string) --> s.
  hostName(implode(H)) --> alphaDashStar(H)!.

  alphaDashStar:all s ~~ stream[s->>integer] |: (list[integer]) --> s.
  alphaDashStar([C,..S]) --> (alpha(C) | digit(C) | minus(C) | dot(C)) , alphaDashStar(S).
  alphaDashStar([]) --> [].

  port:all s ~~ stream[s->>integer] |: (string) --> s.
  port(implode(P)) --> digits(P).

  optAbsolutePath:all s ~~ stream[s->>integer] |: (resourcePath) --> s.
  optAbsolutePath(P) --> absoluteRsrc(P).
  optAbsolutePath(relPath([])) --> "?"+.
  optAbsolutePath(relPath([])) --> eof.

  plus:(list[integer]) => option[(list[integer],integer].
  plus(S) => term(isK(0c+))(S).

  minus:(list[integer]) => option[(list[integer],integer].
  minus(S) => term(isK(0c-))(S).

  dot:(list[integer]) => option[(list[integer],integer].
  dot(S) => term(isK(0c.))(S).

  alpha:(list[integer]) => option[(list[integer],integer)].
  alpha([A,..L]) where isLowAlpha(A) || isUpAlpha(A) => some((L,A)).
  alpha(_) => none.

  alphaNum:(list[integer]) => option[(list[integer],integer)].
  alphaNum(S) => alt(alpha,digit)(S).

  alphaStar:(list[integer]) => option[(list[integer],integer)].
  alphaStar(S) => alt(alpha,alt(digit,alt(plus,alt(minus,dot))))(S).

  digit:(list[integer]) => option[(list[integer],integer)].
  digit([D,..S]) where isDigit(D) => some((S,D)).
  digit(_) => none.

  digits:(list[integer]) => option[(list[integer],list[integer])].
  digits(S) => iter(S,term(isDigit),(D,Ds)=>[Ds..,D],[]).

  hex:(list[integer]) => option[(list[integer],integer)].
  hex([D,..S]) where isDigit(D) => some((S,D)).
  hex([D,..S]) where isHexDigit(D) => some((S,D)).
  hex(_) => none.

  uric:(list[integer]) => option[(list[integer],integer)].
  uric(S) => alt(reserved,unreserved)(S).

  reserved:(list[integer]) => option[(list[integer],integer)].
  reserved([C,..S]) where isReserved(C) => some((S,C)).
  reserved(_) => none.

  unreserved:(list[integer]) => option[(list[integer],integer)].
  unreserved = alt(alphaNum,mark).

  mark:(list[integer]) => option[(list[integer],integer)].
  mark([C,..L]) where isMark(C) => some((L,C)).
  mark(_) => none.

  delim:(list[integer]) => option[(list[integer],integer)].
  delim([C,..L]) where isDelim(C) => some((L,C)).
  delim(_) => none.

  isLowAlpha:(integer) => boolean.
  isLowAlpha(0ca) => true.
  isLowAlpha(0cb) => true.
  isLowAlpha(0cc) => true.
  isLowAlpha(0cd) => true.
  isLowAlpha(0ce) => true.
  isLowAlpha(0cf) => true.
  isLowAlpha(0cg) => true.
  isLowAlpha(0ch) => true.
  isLowAlpha(0ci) => true.
  isLowAlpha(0cj) => true.
  isLowAlpha(0ck) => true.
  isLowAlpha(0cl) => true.
  isLowAlpha(0cm) => true.
  isLowAlpha(0cn) => true.
  isLowAlpha(0co) => true.
  isLowAlpha(0cp) => true.
  isLowAlpha(0cq) => true.
  isLowAlpha(0cr) => true.
  isLowAlpha(0cs) => true.
  isLowAlpha(0ct) => true.
  isLowAlpha(0cu) => true.
  isLowAlpha(0cv) => true.
  isLowAlpha(0cw) => true.
  isLowAlpha(0cx) => true.
  isLowAlpha(0cy) => true.
  isLowAlpha(0cz) => true.
  isLowAlpha(_) => false.

  isUpAlpha:(integer)=>boolean.
  isUpAlpha(0cA) => true.
  isUpAlpha(0cB) => true.
  isUpAlpha(0cC) => true.
  isUpAlpha(0cD) => true.
  isUpAlpha(0cE) => true.
  isUpAlpha(0cF) => true.
  isUpAlpha(0cG) => true.
  isUpAlpha(0cH) => true.
  isUpAlpha(0cI) => true.
  isUpAlpha(0cJ) => true.
  isUpAlpha(0cK) => true.
  isUpAlpha(0cL) => true.
  isUpAlpha(0cM) => true.
  isUpAlpha(0cN) => true.
  isUpAlpha(0cO) => true.
  isUpAlpha(0cP) => true.
  isUpAlpha(0cQ) => true.
  isUpAlpha(0cR) => true.
  isUpAlpha(0cS) => true.
  isUpAlpha(0cT) => true.
  isUpAlpha(0cU) => true.
  isUpAlpha(0cV) => true.
  isUpAlpha(0cW) => true.
  isUpAlpha(0cX) => true.
  isUpAlpha(0cY) => true.
  isUpAlpha(0cZ) => true.
  isUpAlpha(_) => false.

  isDigit:(integer)=>boolean.
  isDigit(0c0) => true.
  isDigit(0c1) => true.
  isDigit(0c2) => true.
  isDigit(0c3) => true.
  isDigit(0c4) => true.
  isDigit(0c5) => true.
  isDigit(0c6) => true.
  isDigit(0c7) => true.
  isDigit(0c8) => true.
  isDigit(0c9) => true.
  isDigit(_) => false.

  isHexDigit:(integer)=>boolean.
  isHexDigit(0ca) => true.
  isHexDigit(0cb) => true.
  isHexDigit(0cc) => true.
  isHexDigit(0cd) => true.
  isHexDigit(0ce) => true.
  isHexDigit(0cf) => true.
  isHexDigit(0cA) => true.
  isHexDigit(0cB) => true.
  isHexDigit(0cC) => true.
  isHexDigit(0cD) => true.
  isHexDigit(0cE) => true.
  isHexDigit(0cF) => true.
  isHexDigit(_) => false.

  isReserved:(integer)=>boolean.
  isReserved(0c;) => true.
  isReserved(0c/) => true.
  isReserved(0c?) => true.
  isReserved(0c:) => true.
  isReserved(0c@) => true.
  isReserved(0c&) => true.
  isReserved(0c=) => true.
  isReserved(0c+) => true.
  isReserved(0c$) => true.
  isReserved(0c,) => true.
  isReserved(_) => false.

  isMark:(integer)=>boolean.
  isMark(0c-) => true.
  isMark(0c_) => true.
  isMark(0c.) => true.
  isMark(0c!) => true.
  isMark(0c~) => true.
  isMark(0c*) => true.
  isMark(0c') => true.
  isMark(0c() => true.
  isMark(0c)) => true.
  isMark(_) => false.

  isDelim:(integer)=>boolean.
  isDelim(0c<) => true.
  isDelim(0c>) => true.
  isDelim(0c#) => true.
  isDelim(0c%) => true.
  isDelim(0c") => true.
  isDelim(_) => false.

  -- Resolve a url against a base. The base must be an absolute URI, either net or local.
  public
  resolveUri:(uri,uri) => uri.
  resolveUri(_,U) where U=.absUri(_,_,_) => U.
  resolveUri(absUri(Scheme,Base,_),relUri(Path,Query)) => absUri(Scheme,resolvePath(Base,Path),Query).

  resolvePath:(rsrcName,rsrcName)=>rsrcName.
  resolvePath(_,netRsrc(A,P)) => netRsrc(A,P).
  resolvePath(netRsrc(A,_),localRsrc(absPath(P))) => netRsrc(A,absPath(P)).
  resolvePath(netRsrc(A,absPath(B)),localRsrc(relPath(P))) => netRsrc(A,absPath(edit(P,drop(reverse(B))))).
  resolvePath(localRsrc(_),localRsrc(absPath(P))) => localRsrc(absPath(P)).
  resolvePath(localRsrc(absPath(B)),localRsrc(relPath(P))) => localRsrc(absPath(edit(P,drop(reverse(B))))).

  edit: (list[string],list[string]) => list[string].
  edit([".",..Segs],R) => edit(Segs,R).
  edit(["..",..Segs],[_,..R]) => edit(Segs,R).
  edit(Segs,R) => reverse(R)++Segs.

  public implementation display[uri] => {
    disp(absUri(Scheme,Rsrc,Query)) => ssSeq([ss(Scheme),ss(":"),dispRsrc(Rsrc),dispQuery(Query)]).
    disp(relUri(Rsrc,Query)) => ssSeq([dispRsrc(Rsrc),dispQuery(Query)]).

    private dispQuery:(query)=>ss.
    dispQuery(noQ) => ssSeq([]).
    dispQuery(qry(Q)) => ssSeq([ss("?"),ss(Q)]).
  }

  dispRsrc:(rsrcName)=>ss.
  dispRsrc(netRsrc(H,P)) => ssSeq([dispAuthority(H),dispPath(P)]).
  dispRsrc(localRsrc(P)) => dispPath(P).

  dispAuthority:(authority)=>ss.
  dispAuthority(server(none,H)) => dispHost(H).
  dispAuthority(server(some(U),H)) => ssSeq([dispUser(U),ss("@"),dispHost(H)]).

  dispUser:(userInfo)=>ss.
  dispUser(user(U)) => ss(U).

  dispHost:(host) => ss.
  dispHost(hostPort(H,P)) => ssSeq([ss(H),ss(":"),ss(P)]).
  dispHost(host(H)) => ss(H).

  dispPath:(resourcePath)=>ss.
  dispPath(absPath(Segs)) => ssSeq([ss("/"),..dispSegs(Segs)]).
  dispPath(relPath(Segs)) => ssSeq(dispSegs(Segs)).

  dispSegs:(list[string]) => list[ss].
  dispSegs([]) => [].
  dispSegs([S]) => [ss(S)].
  dispSegs([S,..M]) => [ss(S),ss("/"),..dispSegs(M)].

  public
  getUriPath:(uri)=>string.
  getUriPath(absUri(_,Pth,_)) => formatSS(dispRsrc(Pth)).
  getUriPath(relUri(Pth,_)) => formatSS(dispRsrc(Pth)).

  public implementation coercion[uri,string] => {
    _coerce(U) => formatSS(disp(U)).
  }
}
