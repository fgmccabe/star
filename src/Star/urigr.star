star.uri.grammar{
  import star.
  import star.uri.

  public parseU:(cons[char]) => option[(uri,cons[char])].
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
  

}

  
