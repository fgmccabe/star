star.uri.grammar{
  import star.
  import star.uri.

  uriParse >> U --> absoluteUri >> U.
  uriParse >> U --> relativeUri >> U.

  absoluteUri >> .absUri(Scheme,Hier,Q) --> scheme >> Scheme, hierPart >> Hier, query >> Q.

  relativeUri >> .relUri(Pth,Q) --> rsrcPath >> Pth, query >> Q.

  hierPart >> P --> netPath >> P.
  hierPart >> P --> absoluteRsrc >> P.

  netPath >> .netRsrc(A,P) --> [`/`, `/`], authority>>A, path>>P.

  authority >> .server(some(U),H) --> userInfo>>U, [`@`], hostNamePort >> H.
  authority >> .server(.none,H) --> hostNamePort >> H.

  hostNamePort >> .hostPort(H,P) --> hostName>>H, [`:`], port>>P.
  hostNamePort >> .host(H) --> hostName>>H.

  hostName >> H::string --> alphaDash* >> H.

  alphaDash >> A --> [A], {isAlphaDash(A)}.

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
  

}

  
