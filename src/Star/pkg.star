star.pkg{
  import star.
  import star.parse.

  public pkg ::= pkg(string,version).

  public version ::= .defltVersion | vers(string).

  public pkgName:(pkg)=>string.
  pkgName(pkg(Nm,_))=>Nm.

  public pkgVersion:(pkg)=>version.
  pkgVersion(pkg(_,V))=>V.

  public implementation display[pkg] => {.
    disp(pkg(P,V)) => ssSeq([ss(P),ss(":"),disp(V)]).
  .}

  public implementation equality[pkg] => {.
    pkg(P1,V1) == pkg(P2,V2) => P1==P2 && V1==V2.
  .}

  public implementation display[version] => {.
    disp(.defltVersion) => ss("*").
    disp(vers(V)) => ss(V).
  .}

  public implementation equality[version] => {.
    .defltVersion == .defltVersion => .true.
    vers(V) == vers(W) => V==W.
    _ == _ => .false.
  .}

  public implementation hash[version] => {.
    hash(.defltVersion) => 0.
    hash(vers(V)) => hash(V).
  .}

  public implementation hash[pkg] => {.
    hash(pkg(P,V)) => hash(P)*37+hash(V).
  .}

  public implementation coercion[string,version] => {
    _coerce("*") => some(.defltVersion).
    _coerce(V) => some(vers(V)).
  }

  public implementation coercion[version,string] => {
    _coerce(.defltVersion) => some("*").
    _coerce(vers(V)) => some(V).
  }

  public compatiblePkg:(pkg,pkg)=>boolean.
  compatiblePkg(pkg(P,V1),pkg(P,V2)) => compatibleVersion(V1,V2).
  compatiblePkg(_,_) default => .false.

  public compatibleVersion:(version,version)=>boolean.
  compatibleVersion(.defltVersion,_)=>.true.
  compatibleVersion(_,.defltVersion)=>.true.
  compatibleVersion(vers(V1),vers(V2))=>V1==V2.
  compatibleVersion(_,_) => .false.

  public parsePkgName:(string) => option[pkg].
  parsePkgName(S) => first(parse(pkgParse,S::cons[integer])).

  first([])=>.none.
  first([(E,_),.._])=>some(E).

  public pkgParse:parser[cons[integer],pkg].
  pkgParse = parseName >>= (Pkg) => parseVersion >>= (V) => return pkg(Pkg,V).

  parseName:parser[cons[integer],string].
  parseName = segment.

  segment:parser[cons[integer],string].
  segment=_plus(segChr) >>= (Chrs) => return (Chrs::string).

  segChr:parser[cons[integer],integer].
  segChr = _sat(isSegChr).

  isSegChr:(integer)=>boolean.
  isSegChr(0c.) => .true.
  isSegChr(Ch) => isAlphaNum(Ch).

  parseVersion:parser[cons[integer],version].
  parseVersion = ((_str("#") >>= (_) => segment >>= (Seg) => (return vers(Seg))) +++ (return .defltVersion)).
}
