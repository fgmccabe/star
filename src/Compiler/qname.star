star.compiler.qname{
  import star.

  -- Qualified names have a specific path as a prefix.
  public pthSep ::= .pathSep | .typeSep | .valSep | .overSep | .pkgSep | .closSep | .cnsSep | .fldSep | .trctSep.

  public qName ::= .gId(cons[(pthSep,string)],string).

  implementation display[pthSep] => {
    disp(.pathSep) => ".".
    disp(.pkgSep) => "#".
    disp(.typeSep) => "*".
    disp(.valSep) => "@".
    disp(.cnsSep) => "%".
    disp(.closSep) => "^".
    disp(.trctSep) => "$".
    disp(.fldSep) => "°".
    disp(.overSep) => "!".
  }

  dispPath:(cons[(pthSep,string)]) => string.
  dispPath(Pth) => (Pth//((Sep,Segment))=>"#(Segment)$(Sep)")*.

  public implementation display[qName] => {
    disp(.gId(Pth,Sfx)) => "#(dispPath(Pth))#(Sfx)".
  }

  implementation equality[pthSep] => {
    .pathSep == .pathSep => .true.
    .typeSep == .typeSep => .true.
    .valSep == .valSep => .true.
    .overSep == .overSep => .true.
    .pkgSep == .pkgSep => .true.
    .closSep == .closSep => .true.
    .cnsSep == .cnsSep => .true.
    .fldSep == .fldSep => .true.
    .trctSep == .trctSep => .true.
    _ == _ default => .false
  }

  implementation hashable[pthSep] => {
    hash(.pathSep) => 1.
    hash(.typeSep) => 2.
    hash(.valSep) => 3.
    hash(.overSep) => 5.
    hash(.pkgSep) => 7.
    hash(.closSep) => 11.
    hash(.cnsSep) => 13.
    hash(.fldSep) => 17.
    hash(.trctSep) => 19.
  }

  public implementation equality[qName] => {
    .gId(P1,N1) == .gId(P2,N2) => N1==N2 && samePath(P1,P2)
  }

  samePath([],[]) => .true.
  samePath([(M1,F1),..P1],[(M2,F2),..P2]) =>
    M1==M2 && F1==F2 && samePath(P1,P2).

  pathHash:(cons[(pthSep,string)]) => integer.
  pathHash([]) => 0.
  pathHash([(S,F),..P]) => (pathHash(P)*37+hash(S))*37+hash(F).

  implementation hashable[qName] => {
    hash(.gId(P,S)) => pathHash(P)*37+hash(S)
  }

  public stdNm:(string) => qName.
  stdNm(Nm) => .gId([],Nm).

  localName(.gId(_,Nm)) => Nm.
}
