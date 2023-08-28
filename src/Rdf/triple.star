rdf.triple{
  import star.
  import star.uri.
  import star.location.

  public concept ::= .name(string)
  | .uri(uri)
  | .text(string)
  | .int(integer).

  public implementation equality[concept] => {
    .name(S1) == .name(S2) => S1==S2.
    .uri(U1) == .uri(U2) => U1==U2.
    .text(S1) == .text(S2) => S1==S2.
    .int(I1) == .int(I2) => I1==I2.
    _ == _ default => .false
  }

  public implementation display[concept] => {
    disp(.name(S)) => S.
    disp(.uri(U)) => "<$(U)>".
    disp(.text(S)) => disp(S).
    disp(.int(I)) => disp(I).
  }

  public implementation hashable[concept] => {
    hash(.name(S)) => hash(S)*37+hash("name").
    hash(.uri(U)) => hash(U)*37+hash("U").
    hash(.text(S)) => hash(S)*37+hash("S").
    hash(.int(I)) => hash(I).
  }

  public triple ::= .tr(concept,concept,concept).

  public implementation equality[triple] => {
    .tr(S1,P1,O1) == .tr(S2,P2,O2) => S1==S2 && P1==P2 && O1==O2.
  }

  public implementation display[triple] => {
    disp(.tr(S,P,O)) => "$(S) $(P) $(O)"
  }
}
  
