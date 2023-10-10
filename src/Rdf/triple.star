rdf.triple{
  import star.
  import star.uri.
  import star.location.

  public concept ::= .named(string,string)
  | .uri(string)
  | .text(cons[markup])
  | .int(integer)
  | .flt(float).

  public markup ::= .str(string) | .link(concept,string).

  public implementation equality[concept] => let{.
    eqConcept(.named(P1,S1),.named(P2,S2)) => P1==P2 && S1==S2.
    eqConcept(.uri(U1),.uri(U2)) => U1==U2.
    eqConcept(.text(S1),.text(S2)) => eqConcepts(S1,S2).
    eqConcept(.int(I1),.int(I2)) => I1==I2.
    eqConcept(.flt(I1),.flt(I2)) => I1==I2.
    eqConcept(_,_) default => .false.

    eqConcepts([],[]) => .true.
    eqConcepts([.str(S1),..Ss1],[.str(S2),..Ss2]) =>
      S1==S2 && eqConcepts(Ss1,Ss2).
    eqConcepts(_,_) default => .false.
  .} in {
    X==Y => eqConcept(X,Y)
  }

  public implementation display[concept] => {
    disp(.named("",S)) => S.
    disp(.named(P,S)) => "#(P)\:#(S)".
    disp(.uri(U)) => "<$(U)>".
    disp(.text(S)) => "\"#(interleave(S//disp,";")*)\"".
    disp(.int(N)) => disp(N).
    disp(.flt(N)) => disp(N).
  }

  public implementation display[markup] => {
    disp(.str(S)) => S.
    disp(.link(C,"")) => "\$($(C))".
    disp(.link(C,Fmt)) => "\$($(C)):#(Fmt);".
  }

  public implementation hashable[concept] => {
    hash(.named(P,S)) => ((hash(P)*37)+hash(S)*37)+hash("name").
    hash(.uri(U)) => hash(U)*37+hash("U").
    hash(.text(S)) => hash(S)*37+hash("S").
    hash(.int(I)) => hash(I).
    hash(.flt(I)) => hash(I).
  }

  implementation hashable[markup] => {
    hash(.str(S)) => hash(S).
    hash(.link(L,F)) => hash(L)*37+hash(F)
  }

  public triple ::= .tr(concept,concept,concept).

  public implementation equality[triple] => {
    .tr(S1,P1,O1) == .tr(S2,P2,O2) => S1==S2 && P1==P2 && O1==O2.
  }

  public implementation display[triple] => {
    disp(.tr(S,P,O)) => "$(S) $(P) $(O)"
  }

  public implementation hashable[triple] => {
    hash(.tr(S,P,O)) => (hash(S)*37+hash(P))*37+hash(O)
  }

  public stmt ::= .macro(string,string) | .sent(triple).
}
  
