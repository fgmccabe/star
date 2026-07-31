rdf.triple{
  import star.
  import star.uri.
  import star.location.

  public concept ::= .named(string,string)
  | .anon(integer)
  | .existential(concept,set[triple])
  | .uri(string)
  | .text(cons[markup])
  | .langText(cons[markup],string)
  | .typedText(cons[markup],concept)
  | .int(integer)
  | .flt(float)
  | .bool(boolean)
  | .tripleTerm(triple).

  public isSymbolicConcept:(concept)=>boolean.
  isSymbolicConcept(.named(_,_)) => .true.
  isSymbolicConcept(.uri(_)) => .true.
  isSymbolicConcept(.anon(_)) => .true.
  isSymbolicConcept(_) default => .false.

  public markup ::= .str(string) | .link(concept,string).

  public implementation equality[concept] => let{.
    eqConcept(.named(P1,S1),.named(P2,S2)) => P1==P2 && S1==S2.
    eqConcept(.anon(U1),.anon(U2)) => U1==U2.
    eqConcept(.existential(U1,_),.existential(U2,_)) => eqConcept(U1,U2).
    eqConcept(.uri(U1),.uri(U2)) => U1==U2.
    eqConcept(.text(S1),.text(S2)) => eqConcepts(S1,S2).
    eqConcept(.langText(S1,L1),.langText(S2,L2)) => L1==L2 && eqConcepts(S1,S2).
    eqConcept(.typedText(S1,D1),.typedText(S2,D2)) => eqConcept(D1,D2) && eqConcepts(S1,S2).
    eqConcept(.int(I1),.int(I2)) => I1==I2.
    eqConcept(.flt(I1),.flt(I2)) => I1==I2.
    eqConcept(.bool(B1),.bool(B2)) => B1==B2.
    eqConcept(.tripleTerm(T1),.tripleTerm(T2)) => T1==T2.
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
    disp(.anon(Ix)) => "_$(Ix)".
    disp(.named(P,S)) => "#(P)\:#(S)".
    disp(.existential(Ix,Ts)) => "[$(Ts)]".
    disp(.uri(U)) => "<$(U)>".
    disp(.text(S)) => "\"#(interleave(S//disp,";")*)\"".
    disp(.langText(S,L)) => "\"#(interleave(S//disp,";")*)\"@$(L)".
    disp(.typedText(S,D)) => "\"#(interleave(S//disp,";")*)\"^^$(D)".
    disp(.int(N)) => disp(N).
    disp(.flt(N)) => disp(N).
    disp(.bool(B)) => disp(B).
    disp(.tripleTerm(T)) => "<<($(T))>>".
  }

  public implementation display[markup] => {
    disp(.str(S)) => S.
    disp(.link(C,"")) => "\$($(C))".
    disp(.link(C,Fmt)) => "\$($(C)):#(Fmt);".
  }

  public implementation hashable[concept] => {
    hash(.named(P,S)) => ((hash(P)*37)+hash(S)*37)+hash("name").
    hash(.anon(Ix)) => hash(Ix)*37+hash("_").
    hash(.existential(.anon(Ix),_)) => hash(Ix)*37+hash("_").
    hash(.uri(U)) => hash(U)*37+hash("U").
    hash(.text(S)) => hash(S)*37+hash("S").
    hash(.langText(S,L)) => hash(S)*37+hash(L).
    hash(.typedText(S,D)) => hash(S)*37+hash(D).
    hash(.int(I)) => hash(I).
    hash(.flt(I)) => hash(I).
    hash(.bool(.true)) => hash("bool.true").
    hash(.bool(.false)) => hash("bool.false").
    hash(.tripleTerm(T)) => hash(T)*37+hash("TT").
  }

  Counter = ref -1.
  public genAnon:()=>concept.
  genAnon() => valof{
    Counter := Counter! +1;
    valis .anon(Counter!)
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
  
