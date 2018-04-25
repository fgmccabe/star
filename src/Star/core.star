star.core {
  @"Core definitions of types and interfaces that are really part of the language".

  equality@"defines semantic equality".
  public contract all x ~~ equality[x] ::= {
    (==)@"semantic equality is defined explicitly".

    (==): (x,x)=>boolean.
  }

  (=!=)@"semantic inequality defined in terms of equality".
  public (=!=):all x ~~ equality[x] |: (x,x)=>boolean.
  x =!= y => \+ x==y.

  public contract all x ~~ comp[x] ::= {
    (<): (x,x)=>boolean.
    (>=): (x,x)=>boolean.
  }

  hash@"defines functions associated with hash encoding".
  public contract all x ~~ hash[x] ::= {
    hash:(x)=>integer.
  }

  public contract all c ~~ sizeable[c] ::= {
    size:(c) => integer.
    isEmpty:(c) => boolean.
  }

  -- stream contract
  public contract all S,E ~~ stream[S ->> E] ::= {
    _eof:() <= (S).
    _hdtl:(E,S) <= S.
    _cons:(E,S) => S.
    _apnd:(S,E) => S.
    _back:(S,E) <= S.
    _nil:S.
  }

  -- Structured string.
  public ss ::= ss(string) | sc(integer) | ssSeq(list[ss]).

  -- Displayable contract
  public contract all t ~~ display[t] ::= {
    disp:(t)=>ss.
  }

  -- Formatting contract
  public contract all t ~~ format[t] ::= {
    frmt:(t,string) => ss.
  }

  public implementation display[ss] => {
    disp(X) => X
  }
  
  public contract all k ~~ concat[k] ::= {
    (++) : (k,k)=>k.
  }

  -- Not strictly necessary, but makes for better symmetry.
  public boolean ::= true | false.

  option@"the option type is useful when a value is not always available".
  public all t ~~ option[t] ::= none | some(t).
}
