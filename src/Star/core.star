star.core {
  @"Core definitions of types and interfaces that are really part of the language".

  equality@"defines functions associated with semantic equality".
  public contract all x ~~ equality[x] ::= {
    (==)@"semantic equality is defined explicitly".

    (==): (x,x)=>boolean.

    hash@"hash is an essential part of semantic equality".
    hash:(x)=>integer.
  }

  (=!=)@"semantic inequality defined in terms of equality".
  public (=!=):all x ~~ equality[x] |: (x,x)=>boolean.
  x =!= y => \+ x==y.

  public contract all x ~~ comp[x] ::= {
    (<): (x,x)=>boolean.
    (>=): (x,x)=>boolean.
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

  -- Not strictly necessary, but makes for better symmetry.
  public boolean ::= true | false.
}
