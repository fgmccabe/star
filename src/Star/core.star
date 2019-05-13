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

  public contract all s,k ~~ slice[s->>k] ::= {
    _slice : (s,k,k)=>s.
    _splice : (s,k,k,s)=>s.
  }
    
  public contract all c ~~ sizeable[c] ::= {
    size:(c) => integer.
    isEmpty:(c) => boolean.
  }

  -- The head'n tail contract
  public contract all c,e ~~ head[c->>e] ::= {
    head:(c)=>option[e].
    tail:(c)=>option[c].
  }

  public contract all c,e ~~ back[c->>e] ::= {
    last:(c) => option[e].
    lead:(c) => option[c].
  }

  -- stream contract
  public contract all S,E ~~ stream[S ->> E] ::= {
    _eof:(S) => boolean.
    _hdtl:(S) => option[(E,S)].
    _back:(S) => option[(S,E)].
  }

  -- The sequence contract
  public contract all S,E ~~ sequence[S->>E] ::= {
    _cons:(E,S) => S.
    _apnd:(S,E) => S.
    _nil:S.
  }

  -- Structured string.
  public ss ::= ss(string) | sc(integer) | ssPr(ss,ss) | ssSeq(list[ss]).

  -- Displayable contract
  public contract all t ~~ display[t] ::= {
    disp:(t)=>ss.
  }

  format@"Formatting contract".
  public contract all t ~~ format[t] ::= {
    frmt:(t,string) => ss.
  }

  public contract all t ~~ dump[t] ::= {
    dump:(t)=>ss.
  }

  public implementation display[ss] => {
    disp(X) => X
  }

  public contract all k ~~ concat[k] ::= {
    (++) : (k,k)=>k.
  }

  public contract all t ~~ reversible[t] ::= {
    reverse:(t)=>t.
  }

  -- Not strictly necessary, but makes for better symmetry.
  public boolean ::= true | false.

  public implementation display[boolean] => {
    disp(true) => ss("true").
    disp(false) => ss("false").
  }

  option@"the option type is useful when a value is not always available".
  public all t ~~ option[t] ::= none | some(t).

  public id:all a ~~ (a)=>a.
  id(X)=>X.

  public (•):all a,b,c ~~ ((b)=>c,(a)=>b)=>(a)=>c.
  F • G => (x)=>F(G(x)).

  sysResult@"used by some escapes to encode a result".
  public sysResult ::= ok
	| fail
	| eof
	| error(string).
}
