star.core {
  @"Core definitions of types and interfaces that are really part of the language".

  public boolean ::= .true | .false.

  equality@"defines semantic equality".
  public contract all x ~~ equality[x] ::= {
    (==)@"semantic equality is defined explicitly".

    (==): (x,x)=>boolean.
  }

  (~=)@"semantic inequality defined in terms of equality".
  public (~=):all x ~~ equality[x] |: (x,x)=>boolean.
  x ~= y => ~x==y.

  public implementation equality[boolean] => {
    .true == .true => .true.
    .false == .false => .true.
    _ == _ default => .false
  }

  public contract all x ~~ comp[x] ::= {
    (<): (x,x)=>boolean.
    (>=): (x,x)=>boolean.
  }

  hashable@"defines functions associated with hash encoding".
  public contract all x ~~ hashable[x] ::= {
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

  public implementation all c ~~ sizeable[c] |: sizeable[ref c] => {
    size(M) => size(M!).
    isEmpty(M) => isEmpty(M!)
  }

  public contract all a,b ~~ measured[a->>b] ::= {
    '[||]':(a)=>b.
  }

  -- The head/tail contract
  public contract all c,e ~~ head[c->>e] ::= {
    head:(c)=>option[e].
    tail:(c)=>option[c].
  }

  public contract all c,e ~~ back[c->>e] ::= {
    last:(c) => option[e].
    lead:(c) => option[c].
  }

  public contract all c,e ~~ membership[c->>e] ::= {
    (\+):(c,e)=>c.
    (\-):(c,e)=>c.
    (.<.):(e,c)=>boolean.
  }

  public contract all m,k,v ~~ indexed[m ->> k,v] ::= {
    _index:(m,k) => option[v].
    _put:(m,k,v) => m.
    _remove:(m,k) => m.
    _empty:m.
  }.

  -- stream contract
  public contract all S,E ~~ stream[S ->> E] ::= {
    _eof:(S) => boolean.
    _hdtl:(S) => option[(E,S)].
  }

  -- The sequence contract
  public contract all S,E ~~ sequence[S->>E] ::= {
    _cons:(E,S) => S.
    _nil:S.
  }

  public all t ~~ cons[t] ::= .nil | .cons(t,cons[t]).

  -- Displayable contract
  public contract all t ~~ display[t] ::= {
    disp:(t)=>string.
  }

  format@"Formatting contract".
  public contract all t ~~ format[t] ::= {
    _format:(t,string) => string.
  }

  public contract all k ~~ concat[k] ::= {
    (++) : (k,k)=>k.
    _multicat : (cons[k]) => k.
  }

  public contract all c,e ~~ glue[c->>e] ::= {
    prepend:(e,c) => c.
    append:(c,e) => c.
  }

  public contract all t ~~ reversible[t] ::= {
    reverse:(t)=>t.
  }

  public implementation display[boolean] => {
    disp(.true) => "true".
    disp(.false) => "false".
  }

  option@"the option type is useful when a value is not always available".
  public all t ~~ option[t] ::= .none | .some(t).

  public id:all a ~~ (a)=>a.
  id(X)=>X.

  public (•):all a,b,c ~~ ((b)=>c,(a)=>b)=>(a)=>c.
  F • G => (x)=>F(G(x)).

  public (••):all a,b,c ~~ ((b,b)=>c,(a)=>b)=>(a,a)=>c.
  F •• G => (x,y)=>F(G(x),G(y)).

  public all x,e ~~ result[x,e] ::= ._ok(x) | ._except(e).

  public exception ::= .exception(string).

  public errorCode ::= .eINTRUPT |
  .eNOTDIR |
  .eNOFILE |
  .eNOTFND |
  .eINVAL |
  .eRANGE |
  .eNOPERM |
  .eFAIL |
  .eIOERROR |
  .eCONNECT |
  .eDEAD |
  .divZero |
  .noValue |
  .hasValue.

  public implementation display[errorCode] => {
    disp(.eINTRUPT) => "eINTRUPT".
    disp(.eNOFILE) => "eNOFIL".
    disp(.eNOTDIR) => "eNOTDIR".
    disp(.eNOTFND) => "eNOTFND".
    disp(.eINVAL) => "eINVAL".
    disp(.eRANGE) => "eRANGE".
    disp(.eNOPERM) => "eNOPERM".
    disp(.eFAIL) => "eFAIL".
    disp(.eIOERROR) => "eIOERROR".
    disp(.eCONNECT) => "eCONNECT".
    disp(.eDEAD) => "eDEAD".
    disp(.divZero) => "divZero".
    disp(.noValue) => "noValue".
    disp(.hasValue) => "hasValue".
  }

  public error ::= .error(string,errorCode).
}
