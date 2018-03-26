star.sequence{
  import star.core.

  -- stream contract
  public contract all S,E ~~ stream[S ->> E] ::= {
    _eof:(S) => option[()].
    _hdtl:(E,S) <= S.
    _cons:(E,S)=>S.
    _apnd:(S,E)=>S.
    _back:(S,E)<=S/
    _nil:S.
  }

}
