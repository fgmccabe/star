star.quote{
  import star.core.

  public contract all a,l ~~ quote[a->>l] ::= {
    _name : (l,string) => a.
    _qnme : (l,string) => a.
    _integer : (l,integer) => a.
    _biginteger : (l,bigint) => a.
    _float : (l,float) => a.
    _char : (l,char) => a.
    _string : (l,string) => a.
    _tuple : (l,string,cons[a]) => a.
    _apply : (l,a,a) => a.
  }
}
