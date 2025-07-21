star.quote{
  import star.core.
  import star.location.

  public contract all a ~~ quote[a] ::= {
    _name : (locn,string) => a.
    _qnme : (locn,string) => a.
    _integer : (locn,integer) => a.
    _biginteger : (locn,bigint) => a.
    _float : (locn,float) => a.
    _char : (locn,char) => a.
    _string : (locn,string) => a.
    _tuple : (locn,string,cons[a]) => a.
    _apply : (locn,a,a) => a.
  }
}
