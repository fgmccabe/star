star.quote{
  import star.core.
  import star.location.

  public contract all a ~~ quote[a] ::= {
    _name : (option[locn],string) => a.
    _qnme : (option[locn],string) => a.
    _integer : (option[locn],integer) => a.
    _biginteger : (option[locn],bigint) => a.
    _float : (option[locn],float) => a.
    _char : (option[locn],char) => a.
    _string : (option[locn],string) => a.
    _tuple : (option[locn],string,cons[a]) => a.
    _apply : (option[locn],a,a) => a.
  }
}
