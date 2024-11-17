test.throwing{
  import star.

  public all e,v ~~ rslt[e,v] ::= .ok(v) | .err(e).

  public implementation all e,v ~~ display[e],display[v] |: display[rslt[e,v]] => {
    disp(.ok(V)) => "ok $(V)".
    disp(.err(E)) => "bad $(E)"
  }

  public contract all e ~~ throwable[e] ::= {
    _throw:all k ~~ (e)=>k
  }

}
  
