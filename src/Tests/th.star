test.th{
  import star.
  import star.thunk.

  X := 0.

  Th = thunk(()=> valof do {
      logMsg("called Th");
      X := X!+2;
      valis 34}).

  assert Th()==34.
  assert Th()==34.
  assert (X!)==2.

  thk[e] ::= dun(e) | delaied(()=>e).

  fawce:all e ~~ (thk[e])=>e.
  fawce(Tk) => ff(Tk,Tk).

  ff(dun(XX),_) => XX.
  ff(delaied(F),Tk) where XX .= F() && _ .= _overwrite(Tk,dun(XX)) => XX.

  TTh = delaied(()=>valof do {
    logMsg("called TTh");
    X := X!+3;
    valis 42
    }).

  assert fawce(TTh) == 42.
  assert fawce(TTh) == 42.
  assert (X!) == 5.
}
