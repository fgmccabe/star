test.th{
  import star.
  import star.thunk.

  assign:all a,e/1,f ~~ execution[e->>f] |: (ref a,a) => e[()].
  assign(L,V) => do {
    _ = _assign(L,V);
    valis ()
  }

  XX := 0.

  Thk:thunk[(),integer].
  Thk = do{
    XX := 2;
    valis XX!
  }

  assert valof Thk == 2.

  Thl:thunk[(),integer].
  Thl = do{
    assign(XX,3);
    valis XX!
  }

  assert valof Thl == 3.
  
/*
  X := 0.

  Th = thunk(()=> valof action {
      logMsg("called Th");
      X := X!+2;
      valis 34}).

  assert Th()==34.
  assert Th()==34.
  assert (X!)==2.
*/

/*  thk[e] ::= dun(e) | delaied(()=>e).

  fawce:all e ~~ (thk[e])=>e.
  fawce(Tk) => ff(Tk,Tk).

  ff(dun(XX),_) => XX.
  ff(delaied(F),Tk) where XX .= F() && _ .= _overwrite(Tk,dun(XX)) => XX.

  TTh = delaied(()=>valof action {
    logMsg("called TTh");
    X := X!+3;
    valis 42
    }).

  assert fawce(TTh) == 42.
  assert fawce(TTh) == 42.
  assert (X!) == 5.
*/
}
