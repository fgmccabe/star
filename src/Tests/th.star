test.th{
  import star.
  import star.thunk.
  import star.script.

  assign:all a,e/2 ~~ execution[e] |: (ref a,a) => e[(),()].
  assign(L,V) => do {
    _ .= _assign(L,V);
    valis ()
  }

  XX := 0.

  Thk:thunk[(),integer].
  Thk = do{
    XX := 2;
    valis XX!
  }

  Thl:thunk[(),integer].
  Thl = do{
    assign(XX,3);
    valis XX!
  }
  
  X := 0.

  Th = thunk(()=> valof action {
      logMsg("called Th");
      X := X!+2;
      valis 34
    }).

  thk[e] ::= dun(e) | delaied(()=>e).

  fawce:all e ~~ (thk[e])=>e.
  fawce(Tk) => ff(Tk,Tk).

  ff:all e ~~ (thk[e],thk[e])=>e.
  ff(dun(ZZ),_) => ZZ.
  ff(delaied(F),Tk) where ZZ .= F() && _ .= _overwrite(Tk,dun(ZZ)) => ZZ.

  TTh = delaied(()=>valof action {
    logMsg("called TTh");
    X := X!+3;
    valis 42
    }).

  main:() => action[(),()].
  main() => do{
    assert valof Thk == 2;

    assert valof Thl == 3;

    assert Th()==34;
    assert Th()==34;
    assert (X!)==2;

    assert fawce(TTh) == 42;
    assert fawce(TTh) == 42;
    assert (X!) == 5
  }
}
