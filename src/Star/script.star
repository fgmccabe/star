star.script{
  import star.
  import star.location.

  public assrt:all m/2,e ~~ execution[m], coercion[(string,locn),e] |: (()=>boolean,string,locn) => m[e,()].
  assrt(Tst,Msg,Lc) => do{
    if Tst() then
      valis ()
    else
      throw (Msg,Lc)::e
  }
}
