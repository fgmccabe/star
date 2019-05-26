star.thunk{
  import star.

  public thunk:all x ~~ (()=>x) => ()=>x.
  thunk(F) => let{
    X := none.
    ff() where V^=X! => V.
    ff() => valof action{
      V = F();
      X := some(V);
      valis V
    }
  } in ff.
}

  
