test.gen{
  import star.
  import star.skew.

  genState[c] ::= genState().

  generator:all c,e ~~ iter[c->>e] |: (c) => genState[c].
  generator(C) => let{
    Tg = tag().
    Cont = ref .none.
    Current = ref .none.

    genNext(E,S) => Tg cut (K)=>do{
      Cont := some(K);
      Current := some(E);
      valis ()
    }.

    iter = Tg prompt _iter(C,.none,genNext).
  } in {.
    
    
    
