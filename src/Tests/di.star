test.di{
  import star.
  import star.script.

  Is = cons(1,cons(2,cons(3,cons(4,.nil)))).

  Fc:(cons[integer])=>result[(),integer].
  Fc(I) => do{
    Fx .= ref 1;
    for E in I do {
      Fx := Fx! * E
    };
    valis Fx!
  }

  main:()=>().
  main()=>valof{
    show valof Fc(Is);

    assert valof Fc(Is) == 24;
    valis ()
  }
}
