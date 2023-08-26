test.di{
  import star.
  import star.script.

  Is = .cons(1,.cons(2,.cons(3,.cons(4,.nil)))).

  Fc:(cons[integer])=>integer.
  Fc(I) => valof{
    Fx = ref 1;
    for E in I do {
      Fx := Fx! * E
    };
    valis Fx!
  }

  main:()=>().
  main()=>valof{
    show Fc(Is);

    assert Fc(Is) == 24;
    valis ()
  }
}
