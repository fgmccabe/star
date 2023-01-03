test.q4{
  import star.
  import star.script.

  looking:(string,cons[(string,integer)]) => integer.
  looking(Nm,Vrs) where X ?= {! X| (Nm,X) in Vrs !} => X.

  SS = [("a",1), ("b",2), ("c",3)].

  main:()=>().
  main()=> valof{
    show looking("a",SS);
    assert looking("c",SS) == 3;
    valis ()
  }
}
