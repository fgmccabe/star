test.pr{
  import star.
  import star.script.

  pr ::= pr{
    name : ref string.
    role : string.
  }

  mkPr(Nm,Rl) =>
    pr{
      name = ref Nm.
      role = Rl
    }

    main:()=>().
  main() => valof{
    joe .= mkPr("fred","janitor");

    show joe.name!;

    joe.name := "peter";

    assert joe.name! == "peter";

    tom .= pr{
      name = ref "tom".
      role = "manager".
    };

    assert tom.name! == "tom";

    tom.name := joe.name!;

    assert tom.name! == joe.name!;

    assert tom.role == "manager";
    assert joe.role == "janitor";
    valis ()
  }
}
    
    
