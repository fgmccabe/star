test.rn{
  import star.
  import star.script.

  nameOf:all r,n ~~ r <~ {name:n} |: (r)=>n.
  nameOf(R) => R.name.

  nmeOf:((pp)=>string,pp)=>string.
  nmeOf(F,P) => F(P).

  pp ::= person{
    name:string.
  }

  ch::= chair{
    name:string.
  }

  main:()=>().
  main() => valof{
    P = person{name="fred"};
    C = chair{name="aaron"};

    show nameOf(P);

    show nmeOf(nameOf,P);
    
    assert nameOf(P)=="fred";
    assert nameOf(C) == "aaron";

    assert nmeOf(nameOf,P) == nameOf(P);
    assert nmeOf(nameOf,P) == P.name;
    valis ()
  }
}
