test.ts{
  import star.
  import star.assert.
  import star.pkg.
  import star.location.
  
  import test.lib.fact.

  implementation all a,b ~~ coercion[a,string],coercion[b,string] |: coercion[(a,b),string] =>{
    _coerce((X,Y)) => .some("("++X::string++","++Y::string++")")
  }

  testl3 = valof{
    try{
      assert fact(3)==5
    } catch () in {
      _ => showMsg("fact(3)!=5")
    };
    valis ()
  }

  testl4 = valof{
    show fact(3);
    valis ()
  }

  testl5 = valof{
    try{
      assert fact(3)==6;
      show fact(5);
      assert ~fact(2)==fact(4);
      valis ()
    } catch () in {
      _ => showMsg("something went wrong")
    }
  }

  main:()=>().
  main() => valof{
    try{
      assert fact(3)==6;
      show fact(5);
      assert fact(2)~=fact(4)
    } catch () in {
      _ => showMsg("something went wrong")
    };
    valis ()
  }
}
