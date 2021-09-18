test.ts{
  import star.
  import star.script.
  import star.pkg.
  import star.location.
  
  import test.fact.

  implementation all a,b ~~ coercion[a,string],coercion[b,string] |: coercion[(a,b),string] =>{.
    _coerce((X,Y)) => some("("++X::string++","++Y::string++")")
  .}

  testl3 = valof action{
    try{
      assert fact(3)==5
    } catch {
      logMsg("fact(3)!=5")
    }
  }

  testl4 = valof action{
    show fact(3)
  }

  testl5 = valof action{
    try{
      assert fact(3)==6;
      show fact(5);
      assert ~fact(2)==fact(4)
    } catch {
      logMsg("something went wrong")
    }
  }

  main:()=>action[(),()].
  main() => action{
    try{
      assert fact(3)==6;
      show fact(5);
      assert fact(2)~=fact(4)
    } catch {
      logMsg("something went wrong")
    }
  }
}
