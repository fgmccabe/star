test.ts{
  import star.
  import star.script.
  import star.pkg.
  import star.location.
  
  import test.fact.

  implementation all a,b ~~ coercion[a,string],coercion[b,string] |: coercion[(a,b),string] =>{.
    _coerce((X,Y)) => "("++X::string++","++Y::string++")"
  .}

/*  testl2 = valof action{
    try{
      assrt(()=>fact(3)==5,"failed: fact(3)!=5",locn(__pkg__,0,0,0,0))
    } catch (Err)=> action{
      logMsg("error "++Err);
      valis ()
    };
    valis ()
  }

  testl3 = valof action{
    try{
      assert fact(3)==5
    } catch (Err)=> action{
      logMsg(Err);
      valis ()
    }
  }

  testl4 = valof action{
    show fact(3)
  }
*/
  testl5 = valof script{
    assert fact(3)==6;
    show fact(5);
    assert fact(2)==fact(4);
    valis "alpha"
  }
}
