test.ts{
  import star.
  import star.assert.
  import star.pkg.
  import star.location.
  
  import test.lib.fact.

  implementation all a,b,x ~~ coercion[a,string->>x],coercion[b,string->>x] |= coercion[(a,b),string->>x] =>{
    _coerce((X,Y)) => "("++X::string++","++Y::string++")"
  }

  testl3 = valof{
    try{
      assert fact(3)==5
    } catch {
      _ do showMsg("fact(3)!=5")
    };
    valis ()
  }

  testl4 = valof{
    show fact(3);
    valis ()
  }

  testl5(){
    try{
      assert fact(3)==6;
      show fact(5);
      assert ~fact(2)==fact(4);
    } catch {
      _ do showMsg("something went wrong")
    }
  }

  main:(){}.
  main(){
    try{
      assert fact(3)==6;
      show fact(5);
      assert fact(2)~=fact(4)
    } catch {
      _ do showMsg("something went wrong")
    }
  }
}
