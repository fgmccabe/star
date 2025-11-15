test.do12{
  import star.
  import star.assert.

  -- test case actions

  foo ::= .foo(integer) | .bar(string).

  multiWhr:(foo)=>integer.
  multiWhr(X) => valof{
    case X in {
      | .foo(Ix) do valis Ix
      | .bar(_) do valis 0
    }
  }

  seqCase:(foo)=>integer.
  seqCase(X) => valof{
    res = ref 0;
    case X in {
      | .foo(Ix) do {res:= Ix}
      | .bar(_) do {}
    };
    valis res!
  }

  main:(){}.
  main(){
    try{
      assert multiWhr(.foo(23))==23;
      assert seqCase(.foo(23))==23;
      assert seqCase(.bar(""))==0;
    } catch {
      | _ do showMsg("bad valof")
    };
  }
}

  
