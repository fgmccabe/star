test.do12{
  import star.
  import star.assert.

  -- test case actions

  foo ::= .foo(integer) | .bar(string).

  multiWhr:(foo)=>integer.
  multiWhr(X) => valof{
    case X in {
      .foo(Ix) => valis Ix
      | .bar(_) => valis 0
    }
  }

  seqCase:(foo)=>integer.
  seqCase(X) => valof{
    res = ref 0;
    case X in {
      .foo(Ix) => {res:= Ix}.
      .bar(_) => {}
    };
    valis res!
  }

  main:()=>().
  main()=>valof{
    try{
      assert multiWhr(.foo(23))==23;
      assert seqCase(.foo(23))==23;
      assert seqCase(.bar(""))==0;
    } catch () in {
      _ => showMsg("bad valof")
    };
    valis ()
  }
}

  
