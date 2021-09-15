test.do12{
  import star.
  import star.script.

  -- test case actions

  foo ::= foo(integer) | bar(string).

  multiWhr:(foo)=>action[(),integer].
  multiWhr(X) => action{
    case X in {
      foo(Ix) => valis Ix.
      bar(_) => valis 0
    }
  }

  seqCase:(foo)=>action[(),integer].
  seqCase(X) => action{
    res .= ref 0;
    case X in {
      foo(Ix) => {res:= Ix}.
      bar(_) => {}
    };
    valis res!
  }

  main:()=>action[(),()].
  main()=>action{
    assert valof multiWhr(foo(23))==23;
    assert valof seqCase(foo(23))==23;
    assert valof seqCase(bar(""))==0
  }
}

  
