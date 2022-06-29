test.do2{
  import star.
  import star.script.

  -- Test action notation

  doFirst:all e ~~ ()=>result[e,()].
  doFirst() => do{
    A .= ref 1;

    A := A!+A!;

    assert A!==2;
    valis ()
  }

  
  doIf:all e ~~ (integer)=>result[e,boolean].
  doIf(X) => do{
    Alpha .= 3;

    if Alpha < X then{
      valis .true
    } else {
      valis .false
    }
  }

  doIf2:all e ~~ (integer)=>result[e,boolean].
  doIf2(X) => do{
    Alpha .= ref .true;

    if 2 < X then{
      Alpha := .true
    } else {
      Alpha := .false
    };
    valis Alpha!
  }

  doIf3:all e ~~ (integer)=>result[e,boolean].
  doIf3(X) => do{
    Alpha .= ref .false;

    if 2 < X then{
      Alpha := .true
    };
    valis Alpha!
  }
  

  main:()=>()
  main() => valof{
    try{
      do doFirst();
      assert valof doIf(4);
      assert valof doIf2(4);
      assert valof doIf3(4)
    } catch {
      _ => {}
    };
    valis ()
  }
}
