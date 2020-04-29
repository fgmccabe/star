test.do2{
  import star.
  import star.script.

  -- Test action notation

  doFirst() => do{
    A .= ref 1;

    A := A!!+A!!;

    assert A!!==2
  }

  doIf(X) => action{
    Alpha .= 3;

    if Alpha < X then{
      valis .true
    } else {
      valis .false
    }
  }
  doIf2(X) => action{
    Alpha .= ref .true;

    if 2 < X then{
      Alpha := .true
    } else {
      Alpha := .false
    };
    valis Alpha!!
  }

  doIf3(X) => action{
    Alpha .= ref .false;

    if 2 < X then{
      Alpha := .true
    };
    valis Alpha!!
  }
  

  main:()=>action[(),()].
  main() => do{
    doFirst();
    assert valof doIf(4);
    assert valof doIf2(4);
    assert valof doIf3(4)
  }
}
