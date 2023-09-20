test.do2{
  import star.
  import star.assert.

  -- Test action notation

  doFirst:()=>().
  doFirst() => valof{
    A = ref 1;

    A := A!+A!;

    assert A!==2;
    valis ()
  }

  
  doIf:(integer)=>boolean.
  doIf(X) => valof{
    Alpha = 3;

    if Alpha < X then{
      valis .true
    } else {
      valis .false
    }
  }

  doIf2:(integer)=>boolean.
  doIf2(X) => valof{
    Alpha = ref .true;

    if 2 < X then{
      Alpha := .true
    } else {
      Alpha := .false
    };
    valis Alpha!
  }

  doIf3:(integer)=>boolean.
  doIf3(X) => valof{
    Alpha = ref .false;

    if 2 < X then{
      Alpha := .true
    };
    valis Alpha!
  }
  

  main:()=>().
  main() => valof{
    doFirst();
    assert doIf(4);
    assert doIf2(4);
    assert doIf3(4);
    valis ()
  }
}
