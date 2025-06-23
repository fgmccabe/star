test.jt10{
  import star.
  import star.assert.

  ieq:(integer,integer) => boolean.
  ieq(X,Y) => _int_eq(X,Y).

  ge:(integer,integer) => boolean.
  ge(X,Y) => _int_ge(X,Y).

  add:(integer,integer)=>integer.
  add(X,Y) => _int_plus(X,Y).

  mul:(integer,integer)=>integer.
  mul(X,Y) => _int_times(X,Y).

  fct:(integer)=>integer.
  fct(X) => valof{
    F := 1;
    Ix := 1;
    while ge(X,Ix!) do{
      F := mul(Ix!,F!);
      Ix := add(Ix!,1)
    };
    valis F!
  }

  main:()=>().
  main() => valof{
    assert add(2,3) == 5;
    show fct(4);
    assert fct(4) == 24;
    
    try{
      _jit_compile("#(__pkg__)@add",2);
      _jit_compile("#(__pkg__)@mul",2);
      _jit_compile("#(__pkg__)@ge",2);
      _jit_compile("#(__pkg__)@ieq",2);
      _jit_compile("#(__pkg__)@fct",1);
    } catch {
      X => showMsg("$(X)")
    };

--    assert ieq(add(2,3),5);
    show fct(4);
    assert ieq(fct(4),24);
  }
}

  
  
  
