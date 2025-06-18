test.jt9{
  import star.
  import star.assert.

  -- Test jitting of let expressions

  hello:()=>string.
  hello() => "hello".

  there:(string)=>string.
  there(Th) => let{
    H = hello()
  } in _str_concat(H,_str_concat(" ",Th)).

  main:()=>().
  main() => valof{
    show there("world");

    assert there("there")=="hello there";
    try{
      _jit_compile("#(__pkg__)@hello",0);
      _jit_compile("#(__pkg__)@Γ_0@H",1);
      _jit_compile("#(__pkg__)@there",1);
    } catch {
      X => showMsg("$(X)")
    };

    assert there("world") == "hello world";
    valis ()
  }
}
