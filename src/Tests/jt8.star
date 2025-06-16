test.jt8{
  import star.
  import star.assert.

  -- Test jitting miscellaneous arith operators

  isEven:(integer)=>boolean.
  isEven(X) => (try 0.=_int_mod(X,2)  catch {_ => .false}).

  mod:(integer,integer)=>integer throws string.
  mod(X,Y) => ( try _int_mod(X,Y) catch { _ => throw "divide by zero"}).

  main:()=>().
  main() => valof{
    assert isEven(2);
    assert ~isEven(7);

    try{
      show mod(7,2);
      show mod(-7,2);
      show mod(7,-2);
      show mod(6,0)
    } catch {
      Msg => showMsg("We got exception: #(Msg)")
    };
    
    try{
      _jit_compile("#(__pkg__)@isEven",1);
      _jit_compile("#(__pkg__)@mod",2);
    } catch {
      X => showMsg("$(X)")
    };

    assert isEven(2);
    assert ~isEven(7);

    try{
      show mod(7,2);
      show mod(-7,2);
      show mod(7,-2);
      show mod(6,0)
    } catch {
      Msg => showMsg("We got exception: #(Msg)")
    };
    
    valis ()
  }
}
