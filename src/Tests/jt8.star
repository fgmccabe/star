test.jt8{
  import star.
  import star.assert.

  -- Test jitting miscellaneous arith operators

  isEven:(integer)=>boolean.
  isEven(X) => (try 0.=_int_mod(X,2)  catch {_ => .false}).

  mod:(integer,integer)=>integer throws string.
  mod(X,Y) => ( try _int_mod(X,Y) catch { _ => throw "divide by zero"}).

  abs:(integer)=>integer.
  abs(X) => _int_abs(X).

  ieq:(integer,integer) => boolean.
  ieq(X,Y) => _int_eq(X,Y).

  ilt:(integer,integer) => boolean.
  ilt(X,Y) => _int_lt(X,Y).

  ige:(integer,integer) => boolean.
  ige(X,Y) => _int_ge(X,Y).

  iand:(integer,integer) => integer.
  iand(X,Y) => _band(X,Y).

  ior:(integer,integer) => integer.
  ior(X,Y) => _bor(X,Y).

  ixor:(integer,integer) => integer.
  ixor(X,Y) => _bxor(X,Y).

  main:()=>().
  main() => valof{
    assert isEven(2);
    assert ~isEven(7);

    assert abs(-4) == abs(4);
    assert abs(4) == 4;

    assert ieq(4,4);
    assert ~ieq(4,5);

    assert ilt(4,5);
    assert ~ilt(5,4);
    
    assert ige(5,4);
    assert ige(5,5);
    assert ~ige(4,5);

    assert iand(0x3434,0x4343) == 0x0;
    assert ior(0x3434,0x4343) == 0x7777;

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
      _jit_compile("#(__pkg__)@ieq",2);
      _jit_compile("#(__pkg__)@ilt",2);
      _jit_compile("#(__pkg__)@ige",2);
      _jit_compile("#(__pkg__)@iand",2);
      _jit_compile("#(__pkg__)@ior",2);
      _jit_compile("#(__pkg__)@ixor",2);
      _jit_compile("#(__pkg__)@abs",1);
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

    assert abs(-4) == abs(4);
    assert abs(4) == 4;

    assert ieq(4,4);
    assert ~ieq(4,5);

    assert ilt(4,5);
    assert ~ilt(5,4);
    
    assert ige(5,4);
    assert ige(5,5);
    assert ~ige(4,5);

    assert iand(0x3434,0x4343) == 0x0;
    assert ior(0x3434,0x4343) == 0x7777;
    assert ixor(0x3434,0x4343) == 0x7777;
    valis ()
  }
}
