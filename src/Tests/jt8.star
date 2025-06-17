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

  ilsl:(integer,integer) => integer.
  ilsl(X,Y) => _blsl(X,Y).

  ilsr:(integer,integer) => integer.
  ilsr(X,Y) => _blsr(X,Y).

  iasr:(integer,integer) => integer.
  iasr(X,Y) => _basr(X,Y).

  not:(integer)=>integer.
  not(X) => _bnot(X).

  add:(float,float)=>float.
  add(X,Y) => _flt_plus(X,Y).

  sub:(float,float)=>float.
  sub(X,Y) => _flt_minus(X,Y).

  mul:(float,float)=>float.
  mul(X,Y) => _flt_times(X,Y).

  div:(float,float)=>float throws string.
  div(X,Y) => (try _flt_div(X,Y) catch { _ => throw "div zero"}).

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

    assert ilsl(1,10)==1024;
    assert ilsr(1024,10)==1;
    assert iasr(1024,10)==1;
    assert iasr(-1024,10)==-1;

    show not(56);
    show not(not(56));

    show add(1.0,2.0);
    show add(1.0,-1.0);
    show mul(2.0,4.0);

    try{
      show mod(7,2);
      show mod(-7,2);
      show mod(7,-2);
      show mod(6,0);
      show div(5.0,2.5);
    } catch {
      Msg => showMsg("We got exception: #(Msg)")
    };
    
    try{
      _jit_compile("#(__pkg__)@isEven",1);
      _jit_compile("#(__pkg__)@add",2);
      _jit_compile("#(__pkg__)@sub",2);
      _jit_compile("#(__pkg__)@mul",2);
      _jit_compile("#(__pkg__)@div",2);
      _jit_compile("#(__pkg__)@mod",2);
      _jit_compile("#(__pkg__)@ieq",2);
      _jit_compile("#(__pkg__)@ilt",2);
      _jit_compile("#(__pkg__)@ige",2);
      _jit_compile("#(__pkg__)@iand",2);
      _jit_compile("#(__pkg__)@ior",2);
      _jit_compile("#(__pkg__)@ixor",2);
      _jit_compile("#(__pkg__)@ilsl",2);
      _jit_compile("#(__pkg__)@ilsr",2);
      _jit_compile("#(__pkg__)@iasr",2);
      _jit_compile("#(__pkg__)@abs",1);
      _jit_compile("#(__pkg__)@not",1);
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

    assert ilsl(1,10)==1024;
    assert ilsr(1024,10)==1;
    assert iasr(1024,10)==1;
    assert iasr(-1024,10)==-1;

    assert not(56) == -57;
    assert not(not(56)) == 56;

    assert add(1.0,2.0)==3.0;
    assert add(1.0,-1.0)==2.0;
    assert mul(2.0,4.0)==8.0;
    
    valis ()
  }
}
