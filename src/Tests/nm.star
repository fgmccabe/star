test.nm{
  import star.
  import star.parse.

  first:all e,k ~~ (list[(e,k)])=>option[e].
  first([])=>none.
  first([(E,_),.._])=>some(E).
  
  show disp(first(parse(real,"34.56e10"::list[integer]))).

  assert 34.56e10 ^= first(parse(real,"34.56e10"::list[integer])).

  show disp(first(parse(real,"34.56"::list[integer]))).

  show disp(first(parse(real,"34.56e-45"::list[integer]))).
  show disp(first(parse(real,"34"::list[integer]))).
  show disp(first(parse(real,"-34"::list[integer]))).

  optInt:(float)=>integer.
  optInt(F) => F::integer.

  show disp(optInt(^first(parse(real,"-34"::list[integer])))).

  assert -34 ^= optInt(^first(parse(real,"-34"::list[integer]))).

  assert XX ^= optInt(^first(parse(real,"-34"::list[integer]))) && XX==-34.

  assert XX ^= optInt(^first(parse(real,"-34"::list[integer]))) && -XX==34.

  show disp(first(parse(decimal,"-34"::list[integer]))).
}
