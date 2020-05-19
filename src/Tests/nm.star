test.nm{
  import star.
  import star.parse.
  import star.script.

  first:all e,k ~~ (cons[(e,k)])=>option[e].
  first([])=>.none.
  first([(E,_),.._])=>some(E).

  main:() => action[(),()].
  main() => do{
    show first(parse(real(),"34.56e10"::cons[integer]));

    assert 34.56e10 ^= first(parse(real(),"34.56e10"::cons[integer]));

    show first(parse(real(),"34.56"::cons[integer]));

    show first(parse(real(),"34.56e-45"::cons[integer]));
    show first(parse(real(),"34"::cons[integer]));
    show first(parse(real(),"-34"::cons[integer]));

    show optInt(^first(parse(real(),"-34"::cons[integer])));
    
    assert -34 ^= optInt(^first(parse(real(),"-34"::cons[integer])));

    assert XX ^= optInt(^first(parse(real(),"-34"::cons[integer]))) && XX==-34;

    assert XX ^= optInt(^first(parse(real(),"-34"::cons[integer]))) && -XX==34;

    show disp(first(parse(decimal,"-34"::cons[integer])))
  }
  
  optInt:(float)=>option[integer].
  optInt(F) => some(F::integer).

}
