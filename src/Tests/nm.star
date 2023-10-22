test.nm{
  import star.
  import star.peg.
  import star.assert.

  first:all e,k ~~ (cons[(e,k)])=>option[e].
  first([])=>.none.
  first([(E,_),.._])=>.some(E).

  main:() => ().
  main() => valof{
    show first(parse(real(),"34.56e10"::cons[char]));

    assert R ?= first(parse(real(),"34.56e10"::cons[char])) && R==34.56e10;

    show first(parse(real(),"34.56"::cons[char]));

    show first(parse(real(),"34.56e-45"::cons[char]));
    show first(parse(real(),"34"::cons[char]));

    assert ("-"::cons[char])==[`-`];
    
    show first(parse(decimal,"-34"::cons[char]));

    show first(parse(real(),"-34"::cons[char]));

    show optInt(first(parse(real(),"-34"::cons[char])));
    
    assert -34 ?= optInt(first(parse(real(),"-34"::cons[char])));

    assert XX ?= optInt(first(parse(real(),"-34"::cons[char]))) && XX==-34;

    assert XX ?= optInt(first(parse(real(),"-34"::cons[char]))) && -XX==34;

    show first(parse(decimal,"-34"::cons[char]));
    valis ()
  }
  
  optInt:(option[float])=>option[integer].
  optInt(.some(F)) => .some(F::integer).
  optInt(.none) => .none.

}
