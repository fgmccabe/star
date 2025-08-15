test.impl0{
  import star.
  import star.assert.

  needsI:I|:integer|=(integer)=>integer.
  needsI(X)=>X+I.

  main:()=>().
  main() => valof{
    let{
      I = 42
    } in {
      assert(needsI(24)==66)
    };

    let{
      I = 24
    } in {
      assert(needsI(12)==36)
    }
  }
}
