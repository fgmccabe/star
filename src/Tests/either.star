test.either{
  import star.
  import star.assert.

  main:()=>().
  main() => valof{
    try{
      assert ? .either("fred") == "fred";

      valis ? (.other(34)|:either[(),integer])
    } catch {
      XX => {assert XX == 34}
    };
    valis ()
  }
}
