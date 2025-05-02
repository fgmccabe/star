test.either{
  import star.
  import star.assert.

  main:()=>().
  main() => valof{
    try{
      assert ? .either("fred") == "fred";

      show  ? (.other(34):either[string,integer])
    } catch {
      XX => {show "$(XX) in catch"}
    };
    valis ()
  }
}
