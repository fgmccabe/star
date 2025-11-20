test.either{
  import star.
  import star.assert.

  main:(){}.
  main(){
    try{
      assert ? .either("fred") == "fred";

      show ? (.other(34)|:either[(),integer])
    } catch {
      XX do {assert XX == 34}
    }
  }
}
