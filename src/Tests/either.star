test.either{
  import star.
  import star.assert.

  public contract all x,y ~~ pull[x->>y] ::= {
    (?):(x) =>y
  }

  public implementation all e,o ~~ raises o |: pull[either[e,o]->>e] => {
    ? .either(A) => A.
    ? .other(B) => raise B
  }

  main:()=>().
  main() => valof{
    try{
      assert ? .either("fred")/*:either[string,integer])*/ == "fred";

      show  ? (.other(34):either[string,integer])
    } catch integer in {
      XX => {show "$(XX) in catch"}
    };
    valis ()
  }
}
