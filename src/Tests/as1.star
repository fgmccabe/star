test.as1{
  import star.
  import star.assert.

  notSome:all x ~~ ((ref option[x])) => boolean.
  notSome(R) => ~.some(_).=R.0!.

  main:(){}.
  main(){
    assert notSome((ref .none));
    assert ~notSome((ref .some(1)))
  }
}
