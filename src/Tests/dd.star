test.dd{
  import star.
  import test.fact.

  ff:(integer)=>action[(),integer].
  ff(X) => delay(()=>(return fact(X))).

  AA : action[(),integer].
  AA = do{
    return 34.
  }.

  XX : action[(),integer].
  XX = do{
    A <- ff(4);
    B <- ff(3);
    return A*B.
  }

  YY : action[(),integer].
  YY = do{
    ff(4)
  }

  ZZ : action[(),integer].
  ZZ = do {
    try {
      throw ()
    } catch {
      return 10
    }
  }

  show "AA=\(valof AA)".
  show "XX=\(valof XX)".
  show "YY=\(valof YY)".

  assert valof ZZ == 10.
}
