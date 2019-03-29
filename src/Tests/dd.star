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

  UU : (integer)=>action[(),integer].
  UU = (U)=>do {
    if valof ZZ == U then
      return 1
    else
      YY
  }

  show "AA=$(valof AA)".
  show "XX=$(valof XX)".
  show "YY=$(valof YY)".

  assert valof ZZ == 10.

  show "UU(10)=$(valof UU(10))".
  show "UU(9) = $(valof UU(9))".
}
