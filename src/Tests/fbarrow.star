test.fbarrow{
  import star.
  import star.assert.

  -- (a) "(R =>> S) throws E": parens exclude the fiber's own throws, so the
  -- ENCLOSING function owns it; the fiber itself is plain (non-throwing).
  -- Mixing "=>" and "=>>" without parens at all is a parse error.
  mkPlain: (integer) => (integer =>> integer) throws string.
  mkPlain(X) where X>=0 =>
    _fiber((_,Y) => Y+X).
  mkPlain(_) default => throw "negative".

  -- (b) parenthesized "(R =>> S throws E)": the FIBER owns its own throws;
  -- the enclosing function is plain.
  mkThrowing: (integer) => (integer =>> integer throws string).
  mkThrowing(X) =>
    _fiber((_,Y) => valof{
      if Y<0 then throw "negative" else valis Y+X
    }).

  main:(){}.
  main(){
    try{
      Fb1 = mkPlain(5);
      R1 = Fb1 resume 10;
      show R1;
      assert R1==15
    } catch {
      M do showMsg("unexpected a: $(M)")
    };

    try{
      _ = mkPlain(-1);
      unreachable
    } catch {
      M do {
        show M;
        assert M=="negative"
      }
    };

    try{
      R2 = mkThrowing(100) resume 5;
      show R2;
      assert R2==105
    } catch {
      M do showMsg("unexpected b: $(M)")
    };

    try{
      _ = mkThrowing(100) resume (-1);
      unreachable
    } catch {
      M do {
        show M;
        assert M=="negative"
      }
    };

    showMsg("Ok")
  }
}
