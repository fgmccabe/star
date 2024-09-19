test.hr1{
  import star.
  import star.assert.

  -- First test of effect handlers

  -- Handler contract

  incHndlr[t] ::= incHndlr{
    inc:(t)=>t.
  }

  iNeedAnInc:incHndlr[integer]|:(integer)=>integer.
  iNeedAnInc(X) =>
    (H.inc).(X).

  main:()=>().
  main() => valof{
    R = valof{
      try{
	valis iNeedAnInc(3)
      } handle incHndlr{
	inc(U) => resume U+1
      }
    };
    show R;
    valis ()
  }
}
