test.io1{
  import star.
  import star.assert.
  import star.io.

  all t ~~ ioFuture[t] ::= .ft(ref option[t]).

  ioEvt ::=
    exists e ~~ ioEvt{
      ee ~> e.
      set:(e)=>().
      get:()=>option[e].
    }



  xx = open xc{
    get(.false) => 0.
    get(_) default => 1.

    test = .true.

    check(0)=>.false.
    check(_) default => .true.

    ee ~> integer.
  }

  all t ~~ index[t] ::= exists e 

  yy : xx.ee.
  yy = xx.get(.false).

  main:()=>().
  main()=>valof{
    assert xx.test;

    assert yy==0; -- should report a syntax error

    assert ~ xx.check(yy);
    
    valis ()
  }

  

  pak ::= exists x ~~ pak{
    ready:()=>boolean.
    store:(x)=>().
    pick:()=>x
  }

  _main:(cons[string])=>().
  _main([Fl,.._]) => main(Fl).
  _main([]) => main("io0.star").

  main:(string)=>().
  main(Fl) => valof{
    try{
      In = _openInFile(Fl,3);
      while Ch.=_inchar(In) do{
	logMsg("char: $(Ch)");
      }
    } catch errorCode in {
      | .eof => logMsg("end of file")
      | Cde => logMsg("error code $(Cde)")
    };

    valis ()
  }
}
