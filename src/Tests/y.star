test.y{
  import star.
  import star.assert.

  -- Test out some try catch handling

  -- A function that throws an exception
  fooE:() => string throws exception.
  fooE() => valof{
    showMsg("raising from fooE");
    throw .exception("fooE")
  }

  -- A function that throws a generic exception
  fooG:all e ~~ (()=>e)=>() throws e.
  fooG(E) => throw E().

  -- A function that catches a generic exception
  fooC:all e ~~ ()=>string) => string throws e.
  fooC(F) => 
    (try
      F()
      catch {
	_ => valof{
	  showMsg("we got an exception");
	  valis "we got an exception"
	}
      }).

  fooInner:(integer) => string.
  fooInner(X) => (try
    let{
	inner(U) where U<0 => throw "$(U) is less than zero".
	inner(U) => disp(U*U)
      } in inner(X)
    catch {
      Msg => Msg
    }).

  -- A function that throws one of two kinds of exception
  fooX:throws string, throws exception |: (integer) => ().
  fooX(T) => valof{
    if 1 < T then
      throw .exception("$(T) is bigger than 1")
    else
    throw "$(T) not bigger than 1"
  }

  main:()=>().
  main() => valof{
    try{
      show fooE()
    } catch {
      .exception(M) => {
	showMsg("fooE threw #(M)")
      }
    };

    try{
      show fooG(()=>"test string")
    } catch {
      M => showMsg("fooG throws $(M)")
    };

    show fooC(()=>"world");

    try{
      show fooG(()=>42)
    } catch {
      M => showMsg("fooG throws $(M)")
    };

    -- Try a function with two kinds of exception
    try{
      try{
	fooX(2)
      } catch {
	.exception(M) => { showMsg("we got exception $(M)");
	}
      }
    } catch {
      S => { showMsg("we got string #(S)");
      }
    };

    try{
      try{
	fooX(0)
      } catch {
	.exception(M) => { showMsg("we got exception $(M)");
	}
      }
    } catch {
      S => { showMsg("we got string #(S)");
      }
    };

    assert fooInner(2) == "4";
    assert fooInner(-2) == "-2 is less than zero";

    valis ()
  }
}
