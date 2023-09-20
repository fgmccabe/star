test.y{
  import star.
  import star.assert.

  -- Test out some try catch handling

  -- A function that throws an exception
  fooE:raises exception |: () => string.
  fooE() => valof{
    logMsg("raising from fooE");
    raise .exception("fooE")
  }

  -- A function that throws a generic exception
  fooG:all e ~~ raises e |: (()=>e)=>().
  fooG(E) => raise E().

  -- A function that catches a generic exception
  fooC:all e ~~ (raises e |: ()=>string) => string.
  fooC(F) => 
    (try
      F()
      catch e in {
	_ => valof{
	  logMsg("we got an exception");
	  valis "we got an exception"
	}
      }).

  -- A function that handles two kinds of exception
  fooT:(integer) => string.
  fooT(T) => valof{
    try{
      try{
	if 1<T then
	  raise .exception("$(T) is bigger than 1")
	else
	raise "$(T) not bigger than 1"
      } catch exception in {
	.exception(M) => { logMsg("we got exception $(M)");
	  valis M
	}
      }
    } catch string in {
      S => { logMsg("we got string #(S)");
	valis S
      }
    }
  }

  fooInner:(integer) => string.
  fooInner(X) => (try
    let{
	inner(U) where U<0 => raise "$(U) is less than zero".
	inner(U) => disp(U*U)
      } in inner(X)
    catch string in {
      Msg => Msg
    }).

  -- A function that throws one of two kinds of exception
  fooX:raises string, raises exception |: (integer) => ().
  fooX(T) => valof{
    if 1 < T then
      raise .exception("$(T) is bigger than 1")
    else
    raise "$(T) not bigger than 1"
  }

  main:()=>().
  main() => valof{
    assert fooT(2)=="2 is bigger than 1";
    assert fooT(0)=="0 not bigger than 1";

    try{
      show fooE()
    } catch exception in {
      .exception(M) => {
	logMsg("fooE threw #(M)")
      }
    };

    try{
      show fooG(()=>"test string")
    } catch string in {
      M => logMsg("fooG throws $(M)")
    };

    show fooC(()=>"world");
    show fooC((()=>(.false ?? "world"||(raise .exception("bad world")))):
     raises exception |: ()=>string);
--   show fooC(fooE:raises exception |: ()=>string);

    try{
      show fooG(()=>42)
    } catch integer in {
      M => logMsg("fooG throws $(M)")
    };

    -- Try a function with two kinds of exception
    try{
      try{
	fooX(2)
      } catch exception in {
	.exception(M) => { logMsg("we got exception $(M)");
	}
      }
    } catch string in {
      S => { logMsg("we got string #(S)");
      }
    };

    try{
      try{
	fooX(0)
      } catch exception in {
	.exception(M) => { logMsg("we got exception $(M)");
	}
      }
    } catch string in {
      S => { logMsg("we got string #(S)");
      }
    };

    assert fooInner(2) == "4";
    assert fooInner(-2) == "-2 is less than zero";

    valis ()
  }
}
