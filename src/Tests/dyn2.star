test.dyn2{
  import star.
  import star.assert.

  -- Experiments in throwing, using spawn/retire

  contract all e ~~ raising[e] ::= {
    raiz:all r ~~ (e)=>r
  }

  fe: raising[integer] |: (integer)=>integer.
  fe(X) => (X>5 ?? raiz(10) || 3*X).

  fs: raising[string], raising[integer] |: (integer) => integer.
  fs(X) => (X<0 ?? raiz("negative arg") || X>100 ?? raiz(10) || X*4).

  f:(integer) => integer.
  f(X) => _resume(_fiber((K,_)=>
	let{
	  implementation raising[string] => {
	    raiz(M) => valof{
	      logMsg("error message: #(M)");
	      _retire(K,-1);
	    }
	  }

	  implementation raising[integer] => {
	    raiz(I) => valof{
	      logMsg("error value: $(I)");
	      _retire(K,I);
	    }
	  }
	} in fs(X)),()).

  main:()=>().
  main() => valof{
    logMsg(disp(f(-1)));
    logMsg(disp(f(10)));
    logMsg(disp(f(210)));

    assert f(-1) == -1;
    assert f(10) == 40;
    assert f(200) == 10;
    valis ()
  }
}
    
