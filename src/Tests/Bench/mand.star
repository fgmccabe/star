test.bench.mandelbrot{
  import star.
  import star.assert.
  import test.lib.timer.

  mandelbrot:(integer)=>integer.
  mandelbrot(sze) => valof{
    sum := 0;
    byteAcc := 0;
    bitNum  := 0;

    y := 0;

    try{
      while y! < sze do{
	ci = (2.0 * ((y!::float) / (sze::float))) - 1.0;
	x := 0;

	while x! < sze do {
	  zrzr := 0.0;
	  zi := 0.0;
	  zizi := 0.0;
	  cr = (2.0 * ((x!::float) / (sze::float))) - 1.5;

	  z := 0;
	  notDone := .true;

	  escape := 0;

	  while notDone! && z! < 50 do {
	    zr = zrzr! - zizi! + cr;
	    zi := 2.0 * zr * zi! + ci;

	    -- preserve recalculation
	    zrzr := zr * zr;
	    zizi := zi! * zi!;

	    if zrzr! + zizi! > 4.0 then {
	      notDone := .false;
	      escape  := 1;
	    };
	    z += 1;
	  };

	  byteAcc := (byteAcc! .<<. 1) + escape!;
	  bitNum += 1;

	  -- Code is very similar for these cases, but using separate blocks
	  -- ensures we skip the shifting when its unnecessary, which is most cases.
	  if bitNum! == 8 then {
	    sum := sum! .^. byteAcc!;
	    byteAcc := 0;
	    bitNum  := 0;
	  } else if x! == sze - 1 then {
	    byteAcc := byteAcc! .<<. (8 - bitNum!);
	    sum := sum! .^. byteAcc!;
	    byteAcc := 0;
	    bitNum  := 0;
	  };

	  x += 1;
	};
	y += 1;

--	logMsg(.info,"y=$(y!)");

      };
      valis sum!
    }
    catch{
      _ do {
	logMsg(.severe, "run-time exception");
	_exit(10)
      }
    };
    valis 0
  }

  test(count) => 
    verfy(trace mandelbrot(count),count).

  verfy(191,500) => .true.
  verfy(50, 750) => .true.
  verfy(128, 1) => .true.
  verfy(_,_) => .false.
    
  main:(integer){}.
  main(Count){
    timer = timer_start(Count, "Mandelbrot benchmark");

    assert test(Count);

    timer_finish(timer);
  }
}

    
