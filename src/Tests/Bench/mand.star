test.bench.mandelbrot{
  import star.
  import star.assert.
  import test.lib.timer.

  mandelbrot:(integer)=>integer.
  mandelbrot(sze) => valof{
    sum := 0;
    byteAcc := 0;
    bitNum  := 0;

    try{
      for y in 0..<sze do{
	ci = (2.0 * ((y::float) / (sze::float))) - 1.0;

	for x in 0..<sze do {
	  cr = (2.0 * ((x::float) / (sze::float))) - 1.5;

	  zrzr := 0.0;
	  zi := 0.0;
	  zizi := 0.0;

	  escape := 0;

	  notDone{
	    for z in 0..<50 do{
	      zr = zrzr! - zizi! + cr;
	      zi := 2.0 * zr * zi! + ci;

	      -- preserve recalculation
	      zrzr := zr * zr;
	      zizi := zi! * zi!;

	      if zrzr! + zizi! > 4.0 then {
		escape  := 1;
		break notDone
	      }
	    }
	  };

	  byteAcc := (byteAcc! .<<. 1) + escape!;
	  bitNum += 1;

	  if bitNum! == 8 then {
	    sum := sum! .^. byteAcc!;
	    byteAcc := 0;
	    bitNum  := 0;
	  } else if x == sze - 1 then {
	    byteAcc := byteAcc! .<<. (8 - bitNum!);
	    sum := sum! .^. byteAcc!;
	    byteAcc := 0;
	    bitNum  := 0;
	  };
	};
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
    verfy(mandelbrot(count),count).

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

    
