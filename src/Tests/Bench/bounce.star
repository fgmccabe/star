test.bench.bounce{
  import star.
  import star.assert.

  import test.lib.timer.

  ball::= ball{
    x : ref integer.
    y : ref integer.
    xVel : ref integer.
    yVel : ref integer.
  }

  newBall() => ball{
    x := irand(500).
    y := irand(500).
    xVel := irand(300)-150.
    yVel := irand(300)-150.
  }

  bounce:(ball)=>boolean.
  bounce(B) => valof{
    xLimit = 500;
    yLimit = 500;
    
    bounced := .false;

    B.x += B.xVel!;
    B.y += B.yVel!;

    if B.x!>xLimit then{
      B.x := xLimit;
      B.xVel := -abs(B.xVel!);
      bounced := .true;
    }
    if B.x! < 0 then {
      B.x := 0;
      B.xVel := abs(B.xVel!);
      bounced := .true
    }
    if B.y!>yLimit then{
      B.y := yLimit;
      B.yVel := -abs(B.yVel!);
      bounced := .true;
    }
    if B.y! < 0 then {
      B.y := 0;
      B.yVel := abs(B.yVel!);
      bounced := .true
    }

--    logMsg(.info,bounced! ?? "bounced" || "didn't bounce");
    valis bounced!
  }

  test()=> valof{
    balls : cons[ball];
    seed(123456);
    balls = { newBall() | ix in 0..<100 };

    bounces := 0;

    for ix in 0..< 50 do{
      for B in balls do {
	if bounce(B) then{
	  bounces += 1
	}
      }
    };

    valis bounces!
  }

  main:(integer){}.
  main(Count){
    timer = timer_start(Count, "List tail benchmark");

    for I in 0..<Count do{
      assert test()==1249
    }

    timer_finish(timer);
  }
}

  
      


    

    
