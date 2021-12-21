test.hndl{
  import star.
  import star.script.

  main:()=>action[(),()].
/*
  -- How it is supposed to look like:

  main()=>action{
    try{
      X .= throw test(4);
      assert X==10;
      assert throw check(X)
    } handle {
      test:all y ~~ ((integer)=>>y).(integer) => y.
      test(K).(A) => K.(A+6)
      check:all y ~~ ((boolean)=>>y).(integer)=>y.
      check(K).(X) => K.(X==10)
    }
  }
  */


  -- stateEffect[I] ::= put((I)=>stateEffect[I]) |
  --   get((())=>stateEffect[I]) |
  --   rtn((I)=>stateEffect[I]).

  -- contract hndlr[E->>s] ::= {
  --   effect:(E,s,(())=>>())=>(s)=>((),s).
  --   retn:(E,(())=>>())=>(s)=>s
  -- }

  -- implementation all e ~~ hdnlr[stateEffect[e]->>e] => {
  --   effect(put(X),

  -- What works now ..
  main()=>action{
    let{
      testHdl[X,Y,e] ::= hdlXX{
	test:((integer)=>>Y,integer)=>integer.
	check:((boolean)=>>Y,integer)=>Y.
	dflt:((X)=>>Y)=>Y.
	exc:((X)=>>Y,()=>e)=>Y.
      }
      Tg = tag().
      HH = hdlXX{
	test(K,A) => K.(A+6).
	check(K,B) => K.(B==10).
	dflt(K) => K.(42).
	exc(K,E) => do{ XX.=(E():integer); show "I threw up $(XX)"}.
      }
    } in 
    {Tg prompt {
--	X .= ((Tg cut K in HH.test(K,4)):integer);
--	assert (Tg cut K in HH.check(K,X));
	-- show (Tg cut K in HH.dflt(K));
	Z .= (Tg cut K in HH.exc(K,()=>42));
	show "should not get here"
      };
      show "I am here"
    }
    }
  }
}
      
