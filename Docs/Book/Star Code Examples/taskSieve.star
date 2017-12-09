worksheet{

	-- The sieve as a network of tasks

	sieve(inChannel,results) is valof{
    var nxPrime is valof (wait for recvRv(inChannel));
    perform wait for sendRv(results,nxPrime)
    valis sieve(filter(nxPrime,inChannel),results)
  }

  filter(P,inChannel) is let{
    var outChannel is channel();
    loop() is task{
      while true do {
        var I is valof (wait for recvRv(inChannel));
        if I%P!=0 then -- not a multiple, pass it on
          perform wait for sendRv(outChannel,I)
      }
    };

    { ignore background loop() }
  } in outChannel

  collectTerms(ch,Count) is background task{
    var data := list of [];
    for cx in range(0,Count,1) do {
      var nxt is valof ( wait for recvRv(ch) )
      data := list of [data..,nxt]
    }
    valis data
  }

  primes(Max) is let{
    var naturals is let {
      var natChannel is channel();
      { ignore background task {
          var counter := 3;
          while true do{
            perform wait for sendRv(natChannel,counter);
            counter := counter+2;
          }
        }
      }
    } in natChannel

    var resltCh is channel();

    { ignore background task { valis sieve(naturals, resltCh)}}

  } in valof collectTerms(resltCh,Max)

  show primes(1000)
}