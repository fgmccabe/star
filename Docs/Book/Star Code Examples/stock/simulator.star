simulator is package {
  import stockActor;

  numberOfStocks is 100000;                       -- number of unique stock symbols per exchange

  main() do {
    logMsg(info, "start stock sim"); 
    spawn { generateTicks(StockTicker)};
    generateTicks(StockTicker);
  };
    
  runLevel is true;

  generateTicks(Data) do {
    while runLevel do {
      notify Data with getStockTick() on tick;
    }   	
  } using {
    var ts := 0L;
    getStockTick() is valof {
      ts := ts + 1L;			-- faking a timestamp
      S is "NYSE_" ++ (random(numberOfStocks) as string);
      P is random(100.0);
      V is random(10000);
      valis stockTick{
    		ts=ts;
    		symbol=S;
    		price=P;
    		volume=V;
      } 
    }
  };
	
  StockTicker is actor {
    on (X) on tick do {
      -- request tickCounter's incTickCount to incTickCount();	-- increment tick counter
      incCount();
      stock is stockFactory(X.symbol);  -- look up existing or create new stock actor
      notify stock with X on tick;	-- send tick data to the stock actor
--      delta is query stock with priceChange();	-- query stock actor for price change since last tick
      vwap is query stock's vwap with vwap();
    }
  }

  tickCounter has type actor of {
    incTickCount has type action();	-- request speech action
    getTickCount has type () => long;	-- query speech action
  }
  tickCounter is synchronized actor {
    incTickCount() do {
      tickCount := tickCount + 1L;
    }

    getTickCount() is tickCount;
  } using {
    var tickCount := 0L;		-- "state" variable local to the actor
  }

  var counter := 0L;
  incCount() do sync(counter) { counter := counter+1L; }
  getCount() is counter;

  unwrapLong(long(F)) is F;
  longZero is unwrapLong(0L);
  longOne is unwrapLong(1L);
    
  Monitor is spawn {	-- gather stats from analysis component
    while runLevel do {
      sleep(1000L);
     -- currentTickCount := query tickCounter with getTickCount();
     currentTickCount := getCount();
      currentTime is (now() as long);

      ticksPerSecond is (currentTickCount-previousTickCount)*1000L/(currentTime-previousTime);
      logLine is "Tick Count is $currentTickCount and Stock updates/second is $ticksPerSecond.";
      logMsg(info, logLine);
    		
      previousTickCount := currentTickCount;
      previousTime := currentTime;
    }
  } using {
    var currentTickCount := 0L;
    var previousTickCount := 0L;
    var previousTime := 0L;
  };
}
