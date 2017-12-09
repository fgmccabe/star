stockActor is package {
  timeWindow is 500000L;                          -- size of VWAP window (clock ticks)

  type stockTick is stockTick {
    ts has type long;                           -- synthetic time
    symbol has type string;
    price has type float;
    volume has type integer;
  }

  type perfLog is perfLog {
    ts has type string;                         -- convert current time to a human readable form
    tickCount has type long;
    ticksPerSecond has type long;
    usedHeap has type float;
  }
  type stock is alias of actor of {               -- declare interface to stock actor
    tick has type stream of stockTick;
    priceChange has type () => float;
    vwap has type () => float;
  };

  type symbol is alias of string;
  type windowSize is alias of long;

  /***
  * stock actor factory
  */
  knownStocks has type ref(dictionary of (symbol, stock));	-- keep track of known stock actors
  var knownStocks := dictionary of {};

  stockFactory has type (symbol) => stock;        -- create or retrieve a stock actor
  stockFactory(S) is get S in knownStocks   -- find existing stock actor
     default valof {                   -- create a stock actor if symbol doesn't exist
       y is aStock(S);
       set S in knownStocks to y;
       valis y;
     };

  /***
  * stock actor
  */
  aStock has type(symbol) => stock;
  aStock(S) is actor {
           -- public speech action interface to the actor
           -- handle notify on tick channel
           on X on tick do {
             updateHistory(X, timeWindow);
             -- logMsg(info,"got update $X for $S");
           };

           -- handle queries for information from the actor
           priceChange() is lastChange;
           vwap() is sum_volumePrice/sum_volume as float;
         } using {                         -- state data and analysis
            /**
            * stock history and current state
            */
           mySymbol is S;                      -- keep symbol around for debugging
           var lastPrice := 0.0;
           var maxTime := 0L;
           var lastChange := 0.0;
           var sum_volume := 0;
           var sum_volumePrice := 0.0;
           history has type ref array of stockTick;
           var history := array of {};          -- a buffer for recent tick history

           /***
            * update history with latest tick and trim to windowSize
            */
           updateHistory has type action (stockTick, windowSize);
           updateHistory(T, W) do {
             if T.ts > maxTime then maxTime := T.ts;

             minTime is maxTime - W;

             -- drop old entries
             while history matches array of {F..;t} and t.ts<minTime do{
               sum_volume := sum_volume - t.volume;
               sum_volumePrice := sum_volumePrice - ((t.volume as float) * t.price);
               history := F;
             };

	     -- add new entry
             if T.ts > minTime then {
               -- add new tick data to history and update state variables
               history := array of {T;..history};
               -- logMsg(info, "Stock $mySymbol history $history");
               sum_volume := sum_volume + T.volume;
               sum_volumePrice := sum_volumePrice + ((T.volume as float) * T.price);
               lastChange := T.price-lastPrice;
               lastPrice := T.price;
             }
           }
         }
}
