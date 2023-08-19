star.statistics{
  import star.
  import star.heap.

  contract all state,t,res ~~ WindowedFun[state ->> t,res] ::= {
    getResult:((state)=>res).
    validResult:((state)=>boolean).
    addElement:((t,state)=>state).
    removeElement:((t,state)=>state).
  }

  type all t ~~ identityState[t] ::= statisticsIdentity{
    res : t.
    count : integer.
  } | .emptyidentity.

  implementation all t ~~ arith[t],comp[t] |: WindowedFun[identityState[t]->>t, t] => {
    getResult(statisticsIdentity{res=r})=>r.
    getResult(emptyidentity)=>zero.
    validResult(emptyidentity)=>.false.
    validResult(statisticsIdentity{count=c})=> c > zero.
    addElement(elt, emptyidentity)=>statisticsIdentity{count=1.res=elt}.
    addElement(elt, statisticsIdentity{count=c})=>statisticsIdentity{count=c + one. res=elt}.
    removeElement(elt, statisticsIdentity{count=1})=>emptyidentity.
    removeElement(elt, statisticsIdentity{res=r. count=c})=>
      statisticsIdentity{res=r. count = c - 1}.
    removeElement(elt, .emptyidenity)=>.emptyidentity.
  }.

  type all t ~~ countState[t] ::= cnt{
    c : integer.
  } | .emptycount.

  implementation all t ~~ arith[t] |: WindowedFun[countState[t] ->> t, integer]=>{
    getResult(emptycount)=>0.
    getResult(cnt{c=c})=>c.
    validResult(x)=>.true.
    addElement(elt, .emptycount)=>cnt{c=1}.
    addElement(elt, cnt{c=c})=>cnt{c=c + 1}.
    removeElement(elt, .emptycount)=>cnt{c=-1}.
    removeElement(elt, cnt{c=c})=>cnt{c=c - 1}.
  }.

  type all t ~~ sumState[t] ::= sum{
    res : t.
  } | .emptysum.

  implementation all t ~ arith[t] |: WindowedFun[sumState[t] ->>t,t] =>{
    getResult(emptysum)=>zero.
    getResult(sum{res=r})=>r.
    validResult(x)=>.true.
    addElement(elt, emptysum)=>sum{res=elt}.
    addElement(elt, sum{res=r})=>sum{res=r + elt}.
    removeElement(elt, .emptysum)=>.emptysum.
    removeElement(elt, sum{res=r})=>sum{res=r - elt}.
  }

  type all t ~~ averageState[t] ::= avg{
    count : integer.
    total : sumState[t].
  } | .emptyaverage.

  implementation all t ~~ coercion[t,float],arith[t],comp[t] |:
    WindowedFun[averageState[t]->>t,float] =>  let{.
      getRes(.emptyaverage)=>0.0.
      getRes(avg{count=c. total=t})=>(getResult(t)::float) / (c::float).
      validRes(.emptyaverage)=>false.
      validRes(avg{count=c})=>c > zero.
      addElt(elt, .emptyaverage)=>avg{count=1. total=addElement(elt,.emptysum)}.
      addElt(elt, avg{count=c. total=t})=>avg{count=c + 1. total=addElement(elt, t)}.
      removeElt(elt, .emptyaverage)=>.emptyaverage.
      removeElt(elt, avg{count=1})=> .emptyaverage.
      removeElt(elt, avg{count=c. total=t})=>avg{count=c - 1. total=removeElement(elt, t)}.
    .} in {getResult = getRes.
      validResult = validRes.
      addElement = addElt.
      removeElement = removeElt.
    }.

  type all t ~~ maxState[t] ::= statisticsMax {
    resHeap : heap[t].
    removalHeap : heap[t].
  } | .emptymax.

  implementation all t ~~ arith[t], comp[t],equality[t] |: WindowedFun[maxState[t]->>t,t] => {.
    getResult(.emptymax)=>zero.
    getResult(statisticsMax{resHeap=rh})=>peek(rh).
    validResult(.emptymax)=>.false.
    validResult(statisticsMax{resHeap=rh})=>size(rh) > zero.
    addElement(elt, emptymax)=>statisticsMax{resHeap = push(elt, list of {})}.
    addElement(elt, statisticsMax{resHeap=rh.removalHeap=remh})=>statisticsMax{resHeap = push(elt, rh).removalHeap=remh}.
    removeElement(elt, statisticsMax{resHeap=rh.removalHeap=remh})=>valof {
      var newRes := rh.
      var newRemoval := remh.
      if elt = peek(newRes) then {
        newRes := pop(newRes).
	while not isEmpty(newRes) and not isEmpty(newRemoval) and peek(newRes) = peek(newRemoval) do {
	  newRes := pop(newRes).
	  newRemoval := pop(newRemoval).
	}
      } else {
	newRemoval := push(elt, newRemoval).
      }
      valis statisticsMax{resHeap=newRes.removalHeap=newRemoval}.
    }.
    removeElement(elt, m matching statisticsMax{resHeap=rh.removalHeap = list of {}})=>
      elt = peek(rh) ? m substitute {resHeap=pop(rh)} | m substitute {removalHeap=push(elt,list of {})}.
    removeElement(elt, statisticsMax{resHeap=list of {elt}})=>emptymax.
    removeElement(elt, emptymax)=>emptymax.
  }

  type minState of t=>statisticsMin {
    resHeap : list of t.
    removalHeap : list of t.
    removalHeap default=>list of {}.
  } or emptymin.

  implementation WindowedFun over minState of t determines (t, t) where comparable over t 'n equality over t=>{
    getResult(emptymin)=>(0 cast t).
    getResult(statisticsMin{resHeap=rh})=>peek(rh).
    validResult(emptymin)=>false.
    validResult(statisticsMin{resHeap=rh})=>size(rh) > 0.
    addElement(elt, emptymin)=>statisticsMin{resHeap = pushMin(elt, list of {})}.
    addElement(elt, statisticsMin{resHeap=rh.removalHeap=remh})=>statisticsMin{resHeap = pushMin(elt, rh).removalHeap=remh}.
    removeElement(elt, statisticsMin{resHeap=rh.removalHeap=remh})=>valof {
      var newRes := rh.
      var newRemoval := remh.
      {
        if elt = peek(newRes) then {
	  newRes := pop(newRes).
	  while not isEmpty(newRes) and not isEmpty(newRemoval) and peek(newRes) = peek(newRemoval) do {
	    newRes := popMin(newRes).
	    newRemoval := popMin(newRemoval).
	  }
	}
	else {
	  newRemoval := pushMin(elt, newRemoval).
	}
      }
      valis statisticsMin{resHeap=newRes.removalHeap=newRemoval}.
    }
    removeElement(elt, m matching statisticsMin{resHeap=rh.removalHeap = list of {}})=>
      elt = peek(rh) ? m substitute {resHeap=pop(rh)} | m substitute {removalHeap=pushMin(elt,list of {})}.
    removeElement(elt, statisticsMin{resHeap=list of {elt}})=>emptymin.
    removeElement(elt, emptymin)=>emptymin.
    
  }


  type medianState of t=>statsMedian {
    res : t.
    smallHeap : list of t.
    smallHeap default=>list of {}.
    bigHeap : list of t.
    bigHeap default=>list of {}.
    removalMap : dictionary of (t, integer).
    removalMap default=>dictionary of {}.
    count : integer.
    smallRemovedCount : integer.
    smallRemovedCount default=>0.
    bigRemovedCount : integer.
    bigRemovedCount default=>0.
  } or emptymedian.

  balancedMedian(statsMedian{count=c.smallHeap=small.bigHeap=big.bigRemovedCount=bRemCount.smallRemovedCount=sRemCount})=>(size(small) - sRemCount = size(big) - bRemCount) or (c  2 = 0 and (size(small) - sRemCount = size(big) + 1 - bRemCount)).

    -- The idea=>to keep the size of the smallHeap equal to (or one less than)
    -- half the count minus the number of removed elements

    -- When an element=>added or removed that changes the top of
    -- smallHeap, we check to see if the element at the top of
    -- smallHeap=>in removalMap.  If it is, we pop the element from
    -- the heap, decrease its count in the removalMap, and rebalance.
    -- We continue doing this until we get an element at the top of
    -- the smallHeap that=>either not in the removalMap or has a
    -- count of zero.
  rebalanceMedian : (medianState of t where comparable over t 'n equality over t) => medianState of t.
  rebalanceMedian(s matching statsMedian{smallHeap=small.bigHeap=big.res=r} where size(small) = 0) is
    size(big) = 0 ? s | rebalanceMedian(s substitute{smallHeap=push(r, small).res=peek(big).bigHeap=pop(big)}).
  rebalanceMedian(s matching statsMedian{smallHeap=small.bigHeap=big.res=r} where size(big) = 0) is
    size(small) > 1 ? rebalanceMedian(s substitute{smallHeap=pop(small).bigHeap=pushMin(r, big).res=peek(small)}) | s.
  rebalanceMedian(s)=>let {
    smallTop=>peek(s.smallHeap).
    bigTop=>peek(s.bigHeap).
    newS=>case s in {
      a matching statsMedian{count=c.res=r.smallHeap=small.bigHeap=big.removalMap=rem.smallRemovedCount=remCount} where (sm where sm=smallTop) -> X in rem and X > 0=>a substitute {smallHeap=pop(small).removalMap=_set_indexed(rem, smallTop, X - 1).smallRemovedCount=remCount - 1}.
      a matching statsMedian{count=c.res=r.smallHeap=small.bigHeap=big.removalMap=rem.bigRemovedCount=remCount} where (bt where bt=bigTop) -> X in rem and X > 0=>a substitute {bigHeap=pop(big).removalMap=_set_indexed(rem, bigTop, X - 1).bigRemovedCount=remCount - 1}.
      a matching statsMedian{count=c.res=r.smallHeap=small.bigHeap=big.removalMap=rem.smallRemovedCount=sRemCount.bigRemovedCount=bRemCount} where size(small) - sRemCount < size(big) - bRemCount=>a substitute {smallHeap=push(r, small).res=bigTop.bigHeap=popMin(big)}.
      a matching statsMedian{count=c.res=r.smallHeap=small.bigHeap=big.removalMap=rem.smallRemovedCount=sRemCount.bigRemovedCount=bRemCount} where size(small) - sRemCount > size(big) - bRemCount and not balancedMedian(a)=>a substitute {smallHeap=pop(small).res=smallTop.bigHeap=pushMin(r, big)}.
      _ default=>s.
    }.
  } in (balancedMedian(newS) ? newS | rebalanceMedian(newS)).

  implementation WindowedFun over medianState of t determines (t, t) where comparable over t 'n equality over t=>{
    getResult(emptymedian)=>(0 cast t).
    validResult(emptymedian)=>false.
    validResult(statsMedian{count=c})=>c > 0.
    removeElement(elt, emptymedian)=>emptymedian.
    removeElement(elt, statsMedian{count=1})=>emptymedian.
    addElement(elt, emptymedian)=>statsMedian{count=1.res=elt}.
    getResult(statsMedian{res=r})=>r.
    addElement(elt, s matching statsMedian{smallHeap=small.count=c.bigHeap=big.res=r})=>let {
      x=>elt < r ? s substitute{count=c + 1.smallHeap=push(elt, small)} | s substitute{count=c + 1.bigHeap=pushMin(elt, big)}.
    } in rebalanceMedian(x).
    removeElement(elt, s matching statsMedian{removalMap=rem.smallRemovedCount=remCount.count=c.res=r} where elt < r)=>let {
      x=>s substitute{count=c - 1.smallRemovedCount=remCount + 1.removalMap=_set_indexed(rem, elt, (rem[elt] default 0) + 1)}
    } in rebalanceMedian(x).
    removeElement(elt, s matching statsMedian{removalMap=rem.bigRemovedCount=remCount.count=c.res=r} where elt > r)=>let {
      x=>s substitute{count=c - 1.bigRemovedCount=remCount + 1.removalMap=_set_indexed(rem, elt, (rem[elt] default 0) + 1)}
    } in rebalanceMedian(x).
    removeElement(elt, s matching statsMedian{removalMap=rem.count=c.res=r.smallHeap=small} where elt = r)=>let {
      x=>s substitute{count=c - 1.res=peek(small).smallHeap=pop(small)}
    } in rebalanceMedian(x).
  }.

  type constantState of t=>constant {
    value : t.
  } or emptyconstant.

  implementation WindowedFun over constantState of t determines (x, t)=>{
    getResult(constant{value=val})=>val.
    getResult(emptyconstant)=>(0 cast t).
    validResult(constant{value=val})=>true.
    validResult(emptyconstant)=>false.
    addElement(elt, x)=>x.
    removeElement(elt, x)=>x.
  }

  type varianceState of t=>variance {
    averageOfSquares : averageState of t.
    average : averageState of t.
    count : integer.
  } or emptyvariance.

  implementation WindowedFun over varianceState of t determines (t, float) where coercion over (t, float) 'n arithmetic over t=>{
    getResult=>getRes.
    validResult=>validRes.
    addElement=>addElt.
    removeElement=>removeElt.
  } using {
    getRes(emptyvariance)=>0.0.
    getRes(variance{averageOfSquares=aos.average=a.count=c})=>let {
      aRes=>getResult(a).
    } in (getResult(aos) - (aRes * aRes)) * ((c::float) / ((c::float) - 1.0)).
    validRes(emptyvariance)=>false.
    validRes(variance{count=1})=>false.
    validRes(variance{average=a.averageOfSquares=aos})=>validResult(a) and validResult(aos).
    addElt(elt, emptyvariance)=>variance{averageOfSquares=addElement(elt * elt, emptyaverage).average=addElement(elt, emptyaverage).count=1}.
    addElt(elt, variance{average=a.averageOfSquares=aos.count=c})=>variance{average=addElement(elt, a).averageOfSquares=addElement(elt * elt, aos).count=c + 1}.
    removeElt(elt, emptyvariance)=>emptyvariance.
    removeElt(elt, variance{average=a.averageOfSquares=aos.count=c})=>variance{average=removeElement(elt, a).averageOfSquares=removeElement(elt * elt, aos).count=c - 1}.
  }

  type stdDevState of t=>stdDev {
    vari : varianceState of t.
  } or emptystdDev.

  implementation WindowedFun over stdDevState of t determines (t, float) where coercion over (t, float) 'n arithmetic over t=>{
    getResult=>getRes.
    validResult=>validRes.
    addElement=>addElt.
    removeElement=>removeElt.
  } using {
    getRes(emptystdDev)=>0.0.
    getRes(stdDev{vari=v})=>sqrt(getResult(v)).
    validRes(emptystdDev)=>false.
    validRes(stdDev{vari=v})=>validResult(v).
    addElt(elt, emptystdDev)=>stdDev{vari=addElement(elt, emptyvariance)}.
    addElt(elt, stdDev{vari=v})=>stdDev{vari=addElement(elt, v)}.
    removeElt(elt, emptystdDev)=>emptystdDev.
    removeElt(elt, stdDev{vari=v})=>stdDev{vari=removeElement(elt, v)}.
  }

  type forecastState of (t, t)=>foreCast {
    count : integer.
    sumXY : sumState of t.
    sumX : sumState of t.
    sumY : sumState of t.
    sumXSquared : sumState of t.
    lastX : t.
    period : t.
  } or emptyforecast {
    period : t.
  }.

  #allFloat(?E1 * ?E2) ==> allFloat(?E1) * allFloat(?E2).
  #allFloat(?E1 / ?E2) ==> allFloat(?E1) / allFloat(?E2).
  #allFloat(?E1 + ?E2) ==> allFloat(?E1) + allFloat(?E2).
  #allFloat(?E1 - ?E2) ==> allFloat(?E1) - allFloat(?E2).
  #allFloat((?E)) ==> allFloat(E).
  #allFloat(?E) ==> (E::float).
  implementation WindowedFun over forecastState of (t, t) determines ((t, t), float) where arithmetic over t 'n coercion over (t, float)=>{
    getResult=>getRes.
    validResult=>validRes.
    addElement=>addElt.
    removeElement=>removeElt.
  } using {
    getRes(emptyforecast{})=>0.0.
    validRes(emptyforecast{})=>false.
    removeElt((a, b), x matching emptyforecast{})=>x.
    addElt((x, y), emptyforecast{period=p})=>foreCast{
      count=1.
      sumX=addElement(x, emptysum).
      sumY=addElement(y, emptysum).
      sumXY=addElement(x * y, emptysum).
      sumXSquared=addElement(x * x, emptysum).
      lastX=x.
      period=p.
    }.
    validRes(foreCast{count=c})=>c != 0.
    getRes(foreCast{count=c.sumXY=sxy.sumX=sx.sumY=sy.sumXSquared=sx2.lastX=lastX.period=period})=>let {
      m : float.
      m=>allFloat((c * getResult(sxy) - getResult(sx) * getResult(sy)) / (c * getResult(sx2) - getResult(sx) * getResult(sx))).
      b : float.
      b=>allFloat((getResult(sy) - m * getResult(sx)) / c).
    } in m * ((lastX::float) + (period::float)) + b.
    addElt((x, y), foreCast{period=p.sumX=sx.sumY=sy.sumXY=sxy.sumXSquared=sx2.count=c})=>foreCast{
      count=c + 1.
      sumX=addElement(x, sx).
      sumY=addElement(y, sy).
      sumXY=addElement(x * y, sxy).
      sumXSquared=addElement(x * x, sx2).
      lastX=x.
      period=p.
    }.
    removeElt((x, y), f matching foreCast{sumX=sx.sumY=sy.sumXY=sxy.sumXSquared=sx2.count=c})=>f substitute {
      count=c - 1.
      sumX=removeElement(x, sx).
      sumY=removeElement(y, sy).
      sumXY=removeElement(x * y, sxy).
      sumXSquared=removeElement(x * x, sx2).
    }.
  }.

  -- covariance using pairs
  -- doing this with two streams would be trickier
  type covarianceState of (t, t)=>coVariance {
    xavg : averageState of t.
    yavg : averageState of t.
    xyavg : averageState oft.
  } or emptycovariance.

  implementation WindowedFun over covarianceState of (t, t) determines ((t, t), float) where arithmetic over t 'n coercion over (t, float)=>{
    getResult=>getRes.
    validResult=>validRes.
    addElement=>addElt.
    removeElement=>removeElt.
  } using {
    getRes(emptycovariance)=>0.0.
    validRes(emptycovariance)=>false.
    addElt((x,y), emptycovariance)=>coVariance {
      xavg=addElement(x, emptyaverage).
      yavg=addElement(y, emptyaverage).
      xyavg=addElement(x * y, emptyaverage).
    }.
    removeElt((x,y), emptycovariance)=>emptycovariance.
    getRes(coVariance{xavg=xavg.yavg=yavg.xyavg=xyavg})=>getResult(xyavg) - getResult(xavg) * getResult(yavg).
    validRes(coVariance{xavg=xavg.yavg=yavg.xyavg=xyavg})=>validResult(xyavg) and validResult(xavg) and validResult(yavg).
    addElt((x, y), coVariance{xavg=xavg.yavg=yavg.xyavg=xyavg})=>coVariance {
      xavg=addElement(x, xavg).
      yavg=addElement(y, yavg).
      xyavg=addElement(x * y, xyavg).
    }.
    removeElt((x, y), coVariance{xavg=xavg.yavg=yavg.xyavg=xyavg})=>coVariance {
      xavg=removeElement(x, xavg).
      yavg=removeElement(y, yavg).
      xyavg=removeElement(x * y, xyavg).
    }.
  }.

  -- Pearson's correlation
  -- https://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient

  type correlationState of (t, t)=>correlatioN {
    cov : covarianceState of (t, t).
    stdDevX : stdDevState of t.
    stdDevY : stdDevState of t.
  } or emptycorrelation.

  implementation WindowedFun over correlationState of (t, t) determines ((t, t), float) where arithmetic over t 'n coercion over (t, float)=>{
    getResult=>getRes.
    validResult=>validRes.
    addElement=>addElt.
    removeElement=>removeElt.
  } using {
    getRes(emptycorrelation)=>0.0.
    validRes(emptycorrelation)=>false.
    addElt((x, y), emptycorrelation)=>correlatioN {
      cov=addElement((x, y), emptycovariance).
      stdDevX=addElement(x, emptystdDev).
      stdDevY=addElement(y, emptystdDev).
    }.
    removeElt((x, y), emptycorrelation)=>emptycorrelation.
    getRes(correlatioN{cov=c.stdDevX=sx.stdDevY=sy} where getResult(sx) != 0.0 and getResult(sy) != 0.0)=>getResult(c) / (getResult(sx) * getResult(sy)).
    getRes(correlatioN{cov=c.stdDevX=sx.stdDevY=sy} where getResult(sx) = 0.0 or getResult(sy) = 0.0)=>0.0.
    validRes(correlatioN{cov=c.stdDevX=sx.stdDevY=sy})=>getResult(sx) != 0.0 and getResult(sy) != 0.0 and validResult(c) and validResult(sx) and validResult(sy).
    addElt((x, y), correlatioN{cov=c.stdDevX=sx.stdDevY=sy})=>correlatioN {
      cov=addElement((x, y), c).
      stdDevX=addElement(x, sx).
      stdDevY=addElement(y, sy).
    }.
    removeElt((x, y), correlatioN{cov=c.stdDevX=sx.stdDevY=sy})=>correlatioN {
      cov=removeElement((x, y), c).
      stdDevX=removeElement(x, sx).
      stdDevY=removeElement(y, sy).
    }.
  }.

  type averageDeviationState of t=>avgDev {
    overallAvg : averageState of t.
    belows : list of t.
    aboves : list of t.
    exactlies : list of t.
    removesAbove : list of t.
    removesBelow : list of t.
    belowAvg : averageState of t.
    aboveAvg : averageState of t.
  } or emptyaverageDeviation.

  implementation WindowedFun over averageDeviationState of t determines (t, float) where arithmetic over t 'n coercion over (t, float) 'n comparable over t 'n equality over t=>{
    getResult=>getRes.
    validResult=>validRes.
    addElement=>addElt.
    removeElement=>removeElt.
  } using {
    getRes(emptyaverageDeviation)=>0.0.
    validRes(emptyaverageDeviation)=>false.
    removeElt(x, emptyaverageDeviation)=>emptyaverageDeviation.
    addElt(x, emptyaverageDeviation)=>let {
      newOverall=>addElement(x, emptyaverage).
      newExactlyAvg=>list{x}.
    } in avgDev{overallAvg=newOverall.exactlies=newExactlyAvg.belows=list{}.aboves=list{}.removesAbove=list{}.removesBelow=list{}.belowAvg=emptyaverage.aboveAvg=emptyaverage}.
    getRes(avgDev{overallAvg=oa.belowAvg=bl matching avg{}.aboveAvg=ab matching avg{}.exactlies=ex})=>let {
      weightedBL=>allFloat((getResult(oa) - getResult(bl)) * bl.count).
      weightedAB=>allFloat((getResult(ab) - getResult(oa)) * ab.count).
    } in allFloat((weightedBL + weightedAB) / (ab.count + bl.count + size(ex))).
    getRes(avgDev{belowAvg=emptyaverage.aboveAvg=emptyaverage.exactlies=ex})=>0.0.
    validRes(avgDev{belowAvg=bl matching avg{}.aboveAvg=ab matching avg{}.exactlies=ex})=>ab.count > 0 or bl.count > 0 or size(ex) > 0.
    validRes(avgDev{belowAvg=emptyaverage.aboveAvg=emptyaverage.exactlies=ex})=>size(ex) > 0.
    -- TODO: addElt and removeElt
    addElt(x, d matching avgDev{overallAvg=oa.belows=belows.aboves=aboves.exactlies=ex.removesAbove=ra.removesBelow=rb.belowAvg=bl.aboveAvg=aa})=>let {
      overall=>getResult(oa).
      newOverallState=>addElement(x, oa).
      newOverall=>getResult(newOverallState).
      newAvgDev=>(x::float) > newOverall ? rebalanceAvgDev(d substitute {aboves=pushMin(x, aboves).aboveAvg=addElement(x, aa).overallAvg=addElement(x, oa)}) | ((x::float) < newOverall ? rebalanceAvgDev(d substitute {belows=push(x, belows).belowAvg=addElement(x, bl).overallAvg=addElement(x, oa)}) | rebalanceAvgDev(d substitute {exactlies=_apnd(ex, x).overallAvg=addElement(x, oa)})).
    } in newAvgDev.
    removeElt(x, d matching avgDev{overallAvg=oa.belows=belows.aboves=aboves.exactlies=ex.removesAbove=ra.removesBelow=rb.belowAvg=bl.aboveAvg=aa})=>let {
      overall=>getResult(oa).
      newOverallState=>removeElement(x, oa).
      newOverall=>getResult(newOverallState).
      removeFirst(list{_...y})=>y.
      removeFirst(list{})=>list{}.
      newAvgDev=>(x::float) > newOverall ? rebalanceAvgDev(d substitute {aboveAvg=removeElement(x, aa).overallAvg=newOverallState.removesAbove=pushMin(x,ra)}) | ((x::float) < newOverall ? rebalanceAvgDev(d substitute {removesBelow=push(x, rb).belowAvg=removeElement(x, bl).overallAvg=newOverallState}) | rebalanceAvgDev(d substitute {exactlies=removeFirst(ex).overallAvg=newOverallState})).
    } in newAvgDev.
  }.

  rebalanceAvgDev : (averageDeviationState of t where coercion over(t, float) 'n arithmetic over t 'n comparable over t 'n equality over t) => averageDeviationState of t.
  rebalanceAvgDev(d matching avgDev{overallAvg=oa.belows=belows.aboves=aboves.exactlies=ex.removesAbove=ra.removesBelow=rb.belowAvg=bl.aboveAvg=aa})=>let {
    ad1=>not isEmpty(ra) and not isEmpty(aboves) and peek(ra) = peek(aboves) ? rebalanceAvgDev(d substitute {aboves=popMin(aboves).removesAbove=popMin(ra)}) | d.
    ad2=>not isEmpty(rb) and not isEmpty(belows) and peek(rb) = peek(belows) ? rebalanceAvgDev(ad1 substitute {belows=pop(belows).removesBelow=popMin(rb)}) | d.
    res=>getResult(oa).
    rebalanceExacts(list{ex1...exrest})=>validResult(oa) and res != (ex1::float) ? ((ex1::float) < res ? rebalanceAvgDev(d substitute {belows=push(ex1, belows).belowAvg=addElement(ex1, bl).exactlies=exrest}) | rebalanceAvgDev(d substitute {aboves=pushMin(ex1, belows).aboveAvg=addElement(ex1, aa).exactlies=exrest})) | ad2.
    rebalanceExacts(list{})=>ad2.
    ad3=>rebalanceExacts(ex).
    ad4=>not isEmpty(aboves) and validResult(oa) and (peek(aboves)::float) < res ? rebalanceAvgDev(d substitute {belows=push(peek(aboves), belows).aboves=pop(aboves).belowAvg=addElement(peek(aboves), bl).aboveAvg=removeElement(peek(aboves), aa)}) | ad3.
    ad5=>not isEmpty(belows) and validResult(oa) and (peek(belows)::float) > res ? rebalanceAvgDev(d substitute {aboves=pushMin(peek(belows), aboves).belows=pop(belows).aboveAvg=addElement(peek(belows), aa).belowAvg=removeElement(peek(belows), bl)}) | ad4.
    newAvgDev=>ad5.
  } in newAvgDev.

  type skewnessState of t=>sKew {
    avgOfSquares : averageState of t.
    avgOfCubes : averageState of t.
    sd : stdDevState of t.
    average : averageState of t.
    n : countState of t.
  } or emptyskewness.

  implementation WindowedFun over skewnessState of t determines (t, float) where arithmetic over t 'n coercion over (t, float)=>{
    getResult=>getRes.
    validResult=>validRes.
    addElement=>addElt.
    removeElement=>removeElt.
  } using {
    getRes(emptyskewness)=>0.0.
    validRes(emptyskewness)=>false.
    removeElt(x, emptyskewness)=>emptyskewness.
    addElt(x, emptyskewness)=>sKew{
      avgOfCubes=>addElement(x * x * x, emptyaverage).
      avgOfSquares=>addElement(x * x, emptyaverage).
      sd=>addElement(x, emptystdDev).
      average=>addElement(x, emptyaverage).
      n=>addElement(x, emptycount).
    }.
    getRes(sKew{avgOfSquares=aos.avgOfCubes=aoc.sd=sd.average=average.n=n})=>let {
      a=>getResult(average).
      a3=>a * a * a.
      s=>getResult(sd).
      s3=>s * s * s.
      c=>getResult(n).
      correction=>((c::float) / ((c - 1)::float)).
    } in (s3 = 0.0 ? 0.0 | correction * (getResult(aoc) - 3.0 * a * getResult(aos) + 2.0 * a3) / s3).
    validRes(sKew{avgOfSquares=aos.avgOfCubes=aoc.sd=sd.average=a})=>validResult(aos) and validResult(aoc) and validResult(sd) and validResult(a) and getResult(sd) != 0.0.
    removeElt(x, sKew{avgOfSquares=aos.avgOfCubes=aoc.sd=s.average=aver.n=N})=>sKew {
      avgOfCubes=>removeElement(x * x * x, aoc).
      avgOfSquares=>removeElement(x * x, aos).
      sd=>removeElement(x, s).
      average=>removeElement(x, aver).
      n=>removeElement(x, N).
    }.
    addElt(x, sKew{avgOfSquares=aos.avgOfCubes=aoc.sd=s.average=aver.n=N})=>sKew {
      avgOfSquares=>addElement(x * x, aos).
      avgOfCubes=>addElement(x * x * x, aoc).
      sd=>addElement(x, s).
      average=>addElement(x, aver).
      n=>addElement(x, N).
    }.
  }.

  type kurtosisState of t=>kurTosis {
    n : countState of t.
    mean : averageState of t.
    M2 : sumState of float.
    M3 : sumState of float.
    M4 : sumState of float.
  } or emptykurtosis.

  implementation WindowedFun over kurtosisState of t determines (t, float) where arithmetic over t 'n coercion over (t, float)=>{
    getResult=>getRes.
    validResult=>validRes.
    addElement=>addElt.
    removeElement=>removeElt.
  } using {
    getRes(emptykurtosis)=>0.0.
    validRes(emptykurtosis)=>false.
    removeElt(x, emptykurtosis)=>emptykurtosis.
    addElt(x, emptykurtosis)=>let {
      n=>addElement(x, emptycount).
      mean=>addElement(x, emptyaverage).
      M4=>addElement(0.0, emptysum).
      M3=>addElement(0.0, emptysum).
      M2=>addElement(0.0, emptysum).
    } in kurTosis{n=n.mean=mean.M2=M2.M3=M3.M4=M4}.
    getRes(kurTosis{n=n.M4=M4.M3=M3.M2=M2})=>((getResult(n)::float) * getResult(M4)) / (getResult(M2) * getResult(M2)) - 3.0.
    validRes(kurTosis{n=n.M4=M4.M3=M3.M2=M2})=>validResult(n) and validResult(M4) and validResult(M3) and validResult(M2) and getResult(M2) != 0.0.
    removeElt(x, kurTosis{n=n.M4=M4.M3=M3.M2=M2.mean=mean})=>let {
      newN=>removeElement(x, n).
      newMean=>removeElement(x, mean).
      delta=>getResult(mean) - (x::float).
      nFloat=>(getResult(newN)::float).
      deltaN=>delta / nFloat.
      deltaN2=>deltaN * deltaN.
      term1=>delta * deltaN * (getResult(n)::float).
      newM4=>addElement(term1 * deltaN2 * (nFloat * nFloat - 3.0 * nFloat + 3.0) + 6.0 * deltaN2 * getResult(M2) - 4.0 * deltaN * getResult(M3), M4).
      newM3=>addElement(term1 * deltaN * (nFloat - 2.0) - 3.0 * deltaN * getResult(M2), M3).
      newM2=>addElement(term1, M2).
    } in kurTosis{n=newN.M4=newM4.M3=newM3.M2=newM2.mean=newMean}.
    addElt(x, kurTosis{n=n.M4=M4.M3=M3.M2=M2.mean=mean})=>let {
      newN=>addElement(x, n).
      newMean=>addElement(x, mean).
      delta=>(x::float) - getResult(mean).
      nFloat=>(getResult(newN)::float).
      deltaN=>delta / nFloat.
      deltaN2=>deltaN * deltaN.
      term1=>delta * deltaN * (getResult(n)::float).
      newM4=>addElement(term1 * deltaN2 * (nFloat * nFloat - 3.0 * nFloat + 3.0) + 6.0 * deltaN2 * getResult(M2) - 4.0 * deltaN * getResult(M3), M4).
      newM3=>addElement(term1 * deltaN * (nFloat - 2.0) - 3.0 * deltaN * getResult(M2), M3).
      newM2=>addElement(term1, M2).
    } in kurTosis{n=newN.M4=newM4.M3=newM3.M2=newM2.mean=newMean}.
  }.

  type productState of t=>statProd {
    res : t.
  } or emptyproduct.

  implementation WindowedFun over productState of t determines (t, t) where arithmetic over t=>{
    getResult(emptyproduct)=>(1 cast t).
    validResult(emptyproduct)=>false.
    addElement(elt, emptyproduct)=>statProd{res=elt}.
    removeElement(elt, emptyproduct)=>emptyproduct.
    getResult(statProd{res=r})=>r.
    validResult(statProd{res=r})=>true.
    addElement(elt, statProd{res=r})=>statProd{res=r * elt}.
    removeElement(elt, statProd{res=r})=>statProd{res=(r / elt)}.
  }

  -- One possible implementation of the geometric mean.  The log of
  -- the geometric mean=>also the average of the logs of the inputs.
  -- That would imply a different possible implementation that would
  -- have better characteristics in the sense that it would overflow
  -- less quickly (assuming the input consists of integers), but it is
  -- likely less numerically stable.

  type geomAverageState of t=>gAvg {
    prod : productState of t.
    count : integer.
  } or emptygeomAverage.

  implementation WindowedFun over geomAverageState of t determines (t, float) where arithmetic over t 'n coercion over (t, float)is {
    getResult=>gr.
    validResult=>vr.
    addElement=>ae.
    removeElement=>re.
  } using {
    gr(emptygeomAverage)=>1.0.
    vr(emptygeomAverage)=>false.
    ae(elt, emptygeomAverage)=>gAvg{prod=addElement(elt, emptyproduct).count=1}.
    re(elt, emptygeomAverage)=>emptygeomAverage.
    re(elt, gAvg{count=1})=>emptygeomAverage.
    gr(gAvg{count=c.prod=p})=>(getResult(p)::float) ** (1.0 / (c::float)).
    vr(gAvg{count=c})=>c > 0.
    ae(elt, gAvg{count=c.prod=p})=>gAvg{count=c + 1.prod=addElement(elt, p)}.
    re(elt, gAvg{count=c.prod=p})=>gAvg{count=c - 1.prod=removeElement(elt, p)}.
  }
}


  
  
}
