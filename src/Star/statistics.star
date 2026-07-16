star.statistics{
  import star.
  import star.heap.
  import star.sort.

  public average:all c,e ~~ arith[e], total[e], generate[c->>e], sizeable[c], coercion[integer,e ->> exception] |= (c) => e throws exception.
  average(S) => total(S)/(size(S)::e).

  public contract all e ~~ total[e] ::= {
    total:all c ~~ generate[c->>e] |= (c) => e.
  }

  public implementation total[integer] => {
    total(S) => valof{
      T := 0;
      for X in S do{
        T := T!+X
      };
      valis T!
    }
  }

  -- Compute sum + error
  fast2sum:(float,float)=>(float,float).
  fast2sum(A,B) => valof{
    S = A+B;
    A1 = S-B;
    B1 = S-A;
    D1 = A-A1;
    D2 = B-B1;
    valis (S,D1+D2)
  }

  -- This tries to minimize fp calculation artifacts
  public implementation total[float] => {
    total(S) => valof{
      sum := 0.0;
      C := 0.0;
      for X in S do{
	(s,c) = fast2sum(sum!,X+C!);
	sum := s;
	C := c;
      };
      valis sum!
    }
  }
  

  public minimum:all c,e ~~ comp[e], reducing[c->>e] |= (c) => e.
  minimum(S) => reduceLeft((A,B)=>(A>B??A||B),S).

  public maximum:all c,e ~~ comp[e], reducing[c->>e] |= (c) => e.
  maximum(S) => reduceLeft((A,B)=>(A>B??A||B),S).

  public median:all c,e ~~ comp[e], arith[e], stream[c->>e], sequence[c->>e], indexed[c->>integer,e],coercion[integer,e ->> exception], sizeable[c] |= (c) => e throws exception.
  median(S) => valof{
    SS = sort(S, (<));
    Sz = size(S);
    if Sz>0 then{
      Hf = Sz/2;
      if isOdd(Sz) then
	valis ?SS[Hf]
      else
      valis (?SS[Hf]+?SS[Hf+1])/(2::e)
    } else
    throw .exception("median requires at least one element")
  }

  public variance: all c,e ~~ arith[e], stream[c->>e], sizeable[c] |= (c) => e throws exception.

  isOdd:(integer)=>boolean.
  isOdd(X) => X.&.1==1.
  
/*

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
*/  
  
}
