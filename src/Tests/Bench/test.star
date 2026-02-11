test.bench.test{
  import star.
  import star.assert.

  import test.bench.bounce.
  import test.bench.collections.
  import test.bench.hanoi.
  import test.bench.list.
  import test.bench.mandelbrot.
  import test.bench.nbody.
  import test.bench.sieve.

  geomean:(cons[float])=>float throws exception.
  geomean(Vls) => { (*) <* El <* 1.0 | El in Vls } ** (1.0/(size(Vls)::float)).

  runTests:() => cons[float].
  runTests() =>
    [bounceBenchTest(),
    hanoiBenchTest(),
    listBenchTest(),
    mandelBenchTest(),
    nbodyBenchTest(),
    sieveBenchTest(),..
    collectionBenchTest()].

  main:(){}.
  main(){
    try{
      show geomean(runTests());
    } catch {
      M do logMsg(.severe, "We got exception $(M)")
    }
  }
}


  
