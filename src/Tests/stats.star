test.stats{
  import star.
  import star.assert.
  import star.statistics.

  main:(){}.
  main(){
    input:cons[float];
    input = [10000.0, 3.14159, 2.71828, -10000.0];
    Ttl = total(input);
    show Ttl
  }
}
