test.s{
  import star.
  import star.sort.
  import star.script.

  main:() => action[(),()].
  main() => do{
    assert sort(([2,1,4,1,-1]:cons[integer]),(<)) == [-1,1,1,2,4]
  }
}
