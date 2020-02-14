test.tl{
  import star.
  import star.script.
--  import star.treelist.

  -- Test the treelist implementation

  T1 : list[integer].
  T1 = iota(1,100).

  main:() => action[(),()].
  main() => do{
    show disp(T1)
  }
}
