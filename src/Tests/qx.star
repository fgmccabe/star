test.qx{
  import star.
  import star.script.

  -- Test indexed query conditions

  parent:map[string,string].
  parent = ["a"->"ab","b"->"ab",
    "c"->"a", "ab"->"abc", "d"->"d", "e"->"de"].

  longNmes:map[string,string].
  longNmes = { K->V | K->V in parent && size(V)>1 }

  main:() => action[(),()].
  main() => do{
    show "long names $(longNmes)";

    assert longNmes==["a"->"ab","b"->"ab","ab"->"abc", "e"->"de"]
  }
}
