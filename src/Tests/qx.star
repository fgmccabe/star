test.qx{
  import star.

  -- Test indexed query conditions

  parent:map[string,string].
  parent = ["a"->"ab","b"->"ab",
    "c"->"a", "ab"->"abc", "d"->"d", "e"->"de"].

  longNmes:map[string,string].
  longNmes = { K->V | K->V in parent && size(V)>1 }

  show "long names $(longNmes)".

  assert longNmes==["a"->"ab","b"->"ab","ab"->"abc", "e"->"de"].
  
}
