test.s{
  import star.
  import star.sort.
  import star.script.

  main:() => action[(),()].
  main() => action{
    assert sort(([2,1,4,1,-1]:cons[integer]),(<)) == [-1,1,1,2,4];

    show (sort(["a","b","aa","ab"],(<)):cons[string]);

    assert disp(sort(["a","b","aa","ab"],(<)):cons[string])=="[\"a\",\"aa\",\"ab\",\"b\"]"
  }
}
