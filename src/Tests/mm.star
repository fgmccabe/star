test.mm{
  import star.

  conc:all e ~~ (list[e],list[e])=>list[e].
  conc(x,y) => let{
    cc:(list[e])=> list[e].
    cc([]) => y.
    cc([e,..l]) => [e,..cc(l)].
  } in cc(x).
  
  assert conc([1,2,3],[4,5])==([1,2,3,4,5]:list[integer]).
  
  main:() => action[integer,()].
  main() => do{
    logMsg(disp(conc([1,2,3],[4,5])));
    return ()
  }
    
  _main(_) =>
    valof main()
}
