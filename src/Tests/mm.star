test.mm{
  import star.
  import star.script.

  conc:all e ~~ (list[e],list[e])=>list[e].
  conc(x,y) => let{
    cc:(list[e])=> list[e].
    cc([]) => y.
    cc([e,..l]) => [e,..cc(l)].
  } in cc(x).
  
  
  main:() => action[(),()].
  main() => do{
    logMsg(disp(conc([1,2,3],[4,5]))::string);

    assert conc([1,2,3],[4,5])==([1,2,3,4,5]:list[integer]);
    
    valis ()
  }
}
