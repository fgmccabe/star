test.mm{
  import star.
  import star.assert.

  conc:all e ~~ (cons[e],cons[e])=>cons[e].
  conc(x,y) => let{.
    cc:(cons[e])=> cons[e].
    cc([]) => y.
    cc([e,..l]) => [e,..cc(l)].
  .} in cc(x).
  
  
  main:() => ().
  main() => valof{
    showMsg(disp(conc([1,2,3],[4,5]))::string);

    assert conc([1,2,3],[4,5])==([1,2,3,4,5]:cons[integer]);
    
    valis ()
  }
}
