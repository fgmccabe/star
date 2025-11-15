test.jt13{
  import star.
  import star.assert.

  o:all x,y,z ~~ ((x)=>y,(y)=>z)=>(x)=>z.
  o(F,G) => (X)=>G(F(X)).


  add:(integer,integer)=>integer.
  add(X,Y) => _int_plus(X,Y).

  mul:(integer,integer)=>integer.
  mul(X,Y) => _int_times(X,Y).

  inc:(integer)=>integer.
  inc(X) => add(X,1).

  dbl:(integer)=>integer.
  dbl(X)=>add(X,X).

  main:(){}.
  main(){
    assert add(2,3) == 5;
    show o(dbl,inc)(2);
    show o(inc,dbl)(2);
    
    try{
      _jit_compile("#(__pkg__)@add",2);
      _jit_compile("#(__pkg__)@mul",2);
      _jit_compile("#(__pkg__)@inc",1);
      _jit_compile("#(__pkg__)@inc^",2);
      _jit_compile("#(__pkg__)@dbl",1);
      _jit_compile("#(__pkg__)@dbl^",2);
      _jit_compile("#(__pkg__)@o",2);
      _jit_compile("#(__pkg__)@o^",3);
      _jit_compile("#(__pkg__)λ_0",2);
    } catch {
      | .eNOPERM do showMsg("JIT not enabled")
      | Cde do showMsg("We got errr: $(Cde)")
    };

    assert o(dbl,inc)(2)==5;
    assert o(inc,dbl)(2)==6;
  }
}
  

  
