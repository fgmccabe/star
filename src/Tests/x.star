test.x{
  import star.core.
  import star.arith.
  import star.assert.

  contract all c,k,v ~~ ixfold[c->>k,v] ::= {
    ixRight:all x,xx ~~ (((k,v,x)=>x throws xx),x,c) => x throws xx.
  }

  implementation all e ~~ ixfold[cons[e] ->> integer,e] => {
    ixRight(F,Z,L) => let{.
      fdr:(cons[e], integer) => x throws xx.
      fdr(.nil,_) => Z.
      fdr(.cons(H,T),Ix) => F(Ix,H,fdr(T,Ix+1)).
   .} in fdr(L,0).
  }

  main:(){}.
  main(){
    assert ixRight(((Ix,X,Ac)=>Ac+X*Ix),0,.cons(1,.cons(2,.cons(3,.nil))))==8
  }
}
  
