test.x{
  import star.core.
  import star.arith.
  import star.assert.

  -- contract all c,k,v ~~ ixfold[c->>k,v] ::= {
  --   ixRight:all x,xx ~~ (((k,v,x)=>x throws xx),x,c) => x throws xx.
  -- }

  -- implementation all e ~~ ixfold[cons[e] ->> integer,e] => {
  --   ixRight(F,Z,L) => let{.
  --     fdr:(cons[e], integer) => x throws xx.
  --     fdr(.nil,_) => Z.
  --     fdr(.cons(H,T),Ix) => F(Ix,H,fdr(T,Ix+1)).
  --  .} in fdr(L,0).
  -- }

  walk:all x,y ~~ (vld:(x,x)=>boolean) |= (cons[x],y,(x,y)=>y)=>y.
  walk(.nil,X,_) => X.
  walk(.cons(H,T),X,F) where vld(H,H) => walk(T,F(H,X),F).

  contract all s,t ~~ iter[s->>t] ::= {
    _iter:all e ~~ (s,e,(t,e)=>e) => e
  }

  implementation all x ~~ (vld:(x,x)=>boolean)|=iter[cons[x]->>x] => {
    _iter(L,Z,F) => walk(L,Z,F)
  }

  -- main:(){}.
  -- main(){
  --   assert ixRight(((Ix,X,Ac)=>Ac+X*Ix),0,.cons(1,.cons(2,.cons(3,.nil))))==8
  -- }
}
  
