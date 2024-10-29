test.f0{
  -- Compile fact without core

  -- public fact:(integer)=>integer.
  -- fact(0)=>1.
  -- fact(N)=>times(N,fact(minus(N,1))).

  -- contract all x ~~ ar[x] ::= {
  --   plus:(x,x)=>x.
  --   minus:(x,x)=>x.
  --   times:(x,x)=>x.
  -- }

  -- implementation ar[integer] => {
  --   plus(x,y) => _int_plus(x,y).
  --   times(x,y) => _int_times(x,y).
  --   minus(x,y) => _int_minus(x,y).
  -- }

  caseTest(Ix) => case Ix in {
    | 0 => 0
    | 1 => 1
    | 2 => 2
    | _ default => _int_times(2,Ix)
  }

  -- _main(_) => valof{
  --   _logmsg("hello world");
  --   valis ()
  -- }
}
  
  
  
