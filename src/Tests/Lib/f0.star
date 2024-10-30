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

  contract all x ~~ cmp[x] ::= {
    less:(x,x)=>boolean.
  }

  implementation cmp[integer] => {
    less(A,B) => _int_lt(A,B)
  }

  -- caseTest(Ix) => case Ix in {
  --   | 0 => 0
  --   | 1 => 1
  --   | 2 => 2
  --   | _ default => _int_times(2,Ix)
  -- }

  -- condT0:(integer,integer)=>string.
  -- condT0(A,B) => ((less(A,B) || less(B,A)) ?? "alpha" || "beta").

  -- condT1:(integer,integer)=>string.
  -- condT1(A,B) => (~(~less(A,B) || ~less(B,A)) ?? "alpha" || "beta").


  condTest:(integer,integer)=>string.
  condTest(A,B) => (((.true && ~ less(A,B)) || ~ (less(B,A) && .false)) ?? "alpha" || "beta").

  -- _main(_) => valof{
  --   _logmsg("hello world");
  --   valis ()
  -- }
}
  
  
  
