test.f0{

  equality@"defines semantic equality".
  public contract all x ~~ equality[x] ::= {
    (==)@"semantic equality is defined explicitly".

    (==): (x,x)=>boolean.
  }

  (~=)@"semantic inequality defined in terms of equality".
  public (~=):all x ~~ equality[x] |: (x,x)=>boolean.
  x ~= y => ~x==y.
  
  -- Compile fact without core

  public fact:(integer)=>integer.
  fact(0)=>1.
  fact(N)=>times(N,fact(minus(N,1))).

  fib:(integer)=>integer.
  fib(0) => 0.
  fib(N) => valof{
    x := 0;
    y := 1;
    i := 1;
    while less(i!,N) do{
      z = plus(x!,y!);
      x := y!;
      y := z;
      i := plus(i!,1);
    };
    valis y!
  }

  contract all x ~~ ar[x] ::= {
    plus:(x,x)=>x.
    minus:(x,x)=>x.
    times:(x,x)=>x.
  }

  implementation ar[integer] => {
    plus(x,y) => _int_plus(x,y).
    times(x,y) => _int_times(x,y).
    minus(x,y) => _int_minus(x,y).
  }

  contract all x ~~ cmp[x] ::= {
    less:(x,x)=>boolean.
  }

  implementation cmp[integer] => {	
    less(A,B) => _int_lt(A,B).
  }

  foo:(integer)=>integer.
  foo(Ix) => let{.
    bar = $$ jump(Ix).

    jump(0) => 1.
    jump(N) => 0.
  .} in bar!!.

  caseTest(Ix) => case Ix in {
    | 0 => 0
    | 1 => 1
    | 2 => 2
    | _ default => _int_times(2,Ix)
  }

  dspEr(.eINTRUPT) => "eINTRUPT".
  dspEr(.eNOFILE) => "eNOFIL".
  dspEr(.eNOTDIR) => "eNOTDIR".
  dspEr(.eNOTFND) => "eNOTFND".
  dspEr(.eINVAL) => "eINVAL".
  dspEr(.eRANGE) => "eRANGE".
  dspEr(.eNOPERM) => "eNOPERM".
  dspEr(.eFAIL) => "eFAIL".
  dspEr(.eIOERROR) => "eIOERROR".
  dspEr(.eCONNECT) => "eCONNECT".
  dspEr(.eDEAD) => "eDEAD".
  dspEr(.divZero) => "divZero".
  dspEr(.noValue) => "noValue".
  dspEr(.hasValue) => "hasValue".
  dspEr(.eof) => "eof".

  funnyComp((0,0)) => 0.
  funnyComp((1,X)) => X.
  funnyComp((X,1)) => X.
  funnyComp((2,X where less(2,X))) => X.

  condT0:(integer,integer)=>string.
  condT0(A,B) => ((less(A,B) || less(B,A)) ?? "alpha" || "beta").

  condT1:(integer,integer)=>string.
  condT1(A,B) => (~(~less(A,B) || ~less(B,A)) ?? "alpha" || "beta").


  condTest:(integer,integer)=>string.
--  condTest(A,B) => (((.true && less(A,B)) || (less(B,A) && .false)) ?? "alpha" || "beta").
  condTest(A,B) => (((.true && ~ less(A,B)) || ~ (less(B,A) && .true)) ?? "alpha" || "beta").
--  condTest(A,B) => (~(less(B,A) && less(A,B)) ?? "alpha" || "beta").
--  condTest(A,B) => (( ~ less(A,B) || ~ less(B,A)) ?? "alpha" || "beta").

  public exception ::= .exception(string).

  thrw:()=>().
  thrw() => valof{
    try{
      raise .exception("bong");
    } catch exception in {
      .exception(Msg) => { _logmsg(_str_concat("out with a ",Msg)); valis () }
    }
  }

  nothrw:()=>option[string].
  nothrw() => valof{
    try{
      valis .some("no throw")
    } catch exception in {
      .exception(Msg) => { _logmsg(_str_concat("out with a ",Msg)); valis .none }
    }
  }
  

  _main(_) => valof{
    X = fact(4);
    _logmsg("hello world");
    _logmsg(_stringOf(X,0));
    _logmsg(_stringOf(fib(32),0));

    _logmsg(dspEr(.eNOFILE));
    _logmsg(dspEr(.eINTRUPT));

    thrw();
    _logmsg(_stringOf(nothrw(),0));
    valis ()
  }
}
