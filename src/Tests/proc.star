test.proc{
  import star.
  import star.assert.

  -- Test the new procedure notation


  -- A simple procedure type
  do1: (integer){}.
  do1(Ix){
    show "do 1 with $(Ix)"
  }

  -- A procedure type with a throws clause
  do2: (string,integer){} throws exception.
  do2(Msg,Code){
    show "Try to divide";

    do1(Code/0);
  }

  /*
  v1:(string)=>integer.
  v1(S) => valof{
    try{
      do2("there",42) -- should report a syntax error
    } catch {
      _ => valis 42
    }
  }
  */

  -- v2:(string)=>integer.
  -- v2(S) => valof{
  --   try{
  --     do2("there",42)
  --   } catch {
  --     _ do valis 42
  --   };
  --   valis 43
  -- }

  -- v:all e ~~ ((e){}) => (e)=>().
  -- v(P) => (X)=> valof{
  --   P(X);
  --   valis ()
  -- }

  -- p:all e ~~ ((e)=>()) => (e){}.
  -- p(F) => (X){ F(X) }

  -- q:all e ~~ ((e)=>()) => (e,e){}.
  -- q(F) => (X,_){ F(X) }
  

  visitList:all e ~~ (cons[e],(e){}){}.
  visitList([],_) do {}.
  visitList([E,..Es],P) do{
    P(E);
    visitList(Es,P)
  }

  _main:(cons[string]) => integer.
  _main([]) => main(41).
  _main([A,.._]) => main(A:?integer).

  main:(integer)=>integer.
  main(Ix) => valof{
    do1(Ix);

    -- try{
    --   do2("hello",42);
    --   do1(43)
    -- } catch {
    -- | .exception(M) do logMsg(.info,"we got an exception: #(M)")
    -- };

    visitList([1,2,10,-10],do1);
    visitList([3,5,7],(X){ show "$(X)" });
    visitList([3,5,7],(X){ assert X > 0 });
    valis 0
  }
}
