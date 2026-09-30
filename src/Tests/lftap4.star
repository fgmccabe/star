test.lftap4{
  import star.
  import star.assert.

  -- Minimal form of the liftAppend problem.
  --
  -- bus is a quantified, constrained *variable* whose value is a function.
  -- It normalizes to a definition taking just the dictionary:
  --     bus(D) => <closure>
  -- but a call bus(X,Y) normalizes to one call carrying the dictionary and
  -- the arguments together:
  --     bus(D, X, Y)
  -- so the inliner sees 3 arguments against 1 parameter.

  flip:all a,b,c ~~ ((a,b)=>c) => (b,a)=>c.
  flip(F) => (Y,X) => F(X,Y).

  sub:all e ~~ arith[e] |= (e,e) => e.
  sub(X,Y) => X-Y.

  bus:all e ~~ arith[e] |= (e,e) => e.
  bus = flip(sub).

  -- Called from a constrained function, passing on its own dictionary
  -- (cf. app3 calling liftAppend).
  use:all e ~~ arith[e] |= (e,e) => e.
  use(X,Y) => bus(X,Y).

  main:(){}.
  main(){
    show bus(3,10);
    assert bus(3,10)==7;          -- direct, concrete dictionary
    assert use(3,10)==7;          -- via a constrained caller

    show bus(3.0,10.0);
    assert use(3.0,10.0)==7.0;    -- at a second type
  }
}
