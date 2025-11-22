test.are{
  -- Test arithmetic that might throw
  import star.
  import star.assert.

  exc ::= .exc(string).

  implementation display[exc] => {
    disp(.exc(M)) => "exc #(M)"
  }

  -- adding contract
  contract all a,e ~~ adding[a->>e] ::= {
    add:(a,a)=> a throws e
  }

  -- -- standard contract for arithmetic
  -- contract all a,e ~~ four[a->>e] ::= {
  --   plus:(a,a)=>a throws e.
  --   minus:(a,a)=>a throws e.
  --   times:(a,a)=>a throws e.
  --   div:(a,a)=>a throws e.
  --   zer:a.
  --   unum:a.
  -- }

  -- contract all e ~~ eqq[e] ::= {
  --   eqq:(e,e)=>boolean.
  -- }
  
  -- implementation four[integer->>exc] => {
  --   plus(X,Y) => _int_plus(X,Y).
  --   minus(X,Y) => _int_minus(X,Y).
  --   times(X,Y) => _int_times(X,Y).
  --   div(X,Y) => (try _int_div(X,Y) catch {_ => throw .exc("divide by zero")}).
  --   zer = 0.
  --   unum = 1.
  -- }.

  implementation adding[integer->>exc] => {
    add(X,Y) => _int_plus(X,Y)
  }

  implementation adding[string->>void] => {
    add(X,Y) => _str_concat(X,Y)
  }

  -- implementation eqq[integer] => {
  --   eqq(X,Y) => _int_eq(X,Y)
  -- }

  -- ff:all x,e ~~ four[x->>e],eqq[x] |=(x)=>x throws e.
  -- ff(X) where eqq(X,zer) =>unum.
  -- ff(N) => times(N,ff(minus(N,unum))).

  -- fi:(integer)=>integer throws exc.
  -- fi(X) where eqq(X,zer) =>unum.
  -- fi(N) => times(N,fi(minus(N,unum))).

  public main:(){}.
  main(){
    show add("hello","world");
    try{
      -- show fi(5);
      -- show ff(5);

      -- show 34.56e10;

      -- start := 123.0e10;
      -- show start!;

      show add(1,2);

      -- show div(10,0);

      show (((X,Y)=>add(X,Y))|:(string,string)=>string)("hello","again");
    } catch {
      E do { show E}
    }
  }
}
  
