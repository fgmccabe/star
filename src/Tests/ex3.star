test.ex3{
  except ::= .except(string).

  contract all x,e ~~ ar[x->>e] ::= {
    times:(x,x)=>x.
    div:(x,x) => x throws e.
    one:x.
  }

  implementation ar[integer->>string] => {
    times(X,Y) => _int_times(X,Y).
    div(x,y) => (try
      _int_div(x,y)
      catch {
	_ => throw "divide by zero"
      }).
    one = 1.
  }

  foo:all x,y ~~ ar[x->>y], throws y |: (x) => x.
  foo(x) => div(x,x).

  bar:all x,y ~~ ar[x->>y], ar[integer->>y] |: (x) => (x,integer).
  bar(x) => (foo(x),foo(3)).

  main:()=>().
  main()=>valof{
    try{
--      Tm = (times:(integer,integer)=>integer);
      Dv = div;
      _logmsg(_stringOf(Dv(3,2),0));
--      _logmsg(_stringOf(div(2,0),0));
    } catch {
      Msg => { _logmsg("out with a #(Msg)"); valis () }
    };
  }
}
