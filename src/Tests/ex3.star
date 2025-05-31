test.ex3{
  except ::= .except(string).

  cns[x] ::= .nl | .cns(x,cns[x]).

  conc:all e ~~ (cns[e],cns[e])=>cns[e].
  conc(.nl,X) => X.
  conc(.cns(E,L),X) => .cns(E,conc(L,X)).

  contract all t ~~ display[t] ::= {
    disp:(t)=>string.
  }
  
  implementation display[integer] => {
    disp(X) => _int2str(X).
  }

  implementation all e ~~ display[e] |: display[cns[e]] => let{.
    consDisp(.nl,L) => L.
    consDisp(.cns(X,.nl),L) => .cons(disp(X), L).
    consDisp(.cns(X,R),L) => .cons(disp(X), .cons(",", consDisp(R,L))).
 .} in {
    disp(L) => _str_multicat(.cons("[",consDisp(L,.cons("]",.nil))))
  }
  
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

  foo:all x,y ~~ ar[x->>y] |: (x) => x throws y.
  foo(x) => div(x,x).

  bar:all x,y ~~ ar[x->>y], ar[integer->>y] |: (x) => (x,integer) throws y.
  bar(x) => (foo(x),foo(3)).

  main:()=>().
  main()=>valof{
    _logmsg("$(conc(.cns(3,.cns(2,.cns(1,.nl))),.cns(-1,.cns(-2,.nl))))");
    
    try{
      try{
	Dv = div;
	_logmsg(_stringOf(Dv(3,0),0));
      } catch {
	Msg => { _logmsg("get got an exception: #(Msg)") }
      };
      _logmsg(_stringOf(div(2,0),0));
    } catch {
      Msg => { _logmsg("out with a #(Msg)"); valis () }
    };
  }
}
