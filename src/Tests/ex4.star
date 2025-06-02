test.ex4{
  tipe ::= .void
  | .anon
  | .v(string)
  | .f(string,integer)
  | .n(string)
  | .tpExp(tipe,tipe)
  | .fn(tipe,tipe)
  | .tuple(cns[tipe])
  | .awl(tipe,tipe)
  | .exist(tipe,tipe)
  | .face(cns[(string,tipe)]).
  
  ar:(tipe)=>integer throws string.
  ar(.void) => throw "no ar for void".
  ar(.anon) => throw "no ar for anon".
  ar(.n(_)) => 0.
  ar(.tpExp(_,A)) => ar(A).
  ar(.fn(A,_)) => ar(A).
  ar(.tuple(Els)) => length(Els).
  ar(.awl(_,T)) => ar(T).
  ar(.exist(_,T)) => ar(T).
  ar(.face(Els)) => length(Els).

  implementation display[tipe] => let{.
    dsp(.void) => "void".
    dsp(.anon) => "_".
    dsp(.v(Nm)) => _str_concat("_",Nm).
    dsp(.f(Nm,Ar)) => _str_multicat(.cons(Nm,.cons("/",.cons(disp(Ar),.nil)))).
    dsp(.n(Nm)) => Nm.
    dsp(.tpExp(O,A)) => _str_multicat(.cons(dsp(O),.cons("[",.cons(disp(A),.cons("]",.nil))))).
    dsp(.fn(A,O)) => _str_multicat(.cons(dsp(A),.cons("=>",.cons(disp(O),.nil)))).
    
    dsp(.tuple(Els)) => _str_multicat(.cons("(",.cons(_str_multicat(dspEls(Els,"")),.cons(")",.nil)))).
    dsp(.awl(V,T)) => _str_multicat(.cons("all ",.cons(dsp(V),.cons("~",.cons(disp(T),.nil))))).
    dsp(.exist(V,T)) => _str_multicat(.cons("ex ",.cons(dsp(V),.cons("~",.cons(disp(T),.nil))))).
    dsp(.face(Flds)) => _str_multicat(.cons("{ ",.cons(_str_multicat(dspFlds(Flds,"")),.cons("}",.nil)))).

    dspEls(.nl,_) => .nil.
    dspEls(.cns(E,L),Sep) => .cons(Sep,.cons(dsp(E),dspEls(L,", "))).

    dspFlds(.nl,_) => .nil.
    dspFlds(.cns((N,T),L),Sep) => .cons(Sep,.cons(N,.cons(":",.cons(dsp(T),dspFlds(L,". "))))).
  .} in {
  disp = dsp
  }

  cns[x] ::= .nl | .cns(x,cns[x]).

  conc:all e ~~ (cns[e],cns[e])=>cns[e].
  conc(.nl,X) => X.
  conc(.cns(E,L),X) => .cns(E,conc(L,X)).

  length:all e ~~ (cns[e]) => integer.
  length(.nl) => 0.
  length(.cns(_,L)) => _int_plus(length(L),1).

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
    plus:(x,x) => x.
    minus:(x,x) => x.
    times:(x,x)=>x.
    div:(x,x) => x throws e.
    zero:x.
    one:x.
  }

  implementation ar[integer->>string] => {
    plus(X,Y) => _int_plus(X,Y).
    minus(X,Y) => _int_minus(X,Y).
    times(X,Y) => _int_times(X,Y).
    div(x,y) => (try
      _int_div(x,y)
      catch {
	_ => throw "divide by zero"
      }).
    zero = 0.
    one = 1.
  }

  main:()=>().
  main()=>valof{
    TV = .awl(.v("x"),.fn(.tuple(.cns(.n("int"),.cns(.v("x"),.nl))),.n("int")));

    _logmsg(disp(TV));

    try{
      _logmsg(disp(ar(TV)));
    } catch {
	Msg => { _logmsg("get got an exception: #(Msg)") }
    };
  }
}
