test.gr{
  import star.
  import star.assert.

  expr ::= .sum(expr,expr) |
  .sub(expr,expr) |
  .prod(expr,expr) |
  .div(expr,expr) |
  .var(char) |
  .int(integer).

  implementation display[expr] => let{.
    dsp(.sum(L,R)) => "(#(dsp(L)) + #(dsp(R)))".
    dsp(.sub(L,R)) => "(#(dsp(L)) - #(dsp(R)))".
    dsp(.prod(L,R)) => "(#(dsp(L)) * #(dsp(R)))".
    dsp(.div(L,R)) => "(#(dsp(L)) / #(dsp(R)))".
    dsp(.var(V)) => disp(V).
    dsp(.int(Ix)) => disp(Ix).
  .} in {
    disp(E) => dsp(E)
  }

  eval:raises exception |: (expr,map[char,integer]) => integer.
  eval(.sum(L,R),D) => eval(L,D)+eval(R,D).
  eval(.sub(L,R),D) => eval(L,D)-eval(R,D).
  eval(.prod(L,R),D) => eval(L,D)*eval(R,D).
  eval(.div(L,R),D) => eval(L,D)/eval(R,D).
  eval(.var(V),D) where DV ?=D[V] => DV.
  eval(.int(Ix),_) => Ix.
  eval(E,D) => raise .exception("Cant evaluate $(E) in $(D)").

  parseExp:() >> expr --> cons[char].
  parseExp >> E --> exp0 >> E, end.

  exp0 >> E --> sumExp >> E.

  sumExp >> .sum(L,R) --> prodExp >> L, [`+`], sumExp >> R.
  sumExp >> .sub(L,R) --> prodExp >> L, [`-`], sumExp >> R.
  sumExp >> E --> prodExp >> E.

--  prodExp:(cons[char]) => option[(expr,cons[char])].
  prodExp:()>>expr --> cons[char].
  prodExp >> .prod(L,R) --> unitExp >> L, [`*`], prodExp >> R.
  prodExp >> .div(L,R) --> unitExp >> L, [`/`], prodExp >> R.
  prodExp >> E --> unitExp >> E.

  unitExp >> E --> [`(`], exp0 >> E, ([`)`] | {valof{showMsg("missing )");valis .true}}, *> end).
  unitExp >> .int(digitVal(I)) --> [I], {isDigit(I)}.
  unitExp >> .var(A) --> [A], {isLetter(A)}.

  evalStr:raises exception |: (string,map[char,integer]) => integer.
  evalStr(T,D) => valof{
    if (E,_) ?= parseExp(T::cons[char]) then
      valis eval(E,D)
    else
    raise .exception("Cannot parse #(T)")
  }

  main:()=>().
  main() => valof{
    show parseExp([`a`,`*`,`(`,`1`,`+`,`a`,`)`]);

    try{
      show evalStr("a*(1+a)",{`a`->3});
      show evalStr("(1+a)/(1-a)",{`a`->3});
      assert evalStr("a",{`a`->5})==5;
      show evalStr("(1+a)/(1-a)",{`a`->1});

    } catch exception in {
      .exception(M) => {
	showMsg("arithmetic went wrong: #(M)");
      }
    };
    try{
      show evalStr("(1+a)/(1-a",{`a`->1});
    } catch exception in {
      .exception(M) => {
	showMsg("arithmetic went wrong: #(M)");
      }
    };
    valis ()
  }
}

