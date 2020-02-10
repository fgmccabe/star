test.tsort{
  import star.
  import star.topsort.

  -- Test the topsort implementation

  all x,y ~~ df[x,y] ::= df(x,list[y]).

  implementation all x ~~ equality[x] |: depends[df[x,x]->>x] => {.
    defined(df(X,_),D)=>X==D.
    references(df(_,R)) => R.
  .}

  implementation all x,y ~~ display[x], display[y] |: display[df[x,y]] => {.
    disp(D) => dispDf(D).
  .}

  dispDf: all x,y ~~ display[x], display[y] |: (df[x,y]) => ss.
  dispDf(df(X,Y)) => ssSeq([ss("("),disp(X),ss(","),disp(Y),ss(")")]).

  implementation all x,y ~~ equality[x], equality[y] |: equality[df[x,y]] => {.
    df(X1,Y1)==df(X2,Y2) => X1==X2 && Y1==Y2.
  .}

  -- lots of little groups

  a1:list[df[string,string]].
  a1 = [df("one",["one"]), df("two",["two"]), df("three",["three"]), df("four",["four"])].

  show disp(topsort(a1)).

  assert size(topsort(a1)) == 4.

  -- one big group

  a2:list[df[string,string]].
  a2 = [df("1",["2"]),df("2",["4"]),df("3",["1"]),df("4",["3"])].

  show disp(topsort(a2)).

  assert size(topsort(a2)) == 1.

  -- group with a tail

  a3:list[df[string,string]].
  a3 = [df("alpha",["beta"]),df("beta",["gamma"]),df("gamma",["alpha"]),df("delta",["gamma"])].

  show disp(topsort(a3)).

  assert size(topsort(a3)) == 2.
}
