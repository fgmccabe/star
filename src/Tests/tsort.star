test.tsort{
  import star.
  import star.topsort.
  import star.script.

  -- Test the topsort implementation

  all x,y ~~ df[x,y] ::= df(x,cons[y]).

  implementation all x ~~ equality[x] |: depends[df[x,x]->>x] => {.
    defined(df(X,_),D)=>X==D.
    references(df(_,R)) => R.
  .}

  implementation all x,y ~~ display[x], display[y] |: display[df[x,y]] => {.
    disp(D) => dispDf(D).
  .}

  dispDf: all x,y ~~ display[x], display[y] |: (df[x,y]) => string.
  dispDf(df(X,Y)) => "($(X),$(Y))".

  implementation all x,y ~~ equality[x], equality[y] |: equality[df[x,y]] => {.
    df(X1,Y1)==df(X2,Y2) => X1==X2 && Y1==Y2.
  .}

  -- lots of little groups

  a1:cons[df[string,string]].
  a1 = [df("one",["one"]), df("two",["two"]), df("three",["three"]), df("four",["four"])].

  -- one big group

  a2:cons[df[string,string]].
  a2 = [df("1",["2"]),df("2",["4"]),df("3",["1"]),df("4",["3"])].

  -- group with a tail

  a3:cons[df[string,string]].
  a3 = [df("alpha",["beta"]),df("beta",["gamma"]),df("gamma",["alpha"]),df("delta",["gamma"])].

  main:()=>action[(),()].
  main() => action{
    show topsort(a1);

    assert size(topsort(a1)) == 4;

    show topsort(a2);

    assert size(topsort(a2)) == 1;

    show topsort(a3);

    assert size(topsort(a3)) == 2
  }
}
