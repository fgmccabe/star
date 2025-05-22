test.dte{
  import star.
  import star.assert.
  import star.date.

  time2Date(.time(N)) => _time2date(N).

  -- 7-tuples
  public implementation all s,t,u,v,w,x,y,z ~~ display[s],display[t],display[u],display[v],display[w],display[x], display[y], display[z] |: display[(s,t,u,v,w,x,y,z)] => {
    disp((a,b,c,d,e,f,g,h)) => "($(a),$(b),$(c),$(d),$(e),$(f),$(g),$(h))".
  }

  main:()=>().
  main() => valof{
    showMsg("$(time2Date(now()))");

    D = _date2time(2025,5,21,20,28,45.2,-25200);
    T = _time2date(D);

    showMsg("D = $(D), T = $(T)");

    assert "$(.time(D)):yyyy-mm-dd;" == "2025-05-21";
  }
}
