star.p1{
  import star.
  import star.parse.

  p:parser[(integer,integer)].
  p = item >>= ((C) =>
      item >>= ((_) =>
      item >>= ((D) =>
      return (C,D)))).

  assert parse(p,[1,2,3]) == [((1,3),[])].

  q:parser[()].
  q = chr(0c() >>= ((_) =>
    chr(0c)) >>= ((_) =>
    return ())).

  assert parse(q,[0c(,0c)]) == [((),[])].

  assert parse(str("alpha"),"alpha0"::list[integer]) == [((),[0c0])].
}
