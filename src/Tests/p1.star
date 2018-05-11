star.p1{
  import star.
  import star.parse.

  p:parser[(integer,integer)].
  p = item >>= ((C) =>
      item >>= ((_) =>
      item >>= ((D) =>
      return (C,D)))).

  assert parse(p,[1,2,3]) == [((1,3),[])].
}
