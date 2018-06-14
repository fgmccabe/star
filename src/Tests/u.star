test.u{
  import star.
  import star.parse.
  import star.uri.

  loop:(()=>(),integer)=>().
  loop(_,0) => ().
  loop(F,N) where F()=._ => loop(F,N-1).

  tt() => loop(() where _.=parse(uriParse,"http://foo.bar.com?query"::list[integer])=>(),1000).

  assert _.=tt().

  assert parse(uriParse,"http://foo.bar.com?query"::list[integer]) ==
    [(absUri("http",netRsrc(server(none,host("foo.bar.com")),relPath([""])),qry("query")),[])].
}
