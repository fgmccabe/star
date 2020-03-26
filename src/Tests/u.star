test.u{
  import star.
  import star.script.
  import star.parse.
  import star.uri.

  loop:(()=>(),integer)=>().
  loop(_,0) => ().
  loop(F,N) where _.=F() => loop(F,N-1).

  tt() => loop(() where _.=parse(uriParse,"http://foo.bar.com?query"::cons[integer])=>(),100).

  main:()=>action[(),()].
  main() => action{
    assert _.=tt();

    assert parse(uriParse,"http://foo.bar.com?query"::cons[integer]) ==
      [(absUri("http",netRsrc(server(.none,host("foo.bar.com")),relPath([""])),qry("query")),[])]
  }
}
