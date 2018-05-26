test.u{
  import star.
  import star.parse.
  import star.uri.

  assert parse(uriParse,"http://foo.bar.com?query"::list[integer]) ==
    [(absUri("http",netRsrc(server(none,host("foo.bar.com")),relPath([""])),qry("query")),[])].
}
