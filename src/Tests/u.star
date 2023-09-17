test.u{
  import star.
  import star.script.
  import star.uri.

  loop:(()=>(),integer)=>().
  loop(_,0) => ().
  loop(F,N) where _.=F() => loop(F,N-1).

  tt() => loop(() where _.= "http://foo.bar.com?query"::uri => (),100).

  main:()=> ().
  main() => valof{
    tt();

    assert "http://foo.bar.com?query"::uri ==
      .absUri("http",.netRsrc(.server(.none,.host("foo.bar.com")),.relPath([""])),.qry("query"));

    valis ()
  }
}
