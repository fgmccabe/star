test.u2{
  import star.
  import star.assert.
  import star.uri.

  main:()=>().
  main()=>valof{
--    show parseUri("https://fred@foo.bar.com:9090/local/path?query");

    show parseUri("file:/Users/fgm/Compiler");
    show parseUri("file:/Users/fgm/Projects/star/src/Compiler/");

    assert .absUri("https",.netRsrc(.server(.some(.user("fred")),
	  .hostPort("foo.bar.com","9090")),
     	.absPath(["local","path"])),.qry("query")) ?= parseUri("https://fred@foo.bar.com:9090/local/path?query");
    valis ()
  }
}
