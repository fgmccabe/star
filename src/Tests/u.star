test.u{
  import star.
  import star.assert.
  import star.uri.

  loop:(()=>(),integer)=>().
  loop(_,0) => ().
  loop(F,N) where _.=F() => loop(F,N-1).

  tt:()=> () throws exception.
  tt() => loop(() where _.= "http://foo.bar.com?query":?uri => (),100).

  main:(){}.
  main(){
    try{
      tt();

      assert "http://foo.bar.com?query"::uri ==
	.absUri("http",.netRsrc(.server(.none,.host("foo.bar.com")),.relPath([""])),.qry("query"));
    } catch { Ex do {
	_show("We got an exception $(Ex)");
    }
    }
  }
}
