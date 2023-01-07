test.j{
  import star.
  import star.json.
  import star.parse.
  import star.script.

  main:()=>().
  main()=>valof{
    assert "11"::json == .jNum(11.0);

    assert "[1]"::json == .jSeq([.jNum(1.0)]);

    show "[1,2]" :: json;
    assert "[1,2]"::json == .jSeq([.jNum(1.0),.jNum(2.0)]);

    assert "\"alpha\""::json == .jTxt("alpha");

    assert "[\"alpha\"]"::json == .jSeq([.jTxt("alpha")]);

    assert "{}"::json == .jColl([]);

    assert "{\"alpha\":1}"::json == .jColl({"alpha"->.jNum(1.0)});

    assert "{\"beta\":2}"::json == .jColl({"beta"->.jNum(2.0)});

    assert ({"alpha"->.jNum(1.0),"beta"->.jNum(2.0)}:map[string,json]) ==
      {"alpha"->.jNum(1.0),"beta"->.jNum(2.0)};

    show disp("{\"alpha\":1,\"beta\":2}"::json);

    assert "{\"alpha\":1,\"beta\":2}"::json ==
      .jColl({"alpha"->.jNum(1.0),"beta"->.jNum(2.0)});

    show disp("{\"alpha\":1,\"beta\":[2,null]}"::json);

    assert "{\"alpha\":1,\"beta\":[2,null]}"::json ==
      .jColl({"alpha"->.jNum(1.0),"beta"->.jSeq([.jNum(2.0),.jNull])});

    str = """
  {
	"star.core": {
		"1.0.0": {
			"source": "file:/Users/fgm/Projects/cafe/src/Star/core.star",
			"code": "star.core5751615246627866486.cafe"
		}
	}
}
    """;

    show disp(str::json);
    valis ()
  }
}
