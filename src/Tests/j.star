test.j{
  import star.
  import star.json.
  import star.parse.
  import star.script.

  main:()=>action[(),()].
  main()=>do{
    assert "11"::json == jNum(11.0);

    assert "[1]"::json == jSeq([jNum(1.0)]);

    assert "[1,2]"::json == jSeq([jNum(1.0),jNum(2.0)]);

    assert "\"alpha\""::json == jTxt("alpha");

    assert "[\"alpha\"]"::json == jSeq([jTxt("alpha")]);

    assert "{}"::json == jColl([]);

    assert "{\"alpha\":1}"::json == jColl(["alpha"->jNum(1.0)]);

    assert "{\"beta\":2}"::json == jColl(["beta"->jNum(2.0)]);

    assert (["alpha"->jNum(1.0),"beta"->jNum(2.0)]:map[string,json]) == ["alpha"->jNum(1.0),"beta"->jNum(2.0)];

    show disp("{\"alpha\":1,\"beta\":2}"::json);

    assert "{\"alpha\":1,\"beta\":2}"::json ==
      jColl(["alpha"->jNum(1.0),"beta"->jNum(2.0)]);

    show disp("{\"alpha\":1,\"beta\":[2,null]}"::json);

    assert "{\"alpha\":1,\"beta\":[2,null]}"::json ==
      jColl(["alpha"->jNum(1.0),"beta"->jSeq([jNum(2.0),jNull])]);

    str .= """
  {
	"star.core": {
		"1.0.0": {
			"source": "file:/Users/fgm/Projects/cafe/src/Star/core.star",
			"code": "star.core5751615246627866486.cafe",
			"signature": "n6o6star.core$equality'(k'xar.cme':k't'C(k't')Uz1's't''id':k'a'F(k'a')k'a''•':k'a':k'b':k'c'F(F(k'b')k'c'F(k'a')k'b')F(k'a')k'c'}{'boolean'YlI{}{}'option':k't'YUz1'star.core*option'k't'I{}{}'ss'Yt'star.core*ss'I{}{}}\"n7o7'()7's'star.core#true's'star.core#ss's'star.core#sc's'star.core#ssSeq's'star.core#false's'star.core#none's'star.core#some'n9o9'()9'n3o3'()3's'equality's'star.core$equality's\":k'x'Zc'star.core$equality'(k'x')()I{'=='F(k'x'k'x')l}{}\"n3o3'()3's'comp's'star.core$comp's\":k'x'Zc'star.core$comp'(k'x')()I{'>='F(k'x'k'x')l'<'F(k'x'k'x')l}{}\"n3o3'("
		}
	}
}
    """;

    show disp(str::json)
}
}
