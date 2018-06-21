test.j{
  import star.
  import star.json.
  import star.parse.

  assert parse(pJson,"11"::list[integer]) == [(jNum(11.0),[])].

  assert parse(pJson,"[1]"::list[integer]) == [(jSeq([jNum(1.0)]),[])].

  assert parse(pJson,"[1,2]"::list[integer]) == [(jSeq([jNum(1.0),jNum(2.0)]),[])].

  assert parse(pJson,"\"alpha\""::list[integer]) == [(jTxt("alpha"),[])].

  assert parse(pJson,"[\"alpha\"]"::list[integer]) == [(jSeq([jTxt("alpha")]),[])].

  assert parse(pJson,"{}"::list[integer]) == [(jColl([]),[])].

  assert parse(pJson,"{\"alpha\":1,\"beta\":2}"::list[integer]) == [(jColl(["alpha"->jNum(1.0),"beta"->jNum(2.0)]),[])].
}
