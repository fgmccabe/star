test.j{
  import star.
  import star.json.
  import star.parse.

  assert parse(pJson,"11"::list[integer]) == [(jNum(11.0),[])].

  assert parse(pJson,"1.1"::list[integer]) == [(jNum(1.1),[])].

  assert parse(pJson,"[1,2]"::list[integer]) == [(jSeq([jNum(1.0),jNum(2.0)]),[])].

  assert parse(pJson,"[1,{\"f\":\"alpha\",\"g\":{}}]"::list[integer]) ==
   [(jSeq([jNum(1.0),jColl(["f"->jTxt("alpha"),"g"->jColl([])])]),[])].
}
