star.json{
  import star.
  import star.parse.

  public json ::=
    jTrue | jFalse | jNull |
    jTxt(string) | jColl(map[string,json]) | jSeq(list[json]) |
    jNum(float).

  
}
