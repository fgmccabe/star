test.j{
  import star.

  public json ::=
    .jNull |
      jSeq(cons[json]) .


  public implementation equality[json] => {.
    T1 == T2 => equalJson(T1,T2).
  .}

  equalJson:(json,json)=>boolean.
  equalJson(.jNull,.jNull) => .true.
  equalJson(jSeq(L1),jSeq(L2)) => L1==L2.
  equalJson(_,_) => .false.

}
