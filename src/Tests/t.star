test.t{
  import star.
  import star.script.

  all k,v ~~ kv[k,v] ::= pair(k,v).

  all e ~~ myKv[e] ~> kv[integer,e].

  t:myKv[string].
  t = pair(3,"fred").

  main:() => action[(),()].
  main() => action{
    assert pair(K,V).=t && K==3
  }
}
