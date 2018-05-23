test.t{
  import star.

  all k,v ~~ kv[k,v] ::= pair(k,v).

  all e ~~ myKv[e] ~> kv[integer,e].

  t:myKv[string].
  t = pair(3,"fred").

  assert t =. pair(K,V) && K==3.
}
