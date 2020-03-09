test.o{
  import star.
  
  qry[t] ::= .noQ | qry(t).

  sameQuery:(qry[string],qry[string])=>boolean.

  sameQuery(qry(S1),qry(S2)) => S1==S2.
  sameQuery(.noQ,.noQ) => .true.
  sameQuery(_,_) => .false.

}
  
