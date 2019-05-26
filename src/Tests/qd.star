test.qd{
  import star.

  ast ::= nme(integer,string).
  
  isName:(ast) => option[(integer,string)].
  isName(nme(L,S)) => some((L,S)).
  isName(_) default => none.
  
  filterOut:(list[string],list[ast]) => list[string].
  filterOut([],_) => [].
  filterOut([Nm,..As],Q) where (V where (_,Nm)^=isName(V)) in Q =>
    filterOut(As,Q).
  filterOut([Sp,..As],Q) => [Sp,..filterOut(As,Q)].

  show disp(filterOut(["alpha","beta","one","gamma","two"],
      [nme(1,"one"),nme(2,"two"),nme(3,"three")])).

  assert filterOut(["alpha","beta","one","gamma","two"],
    [nme(1,"one"),nme(2,"two"),nme(3,"three")]) == ["alpha","beta","gamma"].
}
