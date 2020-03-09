test.qi{
  import star.
  import star.script.

  -- Test query conditions

  parent:cons[(string,string)].
  parent = [("a","ab"),("b","ab"),
	    ("b","c"),("a","c"),
	    ("c","aa"),
	    ("ab","abc"),("de","abc"),
	    ("d","de"),("e","de"),
            ("f","a"),("g","f")].

  m:cons[string].
  m = ["a","ab","d","f"].

  parentOf:(string)=>option[string].
  parentOf(X) where (P,X) in parent => some(P).
  parentOf(_) default => .none.

  gparentOf:(string)=>option[string].
  gparentOf(X) where (P,X) in parent && (G,P) in parent  => some(G).
  gparentOf(_) default => .none.

  main:() => action[(),()].
  main() => do{
    show "parent = $(parent)";
    show "parent of ab = $(parentOf("ab"))";

    show "grandparent of abc = $(parentOf("abc"))"
  }
}
