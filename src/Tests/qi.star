test.qi{
  import star.

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

  show "parent = $(parent)".

  parentOf:(string)=>option[string].
  parentOf(X) where (P,X) in parent => some(P).
  parentOf(_) default => none.

  show "parent of ab = $(parentOf("ab"))".

  gparentOf:(string)=>option[string].
  gparentOf(X) where (P,X) in parent && (G,P) in parent  => some(G).
  gparentOf(_) default => none.

  show "grandparent of abc = $(parentOf("abc"))".
}
