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
  parentOf(X) => {! P | (P,X) in parent !}.

  gparentOf:(string)=>option[string].
  gparentOf(X) => {! G | (P,X) in parent && (G,P) in parent !}.

  main:() => action[(),()].
  main() => action{
    show parent;
    show parentOf("ab");

    show gparentOf("abc")
  }
}
