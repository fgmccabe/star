test.q3{
  import star.
  import star.script.

  -- Test slightly more complex query expressions

  gender ::= .male | .female.

  parents:cons[(string,cons[string])].
  parents = [("a", ["ab","c","aa"]),
    ("b", ["ab"]),
    ("ab", ["abc"]),
    ("de", ["abc"]),
    ("d", ["de"]),
    ("e", ["de"]),
    ("f", ["a"]),
    ("g", ["f"])].

  gender:map[string,gender].
  gender = ["a" -> .male,
    "b" -> .female,
    "ab" -> .male,
    "de" -> .female,
    "d" -> .male,
    "e" -> .female,
    "f" -> .male,
    "g" -> .male].

  maleOff(


  pp = [X|(X,"ab") in parent || (X,D) in parent && "de".=D].

  -- A different example, filtering positive numbers
  someInts : cons[integer].
  someInts = [0,2,-1,-10,4,6,7,-3].

  pos : cons[integer].
  pos = { X | X in someInts && X>0 }.

  main:() => action[(),()].
  main() => do{
    show "$(parent)";

    assert size(parent)==10;

    show "$(gp)";

    show "$(pp)";

    show "$(pos)"
  }
}
