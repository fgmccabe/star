test.q1{
  import star.
  import star.script.

  visibility ::= .priVate | .pUblic.

  implementation equality[visibility] => {
    .priVate == .priVate => .true.
    .pUblic == .pUblic => .true.
    _ == _ default => .false.
  }

  parent:cons[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
            ("de","abc"),("d","de"),("e","de"),
    ("f","a"),("g","f")].


  status : cons[(string,visibility)].
  status = [("a",.pUblic),("b",.priVate),("c",.priVate),("d",.pUblic)].

--  fp(P) => [X|(P,X) in parent && (P,.pUblic) in status].

  pub = .pUblic.
  
--  fp(P) => (P,pub) in status.
  fp(P) => {? (P,.pUblic) in status ?}.

  main:()=>action[(),()].
  main()=>action{
    show "$(fp("ab"))"
  }
}
