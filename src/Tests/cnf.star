test.cnf{
  import star.
  import test.ac0.

  parent:cons[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
    ("de","abc"),("d","de"),("e","de"),
    ("f","a"),("g","f")].

  gp:(string) => cons[string].
  gp(X) => {G | (Y,X) in parent && (G,Y) in parent}.

  sibs:() => cons[(string,string)].
  sibs() => { (X,Y) | (Z,X) in parent && (Z,Y) in parent && X~=Y }.

  ms : cons[string].
  ms = ["b","c"].

  main:()=>action[(),()].
  main() => action{
    for (P,C) in parent do{
      logM("Parent is $(P), child is $(C)")
    };
    valis ()
  }

}  

  
