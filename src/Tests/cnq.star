test.cnq{
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

  onlySons(P) => {! () | (P,S) in parent *> S in ms !}.

  hasD(P) => {! D | (P,D) in parent && ~ D in ms !}.
  isF(P) => {! () | ~ P in ms !}.

  fatherOf:(string)=>cons[string].
  fatherOf(A) => { F | (F,A) in parent && F in ms}.

  main:()=>().
  main() => valof{
    logM("gp(abc)=$(gp("abc"))");
    logM("sibs = $(sibs())");
    logM("father(ab) = $(fatherOf("ab"))");
    logM("hasD(a) = $(hasD("a"))");
    valis ()
  }
}  

  
