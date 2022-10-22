test.implies{
  import star.
  import star.script.

  pars : cons[(string,string)].
  pars = [("a","b"),("c","b"),("b","d"),("e","d"),("f","e")].

  ms : cons[string].
  ms = ["b","a"].

  onlySons(P) => {! () | (P,S) in pars *> S in ms !}.

  hasD(P) => {! () | (P,S) in pars && ~ S in ms !}.

  foldOnlySons:(string)=>option[()].
  foldOnlySons(P) => foldRight(
     let{
       checkSon((P,S),So) => foldRight(
  	  let{
  	    checkMale(S,.none) => some(()).
  	    checkMale(_,St) => St.
  	  } in checkMale, So, ms).
       checkSon(_,So) => So.
     } in checkSon,
     .none,pars).

  maleSons = ({X | (X,_) in pars && (X,C) in pars *> C in ms}:cons[string]).

  main:()=>().
  main()=>valof{
    show hasD("a");
    show hasD("f");
  
    assert _?=onlySons("a");

    show onlySons("a");
    show onlySons("f");

    assert .none.=onlySons("f");

    show ({X | (X,_) in pars && (X,C) in pars *> C in ms}:cons[string]);
    show maleSons;

    show foldOnlySons("a");
    show foldOnlySons("f");
    valis ()
  }
}
