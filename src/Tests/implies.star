test.implies{
  import star.
  import star.script.

  pars : cons[(string,string)].
  pars = [("a","b"),("c","b"),("b","d"),("e","d"),("f","e")].

  ms : cons[string].
  ms = ["b","a"].

  onlySons(P) => (P,S) in pars *> S in ms.

  hasD(P) => (P,S) in pars && ~ S in ms.

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

  actionOnlySons:(string) => action[(),option[()]].
  actionOnlySons(P) =>
    _iter(pars, _valis(.none),
  	  let{
  	    checkSon((P,S),So) =>
  	      _iter(ms, _valis(So),
  		    let{
  		      checkMale(S,.none) => _valis(some(())).
  		      checkMale(_,St) => _valis(St)
  		    } in checkMale).
  	    checkSon(_,So) => _valis(So)
      } in checkSon).

  maleSons = [X | (X,_) in pars && (X,C) in pars *> C in ms].

  main:()=>action[(),()].
  main()=>do{
    show "hasD(a) = $(hasD("a"))";
    show "hasD(f) = $(hasD("f"))";
  
    show some(_).=(valof actionOnlySons("a"));
    show valof actionOnlySons("f");

    assert onlySons("a");

    show onlySons("a");
    show onlySons("f");

    assert ~onlySons("f");


    show [X | (X,_) in pars];
    show [X | (X,_) in pars && (X,C) in pars *> C in ms];
    show maleSons;

    show foldOnlySons("a");
    show foldOnlySons("f")
  }
}
