test.implies{
  import star.
  import star.script.

  pars : cons[(string,string)].
  pars = [("a","b"),("c","b"),("b","d"),("e","d"),("f","e")].

  ms : cons[string].
  ms = ["b","a"].

/*  onlySons(P) => (P,S) in pars *> S in ms.

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
  */

--  maleSons = [X | (X,_) in pars && (X,C) in pars *> C in ms].

  main:()=>action[(),()].
  main()=>do{
/*    show "hasD(a) = $(hasD("a"))";
    show "hasD(f) = $(hasD("f"))";
  
    show "actionOnlySons(a) = $(valof actionOnlySons("a"))";
    show "actionOnlySons(f) = $(valof actionOnlySons("f"))";

    assert onlySons("a");

    show disp(onlySons("a"));
    show disp(onlySons("f"));

    assert ~onlySons("f");
*/

    show disp([X | (X,_) in pars])
--    show disp([X | (X,_) in pars && (X,C) in pars *> C in ms])
--    show disp(maleSons)
/*
    show disp(foldOnlySons("a"));
    show disp(foldOnlySons("f"))
*/
  }
}
