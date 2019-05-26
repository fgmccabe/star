test.implies{
  import star.

  pars : list[(string,string)].
  pars = [("a","b"),("c","b"),("b","d"),("e","d"),("f","e")].

  ms : list[string].
  ms = ["b","a"].

  onlySons(P) => (P,S) in pars *> S in ms.

  hasD(P) => (P,S) in pars && \+ S in ms.

  show "hasD(a) = $(hasD("a"))".
  show "hasD(f) = $(hasD("f"))".
  

  foldOnlySons:(string)=>option[()].
  foldOnlySons(P) => foldRight(
     let{
       checkSon((P,S),So) => foldRight(
  	  let{
  	    checkMale(S,none) => some(()).
  	    checkMale(_,St) => St.
  	  } in checkMale, So, ms).
       checkSon(_,So) => So.
     } in checkSon,
     none,pars).

  show disp(foldOnlySons("a")).
  show disp(foldOnlySons("f")).

  actionOnlySons:(string) => action[(),option[()]].
  actionOnlySons(P) =>
    _iter(pars, _valis(none),
  	  let{
  	    checkSon((P,S),So) =>
  	      _iter(ms, _valis(So),
  		    let{
  		      checkMale(S,none) => _valis(some(())).
  		      checkMale(_,St) => _valis(St)
  		    } in checkMale).
  	    checkSon(_,So) => _valis(So)
  	  } in checkSon).

  show "actionOnlySons(a) = $(valof actionOnlySons("a"))".
  show "actionOnlySons(f) = $(valof actionOnlySons("f"))".

  assert onlySons("a").

  show disp(onlySons("a")).
  show disp(onlySons("f")).

  assert \+onlySons("f").

  show disp([X | (X,_) in pars && (X,C) in pars *> C in ms]).

  _main:(list[string])=>().
  _main([])=>().
}
