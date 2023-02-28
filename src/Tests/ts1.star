test.ts1{
  import star.
  import star.script.
  
  -- Simple test of fiber generator pattern

  scomm[e] ::= .yild(e) | .end.
  rcomm ::= .next | .cancel.

  consIter:all e,x ~~ (cons[e],x,(x,e)=>x)=>x.
  consIter(.nil,X,_) => X.
  consIter(.cons(H,T),X,F) => consIter(T,F(X,H),F).

  iterGen:all e ~~ (cons[e]) => fiber[rcomm,scomm[e]].
  iterGen(L) => fiber{
    let{
      yildFn:((),e)=>().
      yildFn(_,E) => valof{
	case _suspend(this,.yild(E)) in {
	  .next => valis ().
	  .cancel => _retire(this,.end)
	}
      }
    } in {consIter(L,(),yildFn)};
    valis .end
  }

  evens:(cons[integer]) => integer.
  evens(L) => valof{
    TT = iterGen(L);
    Tl = ref 0;

    while .true do {
      case _resume(TT,.next) in {
	.yild(X) where X%2==0 => {
	  Tl := Tl! + X;
	}.
	.yild(X) default => {
	  Tl := Tl! * X
	}.
	.end => valis Tl!
      }
    };
  }

  iterTask:all c,e ~~ iter[c->>e] |: (c) => fiber[rcomm,scomm[e]].
  iterTask(L) => fiber{
    let{
      yildFn(E,Cx) => valof{
	case _suspend(this,.yild(E)) in {
	  .next => valis Cx
	}
      }
    } in {_iter(L,(),yildFn)};
    valis .end
  }

  odds:(cons[integer]) => ().
  odds(L) => valof{
    try{
      for (X where X%2==1) in L do{
	_logmsg(disp(X));
	if X>6 then
	  raise ()
      }
    } catch {
      _ => {}
    };
    valis ()
  }

  iota:(integer,integer)=>cons[integer].
  iota(F,F) => .nil.
  iota(F,T) => .cons(F,iota(F+1,T)).

  main:() => ().
  main() => valof{
    LL = iota(1,12);
    odds(LL);
    valis _logmsg(disp(evens(LL)));
  }
}
