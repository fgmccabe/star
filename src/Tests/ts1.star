test.ts1{
  import star.
  import star.assert.
  
  -- Another test of generator pattern

  scomm[e] ::= .yild(e) | .end.
  rcomm ::= .next | .cancel.

  consIter:all e,x ~~ (cons[e],x,(x,e)=>x)=>x.
  consIter(.nil,X,_) => X.
  consIter(.cons(H,T),X,F) => consIter(T,F(X,H),F).

  iterFn:all e ~~ (fiber[rcomm,scomm[e]],cons[e]) => scomm[e].
  iterFn(this,L) => let{
    yildFn:((),e)=>().
    yildFn(_,E) => valof{
      case suspend .yild(E) in {
	| .next => valis ()
	| .cancel => retire .end
      }
    }
  } in valof{
    consIter(L,(),yildFn);
    valis .end
  }

  iterGen:all e ~~ (cons[e]) => fiber[rcomm,scomm[e]].
  iterGen(L) => _fiber((Th,_) => iterFn(Th,L)).

  itrFn:all e ~~ (cons[e]) => fiber[rcomm,scomm[e]].
  itrFn(L) => _fiber((this,_) => let{
      yildFn:((),e)=>().
      yildFn(_,E) => valof{
	case suspend .yild(E) in {
	  | .next => valis ()
	  | .cancel => retire .end
	}
      }
    } in valof{
      consIter(L,(),yildFn);
      valis .end
    }).

  isEven(X) => X.&.1==0.

  evens:(cons[integer]) => integer.
  evens(L) => valof{
    TT = iterGen(L);
    Tl = ref 0;

    while .true do {
      case TT resume .next in {
	| .yild(X) where isEven(X) => {
	  Tl := Tl! + X;
	}
	| .yild(X) default => {
	  Tl := Tl! * X
	}
	| .end => valis Tl!
      }
    };
  }

  odds:(cons[integer]) => integer.
  odds(L) => valof{
    TT = itrFn(L);
    Tl = ref 0;

    while .true do {
      case TT resume .next in {
	| .yild(X) where ~isEven(X) => {
	  Tl := Tl! + X;
	}
	| .yild(X) default => {
	  Tl := Tl! * X
	}
	| .end => valis Tl!
      }
    };
  }

  iota:(integer,integer)=>cons[integer].
  iota(F,F) => .nil.
  iota(F,T) => .cons(F,iota(F+1,T)).

  main:() => ().
  main() => valof{
    LL = iota(1,12);
    showMsg(disp(odds(LL)));
    valis showMsg(disp(evens(LL)));
  }
}
