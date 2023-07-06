test.ts1{
  import star.
  import star.script.
  
  -- Another test of generator pattern

  scomm[e] ::= .yild(e) | .end | .identify(rcomm=>>scomm[e]).
  rcomm ::= .next | .cancel.

  consIter:all e,x ~~ (cons[e],x,(x,e)=>x)=>x.
  consIter(.nil,X,_) => X.
  consIter(.cons(H,T),X,F) => consIter(T,F(X,H),F).

  iterFn:all e ~~ (rcomm=>>scomm[e],cons[e]) => scomm[e].
  iterFn(this,L) => let{
    yildFn:((),e)=>().
    yildFn(_,E) => valof{
      case this suspend .yild(E) in {
	.next => valis ().
	.cancel => this retire .end
      }
    }
  } in valof{
    consIter(L,(),yildFn);
    this retire .end
  }

  iterGen:all e ~~ (cons[e]) => rcomm=>>scomm[e].
  iterGen(L) => case (this spawn valof{
      case (this suspend .identify(this)) in {
	.next => {
	  iterFn(this,L)
	}
      }
    }) in {
    .identify(G) => G
    }.

  itrFn:all e ~~ (cons[e]) => (rcomm=>>scomm[e]).
  itrFn(L) => this spawn first =>> let{
    yildFn:((),e)=>().
    yildFn(_,E) => valof{
      case this suspend .yild(E) in {
	.next => valis ().
	.cancel => this retire .end
      }
    }
  } in valof{
    case first in {
      .next => 
	consIter(L,(),yildFn)
    };
    this retire .end
  }

  evens:(cons[integer]) => integer.
  evens(L) => valof{
    TT = iterGen(L);
    Tl = ref 0;

    while .true do {
      case TT resume .next in {
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

  odds:(cons[integer]) => integer.
  odds(L) => valof{
    TT = itrFn(L);
    Tl = ref 0;

    while .true do {
      case TT resume .next in {
	.yild(X) where X%2==1 => {
	  Tl := Tl! + X;
	}.
	.yild(X) default => {
	  Tl := Tl! * X
	}.
	.end => valis Tl!
      }
    };
  }

  iota:(integer,integer)=>cons[integer].
  iota(F,F) => .nil.
  iota(F,T) => .cons(F,iota(F+1,T)).

  main:() => ().
  main() => valof{
    LL = iota(1,12);
    logMsg(disp(odds(LL)));
    valis logMsg(disp(evens(LL)));
  }
}
