test.ts1{
  import star.core.
  import star.arith.
  import star.coerce.
  import star.iterable.
  
  -- Simple test of task generator pattern

  scomm[e] ::= yield(e) | .end.
  rcomm ::= .next | .cancel.

  all s,r ~~ task[s,r] <~ {}.		-- tasks have a type ...

  consIter:all e,x ~~ (cons[e],x,(x,e)=>_step[x])=>x.
  consIter(.nil,X,_) => X.
  consIter(cons(H,T),X,F) =>
    case F(X,H) in {
      _more(C) => consIter(T,C,F).
      _end(C) => C
    }.

  iterGen:all e ~~ (cons[e]) => task[scomm[e],rcomm].
  iterGen(L) => task{
    let{
      yieldFn:((),e)=>_step[()].
      yieldFn(_,E) => valof{
	suspend yield(E) in {
	  .next => valis _more(()).
	  .cancel => valis _end(())
	}
      }
    } in {_ .= consIter(L,(),yieldFn)};
    retire .end
  }

  evens:(cons[integer]) => integer.
  evens(L) => valof{
    TT .= iterGen(L);
    Tl .= ref 0;

    try{
      while .true do {
	TT resume .next in {
	  yield(X) where X%2==0 => {
	    Tl := Tl! + X
	  }.
	  yield(X) default => {
	    Tl := Tl! * X
	  }.
	  .end => raise ()
	}
      }
    } catch {
      () => valis Tl!
    }
  }

  implementation all e ~~ iter[cons[e]->>e] => {.
    _iter(.nil,X,_) => X.
    _iter(cons(H,T),X,F) =>
      _iter(T,F(H,X),F).
  .}

  iterTask:all c,e ~~ iter[c->>e] |: (c) => task[scomm[e],rcomm].
  iterTask(L) => task{
    let{
      yieldFn(E,Cx) => valof{
	suspend yield(E) in {
	  .next => valis Cx
	}
      }
    } in {_ .= _iter(L,(),yieldFn)};
    retire .end
  }

  odds:(cons[integer]) => ().
  odds(L) => valof{
    try{
      for (X where X%2==1) in L do{
	_ .= _logmsg(disp(X));
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
  iota(F,T) => cons(F,iota(F+1,T)).

  public result[e,a] ::= ok(a) | bad(e).

  main:() => ().
  main() => valof{
    LL .= iota(1,12);
    _ .= odds(LL);
    valis _logmsg(disp(evens(LL)));
  }
}
