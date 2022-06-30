test.ts1{
  import star.
  import star.script.
  
  -- Simple test of task generator pattern

  scomm[e] ::= yild(e) | .end.
  rcomm ::= .next | .cancel.

  consIter:all e,x ~~ (cons[e],x,(x,e)=>x)=>x.
  consIter(.nil,X,_) => X.
  consIter(cons(H,T),X,F) => consIter(T,F(X,H),F).

  iterGen:all e ~~ (cons[e]) => task[scomm[e],rcomm].
  iterGen(L) => task{
    let{
      yildFn:((),e)=>().
      yildFn(_,E) => valof{
	suspend yild(E) in {
	  .next => valis ().
	  .cancel => valis ()
	}
      }
    } in {_ .= consIter(L,(),yildFn)};
    retire .end
  }

  evens:(cons[integer]) => integer.
  evens(L) => valof{
    TT .= iterGen(L);
    Tl .= ref 0;

    try{
      while .true do {
	TT resume .next in {
	  yild(X) where X%2==0 => {
	    Tl := Tl! + X
	  }.
	  yild(X) default => {
	    Tl := Tl! * X
	  }.
	  .end => raise ()
	}
      }
    } catch {
      () => valis Tl!
    }
  }

  iterTask:all c,e ~~ iter[c->>e] |: (c) => task[scomm[e],rcomm].
  iterTask(L) => task{
    let{
      yildFn(E,Cx) => valof{
	suspend yild(E) in {
	  .next => valis Cx
	}
      }
    } in {_ .= _iter(L,(),yildFn)};
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
