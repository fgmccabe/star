test.jt15{
  import star.
  import star.assert.

  -- Test labeled statements

  iota:(integer) => cons[integer].
  iota(M) => let{.
    io(N) where ilt(M,N) => .nil.
    io(N) where ige(M,N) => .cons(N,io(iadd(N,1))).
  .} in io(1).

  labeled:(integer) => integer.
  labeled(X) => valof{
    a{
      if ige(X,0) then {
	a{
	  if ilt(0,X) then{
	    break a;
	    logM("should not be here");
	  } else
	  valis 0;
	};
	valis 1
      }
    };
    valis -1
  }

  firstMultiple:(cons[integer],integer)=>string throws string.
  firstMultiple(X,M) => valof{
    L{
      for ix in X do{
	if ieq(imod(ix,M),0) then
	  break L
      };
      valis "not found"
    };
    valis "found"
  }

  main:(){}.
  main(){
    show labeled(1);
    show labeled(-1);
    show labeled(0);
    assert labeled(1) == 1;
    assert labeled(-1) == -1;
    assert labeled(0) == 0;

    show iota(10);
    show iota(3);

    try{
      show firstMultiple(iota(10),3);
      show firstMultiple(iota(3),10);

      assert firstMultiple(iota(10),3) == "found";
      assert firstMultiple(iota(3),10) == "not found";
    } catch {
      _ do logM("we got an exception")
    }
  }

  logM:(string)=>().
  logM(M) => valof{
    try{
      _logmsg(M)
    } catch {_ do {}};
    valis ()
  }

  ieq:(integer,integer) => boolean.
  ieq(X,Y) => _int_eq(X,Y).

  ilt:(integer,integer) => boolean.
  ilt(X,Y) => _int_lt(X,Y).

  ige:(integer,integer) => boolean.
  ige(X,Y) => _int_ge(X,Y).

  iadd:(integer,integer)=>integer.
  iadd(X,Y) => _int_plus(X,Y).

  imod:(integer,integer)=>integer throws string.
  imod(X,Y) => ( try _int_mod(X,Y) catch { _ => throw "divide by zero"}).

  iShow:(integer)=>string.
  iShow(Ix) => _int2str(Ix).

  implementation all e ~~ generate[cons[e]->>e] => {
    _generate(L) => generator{
      LL = ref L;
      while .cons(H,T) .= LL! do{
	yield H;
	LL := T
      }
    }
  }
}
  
