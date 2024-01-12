star.vector{
  import star.core.
  import star.arith.
  import star.bits.
  import star.coerce.
  import star.collection.
  import star.index.
  import star.iterable.
  import star.option.
  import star.cons.
  import star.log.

  public all x ~~ vect[x] ::= .vector(integer,vct[x]).
  
  all x ~~ vct[x] ::= .e
  | .vct1(vct[x])
  | .vct2(vct[x],vct[x])
  | .vct3(vct[x],vct[x],vct[x])
  | .vct4(vct[x],vct[x],vct[x],vct[x])
  | .lf1(x)
  | .lf2(x,x)
  | .lf3(x,x,x)
  | .lf4(x,x,x,x).

  public nullV:all x ~~ ()=>vect[x].
  nullV()=>.vector(0,.e).

  public vindex:all x~~(vect[x],integer)=>option[x].
  vindex(.vector(Dp,V),Ix) => lookup(V,Ix,Dp).

  lookup:all x ~~ (vct[x],integer,integer) => option[x].
  lookup(.e,_,_) => .none.
  lookup(V,Ky,Sx) where Sx>0 => valof{
    (Fst,RKy) = splitKey(Ky,Sx);
    valis pick(V,Fst,RKy,Sx-2)
  }.
  lookup(V,Ky,0) => pick(V,Ky.&.3,Ky,0).

  splitKey:(integer,integer) => (integer,integer).
  splitKey(Ky,Sx) => valof{
    S2 = Sx-2;
    Fst = (Ky.>>.S2).&.3;
    Snd = Ky.&.((1.<<.S2)-1);
    valis (Fst,Snd)
  }

  lg2(X) => (try _int_lg2(X) catch errorCode in {_ => -1}).

  bitcnt(X) => ((lg2(X)+1).>>.1)*2. -- Round up the log to the next even number

  pick:all x ~~ (vct[x],integer,integer,integer)=>option[x].
  pick(T,Ix,K,Dp) => case Ix in {
    0 => case T in {
      | .e => .none
      | .vct1(X) => lookup(X,K,Dp)
      | .vct2(X,_) => lookup(X,K,Dp)
      | .vct3(X,_,_) => lookup(X,K,Dp)
      | .vct4(X,_,_,_) => lookup(X,K,Dp)
      | .lf1(X) => .some(X)
      | .lf2(X,_) => .some(X)
      | .lf3(X,_,_) => .some(X)
      | .lf4(X,_,_,_) => .some(X)
    }
    | 1 => case T in {
      .e => .none
      | .vct1(_) => .none
      | .vct2(_,X) => lookup(X,K,Dp)
      | .vct3(_,X,_) => lookup(X,K,Dp)
      | .vct4(_,X,_,_) => lookup(X,K,Dp)
      | .lf1(_) => .none
      | .lf2(_,X) => .some(X)
      | .lf3(_,X,_) => .some(X)
      | .lf4(_,X,_,_) => .some(X)
    }
    | 2 => case T in {
      .e => .none
      | .vct1(_) => .none
      | .vct2(_,_) => .none
      | .vct3(_,_,X) => lookup(X,K,Dp)
      | .vct4(_,_,X,_) => lookup(X,K,Dp)
      | .lf1(_) => .none
      | .lf2(_,_) => .none
      | .lf3(_,_,X) => .some(X)
      | .lf4(_,_,X,_) => .some(X)
    }
    | 3 => case T in {
      .e => .none
      | .vct1(_) => .none
      | .vct2(_,_) => .none
      | .vct3(_,_,_) => .none
      | .vct4(_,_,_,X) => lookup(X,K,Dp)
      | .lf1(_) => .none
      | .lf2(_,_) => .none
      | .lf3(_,_,_) => .none
      | .lf4(_,_,_,X) => .some(X)
    }
  }

  public vupdate:all x~~(vect[x],integer,x)=>vect[x].
  vupdate(.vector(Dp,V),Ix,X) => .vector(Dp,update(V,Ix,Dp,X)).

  update:all x ~~ (vct[x],integer,integer,x) => vct[x].
  update(.e,_,_,_) => .e.
  update(V,Ky,Sx,X) where Sx>0 => valof{
    (Fst,RKy) = splitKey(Ky,Sx);
    valis updte(V,Fst,RKy,Sx-2,X)
  }.
  update(V,Ky,0,X) => updte(V,Ky.&.3,Ky,0,X).

  updte:all x ~~ (vct[x],integer,integer,integer,x)=>vct[x].
  updte(V,Ix,K,Dp,X) => case Ix in {
    0 => case V in {
      .vct1(T0) => .vct1(update(T0,K,Dp,X)).
      .vct2(T0,T1) => .vct2(update(T0,K,Dp,X),T1).
      .vct3(T0,T1,T2) => .vct3(update(T0,K,Dp,X),T1,T2).
      .vct4(T0,T1,T2,T3) => .vct4(update(T0,K,Dp,X),T1,T2,T3).
      .lf1(_) => .lf1(X).
      .lf2(_,L1) => .lf2(X,L1).
      .lf3(_,L1,L2) => .lf3(X,L1,L2).
      .lf4(_,L1,L2,L3) => .lf4(X,L1,L2,L3).
    }.
    1 => case V in {
      .vct1(T0) => V.
      .vct2(T0,T1) => .vct2(T0,update(T1,K,Dp,X)).
      .vct3(T0,T1,T2) => .vct3(T0,update(T1,K,Dp,X),T2).
      .vct4(T0,T1,T2,T3) => .vct4(T0,update(T1,K,Dp,X),T2,T3).
      .lf1(L0) => .lf2(L0,X).
      .lf2(L0,_) => .lf2(L0,X).
      .lf3(L0,_,L2) => .lf3(L0,X,L2).
      .lf4(L0,_,L2,L3) => .lf4(L0,X,L2,L3).
    }
    2 => case V in {
      .vct1(_) => V.
      .vct2(_,_) => V.
      .vct3(T0,T1,T2) => .vct3(T0,T1,update(T2,K,Dp,X)).
      .vct4(T0,T1,T2,T3) => .vct4(T0,T1,update(T2,K,Dp,X),T3).
      .lf1(L0) => .lf2(L0,X).
      .lf2(L0,L1) => .lf3(L0,L1,X).
      .lf3(L0,L1,L2) => .lf3(L0,L1,X).
      .lf4(L0,L1,L2,L3) => .lf4(L0,L1,X,L3).
    }
    3 => case V in {
      .vct1(_) => V.
      .vct2(_,_) => V.
      .vct3(_,_,_) => V.
      .vct4(T0,T1,T2,T3) => .vct4(T0,T1,T2,update(T3,K,Dp,X)).
      .lf1(L0) => .lf2(L0,X).
      .lf2(L0,L1) => .lf3(L0,L1,X).
      .lf3(L0,L1,L2) => .lf4(L0,L1,L2,X).
      .lf4(L0,L1,L2,_) => .lf4(L0,L1,L2,X).
    }
  }.

  isSome:all x ~~ (option[x])=>boolean.
  isSome(.some(_)) => .true.
  isSome(.none) => .false.

  public isFullVect:all x ~~ (vect[x])=>boolean.
  isFullVect(.vector(_,V)) => isFull(V).
  
  isFull:all x ~~ (vct[x])=>boolean.
  isFull(.lf4(_,_,_,_)) => .true.
  isFull(.vct4(_,_,_,T3)) => isFull(T3).
  isFull(_) default => .false.

  public appnd:all x ~~ (vect[x],x)=>vect[x].
  appnd(.vector(0,.e),x) => .vector(2,.lf1(x)).
  appnd(.vector(Dp,V),x) where Dp>0 => valof{
    if isFull(V) then{
      valis .vector(Dp+2,.vct2(V,apnd(.e,Dp,x)))
    }
    else{
      valis .vector(Dp,apnd(V,Dp,x))
    }
  }

  apnd:all x ~~ (vct[x],integer,x)=>vct[x].
  apnd(.e,2,X) => .lf1(X).
  apnd(.lf1(L0),_,X) => .lf2(L0,X).
  apnd(.lf2(L0,L1),_,X) => .lf3(L0,L1,X).
  apnd(.lf3(L0,L1,L2),_,X) => .lf4(L0,L1,L2,X).
  apnd(.vct1(L0),Dp,X) where Dp>0 =>
    (isFull(L0) ??
      .vct2(L0,apnd(.e,Dp-2,X)) ||
      .vct1(apnd(L0,Dp-2,X))).
  
  apnd(.vct2(L0,L1),Dp,X) where Dp>0 =>
    (isFull(L1) ??
      .vct3(L0,L1,apnd(.e,Dp-2,X)) ||
      .vct2(L0,apnd(L1,Dp-2,X))).
  apnd(.vct3(L0,L1,L2),Dp,X) where Dp>0 =>
    (isFull(L2) ??
      .vct4(L0,L1,L2,apnd(.e,Dp-2,X)) ||
      .vct3(L0,L1,apnd(L2,Dp-2,X))).
  apnd(.vct4(L0,L1,L2,L3),Dp,X) where Dp>0 => .vct4(L0,L1,L2,apnd(L3,Dp-2,X)).
  apnd(.e,Dp,X) where Dp>2 => .vct1(apnd(.e,Dp-2,X)).

  shLft:all x ~~ (vct[x],option[x]) => (option[x],vct[x]).
  shLft(.e,x) => (x,.e).
  shLft(.lf1(N1),.some(X)) => (.some(N1),.lf1(X)).
  shLft(.lf1(N1),.none) => (.some(N1),.e).
  shLft(.lf2(N1,N2),.some(X)) => (.some(N1),.lf2(N2,X)).
  shLft(.lf2(N1,N2),.none) => (.some(N1),.lf1(N2)).
  shLft(.lf3(N1,N2,N3),.some(X)) => (.some(N1),.lf3(N2,N3,X)).
  shLft(.lf3(N1,N2,N3),.none) => (.some(N1),.lf2(N2,N3)).
  shLft(.lf4(N1,N2,N3,N4),.some(X)) => (.some(N1),.lf4(N2,N3,N4,X)).
  shLft(.lf4(N1,N2,N3,N4),.none) => (.some(N1),.lf3(N2,N3,N4)).
  shLft(.vct1(T0),X) => valof{
    (L,R0) = shLft(T0,X);
    if .e .= R0 then
      valis (L,.e)
    else
    valis (L,.vct1(R0))
  }
  shLft(.vct2(T0,T1),X) => valof{
    (L1,R1) = shLft(T1,X);
    (L0,R0) = shLft(T0,L1);
    if .e .= R1 then
      valis (L0,.vct1(R0))
    else
    valis (L0,.vct2(R0,R1))
  }
  shLft(.vct3(T0,T1,T2),X) => valof{
    (L2,R2) = shLft(T2,X);
    (L1,R1) = shLft(T1,L2);
    (L0,R0) = shLft(T0,L1);
    if .e .= R2 then
      valis (L0,.vct2(R0,R1))
    else
    valis (L0,.vct3(R0,R1,R2))
  }
  shLft(.vct4(T0,T1,T2,T3),X) => valof{
    (L3,R3) = shLft(T3,X);
    (L2,R2) = shLft(T2,L3);
    (L1,R1) = shLft(T1,L2);
    (L0,R0) = shLft(T0,L1);
    if .e .= R3 then
      valis (L0,.vct3(R0,R1,R2))
    else
    valis (L0,.vct4(R0,R1,R2,R3))
  }

  public vdelete:all x ~~ (vect[x],integer) => vect[x].
  vdelete(.vector(Dp,V),Ix) => .vector(Dp,delete(V,Ix,Dp,.none)).

  delete:all x ~~ (vct[x],integer,integer,option[x]) => vct[x].
  delete(.e,_,_,.none) => .e.
  delete(.e,_,_,.some(X)) => .lf1(X).
  delete(V,Ky,Sx,X) where Sx>0 => valof{
    (Fst,RKy) = splitKey(Ky,Sx);
    valis delte(V,Fst,RKy,Sx-2,X)
  }.
  delete(V,Ky,0,X) => delte(V,Ky.&.3,Ky,0,X).

  delte:all x ~~ (vct[x],integer,integer,integer,option[x])=>vct[x].
  delte(V,Ix,K,Dp,X) => case Ix in {
    0 => case V in {
      .vct1(T0) => valof{
	Tx0 = delete(T0,K,Dp,X);
	valis bldVct(Tx0,.e,.e,.e)
      }.
      .vct2(T0,T1) => valof{
	(Xs,Tx1) = shLft(T1,X); -- Shift the right tree left by one
	Tx0 = delete(T0,K,Dp,Xs); -- Delete from the left tree
	valis bldVct(Tx0,Tx1,.e,.e)
      }.
      .vct3(T0,T1,T2) => valof{
	(Xs,Tx2) = shLft(T2,X); -- Shift the right tree left by one
	(Xs0,Tx1) = shLft(T1,Xs); -- Shift the middle tree left by one
	Tx0 = delete(T0,K,Dp,Xs0); -- Delete from the left tree
	valis bldVct(Tx0,Tx1,Tx2,.e)
      }.
      .vct4(T0,T1,T2,T3) => valof {
	(Xs,Tx3) = shLft(T3,X); -- Shift the right tree left by one
	(Xs0,Tx2) = shLft(T2,Xs); 
	(Xs1,Tx1) = shLft(T1,Xs0); 
	Tx0 = delete(T0,K,Dp,Xs1); -- Delete from the left tree
	valis bldVct(Tx0,Tx1,Tx2,Tx3)
      }.
      .lf1(_) => bldLf(X,.none,.none,.none).
      .lf2(_,L1) => bldLf(.some(L1),X,.none,.none).
      .lf3(_,L1,L2) => bldLf(.some(L1),.some(L2),X,.none).
      .lf4(_,L1,L2,L3) => bldLf(.some(L1),.some(L2),.some(L3),X).
    }.
    1 => case V in {
      .vct1(T0) => V.      -- trying to delete beyond edge
      .vct2(T0,T1) => valof {
	Tx1 = delete(T1,K,Dp,X); -- Delete from the right tree
	valis bldVct(T0,Tx1,.e,.e)
      }.
      .vct3(T0,T1,T2) => valof {
	(Xs,Tx2) = shLft(T2,X); -- Shift the right tree left by one
	Tx1 = delete(T1,K,Dp,Xs); -- Delete from the middle tree
	valis bldVct(T0,Tx1,Tx2,.e)
      }.
      .vct4(T0,T1,T2,T3) => valof{
	(Xs,Tx3) = shLft(T3,X); -- Shift the right tree left by one
	(Xs0,Tx2) = shLft(T2,Xs); 
	Tx1 = delete(T1,K,Dp,Xs0); 
	valis bldVct(T0,Tx1,Tx2,Tx3)
      }.
      .lf1(_) => V.
      .lf2(L0,L1) => bldLf(.some(L0),X,.none,.none).
      .lf3(L0,_,L2) => bldLf(.some(L0),.some(L2),X,.none).
      .lf4(L0,_,L2,L3) => bldLf(.some(L0),.some(L2),.some(L3),X).
    }.
    2 => case V in {
      .vct1(_) => V.      -- trying to delete beyond edge
      .vct2(_,_) => V.    -- Also.
      .vct3(T0,T1,T2) => valof {
	Tx2 = delete(T2,K,Dp,X); -- Delete from the middle tree
	valis bldVct(T0,T1,Tx2,.e)
      }.
      .vct4(T0,T1,T2,T3) => valof {
	(Xs,Tx3) = shLft(T3,X); -- Shift the right tree left by one
	Tx2 = delete(T2,K,Dp,Xs); 
	valis bldVct(T0,T1,Tx2,Tx3)
      }.
      .lf1(_) => V.
      .lf2(_,_) => V.
      .lf3(L0,L1,_) => bldLf(.some(L0),.some(L1),X,.none).
      .lf4(L0,L1,_,L3) => bldLf(.some(L0),.some(L1),.some(L3),X).
    }.
    3 => case V in {
      .vct1(_) => V.      -- trying to delete beyond edge
      .vct2(_,_) => V.    -- Also.
      .vct3(_,_,_) => V.  -- Also.
      .vct4(T0,T1,T2,T3) => valof {
	Tx3 = delete(T3,K,Dp,X); 
	valis bldVct(T0,T1,T2,Tx3)
      }.
      .lf1(_) => V.
      .lf2(_,_) => V.
      .lf3(_,_,_) => V.
      .lf4(L0,L1,L2,_) => bldLf(.some(L0),.some(L1),.some(L2),X).
    }.
  }.

  bldLf:all x ~~ (option[x],option[x],option[x],option[x])=>vct[x].
  bldLf(.some(X0),.some(X1),.some(X2),.some(X3)) => .lf4(X0,X1,X2,X3).
  bldLf(.some(X0),.some(X1),.some(X2),.none) => .lf3(X0,X1,X2).
  bldLf(.some(X0),.some(X1),.none,.none) => .lf2(X0,X1).
  bldLf(.some(X0),.none,.none,.none) => .lf1(X0).
  bldLf(.none,.none,.none,.none) => .e.
	
  bldVct:all x ~~ (vct[x],vct[x],vct[x],vct[x]) => vct[x].
  bldVct(Tx0,Tx1,Tx2,Tx3) => valof{
    if isEmptyVct(Tx3) then{
      if isEmptyVct(Tx2) then{
	if isEmptyVct(Tx1) then{
	  if isEmptyVct(Tx0) then
	    valis .e
	  else{
	    valis .vct1(Tx0)
	  }
	} else if isEmptyVct(Tx0) then{
	  valis .vct1(Tx1)
	} else {
	  valis .vct2(Tx0,Tx1)
	}
      } else{				-- Tx2 not empty
	if isEmptyVct(Tx1) then{
	  if isEmptyVct(Tx0) then {
	    valis .vct1(Tx2)
	  } else{
	    valis .vct2(Tx0,Tx2)
	  }
	} else if isEmptyVct(Tx0) then{
	  valis .vct2(Tx1,Tx2)
	} else{
	  valis .vct3(Tx0,Tx1,Tx2)
	}
      }
    } else { -- Tx3 not empty
      if isEmptyVct(Tx2) then{
	if isEmptyVct(Tx1) then{
	  if isEmptyVct(Tx0) then
	    valis .vct1(Tx3)
	  else{
	    valis .vct2(Tx0,Tx3)
	  }
	} else if isEmptyVct(Tx0) then{
	  valis .vct2(Tx1,Tx3)
	} else {
	  valis .vct3(Tx0,Tx1,Tx3)
	}
      } else{				-- Tx2,Tx3 not empty
	if isEmptyVct(Tx1) then{
	  if isEmptyVct(Tx0) then {
	    valis .vct2(Tx2,Tx3)
	  } else{
	    valis .vct3(Tx0,Tx2,Tx3)
	  }
	} else if isEmptyVct(Tx0) then{
	  valis .vct3(Tx1,Tx2,Tx3)
	} else{
	  valis .vct4(Tx0,Tx1,Tx2,Tx3)
	}
      }
    }
  }

  isEmptyVct:all x ~~ (vct[x]) => boolean.
  isEmptyVct(.e) => .true.
  isEmptyVct(_) default => .false.

  public implementation all x ~~ iter[vect[x]->>x] => let{.
    walk(.e,A,_) => A.
    walk(.vct1(T0),A,F) => walk(T0,A,F).
    walk(.vct2(T0,T1),A,F) => walk(T1,walk(T0,A,F),F).
    walk(.vct3(T0,T1,T2),A,F) => walk(T2,walk(T1,walk(T0,A,F),F),F).
    walk(.vct4(T0,T1,T2,T3),A,F) => walk(T3,walk(T2,walk(T1,walk(T0,A,F),F),F),F).
    walk(.lf1(L0),A,F) => F(L0,A).
    walk(.lf2(L0,L1),A,F) => F(L1,F(L0,A)).
    walk(.lf3(L0,L1,L2),A,F) => F(L2,F(L1,F(L0,A))).
    walk(.lf4(L0,L1,L2,L3),A,F) => F(L3,F(L2,F(L1,F(L0,A)))).
  .} in {
    _iter(.vector(_,V),A,F) => walk(V,A,F)
  }

  public implementation all x ~~ generate[vect[x]->>x] => {
    _generate(V) => iterGenerator(V)
  }

  public implementation all x ~~ display[x] |: display[vect[x]] => let{.
    dv(.e) => "ε".
    dv(.lf1(L0)) => "{$(L0)}".
    dv(.lf2(L0,L1)) => "{$(L0) $(L1)}".
    dv(.lf3(L0,L1,L2)) => "{$(L0) $(L1) $(L2)}".
    dv(.lf4(L0,L1,L2,L3)) => "[$(L0) $(L1) $(L2) $(L3)]".
    dv(.vct1(T0)) => "(#(dv(T0)) ... )".
    dv(.vct2(T0,T1)) => "(#(dv(T0)) #(dv(T1)) .. )".
    dv(.vct3(T0,T1,T2)) => "(#(dv(T0)) #(dv(T1)) #(dv(T2)) . )".
    dv(.vct4(T0,T1,T2,T3)) => "<#(dv(T0)) #(dv(T1)) #(dv(T2)) #(dv(T3))>".
  .} in {
    disp(.vector(Sz,V)) => "vector[$(Sz)\:#(dv(V))]"
  }

  public implementation all x ~~ coercion[cons[x],vect[x]] => let{.
    grab4:(cons[x]) => (vct[x],cons[x]).
    grab4([]) => (.e,[]).
    grab4([X,..Xs]) => grab3(X,Xs).
    
    grab3:(x,cons[x]) => (vct[x],cons[x]).
    grab3(X,[]) => (.lf1(X),[]).
    grab3(X0,[X1,..Xs]) => grab2(X0,X1,Xs).

    grab2:(x,x,cons[x]) => (vct[x],cons[x]).
    grab2(X0,X1,[]) => (.lf2(X0,X1),[]).
    grab2(X0,X1,[X2,..Xs]) => grab1(X0,X1,X2,Xs).

    grab1:(x,x,x,cons[x]) => (vct[x],cons[x]).
    grab1(X0,X1,X2,[]) => (.lf3(X0,X1,X2),[]).
    grab1(X0,X1,X2,[X3,..Xs]) => (.lf4(X0,X1,X2,X3),Xs).

    grab:(integer,cons[x]) => (vct[x],cons[x]).
    grab(_,[]) => (.e,[]).
    grab(2,Xs) => grab4(Xs).
    grab(Dp,Xs) where Dp>2 => valof{
      (T0,Xs0) = grab(Dp-2,Xs);
      if isEmpty(Xs0) then
	valis (.vct1(T0),Xs0)
      else{
	(T1,Xs1) = grab(Dp-2,Xs0);
	if isEmpty(Xs1) then
	  valis (.vct2(T0,T1),Xs1)
	else{
	  (T2,Xs2) = grab(Dp-2,Xs1);
	  if isEmpty(Xs2) then
	    valis (.vct3(T0,T1,T2),Xs2)
	  else{
	    (T3,Xs3) = grab(Dp-2,Xs2);
	    valis (.vct4(T0,T1,T2,T3),Xs3)
	  }
	}
      }
    }
    
  .} in {
    _coerce(L) => valof{
      Dp = bitcnt(size(L)*2);
      (Tr,Xs) = grab(Dp,L);
      valis .some(.vector(Dp,Tr))
    }
  }

  public implementation all x ~~ sizeable[vect[x]] => let{.
    sz(.e) => 0.
    sz(.lf1(_)) => 1.
    sz(.lf2(_,_)) => 2.
    sz(.lf3(_,_,_)) => 3.
    sz(.lf4(_,_,_,_)) => 4.
    sz(.vct1(T0)) => sz(T0).
    sz(.vct2(T0,T1)) => sz(T0)+sz(T1).
    sz(.vct3(T0,T1,T2)) => sz(T0)+sz(T1)+sz(T2).
    sz(.vct4(T0,T1,T2,T3)) => sz(T0)+sz(T1)+sz(T2)+sz(T3).
  .} in {
    size(.vector(_,V))=>sz(V).
    isEmpty(.vector(_,.e)) => .true.
    isEmpty(_) => .false.
  }

  public implementation all x ~~ equality[x] |: equality[vect[x]] => let{.
    eq(.e,.e) => .true.
    eq(.lf1(L0),.lf1(R0)) => L0==R0.
    eq(.lf2(L0,L1),.lf2(R0,R1)) => L0==R0 && L1==R1.
    eq(.lf3(L0,L1,L2),.lf3(R0,R1,R2)) => L0==R0 && L1==R1 && L2==R2.
    eq(.lf4(L0,L1,L2,L3),.lf4(R0,R1,R2,R3)) => L0==R0 && L1==R1 && L2==R2 && L3==R3.
    eq(.vct1(L0),.vct1(R0)) => eq(L0,R0).
    eq(.vct2(L0,L1),.vct2(R0,R1)) => eq(L0,R0) && eq(L1,R1).
    eq(.vct3(L0,L1,L2),.vct3(R0,R1,R2)) => eq(L0,R0) && eq(L1,R1) && eq(L2,R2).
    eq(.vct4(L0,L1,L2,L3),.vct4(R0,R1,R2,R3)) =>
      eq(L0,R0) && eq(L1,R1) && eq(L2,R2) && eq(L3,R3).
    eq(_,_) default => .false.
  .} in {
    .vector(_,L) == .vector(_,R) => eq(L,R).
  }

  public implementation all e ~~ folding[vect[e]->>e] => let{.
    foldL:all x,a ~~ ((x,a)=>a,a,vct[x])=>a.
    foldL(_,A,.e) => A.
    foldL(F,A,.lf1(L0)) => F(L0,A).
    foldL(F,A,.lf2(L0,L1)) => F(L1,F(L0,A)).
    foldL(F,A,.lf3(L0,L1,L2)) => F(L2,F(L1,F(L0,A))).
    foldL(F,A,.lf4(L0,L1,L2,L3)) => F(L3,F(L2,F(L1,F(L0,A)))).
    foldL(F,A,.vct1(L0)) => foldL(F,A,L0).
    foldL(F,A,.vct2(L0,L1)) => foldL(F,foldL(F,A,L0),L1).
    foldL(F,A,.vct3(L0,L1,L2)) => foldL(F,foldL(F,foldL(F,A,L0),L1),L2).
    foldL(F,A,.vct4(L0,L1,L2,L3)) =>
      foldL(F,foldL(F,foldL(F,foldL(F,A,L0),L1),L2),L3).

    foldR:all x,a ~~ ((x,a)=>a,a,vct[x])=>a.
    foldR(_,A,.e) => A.
    foldR(F,A,.lf1(E0)) => F(E0,A).
    foldR(F,A,.lf2(E0,E1)) => F(E0,F(E1,A)).
    foldR(F,A,.lf3(E0,E1,E2)) => F(E0,F(E1,F(E2,A))).
    foldR(F,A,.lf4(E0,E1,E2,E3)) => F(E0,F(E1,F(E2,F(E3,A)))).
    foldR(F,A,.vct1(L0)) => foldR(F,A,L0).
    foldR(F,A,.vct2(L0,L1)) => foldR(F,foldR(F,A,L0),L1).
    foldR(F,A,.vct3(L0,L1,L2)) => foldR(F,foldR(F,foldR(F,A,L0),L1),L2).
    foldR(F,A,.vct4(L0,L1,L2,L3)) =>
      foldR(F,foldR(F,foldR(F,foldR(F,A,L0),L1),L2),L3).
  .} in {
    foldRight(F,A,.vector(_,V)) => foldR(F,A,V).
    foldLeft(F,A,.vector(_,V)) => foldL(F,A,V).
  }

  public implementation all e ~~ ixfold[vect[e]->>integer,e] => let{.
    fold:all x,a ~~ ((integer,x,a)=>a,integer,a,vct[x])=>(integer,a).
    fold(_,Ix,A,.e) => (Ix,A).
    fold(F,Ix,A,.lf1(L0)) => (Ix+1,F(Ix,L0,A)).
    fold(F,Ix,A,.lf2(L0,L1)) => (Ix+2,F(Ix+1,L1,F(Ix,L0,A))).
    fold(F,Ix,A,.lf3(L0,L1,L2)) => (Ix+3,F(Ix+2,L2,F(Ix+1,L1,F(Ix,L0,A)))).
    fold(F,Ix,A,.lf4(L0,L1,L2,L3)) => (Ix+4,F(Ix+3,L3,F(Ix+2,L2,F(Ix+1,L1,F(Ix,L0,A))))).
    fold(F,Ix,A,.vct1(L0)) => fold(F,Ix,A,L0).
    fold(F,Ix,A,.vct2(L0,L1)) => valof{
      (Ix1,A1) = fold(F,Ix,A,L0);
      valis fold(F,Ix1,A1,L1)
    }
    fold(F,Ix,A,.vct3(L0,L1,L2)) => valof{
      (Ix1,A1) = fold(F,Ix,A,L0);
      (Ix2,A2) = fold(F,Ix1,A1,L1);
      valis fold(F,Ix2,A2,L2)
    }
    fold(F,Ix,A,.vct4(L0,L1,L2,L3)) => valof{
      (Ix1,A1) = fold(F,Ix,A,L0);
      (Ix2,A2) = fold(F,Ix1,A1,L1);
      (Ix3,A3) = fold(F,Ix2,A2,L2);
      valis fold(F,Ix3,A3,L3)
    }
  .} in {
    ixLeft(F,A,.vector(_,V)) => fold(F,0,A,V).1.
    ixRight(F,A,.vector(_,V)) => fold(F,0,A,V).1.
  }

  public implementation all e ~~ build[vect[e]->>e] => {
    _push(E,V) => appnd(V,E).
    _null = .vector(0,.e)
  }

  public implementation all e ~~ concat[vect[e]] => let{.
    conc:all b ~~ (vect[b],vect[b])=>vect[b].
    conc(V1,V2) =>
      foldLeft((E,T)=>appnd(T,E),V1,V2).
    
    _cat:all x ~~ (cons[vect[x]])=>vect[x].
    _cat(.nil) => nullV().
    _cat(.cons(H,T)) => conc(H,_cat(T)).
  .} in {
    (++) = conc.
    _multicat = _cat
  }

  public implementation all e ~~ indexed[vect[e]->>integer,e] => {
    _index = vindex.
    _put = vupdate.
    _remove = vdelete.
    _empty = .vector(0,.e).
  }

  public implementation all e,f ~~ mapping[vect->>e,f] => let{.
    mpVct:(vct[e],(e)=>f)=>vct[f].
    mpVct(.e,_) => .e.
    mpVct(.lf1(x),F) => .lf1(F(x)).
    mpVct(.lf2(x1,x2),F) => .lf2(F(x1),F(x2)).
    mpVct(.lf3(x1,x2,x3),F) => .lf3(F(x1),F(x2),F(x3)).
    mpVct(.lf4(x1,x2,x3,x4),F) => .lf4(F(x1),F(x2),F(x3),F(x4)).
    mpVct(.vct1(V),F) => .vct1(mpVct(V,F)).
    mpVct(.vct2(V1,V2),F) => .vct2(mpVct(V1,F),mpVct(V2,F)).
    mpVct(.vct3(V1,V2,V3),F) => .vct3(mpVct(V1,F),mpVct(V2,F),mpVct(V3,F)).
    mpVct(.vct4(V1,V2,V3,V4),F) => .vct4(mpVct(V1,F),mpVct(V2,F),mpVct(V3,F),mpVct(V4,F)).
  .} in {
    (.vector(Dp,V) // F) => .vector(Dp,mpVct(V,F))
  }
}
