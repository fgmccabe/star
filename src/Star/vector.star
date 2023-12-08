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
  
  public all x ~~ vct[x] ::= .e
  | .vct(boolean,vct[x],vct[x],vct[x],vct[x])
  | .lf(boolean,option[x],option[x],option[x],option[x]).

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
  pick(.vct(_,X,_,_,_),0,K,Dp) => lookup(X,K,Dp).
  pick(.vct(_,_,X,_,_),1,K,Dp) => lookup(X,K,Dp).
  pick(.vct(_,_,_,X,_),2,K,Dp) => lookup(X,K,Dp).
  pick(.vct(_,_,_,_,X),3,K,Dp) => lookup(X,K,Dp).
  pick(.lf(_,X,_,_,_),0,_,0) => X.
  pick(.lf(_,_,X,_,_),1,_,0) => X.
  pick(.lf(_,_,_,X,_),2,_,0) => X.
  pick(.lf(_,_,_,_,X),3,_,0) => X.


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
  updte(.vct(_,T0,T1,T2,T3),Ix,K,Dp,X) => case Ix in {
    0 => vctFull(update(T0,K,Dp,X),T1,T2,T3).
    1 => vctFull(T0,update(T1,K,Dp,X),T2,T3).
    2 => vctFull(T0,T1,update(T2,K,Dp,X),T3).
    3 => vctFull(T0,T1,T2,update(T3,K,Dp,X)).
  }.
  updte(.lf(F,T0,T1,T2,T3),Ix,K,Dp,X) => case Ix in {
    0 => .lf(isSome(T1)&&isSome(T2)&&isSome(T3),.some(X),T1,T2,T3).
    1 => .lf(isSome(T0)&&isSome(T2)&&isSome(T3),T0,.some(X),T2,T3).
    2 => .lf(isSome(T0)&&isSome(T1)&&isSome(T3),T0,T1,.some(X),T3).
    3 => .lf(isSome(T0)&&isSome(T1)&&isSome(T2),T0,T1,T2,.some(X)).
  }

  isSome:all x ~~ (option[x])=>boolean.
  isSome(.some(_)) => .true.
  isSome(.none) => .false.

  public isFullVect:all x ~~ (vect[x])=>boolean.
  isFullVect(.vector(_,V)) => isFull(V).
  
  isFull:all x ~~ (vct[x])=>boolean.
  isFull(.e) => .false.
  isFull(.lf(F,_,_,_,_)) => F.
  isFull(.vct(F,_,_,_,_)) => F.

  vctFull(L0,L1,L2,L3) =>
    .vct(isFull(L0) && isFull(L1) && isFull(L2) && isFull(L3),
      L0,L1,L2,L3).
  
  public appnd:all x ~~ (vect[x],x)=>vect[x].
  appnd(.vector(0,.e),x) =>
    .vector(2,.lf(.false,.some(x),.none,.none,.none)).
  appnd(.vector(Dp,V),x) where Dp>0 => valof{
    if isFull(V) then{
      valis .vector(Dp+2,vctFull(V,apnd(.e,Dp,x),.e,.e))
    }
    else{
      valis .vector(Dp,apnd(V,Dp,x))
    }
  }

  apnd:all x ~~ (vct[x],integer,x)=>vct[x].
  apnd(.lf(_,.some(A),.none,L2,L3),_,X) =>
    .lf(.false,.some(A),.some(X),L2,L3).
  apnd(.lf(_,.some(A),.some(B),.none,L3),_,X) =>
    .lf(.false,.some(A),.some(B),.some(X),L3).
  apnd(.lf(_,.some(A),.some(B),.some(C),.none),_,X) =>
    .lf(.true,.some(A),.some(B),.some(C),.some(X)).
  apnd(.vct(_,L0,L1,L2,L3),Dp,X) where Dp>0 =>
    (isFull(L0) ??
      (isFull(L1) ??
	(isFull(L2) ??
	  vctFull(L0,L1,L2,apnd(L3,Dp-2,X)) ||
	  vctFull(L0,L1,apnd(L2,Dp-2,X),L3)) ||
	vctFull(L0,apnd(L1,Dp-2,X),L2,L3)) ||
      vctFull(apnd(L0,Dp-2,X),L1,L2,L3)).
  apnd(.e,2,X) => .lf(.false,.some(X),.none,.none,.none).
  apnd(.e,Dp,X) where Dp>2 => .vct(.false,apnd(.e,Dp-2,X),.e,.e,.e).

  public implementation all x ~~ iter[vect[x]->>x] => let{.
    walk(.e,A,_) => A.
    walk(.vct(_,L0,L1,L2,L3),A,F) =>
      walk(L3,walk(L2,walk(L1,walk(L0,A,F),F),F),F).
    walk(.lf(_,L0,L1,L2,L3),A,F) =>
      wO(L3,wO(L2,wO(L1,wO(L0,A,F),F),F),F).

    wO(.none,A,_) => A.
    wO(.some(E),A,F) => F(E,A).
  .} in {
    _iter(.vector(_,V),A,F) => walk(V,A,F)
  }

  public implementation all x ~~ generate[vect[x]->>x] => {
    _generate(V) => iterGenerator(V)
  }

  public implementation all x ~~ display[x] |: display[vect[x]] => let{.
    dv(.e) => "ε".
    dv(.lf(_,L0,L1,L2,L3)) => "{$(L0) $(L1) $(L2) $(L3)}".
    dv(.vct(.true,L0,L1,L2,L3)) => "<#(dv(L0)) #(dv(L1)) #(dv(L2)) #(dv(L3))>".
    dv(.vct(.false,L0,L1,L2,L3)) => "(#(dv(L0)) #(dv(L1)) #(dv(L2)) #(dv(L3)))".
  .} in {
    disp(.vector(Sz,V)) => "vector[$(Sz)\:#(dv(V))]"
  }

  public implementation all x ~~ coercion[cons[x],vect[x]] => let{.
    grab4:(cons[x]) => (vct[x],cons[x]).
    grab4([]) => (.e,[]).
    grab4([X,..Xs]) => grab3(X,Xs).
    
    grab3:(x,cons[x]) => (vct[x],cons[x]).
    grab3(X,[]) => (.lf(.false,.some(X),.none,.none,.none),[]).
    grab3(X0,[X1,..Xs]) => grab2(X0,X1,Xs).

    grab2:(x,x,cons[x]) => (vct[x],cons[x]).
    grab2(X0,X1,[]) => (.lf(.false,.some(X0),.some(X1),.none,.none),[]).
    grab2(X0,X1,[X2,..Xs]) => grab1(X0,X1,X2,Xs).

    grab1:(x,x,x,cons[x]) => (vct[x],cons[x]).
    grab1(X0,X1,X2,[]) => (.lf(.false,.some(X0),.some(X1),.some(X2),.none),[]).
    grab1(X0,X1,X2,[X3,..Xs]) => (.lf(.true,.some(X0),.some(X1),.some(X2),.some(X3)),Xs).

    grab:(integer,cons[x]) => (vct[x],cons[x]).
    grab(_,[]) => (.e,[]).
    grab(2,Xs) => grab4(Xs).
    grab(Dp,Xs) where Dp>2 => valof{
      (T0,Xs0) = grab(Dp-2,Xs);
      (T1,Xs1) = grab(Dp-2,Xs0);
      (T2,Xs2) = grab(Dp-2,Xs1);
      (T3,Xs3) = grab(Dp-2,Xs2);
      valis (.vct(isFull(T3),T0,T1,T2,T3),Xs3)
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
    sz(.lf(.true,_,_,_,_)) => 4.
    sz(.lf(.false,.some(_),.some(_),.some(_),.none)) => 3.
    sz(.lf(.false,.some(_),.some(_),.none,.none)) => 2.
    sz(.lf(.false,.some(_),.none,_,_)) => 1.
    sz(.lf(.false,.none,_,_,_))=> 0. -- should never get here
    sz(.vct(.true,T0,_,_,_)) => 4*(1.<<.depth(T0)).
    sz(.vct(.false,T0,T1,T2,T3)) => sz(T0)+sz(T1)+sz(T2)+sz(T3).

    depth(.e) => 0.
    depth(.lf(_,_,_,_,_)) => 2.
    depth(.vct(_,T0,_,_,_)) => depth(T0)+2.
  .} in {
    size(.vector(_,V))=>sz(V).
    isEmpty(.vector(_,.e)) => .true.
    isEmpty(_) => .false.
  }

  public implementation all x ~~ equality[x] |: equality[vect[x]] => let{.
    eq(.e,.e) => .true.
    eq(.lf(_,L0,L1,L2,L3),.lf(_,R0,R1,R2,R3)) =>
      L0==R0 && L1==R1 && L2==R2 && L3==R3.
    eq(.vct(_,L0,L1,L2,L3),.vct(_,R0,R1,R2,R3)) =>
      eq(L0,R0) && eq(L1,R1) && eq(L2,R2) && eq(L3,R3).
    eq(_,_) default => .false.
  .} in {
    .vector(_,L) == .vector(_,R) => eq(L,R).
  }

  fldLft:all x,a ~~ ((x,a)=>a,a,vect[x])=>a.
  fldLft(F,A,.vector(_,V)) => foldL(F,A,V).

  foldL:all x,a ~~ ((x,a)=>a,a,vct[x])=>a.
  foldL(_,A,.e) => A.
  foldL(F,A,.lf(_,E0,E1,E2,E3)) =>
    someF(F,someF(F,someF(F,someF(F,A,E0),E1),E2),E3).
  foldL(F,A,.vct(_,T0,T1,T2,T3)) =>
    foldL(F,foldL(F,foldL(F,foldL(F,A,T0),T1),T2),T3).

  someF:all a,o ~~ ((o,a)=>a,a,option[o]) => a.
  someF(_,A,.none) => A.
  someF(F,A,.some(O)) => F(O,A).

  public implementation all e ~~ folding[vect[e]->>e] => let{.
    foldR:all x,a ~~ ((x,a)=>a,a,vct[x])=>a.
    foldR(_,A,.e) => A.
    foldR(F,A,.lf(_,E0,E1,E2,E3)) =>
      someF(F,someF(F,someF(F,someF(F,A,E0),E1),E2),E3).
    foldR(F,A,.vct(_,T0,T1,T2,T3)) =>
      foldR(F,foldR(F,foldR(F,foldR(F,A,T0),T1),T2),T3).
  .} in {
    foldRight(F,A,.vector(_,V)) => foldR(F,A,V).
    foldLeft(F,A,V) => fldLft(F,A,V).
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
}
