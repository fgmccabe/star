star.compiler.types{
  import star.
  import star.iterable.
  import star.sort.

  import star.compiler.location.
  import star.compiler.misc.

  public tipe ::=
    voidType |
    thisType |
    kVar(string) |
    kFun(string,integer) |
    tVar(tv,string) |
    tFun(tv,integer,string) |
    tipe(string) |
    tpFun(string,integer) |
    tpExp(tipe,tipe) |
    tupleType(list[tipe]) |
    allType(tipe,tipe) |
    existType(tipe,tipe) |
    faceType(list[(string,tipe)],list[(string,tipe)]) |
    typeLambda(tipe,tipe) |
    typeExists(tipe,tipe) |
    constrainedType(tipe,constraint) |
    conType(string,list[tipe],list[tipe]).

  public constraint ::=
    conConstraint(string,list[tipe],list[tipe]) |
    fieldConstraint(tipe,tipe).

  tv ::= tv{
    binding : ref option[tipe].
    constraints : ref list[constraint].
  }

  public isIdenticalVar:(tipe,tipe) => boolean.
  isIdenticalVar(T1,T2) => isIdent(deRef(T1),deRef(T2)).

  isIdent(tVar(_,K1),tVar(_,K2)) => K1==K2.
  isIdent(tFun(_,A1,K1),tFun(_,A2,K2)) => K1==K2 && A1==A2.
  isIdent(_,_) default => false.

  public isUnbound:(tipe) => boolean.
  isUnbound(tVar(B,_)) => ((T^=B.binding!) ? isUnbound(T) || true).
  isUnbound(tFun(B,_,_)) => ((T^=B.binding!) ? isUnbound(T) || true).
  isUnbound(_) default => false.

  public isUnboundFVar:(tipe) => option[integer].
  isUnboundFVar(tVar(B,_)) => ((T^=B.binding!) ? isUnboundFVar(T) || none).
  isUnboundFVar(tFun(B,Ar,_)) =>
    ((T^=B.binding!) ? isUnboundFVar(T) || some(Ar)).
  isUnboundFVar(_) default => none.

  public setBinding:(tipe,tipe) => action[(),()].
  setBinding(tVar(B,_),T) => bnd(B,T).
  setBinding(tFun(B,_,_),T) => bnd(B,T).

  bnd:(tv,tipe) => action[(),()].
  bnd(B,T) where B.binding! == none => do {
    B.binding := some(T)
  }
  bnd(_,_) default => done(()).

  public resetBinding:(tipe) => action[(),()].
  resetBinding(tVar(B,_)) => do {
    B.binding := none
  }

  public constraintsOf:(tipe) => list[constraint].
  constraintsOf(Tp) => conOf(deRef(Tp)).

  conOf(tVar(T,_)) => T.constraints! .
  conOf(tFun(T,_,_)) => T.constraints! .
  conOf(_) default => [].

  public setConstraints:(tipe,list[constraint]) => action[(),()].
  setConstraints(tVar(V,_),Cx) => do{
    V.constraints := Cx;
    lift ()
  }
  setConstraints(tFun(V,_,_),Cx) => do{
    V.constraints := Cx;
    lift ()
  }
  setConstraints(_,_) default => done(()).

  public deRef:(tipe) => tipe.
  deRef(tVar(B,_)) where T^=B.binding! => deRef(T).
  deRef(tFun(B,_,_)) where T^=B.binding! => deRef(T).
  deRef(Tp) default => Tp.

  public newTypeVar:(string) => tipe.
  newTypeVar(Pre) => tVar(tv{binding := none. constraints := []. },genSym(Pre)).

  public newTypeFun:(string,integer) => tipe.
  newTypeFun(Pre,Ax) => tFun(tv{binding := none. constraints := []. },Ax,genSym(Pre)).

  public mkTypeExp:(tipe,list[tipe])=>tipe.
  mkTypeExp(Tp,[]) => Tp.
  mkTypeExp(Tp,[A,..L]) => mkTypeExp(tpExp(Tp,A),L).

  public skolemFun:(string,integer) => tipe.
  skolemFun(Nm,0) => kVar(genSym(Nm)).
  skolemFun(Nm,Ar) => kFun(genSym(Nm),Ar).

  public implementation equality[tipe] => {
    T1==T2 => identType(T1,T2,[]).
  }

  identType:(tipe,tipe,list[(tipe,tipe)]) => boolean.
  identType(voidType,voidType,_) => true.
  identType(thisType,thisType,_) => true.
  identType(kVar(N1),kVar(N2),_) => N1==N2.
  identType(kFun(N1,A1),kFun(N2,A2),_) => N1==N2 && A1==A2.
  identType(tVar(_,N1),tVar(_,N2),_) => N1==N2.
  identType(tFun(_,A1,N1),tFun(_,A2,N2),_) => N1==N2 && A1==A2.
  identType(tipe(N1),tipe(N2),_) => N1==N2.
  identType(tpFun(N1,A1),tpFun(N2,A2),_) => N1==N2 && A1==A2.
  identType(tpExp(O1,A1),tpExp(O2,A2),Q) =>
    identType(O1,O2,Q) && identType(A1,A2,Q).
  identType(tupleType(E1),tupleType(E2),Q) => identTypes(E1,E2,Q).
  identType(allType(V1,T1),allType(V2,T2),Q) =>
    identType(V1,V2,Q) && identType(T1,T2,Q).
  identType(existType(V1,T1),existType(V2,T2),Q) =>
    identType(V1,V2,Q) && identType(T1,T2,Q).
  identType(typeLambda(V1,T1),typeLambda(V2,T2),Q) =>
    identType(V1,V2,Q) && identType(T1,T2,Q).
  identType(typeExists(V1,T1),typeExists(V2,T2),Q) =>
    identType(V1,V2,Q) && identType(T1,T2,Q).
  identType(faceType(V1,T1),faceType(V2,T2),Q) =>
    identNmTypes(V1,V2,Q) && identNmTypes(T1,T2,Q).
  identType(conType(Nm,A1,D1),conType(Nm,A2,D2),Q) =>
    identTypes(A1,A2,Q) && identTypes(D1,D2,Q).
  identType(constrainedType(T1,C1),constrainedType(T2,C2),Q) =>
    identType(T1,T2,Q).
  identType(_,_,_) default => false.

  identTypes([],[],_) => true.
  identTypes([E1,..L1],[E2,..L2],Q) =>
    identType(E1,E2,Q) &&
	identTypes(L1,L2,Q).

  identNmTypes(L1,L2,Q) => let{
    sortByNm(LL) => sort(LL,(((N1,_),(N2,_)) => N1<N2)).
    identPrs([],[]) => true.
    identPrs([(Nm,E1),..L1],[(Nm,E2),..L2]) =>
      identType(E1,E2,Q) &&
	  identPrs(L1,L2).
  } in identPrs(sortByNm(L1),sortByNm(L2)).

  public implementation equality[constraint] => {.
    conConstraint(Nm,A1,D1) == conConstraint(Nm,A2,D2) => A1==A2 && D1==D2.
    fieldConstraint(V1,T1) == fieldConstraint(V2,T2) => V1==V2 && T1==T2.
    _ == _ default => false.
  .}

  -- in general, hashing types is not reliable because of unification
  public implementation hash[tipe] => let {
    hsh(voidType) => 0.
    hsh(thisType) => -1.
    hsh(kVar(Nm)) => hash(Nm).
    hsh(kFun(Nm,Ar)) => Ar*37+hash(Nm).
    hsh(tVar(_,Nm)) => hash("V")+hash(Nm).
    hsh(tFun(_,Ar,Nm)) => (hash("F")+Ar)*37+hash(Nm).
    hsh(tipe(Nm)) => hash(Nm).
    hsh(tpFun(Nm,Ar)) => Ar*37+hash(Nm).
    hsh(tpExp(O,A)) => hsh(deRef(O))*37+hsh(deRef(A)).
    hsh(tupleType(Els)) => hshEls((hash("()")*37+size(Els))*37,Els).
    hsh(faceType(Els,Tps)) =>
      hshFields(hshFields(hash("{}")*37+size(Els)+size(Tps),Els),Tps).
    hsh(allType(V,T)) => (hash("all")*37+hsh(deRef(V)))*37+hsh(deRef(T)).
    hsh(existType(V,T)) => (hash("exist")*37+hsh(deRef(V)))*37+hsh(deRef(T)).
    hsh(constrainedType(T,C)) => (hash("|:")*37+hsh(deRef(T)))*37+hshCon(C).
    hsh(conType(Nm,A,D)) => hshEls(hshEls(hash(Nm),A),D).

    hshCon(conConstraint(Nm,Args,_)) =>
      hshEls(hash("contract")*37+hash(Nm),Args).
    hshCon(fieldConstraint(V,T)) =>
      (hash("<~")*37+hsh(deRef(V)))*37+hsh(deRef(T)).

    hshEls(H,Els) => foldLeft((Hx,El)=>Hx*37+hsh(deRef(El)),H,Els).

    hshFields(H,Els) => foldLeft((Hx,(Nm,Tp))=>(Hx*37+hash(Nm))*37+hsh(deRef(Tp)),H,Els).
  } in {.
    hash(Tp) => hsh(deRef(Tp)).
  .}

  public implementation display[tipe] => {.
    disp(T) => showType(T,false,10000)
  .}

  public showType:(tipe,boolean,integer) => ss.
  showType(T,Sh,Dp) => shTipe(deRef(T),Sh,Dp).

  shType:(tipe,boolean,integer) => ss.
  shTipe(voidType,_,_) => ss("void").
  shTipe(thisType,_,_) => ss("this").
  shTipe(kVar(Nm),_,_) => ss(Nm).
  shTipe(kFun(Nm,Ar),_,_) => ssSeq([ss(Nm),ss("/"),disp(Ar)]).
  shTipe(tVar(V,Nm),Sh,Dp) => ssSeq([showAllConstraints(V.constraints!,Dp),ss("%"),ss(Nm)]).
  shTipe(tFun(_,Ar,Nm),_,_) => ssSeq([ss("%"),ss(Nm),ss("/"),disp(Ar)]).
  shTipe(tipe(Nm),_,_) => ss(Nm).
  shTipe(tpFun(Nm,Ar),_,_) => ssSeq([ss(Nm),ss("/"),disp(Ar)]).
  shTipe(tpExp(tpExp(tpFun("=>",2),A),R),Sh,Dp) =>
    ssSeq([showType(A,Sh,Dp-1),ss("=>"),showType(R,Sh,Dp-1)]).
  shTipe(tpExp(tpExp(tpFun("<=>",2),A),R),Sh,Dp) =>
    ssSeq([showType(A,Sh,Dp-1),ss("<=>"),showType(R,Sh,Dp-1)]).
  shTipe(tpExp(tpFun("ref",1),A),Sh,Dp) =>
    ssSeq([ss("ref "),showType(A,Sh,Dp-1)]).
  shTipe(tpExp(F,A),Sh,Dp) => ssSeq([showTpExp(F,A,Sh,Dp),ss("]")]).
  shTipe(tupleType(A),Sh,Dp) => ssSeq([ss("("),showTypes(A,Sh,Dp),ss(")")]).
  shTipe(allType(A,T),Sh,Dp) =>
    ssSeq([ss("all "),showBound(A,Dp),..showMoreQuantified(T,Sh,Dp)]).
  shTipe(existType(A,T),Sh,Dp) =>
    ssSeq([ss("exist "),showBound(A,Dp),..showMoreQuantified(T,Sh,Dp)]).
  shTipe(faceType(Els,Tps),Sh,Dp) =>
    ssSeq([ss("{"),showTypeEls(Els,Tps,Sh,Dp),ss("}")]).
  shTipe(typeLambda(A,T),Sh,Dp) =>
    ssSeq([showType(A,Sh,Dp),ss("~>"),showType(T,Sh,Dp)]).
  shTipe(typeExists(A,T),Sh,Dp) =>
    ssSeq([showType(A,Sh,Dp),ss("<~"),showType(T,Sh,Dp)]).
  shTipe(constrainedType(T,C),Sh,Dp) =>
    ssSeq([showConstraint(C,Dp),ss("|:"),showType(T,Sh,Dp)]).
  shTipe(conType(Nm,Args,Deps),Sh,Dp) =>
    ssSeq([disp(Nm),ss("["),showTypes(Args,Sh,Dp),
	ss("->>"),showTypes(Deps,Sh,Dp),ss("]")]).
  
  showTypes(_,_,0) => ss("...").
  showTypes(E,Sh,Dp) => ssSeq(showEls(E,Sh,Dp-1,"")).

  showEls([],_,_,_) => [].
  showEls([T,..Tps],Sh,Dp,Sep) => [ss(Sep),showType(T,Sh,Dp),..showEls(Tps,Sh,Dp,", ")].

  showTypeEls:(list[(string,tipe)],list[(string,tipe)],boolean,integer) => ss.
  showTypeEls(Els,Tps,Sh,Dp) =>
    ssSeq(interleave([ssSeq([ss(Nm),ss(":"),showType(Tp,Sh,Dp)]) | (Nm,Tp) in Els] ++
               [ssSeq([ss(Nm),ss("~>"),showType(Tp,Sh,Dp)]) | (Nm,Tp) in Tps],ss(". "))).

  showTpExp:(tipe,tipe,boolean,integer) => ss.
  showTpExp(tpExp(T,A),B,Sh,Dp) => ssSeq([showTpExp(deRef(T),A,Sh,Dp),ss(","),showType(B,Sh,Dp)]).
  showTpExp(tpFun(Nm,_),B,Sh,Dp) => ssSeq([ss(Nm),ss("["),showType(B,Sh,Dp)]).
  showTpExp(kFun(Nm,_),B,Sh,Dp) => ssSeq([ss(Nm),ss("["),showType(B,Sh,Dp)]).
  showTpExp(tFun(_,_,Nm),B,Sh,Dp) => ssSeq([ss(Nm),ss("["),showType(B,Sh,Dp)]).
  showTpExp(T,B,Sh,Dp) => ssSeq([showType(T,Sh,Dp),ss("["),showType(B,Sh,Dp)]).

  showAllConstraints([],Dp) => ss("").
  showAllConstraints([C,..Cs],Dp) => ssSeq([showConstraint(C,Dp),..showMoreConstraints(Cs,Dp)]).

  showMoreConstraints([],_) => [ss("|:")].
  showMoreConstraints([C,..Cs],Dp) => [ss(", "),showConstraint(C,Dp),..showMoreConstraints(Cs,Dp)].

  showMoreQuantified(allType(V,T),Sh,Dp) => [ss(","),showBound(V,Dp),..showMoreQuantified(T,Sh,Dp)].
  showMoreQuantified(T,Sh,Dp) => [ss(" ~~ "),showType(T,Sh,Dp)].

  showBound(V,Dp) => showType(V,false,Dp).

  showConstraint(conConstraint(Nm,Args,[]),Dp) =>
    ssSeq([disp(Nm),ss("["),showTypes(Args,false,Dp),ss("]")]).
  showConstraint(conConstraint(Nm,Args,Deps),Dp) =>
    ssSeq([disp(Nm),ss("["),showTypes(Args,false,Dp),ss("->>"),showTypes(Deps,false,Dp),ss("]")]).
  showConstraint(fieldConstraint(Tp,Fc),Dp) =>
    ssSeq([showType(Tp,false,Dp),ss("<~"),showType(Fc,false,Dp)]).

  public contract all c ~~ hasType[c] ::= {
    typeOf:(c)=>tipe.
  }

  public implementationName:(constraint) => string.
  implementationName(conConstraint(Nm,Args,_)) => Nm++surfaceNames(Args).

  surfaceNames(Tps) =>
    _str_multicat(flatten(Tps//((T)=>surfaceName(deRef(T))))).

  surfaceName(tipe(Nm)) => ["!",Nm].
  surfaceName(tpExp(O,_)) => surfaceName(deRef(O)).
  surfaceName(kVar(Nm)) => ["!",Nm].
  surfaceName(kFun(Nm,_)) => ["!",Nm].
  surfaceName(tpFun(Nm,_)) => ["!",Nm].
  surfaceName(kVar(Nm)) => ["!",Nm].
  surfaceName(tVar(_,_)) => ["!_"].
  surfaceName(tFun(_,_,_)) => ["!_"].
  surfaceName(allType(_,T)) => surfaceName(deRef(T)).
  surfaceName(existType(_,T)) => surfaceName(deRef(T)).
  surfaceName(constrainedType(T,_)) => surfaceName(deRef(T)).
  surfaceName(typeLambda(_,T)) => surfaceName(deRef(T)).
  surfaceName(tupleType(A)) => ["!()$(size(A))"].
  surfaceName(conType(Nm,_,_)) => [Nm].

  public implementation hasType[constraint] => {.
    typeOf(conConstraint(Nm,Args,Deps)) =>
      mkTypeExp(tpFun(Nm,size(Args)+size(Deps)),Args++Deps).
  .}

  mkTypeExp(Tp,[]) => Tp.
  mkTypeExp(Op,[T,..Rest]) => mkTypeExp(tpExp(Op,T),Rest).
}
