star.compiler.types{
  import star.
  import star.iterable.
  import star.sort.

  import star.compiler.location.
  import star.compiler.misc.

  public tipe ::=
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
      funDeps(tipe,list[tipe]).

  public constraint ::=
    typeConstraint(tipe) |
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
  resetBinding(tFun(B,_,_)) => do{
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
    valis ()
  }
  setConstraints(tFun(V,_,_),Cx) => do{
    V.constraints := Cx;
    valis ()
  }
  setConstraints(_,_) default => done(()).

  public addConstraint:(tipe,constraint) => option[()].
  addConstraint(T,C) => addCon(deRef(T),C).

  addCon:(tipe,constraint) => option[()].
  addCon(tVar(V,_),C) => do{
    V.constraints := [C,..V.constraints!];
    valis ()
  }

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

  public implementation equality[tipe] => {
    T1==T2 => identType(T1,T2,[]).
  }

  identType:(tipe,tipe,list[(tipe,tipe)]) => boolean.
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
  identType(funDeps(T1,D1),funDeps(T2,D2),Q) =>
    identType(T1,T2,Q) && identTypes(D1,D2,Q).
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
    identPrs([(Nm,E1),..l1],[(Nm,E2),..l2]) =>
      identType(E1,E2,Q) &&
	  identPrs(l1,l2).
    identPrs(_,_) => false.
  } in identPrs(sortByNm(L1),sortByNm(L2)).

  public implementation equality[constraint] => {.
    typeConstraint(T1) == typeConstraint(T2) => T1==T2.
    fieldConstraint(V1,T1) == fieldConstraint(V2,T2) => V1==V2 && T1==T2.
    _ == _ default => false.
  .}

  public implementation display[tipe] => {.
    disp(T) => showType(T,false,10000)
  .}

  public implementation display[constraint] => {
    disp(C) => showConstraint(C,2)
  }

  public showType:(tipe,boolean,integer) => ss.
  showType(T,Sh,Dp) => shTipe(deRef(T),Sh,Dp).

  shTipe:(tipe,boolean,integer) => ss.
  shTipe(kVar(Nm),_,_) => ssSeq([ss("$"),ss(Nm)]).
  shTipe(kFun(Nm,Ar),_,_) => ssSeq([ss(Nm),ss("/"),disp(Ar)]).
  shTipe(tVar(V,Nm),false,Dp) => ssSeq([ss("%"),ss(Nm)]).
  shTipe(tVar(V,Nm),true,Dp) => ssSeq([showAllConstraints(V.constraints!,Dp),ss("%"),ss(Nm)]).
  shTipe(tFun(_,Ar,Nm),_,_) => ssSeq([ss("%"),ss(Nm),ss("/"),disp(Ar)]).
  shTipe(tipe(Nm),_,_) => ss(Nm).
  shTipe(tpFun(Nm,Ar),_,_) => ssSeq([ss(Nm),ss("/"),disp(Ar)]).
  shTipe(tpExp(O,A),Sh,Dp) =>
    showTpExp(deRef(O),[A],Sh,Dp).
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
  shTipe(funDeps(Tp,Deps),Sh,Dp) =>
    shTpExp(deRef(Tp),"->>",ssSeq([showEls(Deps,Sh,Dp-1,"")..,ss("]")]),Sh,Dp-1).
  
  showTypes(_,_,0) => ss("...").
  showTypes(E,Sh,Dp) => ssSeq(showEls(E,Sh,Dp-1,"")).

  showEls([],_,_,_) => [].
  showEls([T,..Tps],Sh,Dp,Sep) => [ss(Sep),showType(T,Sh,Dp),..showEls(Tps,Sh,Dp,", ")].

  showTypeEls:(list[(string,tipe)],list[(string,tipe)],boolean,integer) => ss.
  showTypeEls(Els,Tps,Sh,Dp) =>
    ssSeq(interleave([ssSeq([ss(Nm),ss(":"),showType(Tp,Sh,Dp)]) | (Nm,Tp) in Els] ++
               [ssSeq([ss(Nm),ss("~>"),showType(Tp,Sh,Dp)]) | (Nm,Tp) in Tps],ss(". "))).

  showTpExp:(tipe,list[tipe],boolean,integer) => ss.
  showTpExp(tpFun("=>",2),[A,R],Sh,Dp) =>
    ssSeq([shTipe(deRef(A),Sh,Dp-1),ss("=>"),shTipe(deRef(R),Sh,Dp-1)]).
  showTpExp(tpFun("<=>",2),[A,R],Sh,Dp) =>
    ssSeq([shTipe(deRef(A),Sh,Dp-1),ss("<=>"),shTipe(deRef(R),Sh,Dp-1)]).
  showTpExp(tpFun("ref",1),[R],Sh,Dp) =>
    ssSeq([ss("ref"),shTipe(deRef(R),Sh,Dp-1)]).
  showTpExp(tpFun(Nm,Ar),A,Sh,Dp) where size(A)==Ar =>
    ssSeq([ss(Nm),ss("["),ssSeq(showEls(A,Sh,Dp-1,"")),ss("]")]).    
  showTpExp(tpExp(O,A),R,Sh,Dp) =>
    showTpExp(deRef(O),[A,..R],Sh,Dp).
  showTpExp(Op,A,Sh,Dp) =>
    ssSeq([shTipe(deRef(Op),Sh,Dp-1),ss("["),ssSeq(showEls(A,Sh,Dp-1,"")),ss("]")]).    

  shTpExp:(tipe,string,ss,boolean,integer) => ss.
  shTpExp(tpExp(T,A),Sep,R,Sh,Dp) => shTpExp(deRef(T),",",ssSeq([showType(A,Sh,Dp),ss(Sep),R]),Sh,Dp).
  shTpExp(tpFun(Nm,_),Sep,R,_,Dp) => ssSeq([ss(Nm),ss("["),R]).
  shTpExp(kFun(Nm,_),Sep,R,_,Dp) => ssSeq([ss(Nm),ss("["),R]).
  shTpExp(tFun(_,_,Nm),Sep,R,_,Dp) => ssSeq([ss(Nm),ss("["),R]).
  shTpExp(T,Sep,R,Sh,Dp) => ssSeq([showType(T,Sh,Dp),ss("["),R]).

  showAllConstraints([],Dp) => ss("").
  showAllConstraints([C,..Cs],Dp) => ssSeq([showConstraint(C,Dp),..showMoreConstraints(Cs,Dp)]).

  showMoreConstraints([],_) => [ss("|:")].
  showMoreConstraints([C,..Cs],Dp) => [ss(", "),showConstraint(C,Dp),..showMoreConstraints(Cs,Dp)].

  showMoreQuantified(allType(V,T),Sh,Dp) => [ss(","),showBound(V,Dp),..showMoreQuantified(T,Sh,Dp)].
  showMoreQuantified(T,Sh,Dp) => [ss(" ~~ "),showType(T,Sh,Dp)].

  showBound(V,Dp) => showType(V,false,Dp).

  showConstraint(typeConstraint(Tp),Dp) => showType(Tp,false,Dp).
  showConstraint(fieldConstraint(Tp,Fc),Dp) =>
    ssSeq([showType(Tp,false,Dp),ss("<~"),showType(Fc,false,Dp)]).

  -- in general, hashing types is not reliable because of unification
  public implementation hash[tipe] => let {
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
    hsh(funDeps(T,D)) => hshEls(hsh(deRef(T)),D).

    hshCon(typeConstraint(Tp)) => hsh(deRef(Tp)).
    hshCon(fieldConstraint(V,T)) =>
      (hash("<~")*37+hsh(deRef(V)))*37+hsh(deRef(T)).

    hshEls(H,Els) => foldLeft((Hx,El)=>Hx*37+hsh(deRef(El)),H,Els).

    hshFields(H,Els) => foldLeft((Hx,(Nm,Tp))=>(Hx*37+hash(Nm))*37+hsh(deRef(Tp)),H,Els).
  } in {.
    hash(Tp) => hsh(deRef(Tp)).
  .}

  public contract all c ~~ hasType[c] ::= {
    typeOf:(c)=>tipe.
  }

  public typeName:(tipe)=>string.
  typeName(Tp) => let{
    tpName(tipe(Nm)) => Nm.
    tpName(tpExp(O,A)) => tpName(deRef(O)).
    tpName(kVar(Nm)) => Nm.
    tpName(kFun(Nm,_)) => Nm.
    tpName(tpFun(Nm,_)) => Nm.
    tpName(kVar(Nm)) => Nm.
    tpName(tVar(_,_)) => "_".
    tpName(tFun(_,_,_)) => "!_".
    tpName(allType(_,T)) => tpName(deRef(T)).
    tpName(existType(_,T)) => tpName(deRef(T)).
    tpName(constrainedType(T,_)) => tpName(deRef(T)).
    tpName(typeLambda(_,T)) => tpName(deRef(T)).
    tpName(tupleType(A)) => "!()$(size(A))".
    tpName(funDeps(T,_)) => tpName(T).
  } in tpName(Tp).

  public implementationName:(tipe) => string.
  implementationName(Tp) => let{
    surfaceName:(tipe,list[string])=>list[string].
    surfaceName(tipe(Nm),R) => [Nm,"!",..R].
    surfaceName(tpExp(O,A),R) => surfaceName(deRef(O),surfaceNm(deRef(A),R)).
    surfaceName(kVar(Nm),R) => [Nm,"!",..R].
    surfaceName(kFun(Nm,_),R) => [Nm,..R].
    surfaceName(tpFun(Nm,_),R) => [Nm,..R].
    surfaceName(kVar(Nm),R) => [Nm,"!",..R].
    surfaceName(tVar(_,_),R) => ["!_",..R].
    surfaceName(tFun(_,_,_),R) => ["!_",..R].
    surfaceName(allType(_,T),R) => surfaceName(deRef(T),R).
    surfaceName(existType(_,T),R) => surfaceName(deRef(T),R).
    surfaceName(constrainedType(T,_),R) => surfaceName(deRef(T),R).
    surfaceName(typeLambda(_,T),R) => surfaceName(deRef(T),R).
    surfaceName(tupleType(A),R) => ["()$(size(A))!",..R].
    surfaceName(funDeps(T,_),R) => surfaceName(deRef(T),R).

    surfaceNm(tipe(Nm),R) => ["!",Nm,..R].
    surfaceNm(tpExp(O,A),R) => surfaceNm(deRef(O),R).
    surfaceNm(kVar(Nm),R) => ["!",Nm,..R].
    surfaceNm(kFun(Nm,_),R) => ["!",Nm,..R].
    surfaceNm(tpFun(Nm,_),R) => ["!",Nm,..R].
    surfaceNm(kVar(Nm),R) => ["!",Nm,..R].
    surfaceNm(tVar(_,_),R) => ["!_",..R].
    surfaceNm(tFun(_,_,_),R) => ["!_",..R].
    surfaceNm(allType(_,T),R) => surfaceNm(deRef(T),R).
    surfaceNm(existType(_,T),R) => surfaceNm(deRef(T),R).
    surfaceNm(constrainedType(T,_),R) => surfaceNm(deRef(T),R).
    surfaceNm(typeLambda(_,T),R) => surfaceNm(deRef(T),R).
    surfaceNm(tupleType(A),R) => ["!()$(size(A))",..R].
    surfaceNm(funDeps(T,_),R) => surfaceNm(deRef(T),R).
  } in  _str_multicat(surfaceName(deRef(Tp),[])).

  public implementation hasType[constraint] => {.
    typeOf(typeConstraint(Tp)) => Tp.
  .}

  mkTypeExp(Tp,[]) => Tp.
  mkTypeExp(Op,[T,..Rest]) => mkTypeExp(tpExp(Op,T),Rest).

  public funType(A,B) => tpExp(tpExp(tpFun("=>",2),A),B).
  public consType(A,B) => tpExp(tpExp(tpFun("<=>",2),A),B).
  public enumType(A) => tpExp(tpExp(tpFun("<=>",2),tupleType([])),A).

  public intType = tipe("star.core*integer").
  public fltType = tipe("star.core*float").
  public strType = tipe("star.core*string").
  public boolType = tipe("star.core*boolean").
  public lstType(Tp) => tpExp(tpFun("star.core*list",1),Tp).
  public refType(Tp) => tpExp(tpFun("star.core*ref",1),Tp).

  public deQuant:(tipe) => (list[tipe],tipe).
  deQuant(T) => let{
    deQ(allType(V,I),Qs) => deQ(I,[Qs..,V]).
    deQ(Tp,Qs) => (Qs,Tp).
  } in deQ(T,[]).

  public reQuant:(list[tipe],tipe) => tipe.
  reQuant([],Tp) => Tp.
  reQuant([Q,..Qs],Tp) => allType(Q,reQuant(Qs,Tp)).

  public reXQuant:(list[tipe],tipe) => tipe.
  reXQuant([],Tp) => Tp.
  reXQuant([Q,..Qs],Tp) => existType(Q,reXQuant(Qs,Tp)).

  public deConstrain:(tipe) => (list[constraint],tipe).
  deConstrain(T) => let{
    deC(constrainedType(I,V),Qs) => deC(I,[Qs..,V]).
    deC(Tp,Qs) => (Qs,Tp).
  } in deC(T,[]).

  public reConstrain:(list[constraint],tipe) => tipe.
  reConstrain([],Tp) => Tp.
  reConstrain([Q,..Qs],Tp) => constrainedType(reConstrain(Qs,Tp),Q).

  public isConstructorType:(tipe)=>boolean.
  isConstructorType(Tp) => typeName(deRef(Tp))=="<=>".

  public isMapType:(tipe)=>boolean.
  isMapType(Tp) => typeName(deRef(Tp))=="map".

  public typeKey:(tipe) => tipe.
  typeKey(allType(K,T)) => typeKey(T).
  typeKey(existType(K,T)) => typeKey(T).
  typeKey(constrainedType(T,C)) => typeKey(T).
  typeKey(typeLambda(T,_)) => typeKey(T).
  typeKey(typeExists(T,_)) => typeKey(T).
  typeKey(tpExp(O,_)) => typeKey(O).
  typeKey(funDeps(T,_)) => typeKey(T).
  typeKey(T) default => T.
}
