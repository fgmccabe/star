star.compiler.types{
  import star.
  import star.iterable.
  import star.sort.

  import star.compiler.location.
  import star.compiler.misc.

  public tipe ::= kFun(string,integer) |
    tVar(tv,string) |
    tFun(tv,integer,string) |
    nomnal(string) |
    tpFun(string,integer) |
    tpExp(tipe,tipe) |
    tupleType(cons[tipe]) |
    allType(tipe,tipe) |
    existType(tipe,tipe) |
    faceType(cons[(string,tipe)],cons[(string,tipe)]) |
    typeLambda(tipe,tipe) |
    typeExists(tipe,tipe) |
    contractExists(constraint,tipe) |
    funDeps(tipe,cons[tipe]) |
    constrainedType(tipe,constraint).

  public constraint ::= conTract(tipe) |
    fieldConstraint(tipe,string,tipe).

  tv ::= tv{
    binding : ref option[tipe].
  }

  hasKind:(tipe)=>integer.
  hasKind(kFun(_,Ar)) => Ar.
  hasKind(tVar(_,_)) => 0.
  hasKind(tFun(_,Ar,_)) => Ar.
  hasKind(nomnal(_)) => 0.
  hasKind(tpFun(_,Ar)) => Ar.
  hasKind(tpExp(Op,_)) => hasKind(Op)-1.
  hasKind(tupleType(_)) => 0.
  hasKind(allType(_,T)) => hasKind(T).
  hasKind(existType(_,T)) => hasKind(T).
  hasKind(faceType(_,_)) => 0.
  hasKind(typeLambda(_,_)) => 0.
  hasKind(typeExists(_,_)) => 0.
  hasKind(constrainedType(T,_)) => hasKind(T).
  hasKind(funDeps(T,_)) => hasKind(T).
  
  public isIdenticalVar:(tipe,tipe) => boolean.
  isIdenticalVar(T1,T2) => isIdent(deRef(T1),deRef(T2)).

  isIdent(tVar(_,K1),tVar(_,K2)) => K1==K2.
  isIdent(tFun(_,A1,K1),tFun(_,A2,K2)) => K1==K2 && A1==A2.
  isIdent(_,_) default => .false.

  public isUnbound:(tipe) => boolean.
  isUnbound(tVar(B,_)) => ((T^=B.binding!) ? isUnbound(T) || .true).
  isUnbound(tFun(B,_,_)) => ((T^=B.binding!) ? isUnbound(T) || .true).
  isUnbound(_) default => .false.

  public isUnboundFVar:(tipe) => option[integer].
  isUnboundFVar(tVar(B,_)) => ((T^=B.binding!) ? isUnboundFVar(T) || .none).
  isUnboundFVar(tFun(B,Ar,_)) =>
    ((T^=B.binding!) ? isUnboundFVar(T) || some(Ar)).
  isUnboundFVar(_) default => .none.

  public setBinding:(tipe,tipe) => result[(),()].
  setBinding(tVar(B,_),T) where 0==hasKind(T) => bnd(B,T).
  setBinding(tFun(B,Ar,_),T) where hasKind(T)==Ar => bnd(B,T).

  bnd:(tv,tipe) => result[(),()].
  bnd(B,T) where B.binding! == .none => do {
    B.binding := some(T);
    valis ()
  }
  bnd(_,_) default => ok(()).

  public resetBinding:(tipe) => action[(),()].
  resetBinding(tVar(B,_)) => action {
    B.binding := .none;
    valis ()
  }
  resetBinding(tFun(B,_,_)) => action{
    B.binding := .none;
    valis ()
  }

  public deRef:(tipe) => tipe.
  deRef(tVar(B,_)) where T^=B.binding! => deRef(T).
  deRef(tFun(B,_,_)) where T^=B.binding! => deRef(T).
  deRef(Tp) default => Tp.

  public newTypeVar:(string) => tipe.
  newTypeVar(Pre) => tVar(tv{ binding = ref .none. },genSym(Pre)).

  public newTypeFun:(string,integer) => tipe.
  newTypeFun(Pre,Ax) => tFun(tv{ binding = ref .none. },Ax,genSym(Pre)).

  public mkTypeExp:(tipe,cons[tipe])=>tipe.
  mkTypeExp(Tp,[]) => Tp.
  mkTypeExp(Tp,[A,..L]) => mkTypeExp(tpExp(Tp,A),L).

  public mkConType:(string,cons[tipe],cons[tipe])=>tipe.
  mkConType(Nm,Ts,[]) => mkTypeExp(tpFun(Nm,size(Ts)),Ts).
  mkConType(Nm,Ts,Ds) => funDeps(mkTypeExp(tpFun(Nm,size(Ts)),Ts),Ds).

  public implementation equality[tipe] => {
    T1==T2 => eqType(T1,T2,[]).
  }
  
  eqType:(tipe,tipe,cons[(tipe,tipe)]) => boolean.
  eqType(T1,T2,L) => identType(deRef(T1),deRef(T2),L).

  identType:(tipe,tipe,cons[(tipe,tipe)]) => boolean.
  identType(kFun(N1,A1),kFun(N2,A2),_) => N1==N2 && A1==A2.
  identType(tVar(_,N1),tVar(_,N2),_) => N1==N2.
  identType(tFun(_,A1,N1),tFun(_,A2,N2),_) => N1==N2 && A1==A2.
  identType(nomnal(N1),nomnal(N2),_) => N1==N2.
  identType(tpFun(N1,A1),tpFun(N2,A2),_) => N1==N2 && A1==A2.
  identType(tpExp(O1,A1),tpExp(O2,A2),Q) =>
    eqType(O1,O2,Q) && eqType(A1,A2,Q).
  identType(funDeps(T1,D1),funDeps(T2,D2),Q) =>
    eqType(T1,T2,Q) && identTypes(D1,D2,Q).
  identType(tupleType(E1),tupleType(E2),Q) where size(E1)==size(E2) => identTypes(E1,E2,Q).
  identType(allType(V1,T1),allType(V2,T2),Q) =>
    eqType(V1,V2,Q) && eqType(T1,T2,Q).
  identType(existType(V1,T1),existType(V2,T2),Q) =>
    eqType(V1,V2,Q) && eqType(T1,T2,Q).
  identType(typeLambda(V1,T1),typeLambda(V2,T2),Q) =>
    eqType(V1,V2,Q) && eqType(T1,T2,Q).
  identType(typeExists(V1,T1),typeExists(V2,T2),Q) =>
    eqType(V1,V2,Q) && eqType(T1,T2,Q).
  identType(contractExists(V1,T1),contractExists(V2,T2),Q) =>
    eqConstraint(V1,V2,Q) && eqType(T1,T2,Q).
  identType(faceType(V1,T1),faceType(V2,T2),Q) =>
    identNmTypes(V1,V2,Q) && identNmTypes(T1,T2,Q).
  identType(constrainedType(T1,C1),constrainedType(T2,C2),Q) =>
    eqType(T1,T2,Q).
  identType(_,_,_) default => .false.

  identTypes([],[],_) => .true.
  identTypes([E1,..L1],[E2,..L2],Q) =>
    eqType(E1,E2,Q) && identTypes(L1,L2,Q).

  eqConstraint(conTract(T1),conTract(T2),Q) => eqType(T1,T2,Q).
  eqConstraint(fieldConstraint(T1,F,R1),fieldConstraint(T2,F,R2),Q) =>
    eqType(T1,T2,Q) && eqType(R1,R2,Q).
  eqConstraint(_,_,_) default => .false.

  identNmTypes(L1,L2,Q) => let{.
    sortByNm(LL) => sort(LL,(((N1,_),(N2,_)) => N1<N2)).
    identPrs([],[]) => .true.
    identPrs([(Nm,E1),..l1],[(Nm,E2),..l2]) =>
      eqType(E1,E2,Q) && identPrs(l1,l2).
    identPrs(_,_) => .false.
  .} in identPrs(sortByNm(L1),sortByNm(L2)).

  public implementation equality[constraint] => {
    conTract(T1) == conTract(T2) => T1==T2.
    fieldConstraint(V1,N1,T1) == fieldConstraint(V2,N2,T2) => N1==N2 && V1==V2 && T1==T2.
    _ == _ default => .false.
  }

  public implementation display[tipe] => {
    disp(T) => showType(T,.true,10000)
  }

  public implementation display[constraint] => {
    disp(C) => showConstraint(C,2)
  }

  public showType:(tipe,boolean,integer) => string.
  showType(T,Sh,Dp) => shTipe(deRef(T),Sh,Dp).

  shTipe:(tipe,boolean,integer) => string.
  shTipe(kFun(Nm,Ar),_,_) => "#(Nm)/$(Ar)".
  shTipe(tVar(V,Nm),_,Dp) => "%#(Nm)".
  shTipe(tFun(_,Ar,Nm),_,_) => "%#(Nm)/$(Ar)".
  shTipe(nomnal(Nm),_,_) => Nm.
  shTipe(tpFun(Nm,Ar),_,_) => "#(Nm)/$(Ar)".
  shTipe(tpExp(O,A),Sh,Dp) => showTpExp(deRef(O),[A],Sh,Dp).
  shTipe(tupleType(A),Sh,Dp) => "(#(showTypes(A,Sh,Dp)*))".
  shTipe(allType(A,T),Sh,Dp) =>
    "all #(showBound(A,Dp)) #(showMoreQuantified(T,Sh,Dp))".
  shTipe(existType(A,T),Sh,Dp) =>
    "exists #(showBound(A,Dp)) #(showMoreQuantified(T,Sh,Dp))".
  shTipe(faceType(Els,Tps),Sh,Dp) => "{#(showTypeEls(Els,Tps,Sh,Dp))}".
  shTipe(typeLambda(A,T),Sh,Dp) =>
    "#(showType(A,Sh,Dp)) ~> #(showType(T,Sh,Dp))".
  shTipe(typeExists(A,T),Sh,Dp) =>
    "#(showType(A,Sh,Dp)) <~ #(showType(T,Sh,Dp))".
  shTipe(contractExists(C,T),Sh,Dp) =>
    "#(showConstraint(C,Dp)) ::= #(showType(T,Sh,Dp))".
  shTipe(constrainedType(T,C),Sh,Dp) =>
    "#(showConstraint(C,Dp)) |: #(showType(T,Sh,Dp))".
  shTipe(funDeps(T,D),Sh,Dp) =>
    shTpExp(deRef(T),"->>","#(showTypes(D,Sh,Dp-1)*)]",Sh,Dp-1).
  
  showTypes:(cons[tipe],boolean,integer) => cons[string].
  showTypes(_,_,0) => ["..."].
  showTypes(E,Sh,Dp) => showEls(E,Sh,Dp-1,"").
  
  showEls:(cons[tipe],boolean,integer,string) => cons[string].
  showEls([],_,_,_) => [].
  showEls([T,..Tps],Sh,Dp,Sep) => [Sep,showType(T,Sh,Dp),..showEls(Tps,Sh,Dp,", ")].

  showTypeEls:(cons[(string,tipe)],cons[(string,tipe)],boolean,integer) => string.
  showTypeEls(Els,Tps,Sh,Dp) =>
    interleave({"#(Nm)\:#(showType(Tp,Sh,Dp))" | (Nm,Tp) in Els} ++
      {"type #(Nm)\:#(showType(Tp,Sh,Dp))" | (Nm,Tp) in Tps},".\n")*.

  showTpExp:(tipe,cons[tipe],boolean,integer) => string.
  showTpExp(tpFun("=>",2),[A,R],Sh,Dp) =>
    "#(showType(A,Sh,Dp-1)) => #(showType(R,Sh,Dp-1))".
  showTpExp(tpFun("<=>",2),[A,R],Sh,Dp) =>
    "#(showType(A,Sh,Dp-1)) <=> #(showType(R,Sh,Dp-1))".
  showTpExp(tpFun("=>>",2),[A,R],Sh,Dp) =>
    "#(showType(A,Sh,Dp-1)) =>> #(showType(R,Sh,Dp-1))".
  showTpExp(tpFun("ref",1),[R],Sh,Dp) =>
    "ref #(showType(R,Sh,Dp-1))".
  showTpExp(tpFun(Nm,Ar),A,Sh,Dp) where size(A)==Ar =>
    "#(Nm)[#(showTypes(A,Sh,Dp-1)*)]".    
  showTpExp(tpExp(O,A),R,Sh,Dp) =>
    showTpExp(deRef(O),[A,..R],Sh,Dp).
  showTpExp(Op,A,Sh,Dp) =>
    "#(showType(Op,Sh,Dp-1))[#(showTypes(A,Sh,Dp-1)*)]".    

  shTpExp:(tipe,string,string,boolean,integer) => string.
  shTpExp(tpExp(T,A),Sep,R,Sh,Dp) => shTpExp(deRef(T),",","#(showType(A,Sh,Dp))#(Sep)#(R)",Sh,Dp).
  shTpExp(tpFun(Nm,_),Sep,R,_,Dp) => "#(Nm)[#(R)".
  shTpExp(kFun(Nm,_),Sep,R,_,Dp) => "#(Nm)[#(R)".
  shTpExp(tFun(_,_,Nm),Sep,R,_,Dp) => "#(Nm)[#(R)".
  shTpExp(T,Sep,R,Sh,Dp) => "#(showType(T,Sh,Dp))[#(R)".

  showAllConstraints([],Dp) => "".
  showAllConstraints([C,..Cs],Dp) => "#(showConstraint(C,Dp))#(showMoreConstraints(Cs,Dp))".

  showMoreConstraints:(cons[constraint],integer) => string.
  showMoreConstraints([],_) => "|:".
  showMoreConstraints([C,..Cs],Dp) => ", #(showConstraint(C,Dp))#(showMoreConstraints(Cs,Dp))".

  showMoreQuantified(allType(V,T),Sh,Dp) => ",#(showBound(V,Dp))#(showMoreQuantified(T,Sh,Dp))".
  showMoreQuantified(T,Sh,Dp) => " ~~ #(showType(T,Sh,Dp))".

  showBound(V,Dp) => showType(V,.false,Dp).

  showConstraint(conTract(C),Dp) => disp(C).
  showConstraint(fieldConstraint(Tp,Fld,Fc),Dp) =>
    "#(showType(Tp,.false,Dp)) <~ #(Fld):#(showType(Fc,.false,Dp))".

  showDeps([],_) => "".
  showDeps(Els,Dp) => "->>#(showTypes(Els,.false,Dp)*)".
  
  -- in general, hashing types is not reliable because of unification
  public implementation hashable[tipe] => let{.
    hsh(kFun(Nm,Ar)) => Ar*37+hash(Nm).
    hsh(tVar(_,Nm)) => hash("V")+hash(Nm).
    hsh(tFun(_,Ar,Nm)) => (hash("F")+Ar)*37+hash(Nm).
    hsh(nomnal(Nm)) => hash(Nm).
    hsh(tpFun(Nm,Ar)) => Ar*37+hash(Nm).
    hsh(tpExp(O,A)) => hsh(deRef(O))*37+hsh(deRef(A)).
    hsh(tupleType(Els)) => hshEls((hash("()")*37+size(Els))*37,Els).
    hsh(faceType(Els,Tps)) =>
      hshFields(hshFields(hash("{}")*37+size(Els)+size(Tps),Els),Tps).
    hsh(allType(V,T)) => (hash("all")*37+hsh(deRef(V)))*37+hsh(deRef(T)).
    hsh(existType(V,T)) => (hash("exist")*37+hsh(deRef(V)))*37+hsh(deRef(T)).
    hsh(constrainedType(T,C)) => (hash("|:")*37+hsh(deRef(T)))*37+hshCon(C).
    hsh(funDeps(T,D)) => hshEls(hsh(deRef(T)),D).

    hshCon(conTract(C)) => hash(C).
    hshCon(fieldConstraint(V,F,T)) =>
      ((hash("<~")*37+hash(F))*37+hsh(deRef(V)))*37+hsh(deRef(T)).

    hshEls(H,Els) => foldLeft((El,Hx)=>Hx*37+hsh(deRef(El)),H,Els).

    hshFields(H,Els) => foldLeft(((Nm,Tp),Hx)=>(Hx*37+hash(Nm))*37+hsh(deRef(Tp)),H,Els).
  .} in {
    hash(Tp) => hsh(deRef(Tp)).
  }

  
  public implementation hashable[constraint] => {
    hash(conTract(T)) => hash(T).
    hash(fieldConstraint(V,F,T)) =>
      ((hash("<~")*37+hash(F))*37+hash(deRef(V)))*37+hash(deRef(T)).
  }

  public contract all c ~~ hasType[c] ::= {
    typeOf:(c)=>tipe.
  }

  public tpName:(tipe)=>string.
  tpName(Tp) => let{.
    tName(nomnal(Nm)) => Nm.
    tName(tpExp(O,A)) => tName(deRef(O)).
    tName(kFun(Nm,_)) => Nm.
    tName(tpFun(Nm,_)) => Nm.
    tName(tVar(_,_)) => "_".
    tName(tFun(_,_,_)) => "!_".
    tName(allType(_,T)) => tName(deRef(T)).
    tName(existType(_,T)) => tName(deRef(T)).
    tName(constrainedType(T,_)) => tName(deRef(T)).
    tName(typeLambda(_,T)) => tName(deRef(T)).
    tName(tupleType(A)) => "!()$(size(A))".
    tName(faceType(Fs,Ts)) => "{}".
    tName(funDeps(T,_)) => tName(T).
  .} in tName(deRef(Tp)).

  public implementationName:(tipe) => string.
  implementationName(Tp) => let{.
    surfaceName:(tipe,cons[string])=>cons[string].
    surfaceName(nomnal(Nm),R) => [Nm,"!",..R].
    surfaceName(tpExp(O,A),R) => surfaceName(deRef(O),surfaceNm(deRef(A),R)).
    surfaceName(kFun(Nm,_),R) => [Nm,..R].
    surfaceName(tpFun(Nm,_),R) => [Nm,..R].
    surfaceName(tVar(_,_),R) => ["!_",..R].
    surfaceName(tFun(_,_,_),R) => ["!_",..R].
    surfaceName(allType(_,T),R) => surfaceName(deRef(T),R).
    surfaceName(existType(_,T),R) => surfaceName(deRef(T),R).
    surfaceName(constrainedType(T,_),R) => surfaceName(deRef(T),R).
    surfaceName(typeLambda(_,T),R) => surfaceName(deRef(T),R).
    surfaceName(tupleType(A),R) => ["()$(size(A))!",..R].
    surfaceName(faceType(Flds,_),R) =>
      ["{}$(hash(interleave(sort(Flds,cmpFlds)//fst,"|")*))",..R].
    surfaceName(funDeps(T,_),R) => surfaceName(deRef(T),R).

    surfaceNm(nomnal(Nm),R) => ["!",Nm,..R].
    surfaceNm(tpExp(O,A),R) => surfaceNm(deRef(O),R).
    surfaceNm(kFun(Nm,_),R) => ["!",Nm,..R].
    surfaceNm(tpFun(Nm,_),R) => ["!",Nm,..R].
    surfaceNm(tVar(_,_),R) => ["!_",..R].
    surfaceNm(tFun(_,_,_),R) => ["!_",..R].
    surfaceNm(allType(_,T),R) => surfaceNm(deRef(T),R).
    surfaceNm(existType(_,T),R) => surfaceNm(deRef(T),R).
    surfaceNm(constrainedType(T,_),R) => surfaceNm(deRef(T),R).
    surfaceNm(typeLambda(_,T),R) => surfaceNm(deRef(T),R).
    surfaceNm(tupleType(A),R) => ["!()$(size(A))",..R].
    surfaceNm(funDeps(T,_),R) => surfaceNm(deRef(T),R).
  .} in surfaceName(deRef(Tp),[])*.

  cmpFlds:((string,tipe),(string,tipe))=>boolean.
  cmpFlds((N1,_),(N2,_))=>N1<N2.

  public implementation hasType[constraint] => {.
    typeOf(conTract(C)) => dropTypeDefs(deRef(C)).
    dropTypeDefs(funDeps(O,D)) => mkTypeExp(O,D).
    dropTypeDefs(T) => T.
  .}

  public implementation hasType[tipe] => {
    typeOf = id
  }

  public implementation all t ~~ hasType[t] |: hasType[cons[t]] => {
    typeOf(L) => tupleType(L//typeOf)
  }

  public fieldTypes:(tipe)=>option[cons[(string,tipe)]].
  fieldTypes(Tp) where faceType(Fs,_) .= deRef(Tp) => some(Fs).
  fieldTypes(_) default => .none.

  public fieldInFace:(tipe,string)=>option[tipe].
  fieldInFace(Tp,Fld) where faceType(Fs,_) .= deRef(Tp) => {! FTp | (Fld,FTp) in Fs !}.

  public arity:(tipe)=>integer.
  arity(Tp) where (A,_) ^= isFunType(Tp) => arity(A).
  arity(Tp) where (A,_) ^= isConsType(Tp) => arity(A).
  arity(Tp) where tupleType(A).=deRef(Tp) => size(A).
  arity(_) default => 0.
  
  public funType(A,B) => fnType(tupleType(A),B).
  public fnType(A,B) => tpExp(tpExp(tpFun("=>",2),A),B).
  public consType(A,B) => tpExp(tpExp(tpFun("<=>",2),A),B).
  public contType(A,B) => tpExp(tpExp(tpFun("=>>",2),A),B).
  public cnsType(A,B) => tpExp(tpExp(tpFun("<=>",2),tupleType(A)),B).
  public enumType(A) => tpExp(tpExp(tpFun("<=>",2),tupleType([])),A).
  public memoType(A) => tpExp(tpFun("memo",1),A).

  public funTypeArg(Tp) where
      tpExp(O,_) .= deRef(Tp) &&
      tpExp(O2,A) .= deRef(O) &&
      tpFun("=>",2).=deRef(O2) => deRef(A).
  funTypeArg(Tp) where
      tpExp(O,_) .= deRef(Tp) &&
      tpExp(O2,A) .= deRef(O) &&
      tpFun("<=>",2).=deRef(O2) => deRef(A).
  funTypeArg(allType(_,Tp)) => funTypeArg(deRef(Tp)).
  funTypeArg(constrainedType(T,_))=>funTypeArg(T).

  public funTypeRes(Tp) where
      tpExp(O,R) .= deRef(Tp) &&
      tpExp(O2,_) .= deRef(O) &&
      tpFun("=>",2).=deRef(O2) => deRef(R).
  funTypeRes(Tp) where
      tpExp(O,R) .= deRef(Tp) &&
      tpExp(O2,A) .= deRef(O) &&
      tpFun("<=>",2).=deRef(O2) => deRef(R).
  funTypeRes(allType(_,Tp)) => funTypeRes(deRef(Tp)).
  funTypeRes(constrainedType(T,_))=>funTypeRes(T).

  public isFunType:(tipe) => option[(tipe,tipe)].
  isFunType(Tp) where
      tpExp(O,B).=deRef(Tp) &&
      tpExp(O2,A) .= deRef(O) &&
      tpFun("=>",2).=deRef(O2) => some((A,B)).
  isFunType(_) default => .none.

  public isConsType:(tipe) => option[(tipe,tipe)].
  isConsType(Tp) where
      tpExp(O,B).=deRef(Tp) &&
      tpExp(O2,A) .= deRef(O) &&
      tpFun("<=>",2).=deRef(O2) => some((A,B)).
  isConsType(_) default => .none.

  public isContType:(tipe) => option[(tipe,tipe)].
  isContType(Tp) where
      tpExp(O,B).=deRef(Tp) &&
      tpExp(O2,A) .= deRef(O) &&
      tpFun("=>>",2).=deRef(O2) => some((A,B)).
  isContType(_) default => .none.

  public isEnumType:(tipe)=>option[tipe].
  isEnumType(Tp) where (A,T)^=isConsType(Tp) && deRef(A)==tupleType([]) => some(T).
  isEnumType(_) default => .none.

  public netEnumType:(tipe)=>tipe.
  netEnumType(T)=>ntEnumTp(deRef(T)).

  ntEnumTp(allType(V,T))=>allType(V,netEnumType(T)).
  ntEnumTp(existType(V,T))=>existType(V,netEnumType(T)).
  ntEnumTp(constrainedType(T,C))=>constrainedType(netEnumType(T),C).
  ntEnumTp(T) where ET ^= isEnumType(T) => ET.
  
  public unitTp = tupleType([]).
  public chrType = nomnal("star.core*char").
  public intType = nomnal("star.core*integer").
  public bigintType = nomnal("star.core*bigint").
  public fltType = nomnal("star.core*float").
  public strType = nomnal("star.core*string").
  public boolType = nomnal("star.core*boolean").
  public lstType(Tp) => tpExp(tpFun("star.core*cons",1),Tp).
  public refType(Tp) => tpExp(tpFun("star.core*ref",1),Tp).

  public isRefType(Tp) => tpExp(Op,_) .= deRef(Tp) &&
      tpFun("star.core*ref",1).=deRef(Op).

  public isLambdaRule(Tp) where (_,T).=deQuant(Tp) => typeLambda(_,_).=deRef(T).

  public deQuant:(tipe) => (cons[tipe],tipe).
  deQuant(T) => let{.
    deQ(allType(V,I),Qs) => deQ(I,[V,..Qs]).
    deQ(Tp,Qs) => (reverse(Qs),Tp).
  .} in deQ(T,[]).

  public reQuant:(cons[tipe],tipe) => tipe.
  reQuant([],Tp) => Tp.
  reQuant([Q,..Qs],Tp) => allType(Q,reQuant(Qs,Tp)).

  public reQuantX:(cons[tipe],tipe) => tipe.
  reQuantX([],Tp) => Tp.
  reQuantX([Q,..Qs],Tp) => existType(Q,reQuantX(Qs,Tp)).

  public deConstrain:(tipe) => (cons[constraint],tipe).
  deConstrain(T) => let{.
    deC(constrainedType(I,V),Qs) => deC(I,[V,..Qs]).
    deC(Tp,Qs) => (reverse(Qs),Tp).
  .} in deC(T,[]).

  public reConstrainType:(cons[constraint],tipe) => tipe.
  reConstrainType([],Tp) => Tp.
  reConstrainType([Q,..Qs],Tp) => constrainedType(reConstrainType(Qs,Tp),Q).

  public isMapType:(tipe)=>boolean.
  isMapType(Tp) => tpName(deRef(Tp))=="map".

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
