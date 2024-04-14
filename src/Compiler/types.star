star.compiler.types{
  import star.
  import star.iterable.
  import star.sort.

  import star.compiler.location.
  import star.compiler.misc.

  public tipe ::= .voidType |
    .anonType |
    .kFun(string,integer) |
    .tVar(tv,string) |
    .tFun(tv,integer,string) |
    .nomnal(string) |
    .tpFun(string,integer) |
    .tpExp(tipe,tipe) |
    .tupleType(cons[tipe]) |
    .allType(tipe,tipe) |
    .existType(tipe,tipe) |
    .faceType(cons[(string,tipe)],cons[(string,typeRule)]) |
    .constrainedType(tipe,constraint).

  public typeRule ::= .typeExists(tipe,tipe) |
    .contractExists(string,cons[tipe],cons[tipe],tipe) |
    .typeLambda(tipe,tipe) |
    .allRule(tipe,typeRule).

  public constraint ::=
    .conTract(string,cons[tipe],cons[tipe]) |
    .hasField(tipe,string,tipe) |
    .implicit(string,tipe) |
    .raisEs(tipe).

  tv ::= tv{
    binding : ref option[tipe].
  }

  hasKind:(tipe)=>integer.
  hasKind(Tp) => case Tp in {
    .anonType => 0.
    .voidType => 0.
    .kFun(_,Ar) => Ar.
    .tVar(_,_) => 0.
    .tFun(_,Ar,_) => Ar.
    .nomnal(_) => 0.
    .tpFun(_,Ar) => Ar.
    .tpExp(Op,_) => hasKind(Op)-1.
    .tupleType(_) => 0.
    .allType(_,T) => hasKind(T).
    .existType(_,T) => hasKind(T).
    .faceType(_,_) => 0.
    .constrainedType(T,_) => hasKind(T).
  }
  
  public isIdenticalVar:(tipe,tipe) => boolean.
  isIdenticalVar(T1,T2) => isIdent(deRef(T1),deRef(T2)).

  isIdent(.tVar(_,K1),.tVar(_,K2)) => K1==K2.
  isIdent(.tFun(_,A1,K1),.tFun(_,A2,K2)) => K1==K2 && A1==A2.
  isIdent(_,_) default => .false.

  public isUnbound:(tipe) => boolean.
  isUnbound(.tVar(B,_)) => ((T?=B.binding!) ?? isUnbound(T) || .true).
  isUnbound(.tFun(B,_,_)) => ((T?=B.binding!) ?? isUnbound(T) || .true).
  isUnbound(_) default => .false.

  public isUnboundFVar:(tipe) => option[integer].
  isUnboundFVar(.tVar(B,_)) => ((T?=B.binding!) ?? isUnboundFVar(T) || .none).
  isUnboundFVar(.tFun(B,Ar,_)) =>
    ((T?=B.binding!) ?? isUnboundFVar(T) || .some(Ar)).
  isUnboundFVar(.kFun(B,Ar)) => .some(Ar).
  isUnboundFVar(_) default => .none.

  public setBinding:(tipe,tipe) => ().
  setBinding(.tVar(B,_),T) where 0==hasKind(T) => bnd(B,T).
  setBinding(.tFun(B,Ar,_),T) where hasKind(T)==Ar => bnd(B,T).

  bnd:(tv,tipe) => ().
  bnd(B,T) where B.binding! == .none => valof {
    B.binding := .some(T);
    valis ()
  }
  bnd(_,_) default => ().

  public resetBinding:(tipe) => ().
  resetBinding(.tVar(B,_)) => valof {
    B.binding := .none;
    valis ()
  }
  resetBinding(.tFun(B,_,_)) => valof{
    B.binding := .none;
    valis ()
  }

  public deRef:(tipe) => tipe.
  deRef(.tVar(B,_)) where T?=B.binding! => deRef(T).
  deRef(.tFun(B,_,_)) where T?=B.binding! => deRef(T).
  deRef(Tp) default => Tp.

  public newTypeVar:(string) => tipe.
  newTypeVar(Pre) => .tVar(tv{ binding = ref .none. },genSym(Pre)).

  public newTypeFun:(string,integer) => tipe.
  newTypeFun(Pre,Ax) => .tFun(tv{ binding = ref .none. },Ax,genSym(Pre)).

  public mkTypeExp:(tipe,cons[tipe])=>tipe.
  mkTypeExp(Tp,[]) => Tp.
  mkTypeExp(Tp,[A,..L]) => mkTypeExp(.tpExp(Tp,A),L).

  public makeTpExp:(string,cons[tipe]) => tipe.
  makeTpExp(Nm,Args) => mkTypeExp(.tpFun(Nm,[|Args|]),Args).

  public getTypeArgs:(tipe) => option[(tipe,cons[tipe])].
  getTypeArgs(T) => case deRef(T) in {
    .tpExp(O,A) where [F,..As] .= unravelTArgs(deRef(O),[A]) => .some((F,As)).
    _ default => .none
  }

  unravelTArgs:(tipe,cons[tipe]) => cons[tipe].
  unravelTArgs(.tpExp(O,A),S) => unravelTArgs(deRef(O),[A,..S]).
  unravelTArgs(T,S) => [T,..S].

  public mkConType:(string,cons[tipe],cons[tipe])=>tipe.
  mkConType(N,T,D) where L .= T++D => mkTypeExp(.tpFun(N,size(L)),L).

  public implementation equality[tipe] => {
    T1==T2 => eqType(T1,T2,[]).
  }

  public implementation equality[typeRule] => {
    T1==T2 => identTypeRule(T1,T2,[]).
  }
  
  eqType:(tipe,tipe,cons[(tipe,tipe)]) => boolean.
  eqType(T1,T2,L) => identType(deRef(T1),deRef(T2),L).

  identType:(tipe,tipe,cons[(tipe,tipe)]) => boolean.
  identType(.voidType,.voidType,_) => .true.
  identType(.anonType,.anonType,_) => .true.
  identType(.kFun(N1,A1),.kFun(N2,A2),_) => N1==N2 && A1==A2.
  identType(.tVar(_,N1),.tVar(_,N2),_) => N1==N2.
  identType(.tFun(_,A1,N1),.tFun(_,A2,N2),_) => N1==N2 && A1==A2.
  identType(.nomnal(N1),.nomnal(N2),_) => N1==N2.
  identType(.tpFun(N1,A1),.tpFun(N2,A2),_) => N1==N2 && A1==A2.
  identType(.tpExp(O1,A1),.tpExp(O2,A2),Q) =>
    eqType(O1,O2,Q) && eqType(A1,A2,Q).
  identType(.tupleType(E1),.tupleType(E2),Q) where size(E1)==size(E2) => identTypes(E1,E2,Q).
  identType(.allType(V1,T1),.allType(V2,T2),Q) =>
    eqType(V1,V2,Q) && eqType(T1,T2,Q).
  identType(.existType(V1,T1),.existType(V2,T2),Q) =>
    eqType(V1,V2,Q) && eqType(T1,T2,Q).
  identType(.faceType(V1,T1),.faceType(V2,T2),Q) =>
    identNmTypes(V1,V2,Q) && identRules(T1,T2,Q).
  identType(.constrainedType(T1,C1),.constrainedType(T2,C2),Q) =>
    eqType(T1,T2,Q).
  identType(_,_,_) default => .false.

  identTypes([],[],_) => .true.
  identTypes([E1,..L1],[E2,..L2],Q) =>
    eqType(E1,E2,Q) && identTypes(L1,L2,Q).

  identTypeRule(.typeExists(V1,T1),.typeExists(V2,T2),Q) =>
    eqType(V1,V2,Q) && eqType(T1,T2,Q).
  identTypeRule(.contractExists(N1,A1,D1,T1),.contractExists(N2,A2,D2,T2),Q) =>
    N1==N2 && identTypes(A1,A2,Q) && identTypes(D1,D2,Q) && eqType(T1,T2,Q).
  identTypeRule(.allRule(Q1,R1),.allRule(Q2,R2),Q) =>
    eqType(Q1,Q2,[]) && identTypeRule(R1,R2,Q).
  identTypeRule(.typeLambda(V1,T1),.typeLambda(V2,T2),Q) =>
    eqType(V1,V2,Q) && eqType(T1,T2,Q).

  eqConstraint(.conTract(N1,T1,D1),.conTract(N2,T2,D2),Q) =>
    N1==N2 && identTypes(T1,T2,Q) && identTypes(D1,D2,Q).
  eqConstraint(.hasField(T1,F,R1),.hasField(T2,F,R2),Q) =>
    eqType(T1,T2,Q) && eqType(R1,R2,Q).
  eqConstraint(.implicit(N1,T1),.implicit(N2,T2),Q) =>
    N1==N2 && eqType(T1,T2,Q).
  eqConstraint(.raisEs(T1),.raisEs(T2),Q) => eqType(T1,T2,Q).
  eqConstraint(_,_,_) default => .false.

  identNmTypes(L1,L2,Q) => let{.
    identPrs([],[]) => .true.
    identPrs([(Nm,E1),..l1],[(Nm,E2),..l2]) =>
      eqType(E1,E2,Q) && identPrs(l1,l2).
    identPrs(_,_) => .false.
  .} in identPrs(sortFields(L1),sortFields(L2)).

  identRules(L1,L2,Q) => let{.
    identRls([],[]) => .true.
    identRls([(N,R1),..Rs1],[(N,R2),..Rs2]) =>
      identTypeRule(R1,R2,Q) && identRls(Rs1,Rs2).
    identRls(_,_) default => .false.
  .} in identRls(sortFields(L1),sortFields(L2)).

  public implementation equality[constraint] => {
    .conTract(N1,T1,D1) == .conTract(N2,T2,D2) => N1==N2 && T1==T2 && D1==D2.
    .hasField(V1,N1,T1) == .hasField(V2,N2,T2) => N1==N2 && V1==V2 && T1==T2.
    .implicit(N1,T1) == .implicit(N2,T2) => N1==N2 && T1==T2.
    .raisEs(T1) == .raisEs(T2) => T1==T2.
    _ == _ default => .false.
  }

  public implementation display[tipe] => {
    disp(T) => showType(T,10000)
  }

  public implementation display[constraint] => {
    disp(C) => showConstraint(C,4)
  }

  public implementation display[typeRule] => {
    disp(T) => shTipeRule(T,10000)
  }

  public showType:(tipe,integer) => string.
  showType(T,Dp) => shTipe(deRef(T),Dp).

  shTipe:(tipe,integer) => string.
  shTipe(Tp,Dp) => case Tp in {
    .anonType => "_".
    .voidType => "void".
    .kFun(Nm,Ar) => "#(Nm)/$(Ar)".
    .tVar(V,Nm) => "%#(Nm)".
    .tFun(_,Ar,Nm) => "%#(Nm)/$(Ar)".
    .nomnal(Nm) => Nm.
    .tpFun(Nm,Ar) => "#(Nm)/$(Ar)".
    .tpExp(O,A) => showTpExp(deRef(O),[A],Dp).
    .tupleType(A) => "(#(showTypes(A,Dp)*))".
    .allType(A,T) =>
      "all #(showBound(A,Dp))#(showMoreQuantified(T,Dp))".
    .existType(A,T) =>
      "exists #(showBound(A,Dp))#(showMoreQuantified(T,Dp))".
    .faceType(Els,Tps) => "{#(showTypeEls(Els,Tps,Dp))}".
    .constrainedType(T,C) =>
      "#(showConstraint(C,Dp)) |: #(showType(T,Dp))".
  }

  shContract(Nm,Tps,[],Dp) => "#(Nm)[#(showTypes(Tps,Dp)*)]".
  shContract(Nm,Tps,Dps,Dp) => "#(Nm)[#(showTypes(Tps,Dp)*)->>#(showTypes(Dps,Dp)*)]".
  
  showTypes:(cons[tipe],integer) => cons[string].
  showTypes(_,0) => ["..."].
  showTypes(E,Dp) => showEls(E,Dp-1,"").

  shTipeRule(.typeExists(A,T),Dp) =>
    "#(showType(A,Dp)) <~ #(showType(T,Dp))".
  shTipeRule(.contractExists(N,A,D,T),Dp) => "#(shContract(N,A,D,Dp)) ::= #(shTipe(T,Dp))".
  shTipeRule(.allRule(Q,R),Dp) =>
    "all #(showBound(Q,Dp)) #(showMoreQRule(R,Dp))".
  shTipeRule(.typeLambda(A,T),Dp) =>
    "#(showType(A,Dp)) ~> #(showType(T,Dp))".

  showMoreQRule(.allRule(Q,R),Dp) =>
    ", #(showBound(Q,Dp))#(showMoreQRule(R,Dp))".
  showMoreQRule(R,Dp) =>
    " ~~ #(shTipeRule(R,Dp))".
  
  showEls:(cons[tipe],integer,string) => cons[string].
  showEls([],_,_) => [].
  showEls([T,..Tps],Dp,Sep) => [Sep,showType(T,Dp),..showEls(Tps,Dp,", ")].

  showTypeEls:(cons[(string,tipe)],cons[(string,typeRule)],integer) => string.
  showTypeEls(Els,Tps,Dp) =>
    interleave({"#(Nm)\:#(showType(Tp,Dp))" | (Nm,Tp) in Els} ++
      { shTipeRule(Rl,Dp) | (Nm,Rl) in Tps},".\n")*.

  showTpExp:(tipe,cons[tipe],integer) => string.
  showTpExp(.tpFun("=>",2),[A,R],Dp) => "#(showType(A,Dp-1)) => #(showType(R,Dp-1))".
  showTpExp(.tpFun("<=>",2),[A,R],Dp) => "#(showType(A,Dp-1)) <=> #(showType(R,Dp-1))".
  showTpExp(.tpFun("ref",1),[R],Dp) => "ref #(showType(R,Dp-1))".
  showTpExp(.tpFun(Nm,Ar),A,Dp) where size(A)==Ar => "#(Nm)[#(showTypes(A,Dp-1)*)]".    
  showTpExp(.tpExp(O,A),R,Dp) => showTpExp(deRef(O),[A,..R],Dp).
  showTpExp(Op,A,Dp) => "#(showType(Op,Dp-1))[#(showTypes(A,Dp-1)*)]".    

  shTpExp:(tipe,string,string,integer) => string.
  shTpExp(.tpExp(T,A),Sep,R,Dp) => shTpExp(deRef(T),",","#(showType(A,Dp))#(Sep)#(R)",Dp).
  shTpExp(.tpFun(Nm,_),Sep,R,Dp) => "#(Nm)[#(R)".
  shTpExp(.kFun(Nm,_),Sep,R,Dp) => "#(Nm)[#(R)".
  shTpExp(.tFun(_,_,Nm),Sep,R,Dp) => "#(Nm)[#(R)".
  shTpExp(T,Sep,R,Dp) => "#(showType(T,Dp))[#(R)".

  showAllConstraints([],Dp) => "".
  showAllConstraints([C,..Cs],Dp) => "#(showConstraint(C,Dp))#(showMoreConstraints(Cs,Dp))".

  showMoreConstraints:(cons[constraint],integer) => string.
  showMoreConstraints([],_) => "|:".
  showMoreConstraints([C,..Cs],Dp) => ", #(showConstraint(C,Dp))#(showMoreConstraints(Cs,Dp))".

  showMoreQuantified(.allType(V,T),Dp) => ", #(showBound(V,Dp))#(showMoreQuantified(T,Dp))".
  showMoreQuantified(T,Dp) => " ~~ #(showType(T,Dp))".

  showBound(V,Dp) => showType(V,Dp).

  showConstraint(.conTract(Nm,T,D),Dp) => shContract(Nm,T,D,Dp).
  showConstraint(.hasField(Tp,Fld,Fc),Dp) => "#(showType(Tp,Dp)) <~ {#(Fld):#(showType(Fc,Dp))}".
  showConstraint(.implicit(Fld,Tp),Dp) => "#(Fld) : #(showType(Tp,Dp))".
  showConstraint(.raisEs(Tp),Dp) => "raises #(showType(Tp,Dp))".

  showDeps([],_) => "".
  showDeps(Els,Dp) => "->>#(showTypes(Els,Dp)*)".
  
  -- in general, hashing types is not reliable because of unification
  public implementation hashable[tipe] => let{.
    hsh(Tp) => case Tp in {
      .kFun(Nm,Ar) => Ar*37+hash(Nm).
      .tVar(_,Nm) => hash("V")+hash(Nm).
      .tFun(_,Ar,Nm) => (hash("F")+Ar)*37+hash(Nm).
      .nomnal(Nm) => hash(Nm).
      .tpFun(Nm,Ar) => Ar*37+hash(Nm).
      .tpExp(O,A) => hsh(deRef(O))*37+hsh(deRef(A)).
      .tupleType(Els) => hshEls((hash("()")*37+size(Els))*37,Els).
      .faceType(Els,Tps) =>
	hshRules(hshFields(hash("{}")*37+size(Els)+size(Tps),Els),Tps).
      .allType(V,T) => (hash("all")*37+hsh(deRef(V)))*37+hsh(deRef(T)).
      .existType(V,T) => (hash("exist")*37+hsh(deRef(V)))*37+hsh(deRef(T)).
      .constrainedType(T,C) => (hash("|:")*37+hsh(deRef(T)))*37+hshCon(C).
    }

    hshCon(.conTract(N,T,D)) => hshEls(hshEls(hash(N)*37,T),D).
    hshCon(.implicit(N,T)) => hash(N)*37+hash(T).
    hshCon(.raisEs(T)) => hash(T).
    hshCon(.hasField(V,F,T)) =>
      ((hash("<~")*37+hash(F))*37+hsh(deRef(V)))*37+hsh(deRef(T)).

    hshEls(H,Els) => foldLeft((El,Hx)=>Hx*37+hsh(deRef(El)),H,Els).

    hshFields(H,Els) => foldLeft(((Nm,Tp),Hx)=>(Hx*37+hash(Nm))*37+hsh(deRef(Tp)),H,Els).
    hshRules(H,Rls) => foldLeft((Rl,Hx)=>hash(Rl),H,Rls).
  .} in {
    hash(Tp) => hsh(deRef(Tp)).
  }

  public implementation hashable[typeRule] => let{.
    hashEls(Els) => foldLeft((El,Hx)=>Hx*37+hash(deRef(El)),0,Els).
    
    hshRl(Rle) => case Rle in {
      .typeExists(L,R) => (hash("<~")*37+hash(deRef(L)))*37+hash(deRef(R)).
      .contractExists(N,Ts,Ds,R) =>
	(((hash("contract")*37+hash(N))*37+hashEls(Ts))*37+hashEls(Ds))*37+
	hash(deRef(R)).
      .typeLambda(L,R) => (hash("~>")*37+hash(deRef(L)))*37+hash(deRef(R)).
      .allRule(V,Rl) => (hash("all")*37+hash(deRef(V)))*37+hshRl(Rl).
    }.
  .} in {
    hash(Rl) => hshRl(Rl)
  }

  public contract all c ~~ hasType[c] ::= {
    typeOf:(c)=>tipe.
  }

  public tpName:(tipe)=>string.
  tpName(Tp) => let{.
    tName(.anonType) => "_".
    tName(.voidType) => "void".
    tName(.nomnal(Nm)) => Nm.
    tName(.tpExp(O,A)) => tName(deRef(O)).
    tName(.kFun(Nm,_)) => Nm.
    tName(.tpFun(Nm,_)) => Nm.
    tName(.tVar(_,_)) => "_".
    tName(.tFun(_,_,_)) => "!_".
    tName(.allType(_,T)) => tName(deRef(T)).
    tName(.existType(_,T)) => tName(deRef(T)).
    tName(.constrainedType(T,_)) => tName(deRef(T)).
    tName(.tupleType(A)) => "!()$(size(A))".
    tName(.faceType(_,_)) => "{}".
  .} in tName(deRef(Tp)).

  public conTractName:(constraint)=>string.
  conTractName(.conTract(Nm,_,_)) => Nm.

  public implementationName:(constraint) => string.
  implementationName(.conTract(Nm,Tps,_)) =>
    interleave([Nm,..(Tps//(T)=>typeSurfaceNm(deRef(T)))],"!")*.
  implementationName(.raisEs(Tp)) =>
    typeSurfaceNm(deRef(Tp)).

  public typeSurfaceNm:(tipe)=>string.
  typeSurfaceNm(Tp) => let{.
    tpSfNm(.anonType) => "_".
    tpSfNm(.voidType) => "void".
    tpSfNm(.nomnal(Nm)) => Nm.
    tpSfNm(.tpExp(O,A)) => tpSfNm(deRef(O)).
    tpSfNm(.kFun(Nm,_)) => Nm.
    tpSfNm(.tpFun(Nm,_)) => Nm.
    tpSfNm(.tVar(_,_)) => "_".
    tpSfNm(.tFun(_,_,_)) => "_".
    tpSfNm(.allType(_,T)) => tpSfNm(deRef(T)).
    tpSfNm(.existType(_,T)) => tpSfNm(deRef(T)).
    tpSfNm(.constrainedType(T,_)) => tpSfNm(deRef(T)).
    tpSfNm(.tupleType(A)) => "()$(size(A))".
    tpSfNm(.faceType(Flds,_)) =>
      "{}$(hash(interleave(sort(Flds,cmpFlds)//fst,"|")*))".
  .} in tpSfNm(deRef(Tp)).

  public tpRuleName:(typeRule) => string.
  tpRuleName(.typeExists(L,_)) => tpName(deRef(L)).
  tpRuleName(.contractExists(Nm,_,_,_)) => Nm.
  tpRuleName(.typeLambda(L,_)) => tpName(deRef(L)).
  tpRuleName(.allRule(_,Rl)) => tpRuleName(Rl).

  cmpFlds:all x ~~ ((string,x),(string,x))=>boolean.
  cmpFlds((N1,_),(N2,_))=>N1<N2.

  public implementation hasType[tipe] => {
    typeOf = id
  }

  public implementation hasType[constraint] => {
    typeOf(.conTract(Nm,T,D)) => mkConType(Nm,T,D).
    typeOf(.hasField(Tp,_,FTp)) => funType([Tp],FTp).
    typeOf(.implicit(_,Tp)) => Tp.
    typeOf(.raisEs(Tp)) => Tp.
  }

  public implementation all t ~~ hasType[t] |: hasType[cons[t]] => {
    typeOf(L) => .tupleType(L//typeOf)
  }

  public fieldTypes:(tipe)=>option[cons[(string,tipe)]].
  fieldTypes(Tp) where .faceType(Fs,_) .= deRef(Tp) => .some(sortFields(Fs)).
  fieldTypes(_) default => .none.

  public fieldInFace:(tipe,string)=>option[tipe].
  fieldInFace(Tp,Fld) where .faceType(Fs,_) .= deRef(Tp) => {! FTp | (Fld,FTp) in Fs !}.

  public sortFields:all x ~~ (cons[(string,x)])=>cons[(string,x)].
  sortFields(Tps) => sort(Tps,cmpFlds).

  public arity:(tipe)=>integer.
  arity(Tp) => ar(deRef(Tp)).
  ar(Tp) where (A,_) ?= isFunType(Tp) => arity(A).
  ar(Tp) where (A,_) ?= isConsType(Tp) => arity(A).
  ar(Tp) where .tupleType(A).=Tp => size(A).
  ar(Tp) where .allType(_,I) .= Tp => arity(I).
  ar(Tp) where .existType(_,I) .= Tp => arity(I).
  ar(Tp) where .constrainedType(T,_).=Tp => arity(T)+1.
  ar(_) default => 0.

  public emptyFace = .faceType([],[]).

  public funType(A,B) => fnType(.tupleType(A),B).
  public fnType(A,B) => .tpExp(.tpExp(.tpFun("=>",2),A),B).
  public consType(A,B) => .tpExp(.tpExp(.tpFun("<=>",2),A),B).
  public enumType(A) => .tpExp(.tpExp(.tpFun("<=>",2),.tupleType([])),A).
  public lstType(Tp) => .tpExp(.tpFun("star.core*cons",1),Tp).
  public refType(Tp) => .tpExp(.tpFun("ref",1),Tp).
  public optType(Tp) => .tpExp(.tpFun("star.core*option",1),Tp).
  public fiberType(R,S) => .tpExp(.tpExp(.tpFun("fiber",2),R),S).
  public singleType(A) => .tpExp(.tpFun("star.core*single",1),A).
  public futureType(A,B) => .tpExp(.tpExp(.tpFun("future",2),A),B).

  public funTypeArg:(tipe) => option[tipe].
  funTypeArg(Tp) => let{.
    funTpArg(.tpExp(O,_)) where .tpExp(O2,A) .= deRef(O) &&
	.tpFun("=>",2).=deRef(O2) => .some(deRef(A)).
    funTpArg(.tpExp(O,_)) where .tpExp(O2,A) .= deRef(O) &&
	.tpFun("<=>",2).=deRef(O2) => .some(deRef(A)).
    funTpArg(.allType(_,T)) => funTpArg(deRef(T)).
    funTpArg(.constrainedType(T,C)) where FTp ?= funTpArg(deRef(T)) =>
      .some(extendArgType(FTp,.some(C))).
    funTpArg(_) default => .none
  .} in funTpArg(deRef(Tp)).

  public extendFunTp:all x ~~ hasType[x] |: (tipe,option[x])=>tipe.
  extendFunTp(Tp,.none) => Tp.
  extendFunTp(Tp,Vs) where (A,B)?=isFunType(Tp) &&
      .tupleType(Es).=deRef(A) =>
    funType(extendTplType(Es,Vs),B).
  extendFunTp(.allType(V,B),Vs) => .allType(V,extendFunTp(B,Vs)).
  extendFunTp(.existType(V,B),Vs) => .existType(V,extendFunTp(B,Vs)).
  extendFunTp(.constrainedType(T,C),Vs) => .constrainedType(extendFunTp(T,Vs),C).

  extendArgType:all x ~~ hasType[x] |: (tipe,option[x])=>tipe.
  extendArgType(Tp,.none) => Tp.
  extendArgType(Tp,.some(C)) => case deRef(Tp) in {
    .tupleType(Els) => .tupleType([typeOf(C),..Els]).
  }.

  public extendTplType:all x ~~ hasType[x] |:
    (cons[tipe],option[x])=>cons[tipe].
  extendTplType(Es,.none) => Es.
  extendTplType(Es,.some(E)) => [typeOf(E),..Es].



  public funTypeRes(Tp) => funRes(deRef(Tp)).

  funRes(.tpExp(O,R)) where .tpExp(O2,_) .= deRef(O) &&
      .tpFun("=>",2).=deRef(O2) => deRef(R).
  funRes(.tpExp(O,R)) where .tpExp(O2,A) .= deRef(O) &&
      .tpFun("<=>",2).=deRef(O2) => deRef(R).
  funRes(.allType(_,Tp)) => funTypeRes(Tp).
  funRes(.existType(_,Tp)) => funTypeRes(Tp).
  funRes(.constrainedType(T,_))=>funTypeRes(T).

  public isFunType:(tipe) => option[(tipe,tipe)].
  isFunType(Tp) where
      .tpExp(O,B).=deRef(Tp) &&
	  .tpExp(O2,A) .= deRef(O) &&
	      .tpFun("=>",2).=deRef(O2) => .some((A,B)).
  isFunType(_) default => .none.

  public isConsType:(tipe) => option[(tipe,tipe)].
  isConsType(Tp) where
      .tpExp(O,B).=deRef(Tp) &&
	  .tpExp(O2,A) .= deRef(O) &&
	      .tpFun("<=>",2).=deRef(O2) => .some((A,B)).
  isConsType(_) default => .none.
  isConsType(.allType(_,Tp)) => isConsType(deRef(Tp)).
  isConsType(.existType(_,Tp)) => isConsType(deRef(Tp)).
  isConsType(.constrainedType(T,_))=>isConsType(T).

  public isConstrainedType:(tipe) => boolean.
  isConstrainedType(Tp) => isConstrained(deRef(Tp)).

  isConstrained(.allType(_,T)) => isConstrainedType(T).
  isConstrained(.existType(_,T)) => isConstrainedType(T).
  isConstrained(.constrainedType(_,T)) => .true.
  isConstrained(_) default => .false.
  
  public isTupleType:(tipe) => option[(integer,cons[tipe])].
  isTupleType(Tp) =>
    (.tupleType(A) .= deRef(Tp) ?? .some((size(A),A)) || .none).

  public isEnumType:(tipe)=>option[tipe].
  isEnumType(Tp) where (A,T)?=isConsType(Tp) && deRef(A)==.tupleType([]) => .some(T).
  isEnumType(_) default => .none.

  public netEnumType:(tipe)=>tipe.
  netEnumType(T)=>ntEnumTp(deRef(T)).

  ntEnumTp(.allType(V,T))=>.allType(V,netEnumType(T)).
  ntEnumTp(.existType(V,T))=>.existType(V,netEnumType(T)).
  ntEnumTp(.constrainedType(T,C))=>.constrainedType(netEnumType(T),C).
  ntEnumTp(T) where ET ?= isEnumType(T) => ET.
  
  public unitTp = .tupleType([]).
  public chrType = .nomnal("star.core*char").
  public intType = .nomnal("star.core*integer").
  public bigintType = .nomnal("star.core*bigint").
  public fltType = .nomnal("star.core*float").
  public strType = .nomnal("star.core*string").
  public boolType = .nomnal("star.core*boolean").
  public thunkType(T) => makeTpExp("thunk",[T]).
  public ioType = .nomnal("ioHandle").

  public isThunkType(Tp) =>
    (.tpExp(Op,E) .= deRef(Tp) && .tpFun("thunk",1).=deRef(Op) ?? .some(E) || .none).

  public isRefType(Tp) => .tpExp(Op,_) .= deRef(Tp) &&
      .tpFun("ref",1).=deRef(Op).

  public isLambdaRule(.typeLambda(_,_)) => .true.
  isLambdaRule(.allRule(_,T)) => isLambdaRule(T).
  isLambdaRule(_) default => .false.

  public deQuant:(tipe) => (cons[tipe],tipe).
  deQuant(T) => let{.
    deQ(.allType(V,I),Qs) => deQ(I,[V,..Qs]).
    deQ(Tp,Qs) => (reverse(Qs),Tp).
  .} in deQ(T,[]).

  public deQuantRule:(typeRule) => (cons[tipe],typeRule).
  deQuantRule(Rl) => let{.
    deQ(.allRule(V,R),Q) => deQ(R,[V,..Q]).
    deQ(R,Q) => (reverse(Q),R)
  .} in deQ(Rl,[]).

  public reQuant:(cons[tipe],tipe) => tipe.
  reQuant([],Tp) => Tp.
  reQuant([Q,..Qs],Tp) => .allType(Q,reQuant(Qs,Tp)).

  public deQuantX:(tipe) => (cons[tipe],tipe).
  deQuantX(T) => let{.
    deQ(.existType(V,I),Qs) => deQ(I,[V,..Qs]).
    deQ(Tp,Qs) => (reverse(Qs),Tp).
  .} in deQ(T,[]).

  public reQuantX:(cons[tipe],tipe) => tipe.
  reQuantX([],Tp) => Tp.
  reQuantX([Q,..Qs],Tp) => .existType(Q,reQuantX(Qs,Tp)).

  public deConstrain:(tipe) => (cons[constraint],tipe).
  deConstrain(T) => let{.
    deC(.constrainedType(I,V),Qs) => deC(I,[V,..Qs]).
    deC(Tp,Qs) => (reverse(Qs),Tp).
  .} in deC(T,[]).

  public reConstrainType:(cons[constraint],tipe) => tipe.
  reConstrainType([],Tp) => Tp.
  reConstrainType([Q,..Qs],Tp) => .constrainedType(reConstrainType(Qs,Tp),Q).

  public isMapType:(tipe)=>boolean.
  isMapType(Tp) => tpName(deRef(Tp))=="map".

  public typeKey:(tipe) => tipe.
  typeKey(Tp) => case Tp in {
    .allType(_K,T) => typeKey(T).
    .existType(_K,T) => typeKey(T).
    .constrainedType(T,_C) => typeKey(T).
    .tpExp(O,_) => typeKey(deRef(O)).
    T default => T.
  }

  public ruleType:(typeRule) => tipe.
  ruleType(.typeExists(L,_)) => L.
  ruleType(.contractExists(Nm,Tps,Dps,_)) =>
    mkTypeExp(.tpFun(Nm,size(Tps)+size(Dps)),Tps++Dps).
  ruleType(.typeLambda(L,_)) => L.
  ruleType(.allRule(V,Rl)) => .allType(V,ruleType(Rl)).

  public contractType:(typeRule) => tipe.
  contractType(.allRule(Q,R)) => .allType(Q,contractType(R)).
  contractType(.contractExists(Nm,Tps,Dps,_)) =>
    mkTypeExp(.tpFun(Nm,size(Tps)+size(Dps)),Tps++Dps).

  public contractTypeRule:(typeRule) => typeRule.
  contractTypeRule(.allRule(Q,R)) => .allRule(Q,contractTypeRule(R)).
  contractTypeRule(.contractExists(Nm,Tps,Dps,Face)) =>
    .typeExists(mkTypeExp(.tpFun(Nm,size(Tps)+size(Dps)),Tps++Dps),Face).

  public occursIn(TV,Tp) where ~isIdenticalVar(TV,Tp) =>
    occIn(vrNm(TV),deRef(Tp)).

  occIn(Id,.tVar(_,Nm)) => Id==Nm.
  occIn(Id,.tFun(_,_,Nm)) => Id==Nm.
  occIn(Id,.nomnal(Nm)) => Id==Nm.
  occIn(Id,.kFun(Nm,_)) => Id==Nm.
  occIn(Id,.tpExp(O,A)) => occIn(Id,deRef(O)) || occIn(Id,deRef(A)).
  occIn(Id,.tupleType(Els)) => occInTps(Id,Els).
  occIn(Id,.allType(.nomnal(Id),_)) => .false.
  occIn(Id,.allType(_,B)) => occIn(Id,deRef(B)).
  occIn(Id,.existType(.nomnal(Id),_)) => .false.
  occIn(Id,.existType(_,B)) => occIn(Id,deRef(B)).
  occIn(Id,.faceType(Flds,Tps)) => occInPrs(Id,Flds) || occInRules(Id,Tps).
  occIn(Id,.constrainedType(T,C)) => occIn(Id,deRef(T)) || occInCon(Id,C).
  occIn(_,_) default => .false.

  occInCon(Id,.conTract(_,Els,Dps)) => {? El in Els && occIn(Id,deRef(El)) ?} ||
  {? El in Dps && occIn(Id,deRef(El)) ?}.
  occInCon(Id,.hasField(T,_,F)) => occIn(Id,deRef(T)) || occIn(Id,deRef(F)).
  occInCon(Id,.implicit(_,T)) => occIn(Id,deRef(T)).
  occInCon(Id,.raisEs(T)) => occIn(Id,deRef(T)).

  occInTps(Id,Tps) => {? El in Tps && occIn(Id,deRef(El)) ?}.

  occInPrs(Id,Tps) => {? (_,El) in Tps && occIn(Id,deRef(El)) ?}.

  occInRules(Id,Rls) => {? (_,Rl) in Rls && occInRule(Id,Rl) ?}.

  occInRule(Id,.typeExists(L,R)) => occIn(Id,deRef(L)) || occIn(Id,deRef(R)).
  occInRule(Id,.contractExists(_,LTs,DTs,T)) =>
    occInTps(Id,LTs) || occInTps(Id,DTs) || occIn(Id,deRef(T)).
  occInRule(Id,.typeLambda(L,R)) => occIn(Id,deRef(L)) || occIn(Id,deRef(R)).
  occInRule(Id,.allRule(_,Rl)) => occInRule(Id,Rl).

  vrNm(.tVar(_,Nm)) => Nm.
  vrNm(.tFun(_,_,Nm)) => Nm.
  vrNm(.kFun(Nm,_)) => Nm.
  vrNm(.nomnal(Nm)) => Nm.
}
