star.compiler.matcher{
  import star.
  import star.sort.

  import star.compiler.core.
  import star.compiler.errors.
  import star.compiler.freevars.
  import star.compiler.misc.
  import star.compiler.normalize.meta.
  import star.compiler.types.

  import star.compiler.location.

  triple ~> (cons[crExp],(locn,cons[(string,crVar)],option[crExp],option[crExp],crExp),integer).

  public functionMatcher:(locn,string,tipe,nameMap,cons[(locn,cons[crExp],option[crExp],crExp)]) => crDefn.
  functionMatcher(Lc,Nm,Tp,Map,Eqns) => valof{
    NVrs .= genVars(Lc,funTypeArg(Tp));
    Trpls .= makeTriples(Eqns);
    Error .= genRaise(Lc,Nm,funTypeRes(Tp));
    logMsg("function triples: $(Trpls)");
    Reslt .= matchTriples(Lc,NVrs,Trpls,Error,Map);
    valis fnDef(Lc,Nm,Tp,NVrs//(crVar(_,V))=>V,Reslt)
  }

  public caseMatcher:(locn,nameMap,crExp,
    cons[(locn,cons[crExp],option[crExp],crExp)],tipe)=>crExp.
  caseMatcher(Lc,Map,Gov,Cs,Tp) => valof{
--    logMsg("match cases $(Cs)\ngoverning expression $(Gov)");
    Trpls .= makeTriples(Cs);
--    logMsg("case triples $(Trpls)");
    Error .= genRaise(Lc,"no case matches",Tp);
    valis matchTriples(Lc,[Gov],Trpls,Error,Map)
  }

  genVars:(locn,tipe) => cons[crExp].
  genVars(Lc,tupleType(L)) => let{.
    genV([]) => [].
    genV([T,..Ts]) => [crVar(Lc,crId(genSym("_"),T)),..genV(Ts)].
  .} in genV(L).

  makeTriples:(cons[(locn,cons[crExp],option[crExp],crExp)]) => cons[triple].
  makeTriples(Eqns) => ixRight((Ix,(Lc,Args,Wh,Exp),Ts)=>
      [(Args,(Lc,[],.none,Wh,Exp),Ix),..Ts],[],Eqns).

  genRaise(Lc,Msg,Tp) => crAbort(Lc,Msg,Tp).

  matchTriples:(locn,cons[crExp],cons[triple],crExp,nameMap) => crExp.
  matchTriples(_,[],Triples,Deflt,_) => conditionalize(Triples,Deflt).
  matchTriples(Lc,Vrs,Triples,Deflt,Map) => valof{
    logMsg("matching triples, $(Vrs) --- $(Triples), default = $(Deflt)");
    Parts .= partitionTriples(Triples);
    logMsg("partitioned $(Parts)");
    Segs .= matchSegments(Parts,Vrs,Lc,Deflt,Map);
    logMsg("segments = $(Segs)");
    valis Segs
  }.

  partitionTriples:(cons[triple]) => cons[(argMode,cons[triple])].
  partitionTriples([]) => [].
  partitionTriples([Tr,..Triples]) where
      M .= tripleArgMode(Tr) &&
      (P,Ts) .= partTriples(M,Triples,[]) =>
    [(M,[Tr,..P]),..partitionTriples(Ts)].

  partTriples:(argMode,cons[triple],cons[triple])=>(cons[triple],cons[triple]).
  partTriples(_,[],S) => (reverse(S),[]).
  partTriples(M,[Tr,..Ts],S) where tripleArgMode(Tr)==M =>
    partTriples(M,Ts,[Tr,..S]).
  partTriples(_,Ts,S) => (S,Ts).

  tripleArgMode:(triple) => argMode.
  tripleArgMode(([A,.._],_,_)) => argMode(A).

  argMode ::= .inVars | .inScalars | .inConstructors.

  implementation display[argMode] => {
    disp(.inVars) => "inVars".
    disp(.inScalars) => "inScalars".
    disp(.inConstructors) => "inConstructors".
  }

  implementation equality[argMode] => {
    .inVars == .inVars => .true.
    .inScalars == .inScalars => .true.
    .inConstructors == .inConstructors => .true.
    _ == _ default => .false.
  }

  argMode(crVar(_,_)) => .inVars.
  argMode(crInt(_,_)) => .inScalars.
  argMode(crBig(_,_)) => .inScalars.
  argMode(crFlot(_,_)) => .inScalars.
  argMode(crStrg(_,_)) => .inScalars.
  argMode(crLbl(_,_,_)) => .inConstructors.
  argMode(crTerm(_,_,_,_)) => .inConstructors.
  argMode(crWhere(_,T,_)) => argMode(T).

  matchSegments([],_,_,Deflt,_) => Deflt.
  matchSegments([(M,Seg),..Segs],Vrs,Lc,Deflt,Map) =>
    compileMatch(M,Seg,Vrs,Lc,matchSegments(Segs,Vrs,Lc,Deflt,Map),Map).

  compileMatch(.inScalars,Seg,Vrs,Lc,Deflt,Map) =>
    matchScalars(Seg,Vrs,Lc,Deflt,Map).
  compileMatch(.inConstructors,Seg,Vrs,Lc,Deflt,Map) =>
    matchConstructors(Seg,Vrs,Lc,Deflt,Map).
  compileMatch(.inVars,Seg,Vrs,Lc,Deflt,Map) =>
    matchVars(Seg,Vrs,Lc,Deflt,Map).

  matchScalars(Seg,[V,..Vrs],Lc,Deflt,Map) => valof{
    ST .= sort(Seg,compareScalarTriple);
    Cases .= formCases(ST,sameScalarTriple,Lc,Vrs,Deflt,Map);
    valis mkCase(Cases,Lc,V,Deflt)
  }

  matchConstructors(Seg,[V,..Vrs],Lc,Deflt,Map) => valof{
    Cases .= formCases(sort(Seg,compareConstructorTriple),
      sameConstructorTriple,Lc,Vrs,Deflt,Map);
    Index .= findIndexMap(typeOf(V),Map);
    valis crUnpack(Lc,V,populateArms(Index,Cases,Lc,Map,Deflt),typeOf(Deflt))
  }

  populateArgs([],_,_,_,_) => [].
  populateArms([(crLbl(Lc,FullNm,Tp),Ix),..Index],Cases,DLc,Map,Deflt) where
      (Ptn,AExp,ALc) ^= armPresent(FullNm,Cases) =>
    [(Ptn,AExp,ALc),..populateArms(Index,Cases,DLc,Map,Deflt)].
  populateArms([(crLbl(Lc,FullNm,Tp),Ix),..Index],Cases,DLc,Map,Deflt) where
      moduleCons(_,CnsTp)^=lookupVarName(Map,FullNm) =>
    [(emptyCase(CnsTp,FullNm),Deflt,DLc),..populateArms(Index,Cases,Map,DLc,Deflt)].

  armPresent(Nm,[(crLbl(Lc,Nm,Tp),Exp,CLc),.._]) => some((crLbl(Lc,Nm,Tp),Exp,CLc)).
  armPresent(Nm,[(crTerm(Lc,Nm,Args,Tp),Exp,CLc),.._]) => some((crTerm(Lc,Nm,Args,Tp),Exp,CLc)).
  armPresent(Nm,[_,..Cases]) => armPresent(Nm,Cases).
  armPresent(_,[]) => .none.
  
  matchVars(Triples,[V,..Vrs],Lc,Deflt,Map) =>
    matchTriples(Lc,Vrs,applyVar(V,Triples),Deflt,Map).

  applyVar:(crExp,cons[triple]) => cons[triple].
  applyVar(V,Triples) where crVar(_,_).=V => let{
    applyToTriple:(triple)=>triple.
    applyToTriple(([crVar(VLc,crId(Vr,_)),..Args],(CLc,B,AG,Gl,Exp),Ix)) => valof{
    Mp .= {Vr->V};
      NArgs .= rewriteTerms(Args,Mp);
      NAG .= fmap((T)=>rewriteTerm(T,Mp),AG);
      NGl .= fmap((T)=>rewriteTerm(T,Mp),Gl);
      NExp .= rewriteTerm(Exp,Mp);
      valis (NArgs, (CLc,B,NAG,NGl,NExp),Ix)
    }
    applyToTriple(([crWhere(Lc,crVar(VLc,crId(Vr,_)),Cond),..Args],(CLc,B,AG,Gl,Exp),Ix)) => valof{
    Mp .= {Vr->V};
      NArgs .= rewriteTerms(Args,Mp);
      NAG .= fmap((T)=>rewriteTerm(T,Mp),AG);
      NCond .= rewriteTerm(Cond,Mp);
      NGl .= fmap((T)=>rewriteTerm(T,Mp),Gl);
      NExp .= rewriteTerm(Exp,Mp);
      valis (NArgs, (CLc,B,mergeGoal(VLc,NAG,some(NCond)),NGl,NExp),Ix)
    }
  } in (Triples//applyToTriple).

  -- we need to be careful to preserve the original order of equations
  formCases:(cons[triple],(triple,triple)=>boolean,locn,cons[crExp],crExp,nameMap) =>
    cons[crCase].
  formCases([],_,_,_,_,_) => [].
  formCases([Tr,..Triples],Eq,Lc,Vrs,Deflt,Map) => valof{
--    logMsg("case lead triple $(Tr)");
    (Tx,More) .= pickMoreCases(Tr,Triples,Eq,[],[]);
--    logMsg("form case from $([Tr,..Tx])");
    Case .= formCase(Tr,sort([Tr,..Tx],compareTriple),Lc,Vrs,Deflt,Map);
    valis [Case,..formCases(sort(More,compareTriple),Eq,Lc,Vrs,Deflt,Map)].
  }

  formCase:(triple,cons[triple],locn,cons[crExp],crExp,nameMap) => crCase.
  formCase(([crInt(LLc,Ix),.._],_,_),Tpls,Lc,Vars,Deflt,Map) =>
    (LLc,crInt(LLc,Ix),matchTriples(Lc,Vars,subTriples(Tpls),Deflt,Map)).
  formCase(([crFlot(LLc,Dx),.._],_,_),Tpls,Lc,Vars,Deflt,Map) =>
    (LLc,crFlot(LLc,Dx),matchTriples(Lc,Vars,subTriples(Tpls),Deflt,Map)).
  formCase(([crStrg(LLc,Sx),.._],_,_),Tpls,Lc,Vars,Deflt,Map) =>
    (LLc,crStrg(LLc,Sx),matchTriples(Lc,Vars,subTriples(Tpls),Deflt,Map)).
  formCase(([crLbl(LLc,Nm,LTp),.._],_,_),Tpls,Lc,Vars,Deflt,Map) =>
    (LLc,crLbl(LLc,Nm,LTp),matchTriples(Lc,Vars,subTriples(Tpls),Deflt,Map)).
  formCase(([crTerm(Lc,Lbl,Args,Tp),.._],_,_),Triples,_,Vars,Deflt,Map) => valof{
    Vrs .= (Args//(E) => crVar(Lc,crId(genSym("_"),typeOf(E))));
    NTriples .= subTriples(Triples);
    Case .= matchTriples(Lc,Vrs++Vars,NTriples,Deflt,Map);
    valis (Lc,crTerm(Lc,Lbl,Vrs,Tp),Case)
  }.

  pickMoreCases:(triple,cons[triple],(triple,triple)=>boolean,
    cons[triple],cons[triple])=> (cons[triple],cons[triple]).
  pickMoreCases(_,[],_,InCase,Others) => (reverse(InCase),reverse(Others)).
  pickMoreCases(Tr,[A,..Triples],Test,InCase,Others) where Test(Tr,A) =>
    pickMoreCases(Tr,Triples,Test,[A,..InCase],Others).
  pickMoreCases(Tr,[A,..Triples],Test,InCase,Others) =>
    pickMoreCases(Tr,Triples,Test,InCase,[A,..Others]).

  mkCase:(cons[crCase],locn,crExp,crExp) => crExp.
  mkCase([(PLc,Ptn,Val)],Lc,Tst,Deflt) => mkCond(Lc,crMatch(PLc,Ptn,Tst),Val,Deflt).
  mkCase(Cases,Lc,V,Deflt) => crCase(Lc,V,Cases,Deflt,typeOf(Deflt)).

  subTriples(Tpls) => (Tpls//subTriple).
    
  subTriple(([crTerm(_,_,CArgs,_),..Args],V,X)) =>
    (CArgs++Args,V,X).
  subTriple(([_,..Args],V,X)) => (Args,V,X).

  conditionalize:(cons[triple],crExp)=>crExp.
  conditionalize([],Deflt) => Deflt.
  conditionalize([(_,(Lc,Bnds,ArgCond,Test,Val),_),..Triples],Deflt) => valof{
    (Vl,Cnd) .= pullWhere(Val,Test);
    EqnCnd .= mergeGoal(Lc,ArgCond,Cnd);
    if Tst ^= EqnCnd then
      valis applyBindings(Bnds,Lc,
	mkCond(Lc,Tst,Vl,conditionalize(Triples,Deflt)))
    else
    valis applyBindings(Bnds,Lc,Vl)
  }

  mkCond(Lc,Tst,Th,El) where 
      crCnd(_,T1,Th1,El1).=Th && El1==El =>
    crCnd(Lc,crCnj(Lc,Tst,T1),Th1,El1).
  mkCond(Lc,Tst,Th,El) =>
    crCnd(Lc,Tst,Th,El).

  applyBindings(Bnds,Lc,Val) => valof{
    VBnds .= (Bnds^/(((Nm,_))=>~isUnderscoreName(Nm)));
    if isEmpty(VBnds) then
      valis Val
    else
    valis crVarNames(Lc,VBnds,Val)
  }

  compareScalarTriple:(triple,triple) => boolean.
  compareScalarTriple(([A,.._],_,_),([B,.._],_,_)) => compareScalar(A,B).

  compareScalar(crInt(_,A),crInt(_,B)) => A=<B.
  compareScalar(crFlot(_,A),crFlot(_,B)) => A=<B.
  compareScalar(crStrg(_,A),crStrg(_,B)) => A=<B.
  compareScalar(_,_) default => .false.

  sameScalarTriple:(triple,triple) => boolean.
  sameScalarTriple(([crInt(_,A),.._],_,_),([crInt(_,B),.._],_,_)) => A==B.
  sameScalarTriple(([crFlot(_,A),.._],_,_),([crFlot(_,B),.._],_,_)) => A==B.
  sameScalarTriple(([crStrg(_,A),.._],_,_),([crStrg(_,B),.._],_,_)) => A==B.
  sameScalarTriple(_,_) default => .false.

  compareTriple:(triple,triple) => boolean.
  compareTriple((_,_,IxA),(_,_,IxB)) => IxA<IxB.

  compareConstructorTriple:(triple,triple) => boolean.
  compareConstructorTriple(([A,.._],_,IxA),([B,.._],_,IxB)) =>
    (sameConstructor(A,B) ? IxA<IxB || compareConstructor(A,B)).

  compareConstructor(crTerm(_,A,_,_),crTerm(_,B,_,_)) => A<B.
  compareConstructor(crLbl(_,A,_),crLbl(_,B,_)) => A<B.
  compareConstructor(crLbl(_,A,_),crTerm(_,B,_,_)) => A<B.
  compareConstructor(crTerm(_,A,_,_),crLbl(_,B,_)) => A<B.
  
  sameConstructorTriple(([A,.._],_,_),([B,.._],_,_)) => sameConstructor(A,B).
  sameConstructor(crTerm(_,A,_,_), crTerm(_,B,_,_)) => A==B.
  sameConstructor(crLbl(_,A,Ar),crLbl(_,B,Br)) => A==B && Ar==Br.
  sameConstructor(_,_) default => .false.

  pullVarLets:(cons[crExp],crExp)=>(cons[crExp],crExp).
  pullVarLets(Vrs,crLtt(Lc,V,A,Exp)) =>
    pullVarLets(Vrs//replaceWith(A,crVar(Lc,V)),Exp).
  pullVarLets(Vrs,Exp) => (Vrs,Exp).

  replaceWith:(crExp,crExp) => (crExp)=>crExp.
  replaceWith(A,B) => (X) => (A==X?B||X).

  pullWhere:(crExp,option[crExp]) => (crExp,option[crExp]).
  pullWhere(crWhere(Lc,V,C),G) where (Val,G1) .= pullWhere(V,G) =>
    (Val,mergeGoal(Lc,some(C),G1)).
  pullWhere(crTerm(Lc,Lbl,Args,Tp),G) where (NArgs,Gx) .= pullWheres(Args,G) =>
    (crTerm(Lc,Lbl,NArgs,Tp),Gx).
  pullWhere(Exp,G) default => (Exp,G).

  pullWheres([],G) => ([],G).
  pullWheres([A,..As],G) where (NA,NG).=pullWhere(A,G) && (NAs,Gx) .= pullWheres(As,NG) =>
    ([NA,..NAs],Gx).

  mergeGoal:(locn,option[crExp],option[crExp])=>option[crExp].
  mergeGoal(_,G,.none) => G.
  mergeGoal(_,.none,G) => G.
  mergeGoal(Lc,some(G),some(H)) => some(crCnj(Lc,G,H)).
}
