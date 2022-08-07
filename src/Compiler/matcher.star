star.compiler.matcher{
  import star.
  import star.sort.

  import star.compiler.term.
  import star.compiler.errors.
  import star.compiler.freevars.
  import star.compiler.misc.
  import star.compiler.normalize.meta.
  import star.compiler.data.
  import star.compiler.types.

  import star.compiler.location.

  triple ~> (cons[crExp],(option[locn],cons[(string,crVar)],option[crExp],option[crExp],crExp),integer).

  public functionMatcher:(option[locn],string,tipe,nameMap,cons[(option[locn],cons[crExp],option[crExp],crExp)]) => crDefn.
  functionMatcher(Lc,Nm,Tp,Map,Eqns) => valof{
    NVrs .= genVars(Lc,funTypeArg(Tp));
    Trpls .= makeTriples(Eqns);
    Error .= genRaise(Lc,Nm,funTypeRes(Tp));
--    logMsg("function triples: $(Trpls)");
    Reslt .= matchTriples(Lc,NVrs,Trpls,Error,Map);
    valis fnDef(Lc,Nm,Tp,NVrs//(idnt(_,V))=>V,Reslt)
  }

  public caseMatcher:(option[locn],nameMap,crExp,
    cons[(option[locn],cons[crExp],option[crExp],crExp)],tipe)=>crExp.
  caseMatcher(Lc,Map,Gov,Cs,Tp) => valof{
--    logMsg("match cases $(Cs)\ngoverning expression $(Gov)");
    Trpls .= makeTriples(Cs);
--    logMsg("case triples $(Trpls)");
    Error .= genRaise(Lc,"no case matches",Tp);
    valis matchTriples(Lc,[Gov],Trpls,Error,Map)
  }

  genVars:(option[locn],tipe) => cons[crExp].
  genVars(Lc,tupleType(L)) => let{.
    genV([]) => [].
    genV([T,..Ts]) => [idnt(Lc,crId(genSym("_"),T)),..genV(Ts)].
  .} in genV(L).

  makeTriples:(cons[(option[locn],cons[crExp],option[crExp],crExp)]) => cons[triple].
  makeTriples(Eqns) => ixRight((Ix,(Lc,Args,Wh,Exp),Ts)=>
      [(Args,(Lc,[],.none,Wh,Exp),Ix),..Ts],[],Eqns).

  genRaise(Lc,Msg,Tp) => crAbort(Lc,Msg,Tp).

  matchTriples:(option[locn],cons[crExp],cons[triple],crExp,nameMap) => crExp.
  matchTriples(_,[],Triples,Deflt,_) => conditionalize(Triples,Deflt).
  matchTriples(Lc,Vrs,Triples,Deflt,Map) => valof{
--    logMsg("matching triples, $(Vrs) --- $(Triples), default = $(Deflt)");
    Parts .= partitionTriples(Triples);
--    logMsg("partitioned $(Parts)");
    Segs .= matchSegments(Parts,Vrs,Lc,Deflt,Map);
--    logMsg("segments = $(Segs)");
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

  argMode(idnt(_,_)) => .inVars.
  argMode(intgr(_,_)) => .inScalars.
  argMode(bigx(_,_)) => .inScalars.
  argMode(flot(_,_)) => .inScalars.
  argMode(strg(_,_)) => .inScalars.
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
--    logMsg("look for index map of $(tpName(typeOf(V)))");
    Index ^= findIndexMap(tpName(typeOf(V)),Map);
--    logMsg("index is $(Index)");
    valis crUnpack(Lc,V,populateArms(Index,Cases,Lc,Deflt,Map),typeOf(Deflt))
  }

  populateArms:(consMap,cons[crCase],option[locn],crExp,nameMap) => cons[crCase].
  populateArms(Index,Cases,Lc,Deflt,Map) =>
    { populateArm(Entry,Cases,Lc,Deflt,Map) | Entry in Index}.

  populateArm((tLbl(FullNm,_),Tp,_),Cases,DLc,Deflt,Map) where
      Arm ^= armPresent(FullNm,Cases) => Arm.
  populateArm((tLbl(FullNm,Ar),CnsTp,_),Cases,Lc,Deflt,Map) =>
    (Lc,emptyCase(Lc,CnsTp,FullNm),Deflt).
    
/*
  populateArms([],_,_,_,_) => [].
  populateArms([(tLbl(FullNm,_),_,Tp),..Index],Cases,DLc,Deflt,Map) where
      Arm ^= armPresent(FullNm,Cases) =>
    [Arm,..populateArms(Index,Cases,DLc,Deflt,Map)].
  populateArms([(tLbl(FullNm,Ar),_,CnsTp),..Index],Cases,Lc,Deflt,Map) =>
    [(Lc,emptyCase(Lc,CnsTp,FullNm),Deflt),..populateArms(Index,Cases,Lc,Deflt,Map)].
*/

  armPresent(Nm,[(CLc,crTerm(Lc,Nm,Args,Tp),Exp),.._]) =>
    some((CLc,crTerm(Lc,Nm,Args,Tp),Exp)).
  armPresent(Nm,[_,..Cases]) => armPresent(Nm,Cases).
  armPresent(_,[]) => .none.

  emptyCase:(option[locn],tipe,string)=>crExp.
  emptyCase(Lc,T,Nm) where (tupleType(ArgTps),ResTp) ^= isConsType(T) =>
    crTerm(Lc,Nm,ArgTps//(ATp)=>idnt(Lc,crId("_",ATp)),ResTp).
  
  matchVars(Triples,[V,..Vrs],Lc,Deflt,Map) =>
    matchTriples(Lc,Vrs,applyVar(V,Triples),Deflt,Map).

  applyVar:(crExp,cons[triple]) => cons[triple].
  applyVar(V,Triples) where idnt(_,_).=V => let{
    applyToTriple:(triple)=>triple.
    applyToTriple(([idnt(VLc,crId(Vr,_)),..Args],(CLc,B,AG,Gl,Exp),Ix)) => valof{
    Mp .= {Vr->V};
      NArgs .= rewriteTerms(Args,Mp);
      NAG .= fmap((T)=>rewriteTerm(T,Mp),AG);
      NGl .= fmap((T)=>rewriteTerm(T,Mp),Gl);
      NExp .= rewriteTerm(Exp,Mp);
      valis (NArgs, (CLc,B,NAG,NGl,NExp),Ix)
    }
    applyToTriple(([crWhere(Lc,idnt(VLc,crId(Vr,_)),Cond),..Args],(CLc,B,AG,Gl,Exp),Ix)) => valof{
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
  formCases:(cons[triple],(triple,triple)=>boolean,option[locn],cons[crExp],crExp,nameMap) =>
    cons[crCase].
  formCases([],_,_,_,_,_) => [].
  formCases([Tr,..Triples],Eq,Lc,Vrs,Deflt,Map) => valof{
--    logMsg("case lead triple $(Tr)");
    (Tx,More) .= pickMoreCases(Tr,Triples,Eq,[],[]);
--    logMsg("form case from $([Tr,..Tx])");
    Case .= formCase(Tr,sort([Tr,..Tx],compareTriple),Lc,Vrs,Deflt,Map);
    valis [Case,..formCases(sort(More,compareTriple),Eq,Lc,Vrs,Deflt,Map)].
  }

  formCase:(triple,cons[triple],option[locn],cons[crExp],crExp,nameMap) => crCase.
  formCase(([intgr(LLc,Ix),.._],_,_),Tpls,Lc,Vars,Deflt,Map) =>
    (LLc,intgr(LLc,Ix),matchTriples(Lc,Vars,subTriples(Tpls),Deflt,Map)).
  formCase(([flot(LLc,Dx),.._],_,_),Tpls,Lc,Vars,Deflt,Map) =>
    (LLc,flot(LLc,Dx),matchTriples(Lc,Vars,subTriples(Tpls),Deflt,Map)).
  formCase(([strg(LLc,Sx),.._],_,_),Tpls,Lc,Vars,Deflt,Map) =>
    (LLc,strg(LLc,Sx),matchTriples(Lc,Vars,subTriples(Tpls),Deflt,Map)).
  formCase(([crTerm(Lc,Lbl,Args,Tp),.._],_,_),Triples,_,Vars,Deflt,Map) => valof{
    Vrs .= (Args//(E) => idnt(Lc,crId(genSym("_"),typeOf(E))));
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

  mkCase:(cons[crCase],option[locn],crExp,crExp) => crExp.
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

  compareScalar(intgr(_,A),intgr(_,B)) => A=<B.
  compareScalar(flot(_,A),flot(_,B)) => A=<B.
  compareScalar(strg(_,A),strg(_,B)) => A=<B.
  compareScalar(_,_) default => .false.

  sameScalarTriple:(triple,triple) => boolean.
  sameScalarTriple(([intgr(_,A),.._],_,_),([intgr(_,B),.._],_,_)) => A==B.
  sameScalarTriple(([flot(_,A),.._],_,_),([flot(_,B),.._],_,_)) => A==B.
  sameScalarTriple(([strg(_,A),.._],_,_),([strg(_,B),.._],_,_)) => A==B.
  sameScalarTriple(_,_) default => .false.

  compareTriple:(triple,triple) => boolean.
  compareTriple((_,_,IxA),(_,_,IxB)) => IxA<IxB.

  compareConstructorTriple:(triple,triple) => boolean.
  compareConstructorTriple(([A,.._],_,IxA),([B,.._],_,IxB)) =>
    (sameConstructor(A,B) ? IxA<IxB || compareConstructor(A,B)).

  compareConstructor(crTerm(_,A,_,_),crTerm(_,B,_,_)) => A<B.
  
  sameConstructorTriple(([A,.._],_,_),([B,.._],_,_)) => sameConstructor(A,B).
  sameConstructor(crTerm(_,A,_,_), crTerm(_,B,_,_)) => A==B.
  sameConstructor(_,_) default => .false.

  pullVarLets:(cons[crExp],crExp)=>(cons[crExp],crExp).
  pullVarLets(Vrs,crLtt(Lc,V,A,Exp)) =>
    pullVarLets(Vrs//replaceWith(A,idnt(Lc,V)),Exp).
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

  mergeGoal:(option[locn],option[crExp],option[crExp])=>option[crExp].
  mergeGoal(_,G,.none) => G.
  mergeGoal(_,.none,G) => G.
  mergeGoal(Lc,some(G),some(H)) => some(crCnj(Lc,G,H)).
}
