star.compiler.matcher{
  import star.
  import star.sort.

  import star.compiler.term.
  import star.compiler.errors.
--  import star.compiler.freevars.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.normalize.meta.
  import star.compiler.data.
  import star.compiler.types.

  import star.compiler.location.

  all e ~~ triple[e] ~>
    (cons[cExp],(option[locn],cons[(string,cId)],option[cExp],option[cExp],e),integer).

  public functionMatcher:(option[locn],string,tipe,nameMap,cons[(option[locn],cons[cExp],option[cExp],cExp)]) => cDefn.
  functionMatcher(Lc,Nm,Tp,Map,Eqns) => valof{
    NVrs = genVars(Lc,funTypeArg(Tp));
    Trpls = makeTriples(Eqns);
    Error = genRaise(Lc,Nm,funTypeRes(Tp));
--    logMsg("function triples: $(Trpls)");
    Reslt = matchTriples(Lc,NVrs,Trpls,Error,Map);
    valis fnDef(Lc,Nm,Tp,NVrs//(cVar(_,V))=>V,Reslt)
  }

  public caseMatcher:all e ~~ reform[e],rewrite[e] |: (option[locn],nameMap,cExp,e,
    cons[(option[locn],cons[cExp],option[cExp],e)])=>e.
  caseMatcher(Lc,Map,Gov,Deflt,Cs) => valof{
--    logMsg("match cases $(Cs)\ngoverning expression $(Gov)");
    Trpls = makeTriples(Cs);
--    logMsg("case triples $(Trpls)");
    valis matchTriples(Lc,[Gov],Trpls,Deflt,Map)
  }

  genVars:(option[locn],tipe) => cons[cExp].
  genVars(Lc,tupleType(L)) => let{.
    genV([]) => [].
    genV([T,..Ts]) => [cVar(Lc,cId(genSym("_"),T)),..genV(Ts)].
  .} in genV(L).

  makeTriples:all e ~~ reform[e] |: (cons[(option[locn],cons[cExp],option[cExp],e)]) => cons[triple[e]].
  makeTriples(Eqns) => ixRight((Ix,(Lc,Args,Wh,Exp),Ts)=> valof{
      (Vl,Cnd) = pullWhere(Exp,.none);
      valis [(Args,(Lc,[],Cnd,Wh,Vl),Ix),..Ts]
    },[],Eqns).

  genRaise(Lc,Msg,Tp) => cAbort(Lc,Msg,Tp).

  matchTriples:all e~~reform[e],rewrite[e] |: (option[locn],cons[cExp],cons[triple[e]],e,nameMap) => e.
  matchTriples(_,[],Triples,Deflt,_) => conditionalize(Triples,Deflt).
  matchTriples(Lc,Vrs,Triples,Deflt,Map) => valof{
--    logMsg("matching triples, $(Vrs) --- $(Triples), default = $(Deflt)");
    Parts = partitionTriples(Triples);
--    logMsg("partitioned $(Parts)");
    Segs = matchSegments(Parts,Vrs,Lc,Deflt,Map);
--    logMsg("segments = $(Segs)");
    valis Segs
  }.

  partitionTriples:all e ~~ (cons[triple[e]]) => cons[(argMode,cons[triple[e]])].
  partitionTriples([]) => [].
  partitionTriples([Tr,..Triples]) where
      M .= tripleArgMode(Tr) &&
      (P,Ts) .= partTriples(M,Triples,[]) =>
    [(M,[Tr,..P]),..partitionTriples(Ts)].

  partTriples:all e ~~ (argMode,cons[triple[e]],cons[triple[e]])=>
    (cons[triple[e]],cons[triple[e]]).
  partTriples(_,[],S) => (reverse(S),[]).
  partTriples(M,[Tr,..Ts],S) where tripleArgMode(Tr)==M =>
    partTriples(M,Ts,[Tr,..S]).
  partTriples(_,Ts,S) => (S,Ts).

  tripleArgMode:all e ~~ (triple[e]) => argMode.
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

  argMode(cVar(_,_)) => .inVars.
  argMode(cInt(_,_)) => .inScalars.
  argMode(cBig(_,_)) => .inScalars.
  argMode(cFloat(_,_)) => .inScalars.
  argMode(cString(_,_)) => .inScalars.
  argMode(cTerm(_,_,_,_)) => .inConstructors.
  argMode(cWhere(_,T,_)) => argMode(T).

  matchSegments:all e ~~ reform[e],rewrite[e] |:
    (cons[(argMode,cons[triple[e]])],cons[cExp],option[locn],e,nameMap) => e.
  matchSegments([],_,_,Deflt,_) => Deflt.
  matchSegments([(M,Seg),..Segs],Vrs,Lc,Deflt,Map) =>
    compileMatch(M,Seg,Vrs,Lc,matchSegments(Segs,Vrs,Lc,Deflt,Map),Map).

  compileMatch:all e ~~ reform[e],rewrite[e] |:
    (argMode,cons[triple[e]],cons[cExp],option[locn],e,nameMap)=>e.
  compileMatch(.inScalars,Seg,Vrs,Lc,Deflt,Map) =>
    matchScalars(Seg,Vrs,Lc,Deflt,Map).
  compileMatch(.inConstructors,Seg,Vrs,Lc,Deflt,Map) =>
    matchConstructors(Seg,Vrs,Lc,Deflt,Map).
  compileMatch(.inVars,Seg,Vrs,Lc,Deflt,Map) =>
    matchVars(Seg,Vrs,Lc,Deflt,Map).

  matchScalars:all e ~~ reform[e],rewrite[e] |:
    (cons[triple[e]],cons[cExp],option[locn],e,nameMap)=>e.
  matchScalars(Seg,[V,..Vrs],Lc,Deflt,Map) => valof{
    ST = sort(Seg,compareScalarTriple);
    Cases = formCases(ST,sameScalarTriple,Lc,Vrs,Deflt,Map);
    valis mkCase(Lc,V,Cases,Deflt)
  }

  matchConstructors:all e ~~ reform[e],rewrite[e] |:
    (cons[triple[e]],cons[cExp],option[locn],e,nameMap)=>e.
  matchConstructors(Seg,[V,..Vrs],Lc,Deflt,Map) => valof{
    Cases = formCases(sort(Seg,compareConstructorTriple),
      sameConstructorTriple,Lc,Vrs,Deflt,Map);
--    logMsg("look for index map of $(tpName(typeOf(V)))");
    Index = ^findIndexMap(tpName(typeOf(V)),Map);
--    logMsg("index is $(Index)");
    valis mkUnpack(Lc,V,populateArms(Index,Cases,Lc,Deflt,Map))
  }

  populateArms:all e ~~ (consMap,cons[cCase[e]],option[locn],e,nameMap) => cons[cCase[e]].
  populateArms(Index,Cases,Lc,Deflt,Map) =>
    { populateArm(Entry,Cases,Lc,Deflt,Map) | Entry in Index}.

  populateArm((tLbl(FullNm,_),Tp,_),Cases,DLc,Deflt,Map) where
      Arm ^= armPresent(FullNm,Cases) => Arm.
  populateArm((tLbl(FullNm,Ar),CnsTp,_),Cases,Lc,Deflt,Map) =>
    (Lc,emptyCase(Lc,CnsTp,FullNm),Deflt).
    
  armPresent(Nm,[(CLc,cTerm(Lc,Nm,Args,Tp),Exp),.._]) =>
    some((CLc,cTerm(Lc,Nm,Args,Tp),Exp)).
  armPresent(Nm,[_,..Cases]) => armPresent(Nm,Cases).
  armPresent(_,[]) => .none.

  emptyCase:(option[locn],tipe,string)=>cExp.
  emptyCase(Lc,T,Nm) where (tupleType(ArgTps),ResTp) ^= isConsType(T) =>
    cTerm(Lc,Nm,ArgTps//(ATp)=>cVar(Lc,cId("_",ATp)),ResTp).
  
  matchVars:all e ~~ reform[e],rewrite[e] |:
    (cons[triple[e]],cons[cExp],option[locn],e,nameMap)=>e.
  matchVars(Triples,[V,..Vrs],Lc,Deflt,Map) =>
    matchTriples(Lc,Vrs,applyVar(V,Triples),Deflt,Map).

  applyVar:all e ~~ rewrite[e] |: (cExp,cons[triple[e]]) => cons[triple[e]].
  applyVar(V,Triples) where cVar(_,_).=V => let{
    applyToTriple:(triple[e])=>triple[e].
    applyToTriple(([cVar(VLc,cId(Vr,_)),..Args],(CLc,B,AG,Gl,Exp),Ix)) => valof{
      Mp = {Vr->V};
      NArgs = rewriteTerms(Args,Mp);
      NAG = fmap((T)=>rewrite(T,Mp),AG);
      NGl = fmap((T)=>rewrite(T,Mp),Gl);
      NExp = rewrite(Exp,Mp);
      valis (NArgs, (CLc,B,NAG,NGl,NExp),Ix)
    }
    applyToTriple(([cWhere(Lc,cVar(VLc,cId(Vr,_)),Cond),..Args],(CLc,B,AG,Gl,Exp),Ix)) => valof{
      Mp = {Vr->V};
      NArgs = rewriteTerms(Args,Mp);
      NAG = fmap((T)=>rewrite(T,Mp),AG);
      NCond = rewrite(Cond,Mp);
      NGl = fmap((T)=>rewrite(T,Mp),Gl);
      NExp = rewrite(Exp,Mp);
      valis (NArgs, (CLc,B,mergeGoal(VLc,NAG,some(NCond)),NGl,NExp),Ix)
    }
  } in (Triples//applyToTriple).

  -- we need to be careful to preserve the original order of equations
  formCases:all e ~~ reform[e],rewrite[e] |: (cons[triple[e]],(triple[e],triple[e])=>boolean,
    option[locn],cons[cExp],e,nameMap) => cons[cCase[e]].
  formCases([],_,_,_,_,_) => [].
  formCases([Tr,..Triples],Eq,Lc,Vrs,Deflt,Map) => valof{
--    logMsg("case lead triple $(Tr)");
    (Tx,More) = pickMoreCases(Tr,Triples,Eq,[],[]);
--    logMsg("form case from $([Tr,..Tx])");
    Case = formCase(Tr,sort([Tr,..Tx],compareTriple),Lc,Vrs,Deflt,Map);
    valis [Case,..formCases(sort(More,compareTriple),Eq,Lc,Vrs,Deflt,Map)].
  }

  formCase:all e ~~ reform[e],rewrite[e] |: (triple[e],cons[triple[e]],option[locn],cons[cExp],e,nameMap) => cCase[e].
  formCase(([cInt(LLc,Ix),.._],_,_),Tpls,Lc,Vars,Deflt,Map) =>
    (LLc,cInt(LLc,Ix),matchTriples(Lc,Vars,subTriples(Tpls),Deflt,Map)).
  formCase(([cFloat(LLc,Dx),.._],_,_),Tpls,Lc,Vars,Deflt,Map) =>
    (LLc,cFloat(LLc,Dx),matchTriples(Lc,Vars,subTriples(Tpls),Deflt,Map)).
  formCase(([cString(LLc,Sx),.._],_,_),Tpls,Lc,Vars,Deflt,Map) =>
    (LLc,cString(LLc,Sx),matchTriples(Lc,Vars,subTriples(Tpls),Deflt,Map)).
  formCase(([cTerm(Lc,Lbl,Args,Tp),.._],_,_),Triples,_,Vars,Deflt,Map) => valof{
    Vrs = (Args//(E) => cVar(Lc,cId(genSym("_"),typeOf(E))));
    NTriples = subTriples(Triples);
    Case = matchTriples(Lc,Vrs++Vars,NTriples,Deflt,Map);
    valis (Lc,cTerm(Lc,Lbl,Vrs,Tp),Case)
  }.

  pickMoreCases:all e ~~ (triple[e],cons[triple[e]],(triple[e],triple[e])=>boolean,
    cons[triple[e]],cons[triple[e]])=> (cons[triple[e]],cons[triple[e]]).
  pickMoreCases(_,[],_,InCase,Others) => (reverse(InCase),reverse(Others)).
  pickMoreCases(Tr,[A,..Triples],Test,InCase,Others) where Test(Tr,A) =>
    pickMoreCases(Tr,Triples,Test,[A,..InCase],Others).
  pickMoreCases(Tr,[A,..Triples],Test,InCase,Others) =>
    pickMoreCases(Tr,Triples,Test,InCase,[A,..Others]).

  subTriples(Tpls) => (Tpls//subTriple).
    
  subTriple(([cTerm(_,_,CArgs,_),..Args],V,X)) =>
    (CArgs++Args,V,X).
  subTriple(([_,..Args],V,X)) => (Args,V,X).

  conditionalize:all e ~~ reform[e] |: (cons[triple[e]],e)=>e.
  conditionalize([],Deflt) => Deflt.
  conditionalize([(_,(Lc,Bnds,ArgCond,Test,Val),_),..Triples],Deflt) => valof{
    (Vl,Cnd) = pullWhere(Val,Test);
    EqnCnd = mergeGoal(Lc,ArgCond,Cnd);
    if Tst ^= EqnCnd then
      valis applyBindings(Lc,Bnds,
	mkCond(Lc,Tst,Vl,conditionalize(Triples,Deflt)))
    else
    valis applyBindings(Lc,Bnds,Vl)
  }

  applyBindings:all e ~~ reform[e] |: (option[locn],cons[(string,cId)],e) => e.
  applyBindings(Lc,Bnds,Val) => valof{
    VBnds = (Bnds^/(((Nm,_))=>~isUnderscoreName(Nm)));
    if isEmpty(VBnds) then
      valis Val
    else
    valis varNames(Lc,VBnds,Val)
  }

  compareScalarTriple:all e ~~ (triple[e],triple[e]) => boolean.
  compareScalarTriple(([A,.._],_,_),([B,.._],_,_)) => compareScalar(A,B).

  compareScalar(cInt(_,A),cInt(_,B)) => A=<B.
  compareScalar(cFloat(_,A),cFloat(_,B)) => A=<B.
  compareScalar(cString(_,A),cString(_,B)) => A=<B.
  compareScalar(_,_) default => .false.

  sameScalarTriple:all e ~~ (triple[e],triple[e]) => boolean.
  sameScalarTriple(([cInt(_,A),.._],_,_),([cInt(_,B),.._],_,_)) => A==B.
  sameScalarTriple(([cFloat(_,A),.._],_,_),([cFloat(_,B),.._],_,_)) => A==B.
  sameScalarTriple(([cString(_,A),.._],_,_),([cString(_,B),.._],_,_)) => A==B.
  sameScalarTriple(_,_) default => .false.

  compareTriple:all e ~~ (triple[e],triple[e]) => boolean.
  compareTriple((_,_,IxA),(_,_,IxB)) => IxA<IxB.

  compareConstructorTriple:all e ~~ (triple[e],triple[e]) => boolean.
  compareConstructorTriple(([A,.._],_,IxA),([B,.._],_,IxB)) =>
    (sameConstructor(A,B) ? IxA<IxB || compareConstructor(A,B)).

  compareConstructor(cTerm(_,A,_,_),cTerm(_,B,_,_)) => A<B.
  
  sameConstructorTriple(([A,.._],_,_),([B,.._],_,_)) => sameConstructor(A,B).
  sameConstructor(cTerm(_,A,_,_), cTerm(_,B,_,_)) => A==B.
  sameConstructor(_,_) default => .false.

  pullVarLets:(cons[cExp],cExp)=>(cons[cExp],cExp).
  pullVarLets(Vrs,cLtt(Lc,V,A,Exp)) =>
    pullVarLets(Vrs//replaceWith(A,cVar(Lc,V)),Exp).
  pullVarLets(Vrs,Exp) => (Vrs,Exp).

  replaceWith:(cExp,cExp) => (cExp)=>cExp.
  replaceWith(A,B) => (X) => (A==X?B||X).
}
