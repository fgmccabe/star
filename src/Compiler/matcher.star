star.compiler.matcher{
  import star.
  import star.sort.

  import star.compiler.term.
  import star.compiler.errors.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.normalize.meta.
  import star.compiler.data.
  import star.compiler.types.

  import star.compiler.location.

  all e ~~ triple[e] ~>
    (cons[cExp],(option[locn],cons[(string,cId)],option[cExp],e),integer).

  public functionMatcher:(option[locn],string,tipe,nameMap,cons[(option[locn],cons[cExp],option[cExp],cExp)]) => option[cDefn].
  functionMatcher(Lc,Nm,Tp,Map,Eqns) => valof{
    if FTp ?= funTypeArg(Tp) then{
      NVrs = genVars(Lc,FTp);
      Trpls = makeTriples(Eqns);

      if traceNormalize! then
	showMsg("generate matcher for #(Nm), new args = $(NVrs), initial triples $(Trpls)");

      Error = genRaise(Lc,Nm,funTypeRes(Tp));
      Reslt = matchTriples(Lc,NVrs,Trpls,Error,0,Map);
      valis .some(.fnDef(Lc,Nm,Tp,NVrs//(.cVar(_,V))=>V,Reslt))
    }
    else{
      reportError("Cant create a match for non function type $(Tp)",Lc);
      valis .none
    }
  }

  public caseMatcher:all e ~~ reform[e],rewrite[e],display[e] |: (option[locn],nameMap,cExp,e,
    cons[(option[locn],cons[cExp],option[cExp],e)])=>e.
  caseMatcher(Lc,Map,Gov,Deflt,Cs) => valof{
    -- if traceNormalize! then
    --   showMsg("match cases $(Cs)\ngoverning expression $(Gov)\:$(typeOf(Gov))");
    Trpls = makeTriples(Cs);
    valis matchTriples(Lc,[Gov],Trpls,Deflt,0,Map)
  }

  genVars:(option[locn],tipe) => cons[cExp].
  genVars(Lc,.tupleType(L)) => let{.
    genV([]) => [].
    genV([T,..Ts]) => [.cVar(Lc,.cId(genId("_"),T)),..genV(Ts)].
  .} in genV(L).

  makeTriples:all e ~~ reform[e] |: (cons[(option[locn],cons[cExp],option[cExp],e)]) => cons[triple[e]].
  makeTriples(Eqns) => ixRight((Ix,(Lc,Args,Wh,Exp),Ts)=> valof{
      (Vl,Cnd) = pullWhere(Exp);
      valis [(Args,(Lc,[],mergeGoal(Lc,Cnd,Wh),Vl),Ix),..Ts]
    },[],Eqns).

  genRaise(Lc,Msg,Tp) => .cAbort(Lc,Msg,Tp).

  matchTriples:all e~~reform[e],rewrite[e],display[e] |: (option[locn],cons[cExp],cons[triple[e]],e,integer,nameMap) => e.
  matchTriples(_,[],Triples,Deflt,_,_) => conditionalize(Triples,Deflt).
  matchTriples(Lc,Vrs,Triples,Deflt,Depth,Map) => valof{
    Parts = partitionTriples(Triples);
    if traceNormalize! then
      showMsg("partition triples into $(Parts)");
    valis matchSegments(Parts,Vrs,Lc,Deflt,Depth,Map)
  }.

  partitionTriples:all e ~~ (cons[triple[e]]) => cons[(argMode,cons[triple[e]])].
  partitionTriples([]) => [].
  partitionTriples([Tr,..Triples]) where
      M .= tripleArgMode(Tr) &&
	  (P,Ts) .= partTriples(M,Triples,[]) =>
    [(M,sort([Tr,..P],byEqnNo)),..partitionTriples(Ts)].

  byEqnNo:all e ~~ (triple[e],triple[e])=>boolean.
  byEqnNo((_,_,E1),(_,_,E2)) => E1<E2.

  partTriples:all e ~~ (argMode,cons[triple[e]],cons[triple[e]])=>
    (cons[triple[e]],cons[triple[e]]).
  partTriples(_,[],S) => (reverse(S),[]).
  partTriples(M,[Tr,..Ts],S) where tripleArgMode(Tr)==M =>
    partTriples(M,Ts,[Tr,..S]).
  partTriples(_,Ts,S) => (S,Ts).

  tripleArgMode:all e ~~ (triple[e]) => argMode.
  tripleArgMode(([A,.._],_,_)) => argMode(A).

  argMode ::= .inVars | .inScalars | .inConstructors | .inTuples.

  implementation display[argMode] => {
    disp(.inVars) => "inVars".
    disp(.inScalars) => "inScalars".
    disp(.inConstructors) => "inConstructors".
    disp(.inTuples) => "inTuples".
  }

  implementation equality[argMode] => {
    .inVars == .inVars => .true.
    .inScalars == .inScalars => .true.
    .inConstructors == .inConstructors => .true.
    .inTuples == .inTuples => .true.
    _ == _ default => .false.
  }

  argMode(.cAnon(_,_)) => .inVars.
  argMode(.cVar(_,_)) => .inVars.
  argMode(.cInt(_,_)) => .inScalars.
  argMode(.cBig(_,_)) => .inScalars.
  argMode(.cFlt(_,_)) => .inScalars.
  argMode(.cChar(_,_)) => .inScalars.
  argMode(.cString(_,_)) => .inScalars.
  argMode(.cTerm(_,Op,_,_)) where isTplLbl(Op) => .inTuples.
  argMode(.cTerm(_,_,_,_)) => .inConstructors.

  matchSegments:all e ~~ reform[e],rewrite[e],display[e] |:
    (cons[(argMode,cons[triple[e]])],cons[cExp],option[locn],e,integer,nameMap) => e.
  matchSegments([],_,_,Deflt,_,_) => Deflt.
  matchSegments([(M,Seg),..Segs],Vrs,Lc,Deflt,Depth,Map) =>
    compileMatch(M,Seg,Vrs,Lc,matchSegments(Segs,Vrs,Lc,Deflt,Depth,Map),Depth,Map).

  compileMatch:all e ~~ reform[e],rewrite[e],display[e] |:
    (argMode,cons[triple[e]],cons[cExp],option[locn],e,integer,nameMap)=>e.
  compileMatch(_,Seg,Vrs,Lc,Deflt,Depth,_Map) where tooDeep(Depth) => valof{
    if traceNormalize! then
      showMsg("generate condition from $(Seg)");
    valis conditionMatch(Seg,Vrs,Deflt)
  }.
  compileMatch(.inScalars,Seg,Vrs,Lc,Deflt,Depth,Map) =>
    matchScalars(Seg,Vrs,Lc,Deflt,Depth,Map).
  compileMatch(.inConstructors,Seg,Vrs,Lc,Deflt,Depth,Map) =>
    matchConstructors(Seg,Vrs,Lc,Deflt,Depth,Map).
  compileMatch(.inTuples,Seg,Vrs,Lc,Deflt,Depth,Map) =>
    matchTuples(Seg,Vrs,Lc,Deflt,Depth,Map).
  compileMatch(.inVars,Seg,Vrs,Lc,Deflt,Depth,Map) =>
    matchVars(Seg,Vrs,Lc,Deflt,Depth,Map).

  -- We use this to convert rules into a conditional expression
  -- when regular match too expensive
  conditionMatch:all e ~~ reform[e],rewrite[e],display[e] |:
    (cons[triple[e]],cons[cExp],e) => e.
  conditionMatch([],_,Deflt) => Deflt.
  conditionMatch([(Args,(Lc,Bnds,Test,Val),_),..M],Vrs,Deflt) => valof{
    (Vl,Cnd) = pullWhere(Val);

    if traceNormalize! then
      showMsg("generate condition match $(Args) .= $(Vrs)");
    (Tst,Res) = mkMatchCond(Args,Vrs,mergeGoal(Lc,Cnd,Test),Lc,Vl);
    if traceNormalize! then
      showMsg("match cond $(Tst) ?? $(Res)");
    Other = conditionMatch(M,Vrs,Deflt);

    if Cond?=Tst then
      valis mkCond(Lc,Cond,Res,Other)
    else
    valis Res
  }

  mkMatchCond:all e ~~ display[e], rewrite[e] |: (cons[cExp],cons[cExp],option[cExp],option[locn],e) =>
    (option[cExp],e).
  mkMatchCond([],[],Test,_,Val) => (Test,Val).
  mkMatchCond([.cAnon(_,_),..Args],[_,..Vars],Test,Lc,Val) =>
    mkMatchCond(Args,Vars,Test,Lc,Val).
  mkMatchCond([.cVar(VLc,.cId(Vr,VTp)),..Args],[V,..Vars],Test,Lc,Val) => valof{
    if traceNormalize! then
      showMsg("match $(Vr) with $(V)");

    Mp = rwVar({Vr->V});
    NArgs = rewriteTerms(Args,Mp);
    if traceNormalize! then
      showMsg("rewritten args $(NArgs)");

    NTst = fmap((T)=>rewrite(T,Mp),Test);
    if traceNormalize! then
      showMsg("rewritten test $(NTst)");

    NVal = rewrite(Val,Mp);
    if traceNormalize! then
      showMsg("rewritten val $(NVal)");

    valis mkMatchCond(NArgs,Vars,NTst,Lc,NVal)
  }
  mkMatchCond([A,..Args],[V,..Vars],Test,Lc,Val) =>
    mkMatchCond(Args,Vars,mergeGoal(Lc,.some(.cMatch(Lc,A,V)),Test),Lc,Val).

  matchScalars:all e ~~ reform[e],rewrite[e],display[e] |:
    (cons[triple[e]],cons[cExp],option[locn],e,integer,nameMap)=>e.
  matchScalars(Seg,[V,..Vrs],Lc,Deflt,Depth,Map) => valof{
    ST = sort(Seg,compareScalarTriple);
--    showMsg("Sorted triples: $(ST)");
    Cases = formCases(ST,sameScalarTriple,Lc,Vrs,Deflt,Depth+1,Map);
--    showMsg("Scalar cases: $(Cases)");
    valis mkCase(Lc,V,Cases,Deflt)
  }

  matchConstructors:all e ~~ reform[e],rewrite[e],display[e] |:
    (cons[triple[e]],cons[cExp],option[locn],e,integer,nameMap)=>e.
  matchConstructors(Seg,[V,..Vrs],Lc,Deflt,Depth,Map) => valof{
    Cases = formCases(sort(Seg,compareConstructorTriple),
      sameConstructorTriple,Lc,Vrs,Deflt,Depth+1,Map);
    valis mkCase(Lc,V,Cases,Deflt)
  }

  matchTuples:all e ~~ reform[e],rewrite[e],display[e] |:
    (cons[triple[e]],cons[cExp],option[locn],e,integer,nameMap)=>e.
  matchTuples(Seg,[V,..Vrs],Lc,Deflt,Depth,Map) => valof{
    Cases = formCases(sort(Seg,compareConstructorTriple),
      sameConstructorTriple,Lc,Vrs,Deflt,Depth+1,Map);
    VTp = typeOf(V);
    Arity = arity(VTp);
    Index = [(.tLbl(tplLbl(Arity),Arity),consType(VTp,VTp),0)];
    Arms = populateArms(Index,Cases,Lc,Deflt,Map);
    valis mkCase(Lc,V,Arms,Deflt)
  }

  populateArms:all e ~~ display[e] |: (consMap,cons[cCase[e]],option[locn],e,nameMap) => cons[cCase[e]].
  populateArms(Index,Cases,Lc,Deflt,Map) =>
    { populateArm(Entry,Cases,Lc,Deflt,Map) | Entry in sort(Index,((_,_,I1),(_,_,I2))=>I1>I2)}.

  populateArm((.tLbl(FullNm,_),Tp,_),Cases,DLc,Deflt,Map) where
      Arm ?= armPresent(FullNm,Cases) => Arm.
  populateArm((.tLbl(FullNm,Ar),CnsTp,_),Cases,Lc,Deflt,Map) => 
    (Lc,emptyCase(Lc,CnsTp,FullNm),Deflt).
    
  armPresent(Nm,[(CLc,.cTerm(Lc,Nm,Args,Tp),Exp),.._]) =>
    .some((CLc,.cTerm(Lc,Nm,Args,Tp),Exp)).
  armPresent(Nm,[_,..Cases]) => armPresent(Nm,Cases).
  armPresent(_,[]) => .none.

  emptyCase:(option[locn],tipe,string)=>cExp.
  emptyCase(Lc,T,Nm) where (.tupleType(ArgTps),ResTp) ?= isConsType(T) =>
    .cTerm(Lc,Nm,ArgTps//(ATp)=>.cVar(Lc,.cId("_",ATp)),ResTp).
  
  matchVars:all e ~~ reform[e],rewrite[e],display[e] |:
    (cons[triple[e]],cons[cExp],option[locn],e,integer,nameMap)=>e.
  matchVars(Triples,[V,..Vrs],Lc,Deflt,Depth,Map) => valof{
    if traceNormalize! then
      showMsg("var match, subsititute for $(V) in $(Triples)");

    valis matchTriples(Lc,Vrs,applyVar(V,Triples),Deflt,Depth,Map)
  }

  applyVar:all e ~~ rewrite[e], display[e] |: (cExp,cons[triple[e]]) => cons[triple[e]].
  applyVar(V,Triples) => let{
    applyToTriple:(triple[e])=>triple[e].
    applyToTriple(([.cAnon(VLc,VTp),..Args],(CLc,B,Gl,Exp),Ix)) => valof{
      valis (Args, (CLc,B,Gl,Exp),Ix)
    }
    applyToTriple(([.cVar(VLc,.cId(Vr,VTp)),..Args],(CLc,B,Gl,Exp),Ix)) => valof{
      Mp = rwVar({Vr->V});

      if traceNormalize! then
	showMsg("replace #(Vr)\:$(VTp) with $(V)");

      NArgs = rewriteTerms(Args,Mp);
      NGl = fmap((T)=>rewrite(T,Mp),Gl);
      NExp = rewrite(Exp,Mp);

      if traceNormalize! then
	showMsg("result $(NExp)");

      valis (NArgs, (CLc,B,NGl,NExp),Ix)
    }
  } in (Triples//applyToTriple).

  -- we need to be careful to preserve the original order of equations
  formCases:all e ~~ reform[e],rewrite[e],display[e] |: (cons[triple[e]],(triple[e],triple[e])=>boolean,
    option[locn],cons[cExp],e,integer,nameMap) => cons[cCase[e]].
  formCases([],_,_,_,_,_,_) => [].
  formCases([Tr,..Triples],Eq,Lc,Vrs,Deflt,Depth,Map) => valof{
    (Tx,More) = pickMoreCases(Tr,Triples,Eq,[],[]);
--    showMsg("More cases for $(Tr)\:$(Tx)");
    Case = formCase(Tr,sort([Tr,..Tx],compareTriple),Lc,Vrs,Deflt,Depth,Map);
    valis [Case,..formCases(More,Eq,Lc,Vrs,Deflt,Depth,Map)].
  }

  formCase:all e ~~ reform[e],rewrite[e],display[e] |: (triple[e],cons[triple[e]],option[locn],cons[cExp],e,integer,nameMap) => cCase[e].
  formCase(([.cInt(LLc,Ix),.._],_,_),Tpls,Lc,Vars,Deflt,Depth,Map) =>
    (LLc,.cInt(LLc,Ix),matchTriples(Lc,Vars,subTriples(Tpls),Deflt,Depth,Map)).
  formCase(([.cFlt(LLc,Dx),.._],_,_),Tpls,Lc,Vars,Deflt,Depth,Map) =>
    (LLc,.cFlt(LLc,Dx),matchTriples(Lc,Vars,subTriples(Tpls),Deflt,Depth,Map)).
  formCase(([.cChar(LLc,Cx),.._],_,_),Tpls,Lc,Vars,Deflt,Depth,Map) =>
    (LLc,.cChar(LLc,Cx),matchTriples(Lc,Vars,subTriples(Tpls),Deflt,Depth,Map)).
  formCase(([.cString(LLc,Sx),.._],_,_),Tpls,Lc,Vars,Deflt,Depth,Map) =>
    (LLc,.cString(LLc,Sx),matchTriples(Lc,Vars,subTriples(Tpls),Deflt,Depth,Map)).
  formCase(([.cTerm(Lc,Lbl,Args,Tp),.._],_,_),Triples,_,Vars,Deflt,Depth,Map) => valof{
    Vrs = (Args//genTplVar);
    NTriples = subTriples(Triples);
    Case = matchTriples(Lc,Vrs++Vars,NTriples,Deflt,Depth,Map);
    valis (Lc,.cTerm(Lc,Lbl,Vrs,Tp),Case)
  }.

  genTplVar(Arg) => .cVar(locOf(Arg),.cId(genId("V"),typeOf(Arg))).

  pickMoreCases:all e ~~ (triple[e],cons[triple[e]],(triple[e],triple[e])=>boolean,
    cons[triple[e]],cons[triple[e]])=> (cons[triple[e]],cons[triple[e]]).
  pickMoreCases(_,[],_,InCase,Others) => (reverse(InCase),reverse(Others)).
  pickMoreCases(Tr,[A,..Triples],Test,InCase,Others) where Test(Tr,A) =>
    pickMoreCases(Tr,Triples,Test,[A,..InCase],Others).
  pickMoreCases(Tr,[A,..Triples],Test,InCase,Others) =>
    pickMoreCases(Tr,Triples,Test,InCase,[A,..Others]).

  subTriples(Tpls) => (Tpls//subTriple).
    
  subTriple(([.cTerm(_,_,CArgs,_),..Args],V,X)) =>
    (CArgs++Args,V,X).
  subTriple(([_,..Args],V,X)) => (Args,V,X).

  conditionalize:all e ~~ reform[e],display[e] |: (cons[triple[e]],e)=>e.
  conditionalize([],Deflt) => Deflt.
  conditionalize([(_,(Lc,Bnds,Test,Val),_),..Triples],Deflt) => valof{
    (Vl,Cnd) = pullWhere(Val);
    EqnCnd = mergeGoal(Lc,Test,Cnd);
    if Tst ?= EqnCnd then
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

  compareScalar(.cChar(_,A),.cChar(_,B)) => A=<B.
  compareScalar(.cInt(_,A),.cInt(_,B)) => A=<B.
  compareScalar(.cFlt(_,A),.cFlt(_,B)) => A=<B.
  compareScalar(.cString(_,A),.cString(_,B)) => A=<B.
  compareScalar(_,_) default => .false.

  sameScalarTriple:all e ~~ (triple[e],triple[e]) => boolean.
  sameScalarTriple(([.cChar(_,A),.._],_,_),([.cChar(_,B),.._],_,_)) => A==B.
  sameScalarTriple(([.cInt(_,A),.._],_,_),([.cInt(_,B),.._],_,_)) => A==B.
  sameScalarTriple(([.cFlt(_,A),.._],_,_),([.cFlt(_,B),.._],_,_)) => A==B.
  sameScalarTriple(([.cString(_,A),.._],_,_),([.cString(_,B),.._],_,_)) => A==B.
  sameScalarTriple(_,_) default => .false.

  compareTriple:all e ~~ (triple[e],triple[e]) => boolean.
  compareTriple((_,_,IxA),(_,_,IxB)) => IxA<IxB.

  compareConstructorTriple:all e ~~ (triple[e],triple[e]) => boolean.
  compareConstructorTriple(([A,.._],_,IxA),([B,.._],_,IxB)) =>
    (sameConstructor(A,B) ?? IxA<IxB || compareConstructor(A,B)).

  compareConstructor(.cTerm(_,A,_,_),.cTerm(_,B,_,_)) => A<B.

  sameConstructorTriple:all e ~~ (triple[e],triple[e])=>boolean.
  sameConstructorTriple(([A,.._],_,_),([B,.._],_,_)) => sameConstructor(A,B).

  sameConstructor(.cTerm(_,A,_,_), .cTerm(_,B,_,_)) => A==B.
  sameConstructor(_,_) default => .false.

  pullVarLets:(cons[cExp],cExp)=>(cons[cExp],cExp).
  pullVarLets(Vrs,.cLtt(Lc,V,A,Exp)) =>
    pullVarLets(Vrs//replaceWith(A,.cVar(Lc,V)),Exp).
  pullVarLets(Vrs,Exp) => (Vrs,Exp).

  replaceWith:(cExp,cExp) => (cExp)=>cExp.
  replaceWith(A,B) => (X) => (A==X??B||X).

  tooDeep(X) => X>0.
}
