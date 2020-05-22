star.compiler.matcher{
  import star.
  import star.sort.

  import star.compiler.canon.
  import star.compiler.core.
  import star.compiler.errors.
  import star.compiler.freevars.
  import star.compiler.misc.
  import star.compiler.types.

  import star.compiler.location.
  import star.compiler.terms.

  triple ~> (cons[crExp],(locn,cons[(string,crVar)],option[crExp],option[crExp],crExp),integer).

  public functionMatcher:(locn,string,tipe,cons[(locn,cons[crExp],option[crExp],crExp)]) => crDefn.
  functionMatcher(Lc,Nm,Tp,Eqns) => valof action{
    NVrs .= genVars(Lc,funTypeArg(Tp));
    Trpls .= makeTriples(Eqns);
    Error .= genRaise(Lc,funTypeRes(Tp));
--    logMsg("function triples: $(Trpls)");
    Reslt .= matchTriples(Lc,NVrs,Trpls,Error);
    valis fnDef(Lc,Nm,Tp,NVrs//(crVar(_,V))=>V,Reslt)
  }

  public caseMatcher:(locn,crExp,cons[(locn,cons[crExp],option[crExp],crExp)],tipe)=>crExp.
  caseMatcher(Lc,Gov,Cs,Tp) => valof action{
--    logMsg("match cases $(Cs)\ngoverning expression $(Gov)");
    Trpls .= makeTriples(Cs);
--    logMsg("case triples $(Trpls)");
    Error .= genRaise(Lc,Tp);
    valis matchTriples(Lc,[Gov],Trpls,Error)
  }

  genVars:(locn,tipe) => cons[crExp].
  genVars(Lc,tupleType(L)) => let{
    genV([]) => [].
    genV([T,..Ts]) => [crVar(Lc,crId(genSym("_"),T)),..genV(Ts)].
  } in genV(L).

  makeTriples:(cons[(locn,cons[crExp],option[crExp],crExp)]) => cons[triple].
  makeTriples(Eqns) => ixRight((Ix,(Lc,Args,Wh,Exp),Ts)=>
      [(Args,(Lc,[],.none,Wh,Exp),Ix),..Ts],[],Eqns).

  genRaise(Lc,Tp) => crAbort(Lc,"no matches",Tp).

  matchTriples:(locn,cons[crExp],cons[triple],crExp) => crExp.
  matchTriples(_,[],Triples,Deflt) => conditionalize(Triples,Deflt).
  matchTriples(Lc,Vrs,Triples,Deflt) => valof action{
--    logMsg("matching triples, $(Vrs) --- $(Triples), default = $(Deflt)");
    Parts .= partitionTriples(Triples);
--    logMsg("partitioned $(Parts)");
    Segs .= matchSegments(Parts,Vrs,Lc,Deflt);
--    logMsg("segments = $(Segs)");
    valis Segs
  }.

  partitionTriples:(cons[triple]) => cons[(argMode,cons[triple])].
  partitionTriples([]) => [].
  partitionTriples([Tr,..Triples]) where
      M .= tripleArgMode(Tr) &&
      (P,Ts) .= partTriples(M,Triples,[]) =>
    [(M,[Tr,..P]),..partitionTriples(Ts)].

  partTriples(_,[],S) => (reverse(S),[]).
  partTriples(M,[Tr,..Ts],S) where tripleArgMode(Tr)==M =>
    partTriples(M,Ts,[Tr,..S]).
  partTriples(_,Ts,S) => (S,Ts).

  tripleArgMode:(triple) => argMode.
  tripleArgMode(([A,.._],_,_)) => argMode(A).

  argMode ::= .inVars | .inScalars | .inConstructors.

  implementation display[argMode] => {.
    disp(.inVars) => ss("inVars").
    disp(.inScalars) => ss("inScalars").
    disp(.inConstructors) => ss("inConstructors").
  .}

  implementation equality[argMode] => {
    .inVars == .inVars => .true.
    .inScalars == .inScalars => .true.
    .inConstructors == .inConstructors => .true.
    _ == _ default => .false.
  }

  argMode(crVar(_,_)) => .inVars.
  argMode(crInt(_,_)) => .inScalars.
  argMode(crFlot(_,_)) => .inScalars.
  argMode(crStrg(_,_)) => .inScalars.
  argMode(crLbl(_,_,_)) => .inConstructors.
  argMode(crTerm(_,_,_,_)) => .inConstructors.
  argMode(crWhere(_,T,_)) => argMode(T).

  matchSegments([],_,_,Deflt) => Deflt.
  matchSegments([(M,Seg),..Segs],Vrs,Lc,Deflt) =>
    compileMatch(M,Seg,Vrs,Lc,matchSegments(Segs,Vrs,Lc,Deflt)).

  compileMatch(.inScalars,Seg,Vrs,Lc,Deflt) =>
    matchScalars(Seg,Vrs,Lc,Deflt).
  compileMatch(.inConstructors,Seg,Vrs,Lc,Deflt) =>
    matchConstructors(Seg,Vrs,Lc,Deflt).
  compileMatch(.inVars,Seg,Vrs,Lc,Deflt) =>
    matchVars(Seg,Vrs,Lc,Deflt).

  matchScalars(Seg,[V,..Vrs],Lc,Deflt) => valof action{
    ST .= sort(Seg,compareScalarTriple);
    Cases .= formCases(ST,sameScalarTriple,Lc,Vrs,Deflt);
    valis mkCase(Cases,Lc,V,Deflt)
  }

  matchConstructors(Seg,[V,..Vrs],Lc,Deflt) =>
    mkCase(formCases(sort(Seg,compareConstructorTriple),sameConstructorTriple,Lc,Vrs,Deflt),
      Lc,V,Deflt).

  matchVars(Triples,[V,..Vrs],Lc,Deflt) =>
    matchTriples(Lc,Vrs,applyVar(V,Triples),Deflt).

  applyVar:(crExp,cons[triple]) => cons[triple].
  applyVar(V,Triples) where crVar(_,_).=V => let{
    applyToTriple:(triple)=>triple.
    applyToTriple(([crVar(VLc,crId(Vr,_)),..Args],(CLc,B,AG,Gl,Exp),Ix)) => valof action{
      Mp .= [Vr->V];
      NArgs .= rewriteTerms(Args,Mp);
      NAG .= fmap((T)=>rewriteTerm(T,Mp),AG);
      NGl .= fmap((T)=>rewriteTerm(T,Mp),Gl);
      NExp .= rewriteTerm(Exp,Mp);
      valis (NArgs, (CLc,B,AG,NGl,NExp),Ix)
    }
    applyToTriple(([crWhere(Lc,crVar(VLc,crId(Vr,_)),Cond),..Args],(CLc,B,AG,Gl,Exp),Ix)) => valof action{
      Mp .= [Vr->V];
      NArgs .= rewriteTerms(Args,Mp);
      NAG .= fmap((T)=>rewriteTerm(T,Mp),AG);
      NCond .= rewriteTerm(Cond,Mp);
      NGl .= fmap((T)=>rewriteTerm(T,Mp),Gl);
      NExp .= rewriteTerm(Exp,Mp);
      valis (NArgs, (CLc,B,mergeGoal(VLc,NAG,some(NCond)),Gl,NExp),Ix)
    }
  } in (Triples//applyToTriple).

  formCases:(cons[triple],(triple,triple)=>boolean,locn,cons[crExp],crExp)=> cons[crCase].
  formCases([],_,_,_,_) => [].
  formCases([Tr,..Triples],Eq,Lc,Vrs,Deflt) => valof action{
    (Tx,More) .= pickMoreCases(Tr,Triples,Eq,[],[]);
--    logMsg("form case from $([Tr,..Tx])");
--    logMsg("other cases $(More)");
    Case .= formCase(Tr,[Tr,..Tx],Lc,Vrs,Deflt);
    valis [Case,..formCases(reverse(More),Eq,Lc,Vrs,Deflt)].
  }

  formCase:(triple,cons[triple],locn,cons[crExp],crExp) => crCase.
  formCase(([crInt(LLc,Ix),.._],_,_),Tpls,Lc,Vars,Deflt) =>
    (LLc,crInt(LLc,Ix),matchTriples(Lc,Vars,subTriples(Tpls),Deflt)).
  formCase(([crFlot(LLc,Dx),.._],_,_),Tpls,Lc,Vars,Deflt) =>
    (LLc,crFlot(LLc,Dx),matchTriples(Lc,Vars,subTriples(Tpls),Deflt)).
  formCase(([crStrg(LLc,Sx),.._],_,_),Tpls,Lc,Vars,Deflt) =>
    (LLc,crStrg(LLc,Sx),matchTriples(Lc,Vars,subTriples(Tpls),Deflt)).
  formCase(([crLbl(LLc,Nm,LTp),.._],_,_),Tpls,Lc,Vars,Deflt) =>
    (LLc,crLbl(LLc,Nm,LTp),matchTriples(Lc,Vars,subTriples(Tpls),Deflt)).
  formCase(([crTerm(Lc,Lbl,Args,Tp),.._],_,_),Triples,_,Vars,Deflt) => valof action{
    Vrs .= (Args//(E) => crVar(Lc,crId(genSym("_"),typeOf(E))));
    NTriples .= subTriples(Triples);
    Case .= matchTriples(Lc,Vrs++Vars,NTriples,Deflt);
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
  mkCase([(PLc,Ptn,Val)],Lc,Tst,Deflt) => crCnd(Lc,crMatch(PLc,Ptn,Tst),Val,Deflt).
  mkCase(Cases,Lc,V,Deflt) => crCase(Lc,V,Cases,Deflt,typeOf(Deflt)).

  subTriples(Tpls) => (Tpls//subTriple).
    
  subTriple(([crTerm(_,_,CArgs,_),..Args],V,X)) =>
    (CArgs++Args,V,X).
  subTriple(([_,..Args],V,X)) => (Args,V,X).

  conditionalize([],Deflt) => Deflt.
  conditionalize([(_,(Lc,Bnds,ArgCond,Test,Val),_),..Triples],Deflt) => valof action{
--    logMsg("conditionalize $(Lc)\:$(Bnds) $(ArgCond) $(Test)");
    (Vl,Cnd) .= pullWhere(Val,Test);
    EqnCnd .= mergeGoal(Lc,ArgCond,Cnd);
--    logMsg("merged conditionalize goal $(EqnCnd)");
    if Tst ^= EqnCnd then
      valis applyBindings(Bnds,Lc,crCnd(Lc,Tst,Vl,conditionalize(Triples,Deflt)))
    else
    valis applyBindings(Bnds,Lc,Vl)
  }

  applyBindings([],_,Val) => Val.

  compareScalarTriple:(triple,triple) => boolean.
  compareScalarTriple(([A,.._],_,_),([B,.._],_,_)) => compareScalar(A,B).

  compareScalar(crInt(_,A),crInt(_,B)) => A=<B.
  compareScalar(crFlot(_,A),crFlot(_,B)) => A=<B.
  compareScalar(crStrg(_,A),crStrg(_,B)) => A=<B.
  compareScalar(crLbl(_,A,_),crLbl(_,B,_)) => A=<B.
  compareScalar(_,_) default => .false.

  sameScalarTriple:(triple,triple) => boolean.
  sameScalarTriple(([crInt(_,A),.._],_,_),([crInt(_,B),.._],_,_)) => A==B.
  sameScalarTriple(([crFlot(_,A),.._],_,_),([crFlot(_,B),.._],_,_)) => A==B.
  sameScalarTriple(([crStrg(_,A),.._],_,_),([crStrg(_,B),.._],_,_)) => A==B.
  sameScalarTriple(([crLbl(_,A,_),.._],_,_),([crLbl(_,B,_),.._],_,_)) => A==B.
  sameScalarTriple(_,_) default => .false.

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
  pullVarLets(Vrs,crLtRec(Lc,V,A,Exp)) =>
    pullVarLets(Vrs//replaceWith(A,crVar(Lc,V)),Exp).
  pullVarLets(Vrs,Exp) => (Vrs,Exp).

  replaceWith:(crExp,crExp) => (crExp)=>crExp.
  replaceWith(A,B) => (X) => (A==X?B||X).

  pullWhere:(crExp,option[crExp]) => (crExp,option[crExp]).
  pullWhere(crWhere(Lc,V,C),G) where (Val,G1) .= pullWhere(V,G) =>
    (Val,mergeGoal(Lc,some(C),G1)).
  pullWhere(Exp,G) default => (Exp,G).

  mergeGoal:(locn,option[crExp],option[crExp])=>option[crExp].
  mergeGoal(_,G,.none) => G.
  mergeGoal(_,.none,G) => G.
  mergeGoal(Lc,some(G),some(H)) => some(crCnj(Lc,G,H)).
}
