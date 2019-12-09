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

  triple ~> (list[crExp],(locn,list[(string,crVar)],crExp),integer).

  public functionMatcher:(locn,string,tipe,list[(locn,list[crExp],crExp)]) => crDefn.
  functionMatcher(Lc,Nm,Tp,Eqns) => valof action{
    NVrs = genVars(funTypeArg(Tp));
    Trpls = makeTriples(Eqns);
    Error = genRaise(Lc,funTypeRes(Tp));
--    logMsg("function triples: $(Trpls)");
    Reslt = matchTriples(Lc,NVrs,Trpls,Error);
    (NArgs,NReslt) = pullVarLets(NVrs,Reslt);
    valis fnDef(Lc,Nm,Tp,NArgs,NReslt)
  }

  genVars:(tipe) => list[crVar].
  genVars(tupleType(L)) => let{
    genV([]) => [].
    genV([T,..Ts]) => [crId(genSym("_"),T),..genV(Ts)].
  } in genV(L).

  makeTriples:(list[(locn,list[crExp],crExp)]) => list[triple].
  makeTriples(Eqns) => ixLeft((Ts,Ix,(Lc,Args,Exp))=>
      [Ts..,(Args,(Lc,[],Exp),Ix)],[],Eqns).

  genRaise(Lc,Tp) => crAbort(Lc,"no matches",Tp).

  matchTriples:(locn,list[crVar],list[triple],crExp) => crExp.
  matchTriples(_,[],Trpls,Deflt) => conditionalize(Trpls,Deflt).
/*  matchTriples(_,[],Trpls,Deflt) => valof action{
    Reslt = conditionalize(Trpls,Deflt);
    logMsg("conditionalized $(Trpls) = $(Reslt)"); valis Reslt
    }.
*/
  matchTriples(Lc,Vrs,Trpls,Deflt) => valof action{
    Parts = partitionTriples(Trpls);
--    logMsg("partitioned $(Parts)");
    Segs = matchSegments(Parts,Vrs,Lc,Deflt);
--    logMsg("segments = $(Segs)");
    valis Segs
  }.

  partitionTriples:(list[triple]) => list[(argMode,list[triple])].
  partitionTriples([]) => [].
  partitionTriples([Tr,..Trpls]) where
      M .= tripleArgMode(Tr) &&
      (P,Ts) .= partTriples(M,Trpls,[]) =>
    [(M,[Tr,..P]),..partitionTriples(Ts)].

  partTriples(_,[],S) => (S,[]).
  partTriples(M,[Tr,..Ts],S) where tripleArgMode(Tr)==M =>
    partTriples(M,Ts,[S..,Tr]).
  partTriples(_,Ts,S) => (S,Ts).

  tripleArgMode(([A,.._],_,_)) => argMode(A).

  argMode ::= inVars | inScalars | inConstructors | inOthers.

  implementation display[argMode] => {.
    disp(inVars) => ss("inVars").
    disp(inScalars) => ss("inScalars").
    disp(inConstructors) => ss("inConstructors").
    disp(inOthers) => ss("inOthers").
  .}

  implementation equality[argMode] => {
    inVars == inVars => true.
    inScalars == inScalars => true.
    inConstructors == inConstructors => true.
    _ == _ default => false.
  }

  argMode(crVar(_,_)) => inVars.
  argMode(crInt(_,_)) => inScalars.
  argMode(crFlot(_,_)) => inScalars.
  argMode(crStrg(_,_)) => inScalars.
  argMode(crLbl(_,_,_,_)) => inScalars.
  argMode(crTerm(_,crLbl(_,_,_,_),_,_)) => inConstructors.
  argMode(_) default => inOthers.

  matchSegments([],_,_,Deflt) => Deflt.
  matchSegments([(M,Seg),..Segs],Vrs,Lc,Deflt) =>
    compileMatch(M,Seg,Vrs,Lc,matchSegments(Segs,Vrs,Lc,Deflt)).

  compileMatch(inScalars,Seg,Vrs,Lc,Deflt) =>
    matchScalars(Seg,Vrs,Lc,Deflt).
  compileMatch(inConstructors,Seg,Vrs,Lc,Deflt) =>
    matchConstructors(Seg,Vrs,Lc,Deflt).
  compileMatch(inVars,Seg,Vrs,Lc,Deflt) =>
    matchVars(Seg,Vrs,Lc,Deflt).

  matchScalars(Seg,[V,..Vrs],Lc,Deflt) => valof action{
    ST = sort(Seg,compareScalarTriple);
    Cases = formCases(ST,sameScalarTriple,Lc,Vrs,Deflt);
    valis mkCase(Cases,Lc,crVar(Lc,V),Deflt)
  }

  matchConstructors(Seg,[V,..Vrs],Lc,Deflt) =>
    mkCase(formCases(sort(Seg,compareConstructorTriple),sameConstructorTriple,Lc,Vrs,Deflt),
      Lc,crVar(Lc,V),Deflt).

  matchVars(Trpls,[V,..Vrs],Lc,Deflt) =>
    matchTriples(Lc,Vrs,applyVar(V,Trpls),Deflt).

  applyVar:(crVar,list[triple]) => list[triple].
  applyVar(V,Trpls) => let{
    applyToTriple:(triple)=>triple.
    applyToTriple(([crVar(VLc,Vr),..Args],(CLc,B,Exp),Ix)) =>
      (Args,(CLc,B,crLtt(VLc,vrDef(VLc,Vr,crVar(VLc,V)),Exp)),Ix).
  } in (Trpls//applyToTriple).

  formCases:(list[triple],(triple,triple)=>boolean,locn,list[crVar],crExp)=>
    list[crCase].
  formCases([],_,_,_,_) => [].
  formCases([Tr,..Trpls],Eq,Lc,Vrs,Deflt) => valof action{
    (Tx,More) = pickMoreCases(Tr,Trpls,Eq,[],[]);
    Case = formCase(Tr,[Tr,..Trpls],Lc,Vrs,Deflt);
    valis [Case,..formCases(More,Eq,Lc,Vrs,Deflt)].
  }

  formCase:(triple,list[triple],locn,list[crVar],crExp) => crCase.
  formCase(([crInt(LLc,Ix),.._],_,_),Tpls,Lc,Vars,Deflt) =>
    (LLc,crInt(LLc,Ix),matchTriples(Lc,Vars,subTriples(Tpls),Deflt)).
  formCase(([crFlot(LLc,Dx),.._],_,_),Tpls,Lc,Vars,Deflt) =>
    (LLc,crFlot(LLc,Dx),matchTriples(Lc,Vars,subTriples(Tpls),Deflt)).
  formCase(([crStrg(LLc,Sx),.._],_,_),Tpls,Lc,Vars,Deflt) =>
    (LLc,crStrg(LLc,Sx),matchTriples(Lc,Vars,subTriples(Tpls),Deflt)).
  formCase(([crLbl(LLc,Nm,Ix,LTp),.._],_,_),Tpls,Lc,Vars,Deflt) =>
    (LLc,crLbl(LLc,Nm,Ix,LTp),matchTriples(Lc,Vars,subTriples(Tpls),Deflt)).
  formCase(([crTerm(Lc,Lbl,Args,Tp),.._],_,_),Trpls,_,Vars,Deflt) => valof action{
    Vrs = Args//(E) => crId(genSym("_"),typeOf(E));
    NTrpls = subTriples(Trpls);
    Case = matchTriples(Lc,Vrs++Vars,NTrpls,Deflt);
    valis (Lc,crTerm(Lc,Lbl,Vrs//(V)=>crVar(Lc,V),Tp),Case)
  }.

  pickMoreCases:(triple,list[triple],(triple,triple)=>boolean,
    list[triple],list[triple])=> (list[triple],list[triple]).
  pickMoreCases(_,[],_,InCase,Others) => (InCase,Others).
  pickMoreCases(Tr,[A,..Trpls],Test,InCase,Others) where Test(Tr,A) =>
    pickMoreCases(Tr,Trpls,Test,[InCase..,A],Others).
  pickMoreCases(Tr,[A,..Trpls],Test,InCase,Others) =>
      pickMoreCases(Tr,Trpls,Test,InCase,[Others..,A]).

  mkCase:(list[crCase],locn,crExp,crExp) => crExp.
  mkCase([(PLc,Ptn,Val)],Lc,Tst,Deflt) => crCnd(Lc,crMatch(PLc,Ptn,Tst),Val,Deflt).
  mkCase(Cases,Lc,V,Deflt) => crCase(Lc,V,Cases,Deflt,typeOf(Deflt)).

  subTriples(Tpls) => (Tpls//subTriple).
    
  subTriple(([crTerm(_,_,CArgs,_),..Args],V,X)) =>
    (CArgs++Args,V,X).
  subTriple(([_,..Args],V,X)) => (Args,V,X).

  conditionalize([],Deflt) => Deflt.
  conditionalize([(_,(Lc,Bnds,Val),_),..Trpls],Deflt) =>
    applyBindings(Bnds,Lc,conditionalize(Trpls,Val)).

  applyBindings([],_,Val) => Val.

  compareScalarTriple:(triple,triple) => boolean.
  compareScalarTriple(([A,.._],_,_),([B,.._],_,_)) => compareScalar(A,B).

  compareScalar(crInt(_,A),crInt(_,B)) => A<B.
  compareScalar(crFlot(_,A),crFlot(_,B)) => A<B.
  compareScalar(crStrg(_,A),crStrg(_,B)) => A<B.
  compareScalar(crLbl(_,A,L1,_),crLbl(_,B,L2,_)) =>
    A<B || A==B && L1<L2.
  compareScalar(_,_) default => false.

  sameScalarTriple:(triple,triple) => boolean.
  sameScalarTriple(([crInt(_,A),.._],_,_),([crInt(_,B),.._],_,_)) => A==B.
  sameScalarTriple(([crFlot(_,A),.._],_,_),([crFlot(_,B),.._],_,_)) => A==B.
  sameScalarTriple(([crStrg(_,A),.._],_,_),([crStrg(_,B),.._],_,_)) => A==B.
  sameScalarTriple(([crLbl(_,A,Ar,_),.._],_,_),([crLbl(_,B,Br,_),.._],_,_)) => A==B && Ar==Br.
  sameScalarTriple(_,_) default => false.

  compareConstructorTriple:(triple,triple) => boolean.
  compareConstructorTriple(([A,.._],_,_),([B,.._],_,_)) => compareConstructor(A,B).

  compareConstructor(crTerm(_,crLbl(_,A,Ar,_),_,_),
    crTerm(_,crLbl(_,B,Br,_),_,_)) => A=<B && Ar==Br.
  
  sameConstructorTriple(([A,.._],_,_),([B,.._],_,_)) => sameConstructor(A,B).
  sameConstructor(crTerm(_,crLbl(_,A,Ar,_),_,_), crTerm(_,crLbl(_,B,Br,_),_,_)) =>
    A==B && Ar==Br.

  pullVarLets:(list[crVar],crExp)=>(list[crVar],crExp).
  pullVarLets(Vrs,crLtt(Lc,vrDef(Vlc,V,crVar(_,A)),Exp)) =>
    pullVarLets(Vrs//replaceWith(A,V),Exp).
  pullVarLets(Vrs,Exp) => (Vrs,Exp).

  replaceWith:(crVar,crVar) => (crVar)=>crVar.
  replaceWith(A,B) => (X) => (A==X?B||X).
}
