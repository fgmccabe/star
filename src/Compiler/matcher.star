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

  functionMatcher:(locn,string,tipe,list[(locn,list[crExp],crExp)],reports) =>
    either[reports,crExp].
  functionMatcher(Lc,Nm,Tp,Eqns,Rp) => do{
    NVrs = genVars(funTypeArg(Tp));
    Trpls = makeTriples(Eqns);
    Error = genRaise(Lc);
    valis matchTriples(Lc,NVrs,Trpls,Error)
  }

  genVars:(tipe) => list[crVar].
  genVars(tupleType(L)) => let{
    genV([]) => [].
    genV([T,..Ts]) => [crId(genSym("_"),T),..genV(Ts)].
  } in genV(L).

  makeTriples:(list[(locn,list[crExp],crExp)]) => list[triple].
  makeTriples(Eqns) => ixLeft((Ts,Ix,(Lc,Args,Exp))=>
      [(Args,(Lc,[],Exp),Ix),..Ts],[],Eqns).

  genRaise(Lc) => crAbort(Lc,"no matches").

  matchTriples:(locn,list[crVar],list[triple],crExp) => crExp.
  matchTriples(_,[],Trpls,Deflt) =>
    conditionalize(Trpls,Deflt).
  matchTriples(Lc,Vrs,Trpls,Deflt) =>
    matchSegments(partitionTriples(Trpls),Vrs,Lc,Deflt).

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

  argMode ::= inVars | inScalars | inConstructors.

  implementation equality[argMode] => {
    inVars == inVars => true.
    inScalars == inScalars => true.
    inConstructors == inConstructors => true.
    _ == _ default => false.
  }

  argMode(crVar(_,_)) => inVars.
  argMode(crLit(_,Lt,_)) => litArgMode(Lt).
  argMode(crApply(_,_,_,_)) => inConstructors.

  litArgMode(intgr(_)) => inScalars.
  litArgMode(flot(_)) => inScalars.
  litArgMode(strg(_)) => inScalars.
  litArgMode(lbl(_,_)) => inScalars.
  litArgMode(term(_,_)) => inConstructors.

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
    valis mkCase(Cases,Lc,crVar(Lc,V),typeOf(Deflt))
  }

  matchConstructors(Seg,[V,..Vrs],Lc,Deflt) =>
    mkCase(formCases(sort(Seg,compareConstructorTriple),sameConstructorTriple,Lc,Vrs,Deflt),
      Lc,crVar(Lc,V),typeOf(Deflt)).

  matchVars(Trpls,[V,..Vrs],Lc,Deflt) =>
    matchTriples(Lc,Vrs,applyVar(V,Trpls),Deflt).

  applyVar:(crVar,list[triple]) => list[triple].
  applyVar(V,Trpls) => let{
    applyToTriple:(triple)=>triple.
    applyToTriple(([crVar(VLc,Vr),..Args],(CLc,B,Exp),Ix)) =>
      (Args,(CLc,B,crLet(VLc,Vr,crVar(VLc,V),Exp)),Ix).
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
  formCase(([crLit(LLc,Lbl,LTp),.._],_,_),Tpls,Lc,Vars,Deflt) where isScalar(Lbl) =>
    (LLc,crLit(LLc,Lbl,LTp),matchTriples(Lc,Vars,subTriples(Tpls),Deflt)).
  formCase(([crApply(Lc,crLit(LLc,Lbl,LTp),Args,Tp),.._],_,_),Trpls,_,Vars,Deflt) => valof action{
    Vrs = Args//(E) => crId(genSym("_"),typeOf(E));
    NTrpls = subTriples(Trpls);
    Case = matchTriples(Lc,Vrs++Vars,NTrpls,Deflt);
    valis (LLc,crApply(LLc,crLit(LLc,Lbl,LTp),Vrs//(V)=>crVar(LLc,V),LTp),Case)
  }.

  pickMoreCases:(triple,list[triple],(triple,triple)=>boolean,
    list[triple],list[triple])=> (list[triple],list[triple]).
  pickMoreCases(_,[],_,InCase,Others) => (InCase,Others).
  pickMoreCases(Tr,[A,..Trpls],Test,InCase,Others) where Test(Tr,A) =>
    pickMoreCases(Tr,Trpls,Test,[InCase..,A],Others).
  pickMoreCases(Tr,[A,..Trpls],Test,InCase,Others) =>
      pickMoreCases(Tr,Trpls,Test,InCase,[Others..,A]).

  mkCase:(list[crCase],locn,crExp,tipe) => crExp.
  mkCase(Cases,Lc,V,Tp) => crCase(Lc,V,Cases,Tp).

  subTriples(Tpls) => (Tpls//subTriple).
    
  subTriple(([crApply(_,_,CArgs,_),..Args],V,X)) =>
    (CArgs++Args,V,X).
  subTriple(([_,..Args],V,X)) => (Args,V,X).

  conditionalize([],Deflt) => Deflt.
  conditionalize([(_,(Lc,Bnds,Val),_),..Trpls],Deflt) =>
    applyBindings(Bnds,Lc,conditionalize(Trpls,Val)).

  applyBindings([],_,Val) => Val.

  compareScalarTriple:(triple,triple) => boolean.
  compareScalarTriple(([crLit(_,A,_),.._],_,_),([crLit(_,B,_),.._],_,_)) => compareScalar(A,B).

  compareScalar(intgr(A),intgr(B)) => A<B.
  compareScalar(flot(A),flot(B)) => A<B.
  compareScalar(strg(A),strg(B)) => A<B.
  compareScalar(lbl(A,L1),lbl(B,L2)) =>
    A<B || A==B && L1<L2.
  compareScalar(_,_) default => false.

  sameScalarTriple:(triple,triple) => boolean.
  sameScalarTriple(([crLit(_,A,_),.._],_,_),([crLit(_,B,_),.._],_,_)) => A==B.
  sameScalarTriple(_,_) default => false.

  compareConstructorTriple:(triple,triple) => boolean.
  compareConstructorTriple(([A,.._],_,_),([B,.._],_,_)) => compareConstructor(A,B).

  compareConstructor(crApply(_,crLit(_,lbl(A,_),_),_,_),
    crApply(_,crLit(_,lbl(B,_),_),_,_)) => A=<B.

  sameConstructorTriple(([A,.._],_,_),([B,.._],_,_)) => sameConstructor(A,B).
  sameConstructor(crApply(_,crLit(_,lbl(A,_),_),_,_),
    crApply(_,crLit(_,lbl(B,_),_),_,_)) => A==B.
}
