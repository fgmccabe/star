star.compiler.canon{
  import star.
  import star.pkg.

  import star.compiler.meta.
  import star.compiler.location.
  import star.compiler.terms.
  import star.compiler.types.

  public pkgSpec::=pkgSpec(pkg,cons[importSpec],tipe,cons[canonDef],cons[implSpec],cons[(string,tipe)]).

  public implSpec ::= implSpec(option[locn],string,string,tipe).

  public canon ::= vr(locn,string,tipe) |
    mtd(locn,string,tipe,tipe) |
    over(locn,canon,tipe,cons[constraint]) |
    intr(locn,integer) |
    flt(locn,float) |
    strng(locn,string) |
    enm(locn,string,tipe) |
    whr(locn,canon,canon) |
    dot(locn,canon,string,tipe) |
    csexp(locn,canon,cons[equation],tipe) |
    match(locn,canon,canon) |
    conj(locn,canon,canon) |
    disj(locn,canon,canon) |
    implies(locn,canon,canon) |
    neg(locn,canon) |
    cond(locn,canon,canon,canon) |
    apply(locn,canon,canon,tipe) |
    tple(locn,cons[canon]) |
    freeze(locn,string,canon) |
    thaw(locn,canon,tipe) |
    lambda(string,cons[equation],tipe) |
    letExp(locn,cons[canonDef],canon) |
    letRec(locn,cons[canonDef],canon) |
    record(locn,option[string],cons[(string,canon)],tipe) |
    update(locn,canon,canon).

  public equation ::= eqn(locn,canon,option[canon],canon).

  public canonDef ::= varDef(locn,string,string,canon,cons[constraint],tipe) |
    typeDef(locn,string,tipe,tipe) |
    conDef(locn,string,string,tipe) |
    cnsDef(locn,string,string,tipe) |
    implDef(locn,string,string,canon,cons[constraint],tipe).

  public implementation hasType[canon] => {
    typeOf(vr(_,_,T)) => T.
    typeOf(mtd(_,_,_,T)) => T.
    typeOf(over(_,_,Tp,_)) => Tp.
    typeOf(intr(_,_)) => intType.
    typeOf(flt(_,_)) => fltType.
    typeOf(strng(_,_)) => strType.
    typeOf(enm(_,_,Tp)) => Tp.
    typeOf(csexp(_,_,_,Tp)) => Tp.
    typeOf(freeze(_,_,E)) => memoType(typeOf(E)).
    typeOf(thaw(_,_,Tp)) => Tp.
    typeOf(lambda(_,_,Tp)) => Tp.
    typeOf(letExp(_,_,E)) => typeOf(E).
    typeOf(letRec(_,_,E)) => typeOf(E).
    typeOf(apply(_,_,_,Tp)) => Tp.
    typeOf(tple(_,Els)) => tupleType(Els//typeOf).
    typeOf(record(_,_,_,Tp)) => Tp.
    typeOf(dot(_,_,_,Tp)) => Tp.
    typeOf(whr(_,E,_)) => typeOf(E).
    typeOf(match(_,_,_)) => boolType.
    typeOf(conj(_,_,_)) => boolType.
    typeOf(disj(_,_,_)) => boolType.
    typeOf(implies(_,_,_)) => boolType.
    typeOf(cond(_,_,L,_)) => typeOf(L).
    typeOf(update(_,R,_)) => typeOf(R).
  }

  public implementation hasLoc[canon] => {
    locOf(vr(Lc,_,_)) => Lc.
    locOf(mtd(Lc,_,_,_)) => Lc.
    locOf(over(Lc,_,_,_)) => Lc.
    locOf(intr(Lc,_)) => Lc.
    locOf(flt(Lc,_)) => Lc.
    locOf(strng(Lc,_)) => Lc.
    locOf(enm(Lc,_,_)) => Lc.
    locOf(whr(Lc,_,_)) => Lc.
    locOf(dot(Lc,_,_,_)) => Lc.
    locOf(csexp(Lc,_,_,_)) => Lc.
    locOf(match(Lc,_,_)) => Lc.
    locOf(conj(Lc,_,_)) => Lc.
    locOf(disj(Lc,_,_)) => Lc.
    locOf(implies(Lc,_,_)) => Lc.
    locOf(neg(Lc,_)) => Lc.
    locOf(cond(Lc,_,_,_)) => Lc.
    locOf(apply(Lc,_,_,_)) => Lc.
    locOf(tple(Lc,_)) => Lc.
    locOf(freeze(Lc,_,_)) => Lc.
    locOf(thaw(Lc,_,_)) => Lc.
    locOf(lambda(_,[E,.._],_)) => locOf(E).
    locOf(letExp(Lc,_,_)) => Lc.
    locOf(letRec(Lc,_,_)) => Lc.
    locOf(record(Lc,_,_,_)) => Lc.
    locOf(update(Lc,_,_)) => Lc.
  }

  public implementation hasLoc[equation] => {
    locOf(eqn(Lc,_,_,_)) => Lc.
  }

  public implementation hasLoc[canonDef] => {
    locOf(varDef(Lc,_,_,_,_,_)) => Lc.
    locOf(typeDef(Lc,_,_,_)) => Lc.
    locOf(conDef(Lc,_,_,_)) => Lc.
    locOf(cnsDef(Lc,_,_,_)) => Lc.
    locOf(implDef(Lc,_,_,_,_,_)) => Lc.
  }

  public implementation display[pkgSpec] => {
    disp(pkgSpec(Pkg,Imports,Face,Cons,Impls,PkgVrs)) =>
      ss("Package: $(Pkg), imports=$(Imports), Signature=$(Face),Contracts=$(Cons),Implementations:$(Impls), pkg vars:$(PkgVrs)").
  }

  public implementation hasType[pkgSpec] => {
    typeOf(pkgSpec(Pkg,Imports,Face,Cons,Impls,PkgVrs)) => Face
  }

  public implementation display[implSpec] => let{
    dispImpl(implSpec(_,Con,Full,Tp)) =>
      ss("implementation for $(Con), full name $(Full), type: $(Tp)")
  } in {
    disp(D) => dispImpl(D)
  }

  public implementation equality[canon] => let{.
    eq(vr(_,N1,T1),vr(_,N2,T2)) => N1==N2 && T1==T2.
    eq(mtd(_,N1,C1,T1),mtd(_,N2,C2,T2)) => N1==N2 && C1==C2 && T1==T2.
    eq(over(_,N1,T1,C1),over(_,N2,T2,C2)) => eq(N1,N2) && T1==T2 && C1==C2.
    eq(intr(_,L1),intr(_,L2)) => L1==L2.
    eq(flt(_,L1),flt(_,L2)) => L1==L2.
    eq(strng(_,L1),strng(_,L2)) => L1==L2.
    eq(enm(_,N1,T1),enm(_,N2,T2)) => N1==N2 && T1==T2.
    eq(tple(_,L1),tple(_,L2)) => eqList(L1,L2).
    eq(apply(_,O1,A1,T1),apply(_,O2,A2,T2)) => eq(O1,O2) && eq(A1,A2) && T1==T2.
    eq(whr(_,T1,C1),whr(_,T2,C2)) => eq(T1,T2) && eq(C1,C2).
    eq(dot(_,T1,F1,_),dot(_,T2,F2,_)) => eq(T1,T2) && F1==F2.

    eqList([],[]) => .true.
    eqList([E1,..L1],[E2,..L2]) => eq(E1,E2) && eqList(L1,L2).
    eqList(_,_) default => .false.
  .} in {
    T1==T2 => eq(T1,T2)
  }

  public implementation hash[canon] => let{.
    hsh(vr(_,N1,_)) => hash(N1).
    hsh(mtd(_,N1,_,_)) => hash(N1).
    hsh(over(_,N1,T1,C1)) => hsh(N1)*37+hash(T1).
    hsh(intr(_,Ix)) => hash(Ix).
    hsh(flt(_,Dx)) => hash(Dx).
    hsh(strng(_,Sx)) => hash(Sx).
    hsh(enm(_,N1,_)) => hash(N1).
    hsh(apply(_,O1,A1,T1)) => hsh(O1)*36+hsh(A1).
    hsh(whr(_,T1,C1)) => hsh(T1) *37+hsh(C1).
    hsh(dot(_,T1,F1,_)) => hsh(T1) *37+hash(F1).
  .} in {
    hash(T1) => hsh(T1)
  }

  showCanon:(canon,string)=>ss.
  showCanon(vr(_,Nm,Tp),_) => ssSeq([ss(Nm)]).
  showCanon(mtd(_,Fld,_,_),_) => ssSeq([ss("µ"),ss(Fld)]).
  showCanon(over(_,V,_,Cx),Sp) => ssSeq([disp(Cx),ss("|:"),showCanon(V,Sp)]).
  showCanon(intr(_,Lt),_) => disp(Lt).
  showCanon(flt(_,Lt),_) => disp(Lt).
  showCanon(strng(_,Lt),_) => disp(Lt).
  showCanon(enm(_,Nm,Tp),_) => ssSeq([ss("."),ss(Nm)]).
  showCanon(whr(_,E,C),Sp) => ssSeq([showCanon(E,Sp),ss(" where "),showCanon(C,Sp)]).
  showCanon(dot(_,R,F,_),Sp) => ssSeq([showCanon(R,Sp),ss("."),ss(F)]).
  showCanon(csexp(_,Exp,Cs,_),Sp) => ssSeq([ss("case "),showCanon(Exp,Sp),ss(" in "),showCases(Cs,Sp)]).
  showCanon(match(_,Ptn,Gen),Sp) => ssSeq([showCanon(Ptn,Sp),ss(" .= "),showCanon(Gen,Sp)]).
  showCanon(conj(_,L,R),Sp) => ssSeq([showCanon(L,Sp),ss(" && "),showCanon(R,Sp)]).
  showCanon(disj(_,L,R),Sp) => ssSeq([ss("("),showCanon(L,Sp),ss(" || "),showCanon(R,Sp),ss(")")]).
  showCanon(implies(_,L,R),Sp) => ssSeq([ss("("),showCanon(L,Sp),ss(" *> "),showCanon(R,Sp),ss(")")]).
  showCanon(neg(_,R),Sp) => ssSeq([ss(" ~ "),showCanon(R,Sp)]).
  showCanon(cond(_,T,L,R),Sp) =>
    ssSeq([ss("("),showCanon(T,Sp),ss("?"),showCanon(L,Sp),ss(" || "),showCanon(R,Sp),ss(")")]).
  showCanon(apply(_,L,R,_),Sp) => ssSeq([showCanon(L,Sp),showCanon(R,Sp)]).
  showCanon(tple(_,Els),Sp) =>
    ssSeq([ss("("),ssSeq(interleave(Els//(El)=>showCanon(El,Sp),ss(","))),ss(")")]).
  showCanon(freeze(_,Nm,E),Sp) => ssSeq([ss("freeze "),ss(Nm),ss("."),showCanon(E,Sp)]).
  showCanon(thaw(_,E,_),Sp) => ssSeq([ss("thaw "),showCanon(E,Sp)]).
  showCanon(lambda(Nm,Rls,Tp),Sp) => ssSeq([ss("("),showRls(Nm,Rls,Sp++"  "),ss(")")]).
  showCanon(letExp(_,Defs,Ep),Sp) where Sp2.=Sp++"  " =>
    ssSeq([ss("let "),ss("{\n"),ss(Sp2),showGroup(Defs,Sp2),ss("\n"),ss(Sp),ss("}"),ss(" in "),showCanon(Ep,Sp2)]).
  showCanon(letRec(_,Defs,Ep),Sp) where Sp2.=Sp++"  " =>
    ssSeq([ss("letrec "),ss("{\n"),ss(Sp2),showGroup(Defs,Sp2),ss("\n"),ss(Sp),ss("}"),ss(" in "),showCanon(Ep,Sp2)]).
  showCanon(record(_,.none,Fields,_),Sp) =>
    ssSeq([ss("{"),showFields(Fields,Sp++"  "),ss("}")]).
  showCanon(record(_,some(Lbl),Fields,_),Sp) =>
    ssSeq([ss(Lbl),ss("{"),showFields(Fields,Sp++"  "),ss("}")]).
  showCanon(update(_,L,R),Sp) => ssSeq([showCanon(L,Sp),ss(" <<- "),showCanon(R,Sp)]).
  
  showCases(Cs,Sp) => ssSeq([ss("{"),showRls("",Cs,Sp),ss("}")]).

  showFields(Fields,Sp) => ssSeq(interleave(Fields//(Fld)=>showField(Fld,Sp),ss(".\n"++Sp))).

  showField((Nm,Val),Sp) => ssSeq([ss(Nm),ss(" = "),showCanon(Val,Sp)]).

  showGroup:(cons[canonDef],string) => ss.
  showGroup(G,Sp) => ssSeq(interleave(G//(D)=>showDef(D,Sp),ss(".\n"++Sp))).

  showDef:(canonDef,string)=>ss.
  showDef(varDef(_,Nm,FullNm,lambda(LamNm,Rls,_),_,Tp),Sp) =>
    ssSeq([ss("Fun: "),ss(Nm),ss(" "),showRls(LamNm,Rls,Sp)]).
  showDef(varDef(_,Nm,FullNm,V,_,Tp),Sp) => ssSeq([ss("Var: "),ss(Nm),ss(" ["),ss(FullNm),ss("] = "),showCanon(V,Sp)]).
  showDef(typeDef(_,Nm,T,_),Sp) => ssSeq([ss("Type: "),ss(Nm),ss("~>"),disp(T)]).
  showDef(conDef(_,_,Nm,Tp),Sp) => ssSeq([ss("Contract: "),ss(Nm),ss("::="),disp(Tp)]).
  showDef(cnsDef(_,_,Nm,Tp),Sp) => ssSeq([ss("Constructor: "),ss(Nm),ss(":"),disp(Tp)]).
  showDef(implDef(_,Nm,FullNm,Exp,_,Tp),Sp) =>
    ssSeq([ss("Implementation: "),ss(Nm),ss(" ["),ss(FullNm),ss("] = "),showCanon(Exp,Sp)]).

  showRls:(string,cons[equation],string) => ss.
  showRls(Nm,Rls,Sp) => ssSeq(interleave(Rls//(Rl)=>showRl(Nm,Rl,Sp),ss(".\n"++Sp))).

  showRl:(string,equation,string) => ss.
  showRl(Nm,eqn(_,Ptn,.none,Val),Sp) => ssSeq([
      ss(Nm),showCanon(Ptn,Sp),ss(" => "),showCanon(Val,Sp)]).
  showRl(Nm,eqn(_,Ptn,some(C),Val),Sp) => ssSeq([
      ss(Nm),showCanon(Ptn,Sp),ss(" where "),showCanon(C,Sp),ss(" => "),showCanon(Val,Sp)]).

  public implementation display[canon] => {
    disp(C) => showCanon(C,"")
  }

  public implementation display[canonDef] => {
    disp(D) => showDef(D,"")
  }

  public implementation display[equation] => {
    disp(Eq) => showRl("λ",Eq,"").
  }

  -- Useful constants
  public trueEnum:(locn)=>canon.
  trueEnum(Lc) => apply(Lc,enm(Lc,"star.core#true",enumType(boolType)),tple(Lc,[]),boolType).

  public isGoal:(canon)=>boolean.
  isGoal(enm(_,"star.core#true",nomnal("star.core*boolean"))) => .true.
  isGoal(enm(_,"star.core#false",nomnal("star.core*boolean"))) => .true.
  isGoal(whr(_,E,_)) => isGoal(E).
  isGoal(match(_,_,_)) => .true.
  isGoal(conj(_,_,_)) => .true.
  isGoal(disj(_,_,_)) => .true.
  isGoal(implies(_,_,_)) => .true.
  isGoal(neg(_,_)) => .true.
  isGoal(cond(_,_,L,R)) => isGoal(L) && isGoal(R).
  isGoal(_) default => .false.

  public isFunDef:(canon)=>boolean.
  isFunDef(lambda(_,_,_)) => .true.
  isFunDef(letExp(_,_,Exp)) => isFunDef(Exp).
  isFunDef(letRec(_,_,Exp)) => isFunDef(Exp).
  isFunDef(_) default => .false.

  public pkgImports:(pkgSpec)=>cons[importSpec].
  pkgImports(pkgSpec(Pkg,Imports,Face,Cons,Impls,PkgVrs)) => Imports.

  public splitPtn:(canon) => (canon,option[canon]).
  splitPtn(P) => let{.
    splitPttrn(apply(Lc,Op,Arg,Tp)) => valof action{
      (SOp,OCond) .= splitPttrn(Op);
      (SArg,SCond) .= splitPttrn(Arg);
      valis (apply(Lc,SOp,SArg,Tp),mergeGl(OCond,SCond))
    }
    splitPttrn(tple(Lc,Els)) => valof action{
      (SEls,SCond) .= splitPttrns(Els);
      valis (tple(Lc,SEls),SCond)
    }
    splitPttrn(whr(Lc,Pt,C)) => valof action{
      (SP,SCond) .= splitPttrn(Pt);
      valis (SP,mergeGl(SCond,some(C)))
    }
    splitPttrn(Pt) => (Pt,.none).

    splitPttrns(Els) => foldLeft(((E,C),(SEls,SCond))=>
	([E,..SEls],mergeGl(C,SCond)),([],.none),Els//splitPttrn).

    mergeGl(.none,C) => C.
    mergeGl(C,.none) => C.
    mergeGl(some(A),some(B)) => some(conj(locOf(A),A,B)).
  .} in splitPttrn(P).

}
