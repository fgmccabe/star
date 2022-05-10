star.compiler.canon{
  import star.
  import star.pkg.

  import star.compiler.meta.
  import star.compiler.location.
  import star.compiler.types.

  public pkgSpec::=pkgSpec(pkg,cons[importSpec],cons[decl]).

  public decl ::= implDec(option[locn],string,string,tipe) |
    accDec(option[locn],tipe,string,string,tipe) |
    updDec(option[locn],tipe,string,string,tipe) |
    conDec(option[locn],string,string,typeRule) |
    tpeDec(option[locn],string,tipe,typeRule) |
    varDec(option[locn],string,string,tipe) |
    funDec(option[locn],string,string,tipe) |
    cnsDec(option[locn],string,string,tipe).

  public canon ::= vd(option[locn],tipe) |
    vr(option[locn],string,tipe) |
    mtd(option[locn],string,constraint,tipe) |
    over(option[locn],canon,cons[constraint]) |
    overaccess(option[locn],canon,string,tipe) |
    intr(option[locn],integer) |
    bintr(option[locn],bigint) |
    kar(option[locn],char) |
    flt(option[locn],float) |
    strng(option[locn],string) |
    enm(option[locn],string,tipe) |
    whr(option[locn],canon,canon) |
    dot(option[locn],canon,string,tipe) |
    csexp(option[locn],canon,cons[equation],tipe) |
    match(option[locn],canon,canon) |
    conj(option[locn],canon,canon) |
    disj(option[locn],canon,canon) |
    implies(option[locn],canon,canon) |
    neg(option[locn],canon) |
    cond(option[locn],canon,canon,canon) |
    apply(option[locn],canon,canon,tipe) |
    tple(option[locn],cons[canon]) |
    lambda(option[locn],string,cons[equation],tipe) |
    owpen(option[locn],canon) |
    letExp(option[locn],cons[canonDef],cons[decl],canon) |
    letRec(option[locn],cons[canonDef],cons[decl],canon) |
    update(option[locn],canon,string,canon).

  public equation ::= eqn(option[locn],canon,option[canon],canon).

  public canonDef ::= varDef(option[locn],string,string,canon,cons[constraint],tipe) |
    typeDef(option[locn],string,tipe,typeRule) |
    conDef(option[locn],string,string,typeRule) |
    cnsDef(option[locn],string,string,tipe) |
    implDef(option[locn],string,string,canon,cons[constraint],tipe) |
    accDef(option[locn],string,string,tipe) |
    updDef(option[locn],string,string,tipe).

  public implementation hasType[canon] => {.
    typeOf(vr(_,_,T)) => T.
    typeOf(mtd(_,_,_,T)) => T.
    typeOf(over(_,T,_)) => typeOf(T).
    typeOf(overaccess(_,_,_,Tp)) => Tp.
    typeOf(intr(_,_)) => intType.
    typeOf(bintr(_,_)) => bigintType.
    typeOf(flt(_,_)) => fltType.
    typeOf(kar(_,_)) => chrType.
    typeOf(strng(_,_)) => strType.
    typeOf(enm(_,_,Tp)) => Tp.
    typeOf(csexp(_,_,_,Tp)) => Tp.
    typeOf(lambda(_,_,_,Tp)) => Tp.
    typeOf(letExp(_,_,_,E)) => typeOf(E).
    typeOf(letRec(_,_,_,E)) => typeOf(E).
    typeOf(apply(_,_,_,Tp)) => Tp.
    typeOf(tple(_,Els)) => tupleType(Els//typeOf).
    typeOf(dot(_,_,_,Tp)) => Tp.
    typeOf(whr(_,E,_)) => typeOf(E).
    typeOf(match(_,_,_)) => boolType.
    typeOf(conj(_,_,_)) => boolType.
    typeOf(disj(_,_,_)) => boolType.
    typeOf(implies(_,_,_)) => boolType.
    typeOf(cond(_,_,L,_)) => typeOf(L).
    typeOf(update(_,R,_,_)) => typeOf(R).
  .}

  public implementation hasLoc[canon] => {
    locOf(vr(Lc,_,_)) => Lc.
    locOf(mtd(Lc,_,_,_)) => Lc.
    locOf(over(Lc,_,_)) => Lc.
    locOf(overaccess(Lc,_,_,_)) => Lc.
    locOf(intr(Lc,_)) => Lc.
    locOf(bintr(Lc,_)) => Lc.
    locOf(flt(Lc,_)) => Lc.
    locOf(kar(Lc,_)) => Lc.
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
    locOf(lambda(Lc,_,_,_)) => Lc.
    locOf(letExp(Lc,_,_,_)) => Lc.
    locOf(letRec(Lc,_,_,_)) => Lc.
    locOf(update(Lc,_,_,_)) => Lc.
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
    locOf(accDef(Lc,_,_,_)) => Lc.
    locOf(updDef(Lc,_,_,_)) => Lc.
  }

  public implementation display[pkgSpec] => {
    disp(pkgSpec(Pkg,Imports,Decls)) =>
      "Package: $(Pkg), imports=$(Imports), exports=$(Decls)".
  }

  public implementation display[decl] => {
    disp(implDec(_,Nm,ImplNm,ImplTp)) =>
      "Impl #(Nm)~#(ImplNm)\:$(ImplTp)".
    disp(accDec(_,Tp,Fld,Fun,FunTp)) =>
      "Acc $(Tp).#(Fld) using #(Fun)\:$(FunTp)".
    disp(updDec(_,Tp,Fld,Fun,FunTp)) =>
      "Update $(Tp).#(Fld) using #(Fun)\:$(FunTp)".
    disp(conDec(_,Nm,_,RlTp)) =>
      "Contract #(Nm)\:$(RlTp)".
    disp(tpeDec(_,Nm,Tp,_)) =>
      "Type #(Nm)\::$(Tp)".
    disp(varDec(_,Nm,_FullNm,Tp)) =>
      "Var #(Nm)\:$(Tp)".
    disp(funDec(_,Nm,_FullNm,Tp)) =>
      "Fun #(Nm)\:$(Tp)".
    disp(cnsDec(_,Nm,_FullNm,Tp)) =>
      "Con #(Nm)\:$(Tp)".
  }

/*  public implementation equality[canon] => let{.
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
    hsh(over(_,N1,C1)) => hsh(N1)*37+hash(C1).
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
*/
  showCanon:(canon,string)=>string.
  showCanon(vd(_,Tp),_) => "void".
  showCanon(vr(_,Nm,Tp),_) => Nm.
  showCanon(mtd(_,Fld,_,_),_) => "µ#(Fld)".
  showCanon(over(_,V,Cx),Sp) => "$(Cx)|:#(showCanon(V,Sp))".
  showcanon(overaccess(_,V,F,T),Sp) => "($(V)<~#(F):$(T))".
  showCanon(intr(_,Lt),_) => disp(Lt).
  showCanon(bintr(_,Lt),_) => disp(Lt).
  showCanon(flt(_,Lt),_) => disp(Lt).
  showCanon(strng(_,Lt),_) => disp(Lt).
  showCanon(enm(_,Nm,Tp),_) => ".#(Nm)".
  showCanon(whr(_,E,C),Sp) => "#(showCanon(E,Sp)) where #(showCanon(C,Sp))".
  showCanon(dot(_,R,F,Tp),Sp) => "#(showCanon(R,Sp))°#(F)\:$(Tp)".
  showCanon(csexp(_,Exp,Cs,_),Sp) => "case #(showCanon(Exp,Sp)) in #(showCases(Cs,Sp))".
  showCanon(match(_,Ptn,Gen),Sp) => "#(showCanon(Ptn,Sp)) .= #(showCanon(Gen,Sp))".
  showCanon(conj(_,L,R),Sp) => "#(showCanon(L,Sp)) && #(showCanon(R,Sp))".
  showCanon(disj(_,L,R),Sp) => "(#(showCanon(L,Sp)) || #(showCanon(R,Sp)))".
  showCanon(implies(_,L,R),Sp) => "(#(showCanon(L,Sp)) *> #(showCanon(R,Sp)))".
  showCanon(neg(_,R),Sp) => " ~ #(showCanon(R,Sp))".
  showCanon(cond(_,T,L,R),Sp) =>
    "(#(showCanon(T,Sp)) ? #(showCanon(L,Sp)) || #(showCanon(R,Sp)))".
  showCanon(apply(_,L,R,_),Sp) => "#(showCanon(L,Sp))#(showCanon(R,Sp))".
  showCanon(tple(_,Els),Sp) =>
    "(#(interleave(Els//(El)=>showCanon(El,Sp),",")*))".
  showCanon(lambda(_,Nm,Rls,Tp),Sp) => "(#(showRls(Nm,Rls,Sp++"  ")))".
  showCanon(letExp(_,Defs,Dcs,Ep),Sp) where Sp2.=Sp++"  " =>
    "let {\n#(Sp2)#(showGroup(Defs,Sp2))\n#(Sp)} in #(showCanon(Ep,Sp2))".
  showCanon(letRec(_,Defs,Dcs,Ep),Sp) where Sp2.=Sp++"  " =>
    "let {.\n#(Sp2)#(showGroup(Defs,Sp2))\n#(Sp),} in #(showCanon(Ep,Sp2))".
  showCanon(update(_,L,F,R),Sp) => "#(showCanon(L,Sp)).#(F) <<- #(showCanon(R,Sp))".

  showCases(Cs,Sp) => "{#(showRls("",Cs,Sp))}".

  showFields(Fields,Sp) => interleave(Fields//(Fld)=>showField(Fld,Sp),".\n"++Sp)*.

  showField((Nm,Val),Sp) => "#(Nm) = #(showCanon(Val,Sp))".

  showGroup:(cons[canonDef],string) => string.
  showGroup(G,Sp) => interleave(G//(D)=>showDef(D,Sp),".\n"++Sp)*.

  showDef:(canonDef,string)=>string.
  showDef(varDef(_,Nm,FullNm,lambda(_,LamNm,Rls,_),_,Tp),Sp) =>
    "Fun: #(Nm) #(showRls(LamNm,Rls,Sp))".
  showDef(varDef(_,Nm,FullNm,V,_,Tp),Sp) => "Var: #(Nm)[#(FullNm)] = #(showCanon(V,Sp))".
  showDef(typeDef(_,Nm,T,_),Sp) => "Type: #(Nm)~>$(T)".
  showDef(conDef(_,_,Nm,Tp),Sp) => "Contract: #(Nm) ::= $(Tp)".
  showDef(cnsDef(_,_,Nm,Tp),Sp) => "Constructor: #(Nm):$(Tp)".
  showDef(implDef(_,Nm,FullNm,Exp,_,Tp),Sp) => "Implementation: #(Nm)\:$(Tp) = $(Exp)".
  showDef(accDef(_,Fld,Nm,Tp),Sp) => "Access: #(Fld):$(Tp) = $(Nm)".
  showDef(updDef(_,Fld,Nm,Tp),Sp) => "Update: #(Fld):$(Tp) = $(Nm)".

  showRls:(string,cons[equation],string) => string.
  showRls(Nm,Rls,Sp) => interleave(Rls//(Rl)=>showRl(Nm,Rl,Sp),".\n"++Sp)*.

  showRl:(string,equation,string) => string.
  showRl(Nm,eqn(_,Ptn,.none,Val),Sp) => "#(Nm)#(showCanon(Ptn,Sp)) => #(showCanon(Val,Sp))".
  showRl(Nm,eqn(_,Ptn,some(C),Val),Sp) => "#(Nm)#(showCanon(Ptn,Sp)) where #(showCanon(C,Sp)) => #(showCanon(Val,Sp))".

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
  public trueEnum:(option[locn])=>canon.
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
  isFunDef(lambda(_,_,_,_)) => .true.
  isFunDef(letExp(_,_,_,Exp)) => isFunDef(Exp).
  isFunDef(letRec(_,_,_,Exp)) => isFunDef(Exp).
  isFunDef(_) default => .false.

  public pkgImports:(pkgSpec)=>cons[importSpec].
  pkgImports(pkgSpec(_,Imports,_)) => Imports.

  public splitPtn:(canon) => (canon,option[canon]).
  splitPtn(P) => let{.
    splitPttrn(apply(Lc,Op,Arg,Tp)) => valof{
      (SOp,OCond) .= splitPttrn(Op);
      (SArg,SCond) .= splitPttrn(Arg);
      valis (apply(Lc,SOp,SArg,Tp),mergeGl(OCond,SCond))
    }
    splitPttrn(tple(Lc,Els)) => valof{
      (SEls,SCond) .= splitPttrns(Els);
      valis (tple(Lc,SEls),SCond)
    }
    splitPttrn(whr(Lc,Pt,C)) => valof{
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
