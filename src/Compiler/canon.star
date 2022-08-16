star.compiler.canon{
  import star.
  import star.pkg.

  import star.compiler.meta.
  import star.compiler.location.
  import star.compiler.types.
  import star.compiler.operators.

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
    mtd(option[locn],string,tipe) |
    over(option[locn],canon,cons[constraint]) |
    overaccess(option[locn],canon,tipe,string,tipe) |
    intr(option[locn],integer) |
    bintr(option[locn],bigint) |
    kar(option[locn],char) |
    flt(option[locn],float) |
    strng(option[locn],string) |
    enm(option[locn],string,tipe) |
    whr(option[locn],canon,canon) |
    dot(option[locn],canon,string,tipe) |
    update(option[locn],canon,string,canon) |
    csexp(option[locn],canon,cons[rule[canon]],tipe) |
    trycatch(option[locn],canon,canon,tipe) |
    match(option[locn],canon,canon) |
    conj(option[locn],canon,canon) |
    disj(option[locn],canon,canon) |
    neg(option[locn],canon) |
    cond(option[locn],canon,canon,canon) |
    apply(option[locn],canon,cons[canon],tipe) |
    tple(option[locn],cons[canon]) |
    lambda(option[locn],string,cons[rule[canon]],tipe) |
    owpen(option[locn],canon) |
    letExp(option[locn],cons[canonDef],cons[decl],canon) |
    letRec(option[locn],cons[canonDef],cons[decl],canon) |
    vlof(option[locn],canonAction,tipe) |
    tsk(option[locn],canon,tipe).

  public canonAction ::= doNop(option[locn]) |
    doSeq(option[locn],canonAction,canonAction) |
    doLbld(option[locn],string,canonAction) |
    doBrk(option[locn],string) |
    doValis(option[locn],canon) |
    doThrow(option[locn],canon) |
    doDefn(option[locn],canon,canon) |
    doMatch(option[locn],canon,canon) |
    doAssign(option[locn],canon,canon) |
    doTryCatch(option[locn],canonAction,cons[rule[canonAction]]) |
    doIfThen(option[locn],canon,canonAction,canonAction) |
    doCase(option[locn],canon,cons[rule[canonAction]]) |
    doWhile(option[locn],canon,canonAction) |
    doLet(option[locn],cons[canonDef],cons[decl],canonAction) |
    doLetRec(option[locn],cons[canonDef],cons[decl],canonAction) |
    doSuspend(option[locn],canon,canon,cons[rule[canonAction]]) |
    doResume(option[locn],canon,canon,cons[rule[canonAction]]) |
    doRetire(option[locn],canon,canon) |
    doCall(option[locn],canon).

  public rule[t] ::= rule(option[locn],canon,option[canon],t).
    
  public canonDef ::= varDef(option[locn],string,string,canon,cons[constraint],tipe) |
    typeDef(option[locn],string,tipe,typeRule) |
    conDef(option[locn],string,string,typeRule) |
    cnsDef(option[locn],string,string,tipe) |
    implDef(option[locn],string,string,canon,cons[constraint],tipe) |
    accDef(option[locn],string,string,tipe) |
    updDef(option[locn],string,string,tipe).

  public implementation hasType[canon] => {.
    typeOf(vd(_,T)) => T.
    typeOf(vr(_,_,T)) => T.
    typeOf(mtd(_,_,T)) => T.
    typeOf(over(_,T,_)) => typeOf(T).
    typeOf(overaccess(_,_,_,_,Tp)) => Tp.
    typeOf(intr(_,_)) => intType.
    typeOf(bintr(_,_)) => bigintType.
    typeOf(flt(_,_)) => fltType.
    typeOf(kar(_,_)) => chrType.
    typeOf(strng(_,_)) => strType.
    typeOf(enm(_,_,Tp)) => Tp.
    typeOf(csexp(_,_,_,Tp)) => Tp.
    typeOf(trycatch(_,_,_,Tp)) => Tp.
    typeOf(lambda(_,_,_,Tp)) => Tp.
    typeOf(letExp(_,_,_,E)) => typeOf(E).
    typeOf(letRec(_,_,_,E)) => typeOf(E).
    typeOf(apply(_,_,_,Tp)) => Tp.
    typeOf(tple(_,Els)) => tupleType(Els//typeOf).
    typeOf(dot(_,_,_,Tp)) => Tp.
    typeOf(update(_,R,_,_)) => typeOf(R).
    typeOf(whr(_,E,_)) => typeOf(E).
    typeOf(match(_,_,_)) => boolType.
    typeOf(conj(_,_,_)) => boolType.
    typeOf(disj(_,_,_)) => boolType.
    typeOf(cond(_,_,L,_)) => typeOf(L).
    typeOf(vlof(_,_,Tp)) => Tp.
    typeOf(tsk(_,_,Tp)) => Tp.
  .}

  public implementation hasLoc[canon] => {
    locOf(vd(Lc,_)) => Lc.
    locOf(vr(Lc,_,_)) => Lc.
    locOf(mtd(Lc,_,_)) => Lc.
    locOf(over(Lc,_,_)) => Lc.
    locOf(overaccess(Lc,_,_,_,_)) => Lc.
    locOf(intr(Lc,_)) => Lc.
    locOf(bintr(Lc,_)) => Lc.
    locOf(flt(Lc,_)) => Lc.
    locOf(kar(Lc,_)) => Lc.
    locOf(strng(Lc,_)) => Lc.
    locOf(enm(Lc,_,_)) => Lc.
    locOf(whr(Lc,_,_)) => Lc.
    locOf(dot(Lc,_,_,_)) => Lc.
    locOf(update(Lc,_,_,_)) => Lc.
    locOf(csexp(Lc,_,_,_)) => Lc.
    locOf(trycatch(Lc,_,_,_)) => Lc.
    locOf(match(Lc,_,_)) => Lc.
    locOf(conj(Lc,_,_)) => Lc.
    locOf(disj(Lc,_,_)) => Lc.
    locOf(neg(Lc,_)) => Lc.
    locOf(cond(Lc,_,_,_)) => Lc.
    locOf(apply(Lc,_,_,_)) => Lc.
    locOf(tple(Lc,_)) => Lc.
    locOf(lambda(Lc,_,_,_)) => Lc.
    locOf(letExp(Lc,_,_,_)) => Lc.
    locOf(letRec(Lc,_,_,_)) => Lc.
    locOf(vlof(Lc,_,_)) => Lc.
    locOf(tsk(Lc,_,_)) => Lc.
  }

  public implementation all x ~~ hasLoc[rule[x]] => {
    locOf(rule(Lc,_,_,_)) => Lc.
  }

  public implementation hasLoc[canonAction] => {
    locOf(doNop(Lc)) => Lc.
    locOf(doSeq(Lc,_,_)) => Lc.
    locOf(doLbld(Lc,_,_)) => Lc.
    locOf(doBrk(Lc,_)) => Lc.
    locOf(doValis(Lc,_)) => Lc.
    locOf(doThrow(Lc,_)) => Lc.
    locOf(doDefn(Lc,_,_)) => Lc.
    locOf(doMatch(Lc,_,_)) => Lc.
    locOf(doAssign(Lc,_,_)) => Lc.
    locOf(doTryCatch(Lc,_,_)) => Lc.
    locOf(doIfThen(Lc,_,_,_)) => Lc.
    locOf(doCase(Lc,_,_)) => Lc.
    locOf(doWhile(Lc,_,_)) => Lc.
    locOf(doLet(Lc,_,_,_)) => Lc.
    locOf(doLetRec(Lc,_,_,_)) => Lc.
    locOf(doSuspend(Lc,_,_,_)) => Lc.
    locOf(doResume(Lc,_,_,_)) => Lc.
    locOf(doRetire(Lc,_,_)) => Lc.
    locOf(doCall(Lc,_)) => Lc.
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
    disp(implDec(_,Nm,ImplNm,ImplTp)) => "Impl #(Nm)~#(ImplNm)\:$(ImplTp)".
    disp(accDec(_,Tp,Fld,Fun,FunTp)) => "Acc $(Tp).#(Fld) using #(Fun)\:$(FunTp)".
    disp(updDec(_,Tp,Fld,Fun,FunTp)) => "Update $(Tp).#(Fld) using #(Fun)\:$(FunTp)".
    disp(conDec(_,Nm,_,RlTp)) => "Contract #(Nm)\:$(RlTp)".
    disp(tpeDec(_,Nm,Tp,_)) => "Type #(Nm)\::$(Tp)".
    disp(varDec(_,Nm,_FullNm,Tp)) => "Var #(Nm)\:$(Tp)".
    disp(funDec(_,Nm,_FullNm,Tp)) => "Fun #(Nm)\:$(Tp)".
    disp(cnsDec(_,Nm,_FullNm,Tp)) => "Con #(Nm)\:$(Tp)".
  }

  showCanon:(canon,integer,string)=>string.
  showCanon(vd(_,_),_,_) => "void".
  showCanon(vr(_,Nm,Tp),_,_) => Nm.
  showCanon(mtd(_,Fld,_),_,_) => "µ#(Fld)".
  showCanon(over(_,V,Cx),Pr,Sp) => "$(Cx)|:#(showCanon(V,Pr,Sp))".
  showcanon(overaccess(_,_,RcTp,F,T),_,Sp) => "($(RcTp)<~#(F):$(T))".
  showCanon(intr(_,Lt),_,_) => disp(Lt).
  showCanon(bintr(_,Lt),_,_) => disp(Lt).
  showCanon(flt(_,Lt),_,_) => disp(Lt).
  showCanon(strng(_,Lt),_,_) => disp(Lt).
  showCanon(enm(_,Nm,Tp),_,_) => ".#(Nm)".
  showCanon(whr(_,E,C),Pr,Sp) where (Lp,OPr,Rp) ^= isInfixOp("where") =>
    "#(leftParen(OPr,Pr))#(showCanon(E,Lp,Sp)) where #(showCanon(C,Rp,Sp))#(rgtParen(OPr,Pr))".
  showCanon(dot(_,R,F,Tp),_,Sp) => "#(showCanon(R,0,Sp))°#(F)\:$(Tp)".
  showCanon(update(_,L,F,R),Pr,Sp) where (Lp,OPr,Rp) ^= isInfixOp("<<-") =>
    "#(leftParen(OPr,Pr))#(showCanon(L,Lp,Sp)).#(F) <<- #(showCanon(R,Rp,Sp))#(rgtParen(OPr,Pr))".
  showCanon(csexp(_,Exp,Cs,_),Pr,Sp) where (OPr,Rp) ^= isPrefixOp("case") =>
    "#(leftParen(OPr,Pr))case #(showCanon(Exp,Rp,Sp)) in #(showCases(Cs,showCanon,Sp))#(rgtParen(OPr,Pr))".
  showCanon(trycatch(_,Exp,H,_),Pr,Sp)  where (OPr,Rp) ^= isPrefixOp("try") =>
    "#(leftParen(OPr,Pr))try #(showCanon(Exp,Rp,Sp)) catch #(showCanon(H,Rp,Sp++"  "))#(rgtParen(OPr,Pr))".
  showCanon(match(_,Ptn,Gen),Pr,Sp) where (Lp,OPr,Rp) ^= isInfixOp(".=") =>
    "#(leftParen(OPr,Pr))#(showCanon(Ptn,Lp,Sp)) .= #(showCanon(Gen,Rp,Sp))#(rgtParen(OPr,Pr))".
  showCanon(conj(_,L,R),Pr,Sp) where (Lp,OPr,Rp) ^= isInfixOp("&&") =>
    "#(leftParen(OPr,Pr))#(showCanon(L,Lp,Sp)) && #(showCanon(R,Rp,Sp))#(rgtParen(OPr,Pr))".
  showCanon(disj(_,L,R),Pr,Sp) where (Lp,OPr,Rp) ^= isInfixOp("||") =>
    "#(leftParen(OPr,Pr))#(showCanon(L,Lp,Sp)) || #(showCanon(R,Rp,Sp))#(rgtParen(OPr,Pr))".
  showCanon(neg(_,R),Pr,Sp)  where (OPr,Rp) ^= isPrefixOp("~") =>
    "#(leftParen(OPr,Pr))~ #(showCanon(R,Rp,Sp))#(rgtParen(OPr,Pr))".
  showCanon(cond(_,T,L,R),Pr,Sp) where (Lp,OPr,Rp) ^= isInfixOp("?") =>
    "(#(showCanon(T,Lp,Sp)) ? #(showCanon(L,Rp,Sp)) || #(showCanon(R,Rp,Sp)))".
  showCanon(apply(_,L,R,_),Pr,Sp) => showApply(L,R,Pr,Sp).
  showCanon(tple(_,Els),_,Sp) => showTuple(Els,Sp).
  showCanon(lambda(_,Nm,Rls,Tp),_,Sp) => "(#(showRls(Nm,Rls,showCanon,Sp++"  ")))".
  showCanon(letExp(_,Defs,Dcs,Ep),Pr,Sp) where Sp2.=Sp++"  " && (Lp,OPr,Rp) ^= isInfixOp("in") =>
    "#(leftParen(OPr,Pr))let {\n#(Sp2)#(showGroup(Defs,Sp2))\n#(Sp)} in #(showCanon(Ep,Rp,Sp2))#(rgtParen(OPr,Pr))".
  showCanon(letRec(_,Defs,Dcs,Ep),Pr,Sp) where Sp2.=Sp++"  " && (Lp,OPr,Rp) ^= isInfixOp("in") =>
    "#(leftParen(OPr,Pr))let {.\n#(Sp2)#(showGroup(Defs,Sp2))\n#(Sp).} in #(showCanon(Ep,Rp,Sp2))#(rgtParen(OPr,Pr))".
  showCanon(vlof(_,A,_),Pr,Sp) where (OPr,Rp) ^= isPrefixOp("valof") =>
    "#(leftParen(OPr,Pr))valof #(showAct(A,Rp,Sp))#(rgtParen(OPr,Pr))".
  showCanon(tsk(_,A,_),Pr,Sp) where (OPr,Rp) ^= isPrefixOp("task") =>
    "#(leftParen(OPr,Pr))task #(showCanon(A,Rp,Sp))#(rgtParen(OPr,Pr))".

  showApply(vr(_,Op,_),[L,R],Pr,Sp) where (Lp,OPr,Rp) ^= isInfixOp(Op) =>
    "#(leftParen(OPr,Pr))#(showCanon(L,Lp,Sp)) #(Op) #(showCanon(R,Rp,Sp)) #(rgtParen(OPr,Pr))".
  showApply(vr(_,Op,_),[R],Pr,Sp) where (OPr,Rp) ^= isPrefixOp(Op) =>
    "#(leftParen(OPr,Pr)) #(Op) #(showCanon(R,Rp,Sp)) #(rgtParen(OPr,Pr))".
  showApply(vr(_,Op,_),[L],Pr,Sp) where (Lp,OPr) ^= isPostfixOp(Op) =>
    "#(leftParen(OPr,Pr))#(showCanon(L,Lp,Sp)) #(Op) #(rgtParen(OPr,Pr))".
  showApply(Op,Args,_,Sp) => "#(showCanon(Op,0,Sp))#(showTuple(Args,Sp))".

  showTuple(Els,Sp) => 
    "(#(interleave(Els//(El)=>showCanon(El,0,Sp),",")*))".

  showAct(doNop(_),_,_) => "{}".
  showAct(doSeq(Lc,L,R),Pr,Sp)  where (Lp,OPr,Rp) ^= isInfixOp(";") =>
    "{\n#(Sp++"  ")#(showActSeq(doSeq(Lc,L,R),Rp,Sp++"  "))\n#(Sp)}".
  showAct(doLbld(_,Lb,A),Pr,Sp) => "#(Lb)\:#(showAct(A,Pr,Sp))".
  showAct(doBrk(_,Lb),_,_) => "break #(Lb)".
  showAct(doValis(_,E),Pr,Sp) where (OPr,Rp) ^= isPrefixOp("valis") =>
    "valis #(showCanon(E,Rp,Sp))".
  showAct(doThrow(_,E),Pr,Sp) where (OPr,Rp) ^= isPrefixOp("throw") =>
    "throw #(showCanon(E,Rp,Sp))".
  showAct(doDefn(_,L,R),Pr,Sp) where (Lp,OPr,Rp) ^= isInfixOp("=") =>
    "#(showCanon(L,Lp,Sp)) = #(showCanon(R,Rp,Sp))".
  showAct(doMatch(_,L,R),Pr,Sp) where (Lp,OPr,Rp) ^= isInfixOp(".=") =>
    "#(showCanon(L,Lp,Sp)) .= #(showCanon(R,Rp,Sp))".
  showAct(doAssign(_,L,R),Pr,Sp)  where (Lp,OPr,Rp) ^= isInfixOp(":=") =>
    "#(showCanon(L,Lp,Sp)) := #(showCanon(R,Rp,Sp))".
  showAct(doTryCatch(_,A,H),Pr,Sp) =>
    "try #(showAct(A,Pr,Sp)) catch {\n#(showCases(H,showAct,Sp))\n}".
  showAct(doIfThen(_,T,Th,El),Pr,Sp) where (Lp,OPr,Rp) ^= isInfixOp("then") =>
    "if #(showCanon(T,Lp,Sp)) then #(showAct(Th,Pr,Sp)) else #(showAct(El,Pr,Sp))".
  showAct(doCase(Lc,G,C),Pr,Sp) where (Lp,OPr,Rp) ^= isInfixOp("in") =>
    "case #(showCanon(G,Lp,Sp)) in #(showCases(C,showAct,Sp))".
  showAct(doWhile(_,G,B),Pr,Sp) where (OPr,Rp) ^= isPrefixOp("while") =>
    "while #(showCanon(G,Rp,Sp)) do #(showAct(B,0,Sp))".
  showAct(doLet(Lc,Defs,_Decs,B),Pr,Sp) where Sp2.=Sp++"  " && (Lp,OPr,Rp) ^= isInfixOp("in") =>
    "let {\n#(Sp2)#(showGroup(Defs,Sp2))\n#(Sp)} in #(showAct(B,Rp,Sp2))".
  showAct(doLetRec(Lc,Defs,Decs,B),Pr,Sp) where Sp2.=Sp++"  " &&
      (Lp,OPr,Rp) ^= isInfixOp("in") =>
    "let {.\n#(Sp2)#(showGroup(Defs,Sp2))\n#(Sp).} in #(showAct(B,Rp,Sp2))". 
  showAct(doSuspend(Lc,T,E,C),Pr,Sp) where (Lp,OPr,Rp) ^= isInfixOp("suspend") =>
    "#(showCanon(T,Lp,Sp)) suspend #(showCanon(E,Rp,Sp)) in #(showCases(C,showAct,Sp))".
  showAct(doResume(Lc,T,E,C),Pr,Sp)  where (Lp,OPr,Rp) ^= isInfixOp("resume") =>
    "#(showCanon(T,Lp,Sp)) resume #(showCanon(E,Rp,Sp)) in #(showCases(C,showAct,Sp))".
  showAct(doRetire(Lc,T,E),Pr,Sp)  where (Lp,OPr,Rp) ^= isInfixOp("retire") =>
    "#(showCanon(T,Lp,Sp)) retire #(showCanon(E,Rp,Sp))".
  showAct(doCall(_,E),Pr,Sp) => "call #(showCanon(E,Pr,Sp))".

  showActSeq(doSeq(_,L,R),Pr,Sp) => "#(showAct(L,Pr-1,Sp));\n#(Sp)#(showActSeq(R,Pr,Sp))".
  showActSeq(A,Pr,Sp) => showAct(A,Pr,Sp).

  showCases:all x ~~ (cons[rule[x]],(x,integer,string)=>string,string)=>string.
  showCases(Cs,Shw,Sp) => "{#(showRls("",Cs,Shw,Sp))}".

  showFields(Fields,Sp) => interleave(Fields//(Fld)=>showField(Fld,Sp),".\n"++Sp)*.

  showField((Nm,Val),Sp) => "#(Nm) = #(showCanon(Val,1000,Sp))".

  showGroup:(cons[canonDef],string) => string.
  showGroup(G,Sp) => interleave(G//(D)=>showDef(D,Sp),".\n"++Sp)*.

  showDef:(canonDef,string)=>string.
  showDef(varDef(_,Nm,FullNm,lambda(_,LamNm,Rls,_),_,Tp),Sp) =>
    "Fun: #(Nm) #(showRls(LamNm,Rls,showCanon,Sp))".
  showDef(varDef(_,Nm,FullNm,V,_,Tp),Sp) => "Var: #(Nm)[#(FullNm)] = #(showCanon(V,0,Sp))".
  showDef(typeDef(_,Nm,T,_),Sp) => "Type: #(Nm)~>$(T)".
  showDef(conDef(_,_,Nm,Tp),Sp) => "Contract: #(Nm) ::= $(Tp)".
  showDef(cnsDef(_,_,Nm,Tp),Sp) => "Constructor: #(Nm):$(Tp)".
  showDef(implDef(_,Nm,FullNm,Exp,_,Tp),Sp) => "Implementation: #(Nm)\:$(Tp) = $(Exp)".
  showDef(accDef(_,Fld,Nm,Tp),Sp) => "Access: #(Fld):$(Tp) = $(Nm)".
  showDef(updDef(_,Fld,Nm,Tp),Sp) => "Update: #(Fld):$(Tp) = $(Nm)".

  showRls:all x ~~ (string,cons[rule[x]],(x,integer,string)=>string,string) => string.
  showRls(Nm,Rls,Shw,Sp) => interleave(Rls//(Rl)=>showRl(Nm,Rl,Shw,Sp),".\n"++Sp)*.

  showRl:all x ~~ (string,rule[x],(x,integer,string)=>string,string) => string.
  showRl(Nm,rule(_,Ptn,.none,Val),Shw,Sp) where (Lp,OPr,Rp) ^= isInfixOp("=>") =>
    "#(Nm)#(showCanon(Ptn,Lp,Sp)) => #(Shw(Val,Rp,Sp))".
  showRl(Nm,rule(_,Ptn,some(C),Val),Shw,Sp) where (Lp,OPr,Rp) ^= isInfixOp("=>") =>
    "#(Nm)#(showCanon(Ptn,Lp,Sp)) where #(showCanon(C,Lp,Sp)) => #(Shw(Val,Rp,Sp))".

  shDeflt(.true) => "default ".
  shDeflt(.false) => "".

  leftParen(OPr,Pr) where OPr>Pr => "(".
  leftParen(_,_) default => "".

  rgtParen(OPr,Pr) where OPr>Pr => ")".
  rgtParen(_,_) default => "".

  public implementation display[canon] => {
    disp(C) => showCanon(C,0,"")
  }

  public implementation display[canonDef] => {
    disp(D) => showDef(D,"")
  }

  public displayDefs:(cons[canonDef]) => string.
  displayDefs(Dfs) => interleave(Dfs//disp,"\n")*.

  public implementation all x ~~ display[x] |: display[rule[x]] => {
    disp(Eq) => showRl("λ",Eq,(X,_,_)=>disp(X),"").
  }

  -- Useful constants
  public trueEnum:(option[locn])=>canon.
  trueEnum(Lc) => apply(Lc,enm(Lc,"star.core#true",enumType(boolType)),[],boolType).

  public isGoal:(canon)=>boolean.
  isGoal(enm(_,"star.core#true",nomnal("star.core*boolean"))) => .true.
  isGoal(enm(_,"star.core#false",nomnal("star.core*boolean"))) => .true.
  isGoal(whr(_,E,_)) => isGoal(E).
  isGoal(match(_,_,_)) => .true.
  isGoal(conj(_,_,_)) => .true.
  isGoal(disj(_,_,_)) => .true.
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
    splitPttrn(apply(Lc,Op,Args,Tp)) => valof{
      (SOp,OCond) = splitPttrn(Op);
      (SEls,SCond) = splitPttrns(Args);
      valis (apply(Lc,SOp,SEls,Tp),mergeGl(OCond,SCond))
    }
    splitPttrn(tple(Lc,Els)) => valof{
      (SEls,SCond) = splitPttrns(Els);
      valis (tple(Lc,SEls),SCond)
    }
    splitPttrn(whr(Lc,Pt,C)) => valof{
      (SP,SCond) = splitPttrn(Pt);
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
