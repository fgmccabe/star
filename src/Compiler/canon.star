star.compiler.canon{
  import star.
  import star.pkg.

  import star.compiler.meta.
  import star.compiler.location.
  import star.compiler.types.
  import star.compiler.operators.

  public canon ::= .anon(option[locn],tipe) |
  .vr(option[locn],string,tipe) |
  .mtd(option[locn],string,tipe) |
  .over(option[locn],canon,cons[constraint]) |
  .overaccess(option[locn],canon,tipe,string,tipe) |
  .intr(option[locn],integer) |
  .bintr(option[locn],bigint) |
  .kar(option[locn],char) |
  .flt(option[locn],float) |
  .strng(option[locn],string) |
  .enm(option[locn],string,tipe) |
  .dot(option[locn],canon,string,tipe) |
  .update(option[locn],canon,string,canon) |
  .csexp(option[locn],canon,cons[rule[canon]],tipe) |
  .trycatch(option[locn],canon,canon,cons[rule[canon]],tipe) |
  .trywith(option[locn],canon,canon,cons[rule[canon]],tipe) |
  .rais(option[locn],canon,canon,tipe) |
  .match(option[locn],canon,canon) |
  .conj(option[locn],canon,canon) |
  .disj(option[locn],canon,canon) |
  .neg(option[locn],canon) |
  .cond(option[locn],canon,canon,canon) |
  .apply(option[locn],canon,cons[canon],tipe) |
  .invoak(option[locn],canon,cons[canon],tipe) |
  .suspnd(option[locn],canon,canon,tipe) |
  .resme(option[locn],canon,canon,tipe) |
  .tple(option[locn],cons[canon]) |
  .lambda(option[locn],string,cons[rule[canon]],tipe) |
  .owpen(option[locn],canon) |
  .letExp(option[locn],cons[canonDef],cons[decl],canon) |
  .letRec(option[locn],cons[canonDef],cons[decl],canon) |
  .vlof(option[locn],canonAction,tipe).

  public canonAction ::= .doNop(option[locn]) |
    .doSeq(option[locn],canonAction,canonAction) |
    .doLbld(option[locn],string,canonAction) |
    .doBrk(option[locn],string) |
    .doValis(option[locn],canon) |
    .doRaise(option[locn],canon,canon) |
    .doDefn(option[locn],canon,canon) |
    .doMatch(option[locn],canon,canon) |
    .doAssign(option[locn],canon,canon) |
    .doTryCatch(option[locn],canonAction,canon,cons[rule[canonAction]]) |
    .doTryWith(option[locn],canonAction,canon,cons[rule[canonAction]]) |
    .doIfThen(option[locn],canon,canonAction,canonAction) |
    .doCase(option[locn],canon,cons[rule[canonAction]]) |
    .doWhile(option[locn],canon,canonAction) |
    .doLet(option[locn],cons[canonDef],cons[decl],canonAction) |
    .doLetRec(option[locn],cons[canonDef],cons[decl],canonAction) |
    .doRetire(option[locn],canon,canon) |
    .doCall(option[locn],canon).

  public rule[t] ::= .rule(option[locn],canon,option[canon],t).
    
  public canonDef ::= .varDef(option[locn],string,canon,cons[constraint],tipe) |
    .typeDef(option[locn],string,tipe,typeRule) |
    .conDef(option[locn],string,string,typeRule) |
    .cnsDef(option[locn],string,string,tipe) |
    .implDef(option[locn],string,string,canon,cons[constraint],tipe) |
    .accDef(option[locn],string,string,tipe) |
    .updDef(option[locn],string,string,tipe).

  public implementation hasType[canon] => {.
    typeOf(Cn) => case Cn in {
      .anon(_,T) => T.
      .vr(_,_,T) => T.
      .mtd(_,_,T) => T.
      .over(_,T,_) => typeOf(T).
      .overaccess(_,T,_,_,_) => typeOf(T).
      .intr(_,_) => intType.
      .bintr(_,_) => bigintType.
      .flt(_,_) => fltType.
      .kar(_,_) => chrType.
      .strng(_,_) => strType.
      .enm(_,_,Tp) => Tp.
      .csexp(_,_,_,Tp) => Tp.
      .trycatch(_,_,_,_,Tp) => Tp.
      .trywith(_,_,_,_,Tp) => Tp.
      .rais(_,_,_,Tp) => Tp.
      .lambda(_,_,_,Tp) => Tp.
      .letExp(_,_,_,E) => typeOf(E).
      .letRec(_,_,_,E) => typeOf(E).
      .apply(_,_,_,Tp) => Tp.
      .invoak(_,_,_,Tp) => Tp.
      .suspnd(_,_,_,Tp) => Tp.
      .resme(_,_,_,Tp) => Tp.
      .tple(_,Els) => .tupleType(Els//typeOf).
      .dot(_,_,_,Tp) => Tp.
      .update(_,R,_,_) => typeOf(R).
      .match(_,_,_) => boolType.
      .conj(_,_,_) => boolType.
      .disj(_,_,_) => boolType.
      .cond(_,_,L,_) => typeOf(L).
      .vlof(_,_,Tp) => Tp.
    }
  .}

  public implementation hasLoc[canon] => {
    locOf(Cn) => case Cn in {
      .anon(Lc,_) => Lc.
      .vr(Lc,_,_) => Lc.
      .mtd(Lc,_,_) => Lc.
      .over(Lc,_,_) => Lc.
      .overaccess(Lc,_,_,_,_) => Lc.
      .intr(Lc,_) => Lc.
      .bintr(Lc,_) => Lc.
      .flt(Lc,_) => Lc.
      .kar(Lc,_) => Lc.
      .strng(Lc,_) => Lc.
      .enm(Lc,_,_) => Lc.
      .dot(Lc,_,_,_) => Lc.
      .update(Lc,_,_,_) => Lc.
      .csexp(Lc,_,_,_) => Lc.
      .trycatch(Lc,_,_,_,_) => Lc.
      .trywith(Lc,_,_,_,_) => Lc.
      .rais(Lc,_,_,_) => Lc.
      .match(Lc,_,_) => Lc.
      .conj(Lc,_,_) => Lc.
      .disj(Lc,_,_) => Lc.
      .neg(Lc,_) => Lc.
      .cond(Lc,_,_,_) => Lc.
      .apply(Lc,_,_,_) => Lc.
      .invoak(Lc,_,_,_) => Lc.
      .suspnd(Lc,_,_,_) => Lc.
      .resme(Lc,_,_,_) => Lc.
      .tple(Lc,_) => Lc.
      .lambda(Lc,_,_,_) => Lc.
      .letExp(Lc,_,_,_) => Lc.
      .letRec(Lc,_,_,_) => Lc.
      .vlof(Lc,_,_) => Lc.
    }
  }

  public implementation all x ~~ hasLoc[rule[x]] => {
    locOf(.rule(Lc,_,_,_)) => Lc.
  }

  public implementation hasLoc[canonAction] => {
    locOf(A) => case A in {
      .doNop(Lc) => Lc.
      .doSeq(Lc,_,_) => Lc.
      .doLbld(Lc,_,_) => Lc.
      .doBrk(Lc,_) => Lc.
      .doValis(Lc,_) => Lc.
      .doRaise(Lc,_,_) => Lc.
      .doDefn(Lc,_,_) => Lc.
      .doMatch(Lc,_,_) => Lc.
      .doAssign(Lc,_,_) => Lc.
      .doTryCatch(Lc,_,_,_) => Lc.
      .doTryWith(Lc,_,_,_) => Lc.
      .doIfThen(Lc,_,_,_) => Lc.
      .doCase(Lc,_,_) => Lc.
      .doWhile(Lc,_,_) => Lc.
      .doLet(Lc,_,_,_) => Lc.
      .doLetRec(Lc,_,_,_) => Lc.
      .doCall(Lc,_) => Lc.
    }
  }

  public implementation hasLoc[canonDef] => {
    locOf(Df) => case Df in {
      .varDef(Lc,_,_,_,_) => Lc.
      .typeDef(Lc,_,_,_) => Lc.
      .conDef(Lc,_,_,_) => Lc.
      .cnsDef(Lc,_,_,_) => Lc.
      .implDef(Lc,_,_,_,_,_) => Lc.
      .accDef(Lc,_,_,_) => Lc.
      .updDef(Lc,_,_,_) => Lc.
    }
  }

  showCanon:(canon,integer,string)=>string.
  showCanon(Cn,Pr,Sp) => case Cn in {
    .anon(_,_) => "_".
    .vr(_,Nm,Tp) => Nm.
    .mtd(_,Fld,_) => "µ#(Fld)".
    .over(_,V,Cx) => "$(Cx)|:#(showCanon(V,Pr,Sp))".
    .overaccess(_,Rc,RcTp,F,T) => "$(RcTp)<~{#(F):$(T)}|:#(showCanon(Rc,Pr,Sp))".
    .intr(_,Lt) => disp(Lt).
    .bintr(_,Lt) => disp(Lt).
    .kar(_,Ch) => disp(Ch).
    .flt(_,Lt) => disp(Lt).
    .strng(_,Lt) => disp(Lt).
    .enm(_,Nm,Tp) => "°#(Nm)".
    .dot(_,R,F,Tp) => "#(showCanon(R,0,Sp))°#(F)\:$(Tp)".
    .update(_,L,F,R) where (Lp,OPr,Rp) ?= isInfixOp("=") =>
      "#(leftParen(OPr,Pr))#(showCanon(L,Lp,Sp)).#(F) = #(showCanon(R,Rp,Sp))#(rgtParen(OPr,Pr))".
    .csexp(_,Exp,Cs,_) where (OPr,Rp) ?= isPrefixOp("case") =>
      "#(leftParen(OPr,Pr))case #(showCanon(Exp,Rp,Sp)) in #(showCases(Cs,showCanon,Sp))#(rgtParen(OPr,Pr))".
    .trycatch(_,Exp,T,H,_) where (OPr,Rp) ?= isPrefixOp("try") =>
      "#(leftParen(OPr,Pr))try #(showCanon(T,Rp,Sp)) in #(showCanon(Exp,Rp,Sp)) catch #(showCases(H,showCanon,Sp++"  "))#(rgtParen(OPr,Pr))".
    .trywith(_,Exp,T,H,_) where (OPr,Rp) ?= isPrefixOp("try") =>
      "#(leftParen(OPr,Pr))try #(showCanon(T,Rp,Sp)) in #(showCanon(Exp,Rp,Sp)) with #(showCases(H,showCanon,Sp++"  "))#(rgtParen(OPr,Pr))".
    .rais(_,_Thrw,Exp,_) where (OPr,Rp) ?= isPrefixOp("raise") =>
      "#(leftParen(OPr,Pr)) raise #(showCanon(Exp,Rp,Sp))#(rgtParen(OPr,Pr))".
    .match(_,Ptn,Gen) where (Lp,OPr,Rp) ?= isInfixOp(".=") =>
      "#(leftParen(OPr,Pr))#(showCanon(Ptn,Lp,Sp)) .= #(showCanon(Gen,Rp,Sp))#(rgtParen(OPr,Pr))".
    .conj(_,L,R) where (Lp,OPr,Rp) ?= isInfixOp("&&") =>
      "#(leftParen(OPr,Pr))#(showCanon(L,Lp,Sp)) && #(showCanon(R,Rp,Sp))#(rgtParen(OPr,Pr))".
    .disj(_,L,R) where (Lp,OPr,Rp) ?= isInfixOp("||") =>
      "#(leftParen(OPr,Pr))#(showCanon(L,Lp,Sp)) || #(showCanon(R,Rp,Sp))#(rgtParen(OPr,Pr))".
    .neg(_,R)  where (OPr,Rp) ?= isPrefixOp("~") =>
      "#(leftParen(OPr,Pr))~ #(showCanon(R,Rp,Sp))#(rgtParen(OPr,Pr))".
    .cond(_,T,L,R) where (Lp,OPr,Rp) ?= isInfixOp("??") =>
      "(#(showCanon(T,Lp,Sp)) ?? #(showCanon(L,Rp,Sp)) || #(showCanon(R,Rp,Sp)))".
    .apply(_,L,R,_) => showApply(L,R,Pr,Sp).
    .invoak(_,K,A,_) where (Lp,OPr,Rp) ?= isInfixOp(".") =>
      "#(leftParen(OPr,Pr))#(showCanon(K,Lp,Sp)).(showTuple(Args,Sp))#(rgtParen(OPr,Pr))".
    .suspnd(Lc,T,E,_)  where (Lp,OPr,Rp) ?= isInfixOp("suspend") =>
      "#(showCanon(T,Lp,Sp)) suspend #(showCanon(E,Rp,Sp))".
    .resme(Lc,T,E,_)  where (Lp,OPr,Rp) ?= isInfixOp("resume") =>
      "#(showCanon(T,Lp,Sp)) resume #(showCanon(E,Rp,Sp))".
    .tple(_,Els) => showTuple(Els,Sp).
    .lambda(_,Nm,Rls,Tp) => "(#(showRls(Nm,Rls,showCanon,Sp++"  ")))".
    .letExp(_,Defs,Dcs,Ep) where Sp2.=Sp++"  " && (Lp,OPr,Rp) ?= isInfixOp("in") =>
      "#(leftParen(OPr,Pr))let {\n#(Sp2)#(showGroup(Defs,Sp2))\n#(Sp)} in #(showCanon(Ep,Rp,Sp2))#(rgtParen(OPr,Pr))".
    .letRec(_,Defs,Dcs,Ep) where Sp2.=Sp++"  " && (Lp,OPr,Rp) ?= isInfixOp("in") =>
      "#(leftParen(OPr,Pr))let {.\n#(Sp2)#(showGroup(Defs,Sp2))\n#(Sp).} in #(showCanon(Ep,Rp,Sp2))#(rgtParen(OPr,Pr))".
    .vlof(_,A,_) where (OPr,Rp) ?= isPrefixOp("valof") =>
      "#(leftParen(OPr,Pr))valof #(showAct(A,Rp,Sp))#(rgtParen(OPr,Pr))".
  }

  showApply(.vr(_,Op,_),[L,R],Pr,Sp) where (Lp,OPr,Rp) ?= isInfixOp(Op) =>
    "#(leftParen(OPr,Pr))#(showCanon(L,Lp,Sp)) #(Op) #(showCanon(R,Rp,Sp)) #(rgtParen(OPr,Pr))".
  showApply(.vr(_,Op,_),[R],Pr,Sp) where (OPr,Rp) ?= isPrefixOp(Op) =>
    "#(leftParen(OPr,Pr)) #(Op) #(showCanon(R,Rp,Sp)) #(rgtParen(OPr,Pr))".
  showApply(.vr(_,Op,_),[L],Pr,Sp) where (Lp,OPr) ?= isPostfixOp(Op) =>
    "#(leftParen(OPr,Pr))#(showCanon(L,Lp,Sp)) #(Op) #(rgtParen(OPr,Pr))".
  showApply(Op,Args,_,Sp) => "#(showCanon(Op,0,Sp))#(showTuple(Args,Sp))".

  showTuple(Els,Sp) => 
    "(#(interleave(Els//(El)=>showCanon(El,0,Sp),",")*))".

  showAct(Ac,Pr,Sp) => case Ac in {
    .doNop(_) => "{}".
    .doSeq(Lc,L,R)  where (Lp,OPr,Rp) ?= isInfixOp(";") =>
      "{\n#(Sp++"  ")#(showActSeq(.doSeq(Lc,L,R),Rp,Sp++"  "))\n#(Sp)}".
    .doLbld(_,Lb,A) => "#(Lb)\:#(showAct(A,Pr,Sp))".
    .doBrk(_,Lb) => "break #(Lb)".
    .doValis(_,E) where (OPr,Rp) ?= isPrefixOp("valis") =>
      "valis #(showCanon(E,Rp,Sp))".
    .doRaise(_,_,E) where (OPr,Rp) ?= isPrefixOp("raise") =>
      "raise #(showCanon(E,Rp,Sp))".
    .doDefn(_,L,R) where (Lp,OPr,Rp) ?= isInfixOp("=") =>
      "#(showCanon(L,Lp,Sp)) = #(showCanon(R,Rp,Sp))".
    .doMatch(_,L,R) where (Lp,OPr,Rp) ?= isInfixOp(".=") =>
      "#(showCanon(L,Lp,Sp)) .= #(showCanon(R,Rp,Sp))".
    .doAssign(_,L,R)  where (Lp,OPr,Rp) ?= isInfixOp(":=") =>
      "#(showCanon(L,Lp,Sp)) := #(showCanon(R,Rp,Sp))".
    .doTryCatch(_,A,T,H) =>
      "try #(showCanon(T,Pr,Sp)) in #(showAct(A,Pr,Sp)) catch {\n#(showCases(H,showAct,Sp))\n}".
    .doTryWith(_,A,T,H) =>
      "try #(showCanon(T,Pr,Sp)) in #(showAct(A,Pr,Sp)) with {\n#(showCases(H,showAct,Sp))\n}".
    .doIfThen(_,T,Th,El) where (Lp,OPr,Rp) ?= isInfixOp("then") =>
      "if #(showCanon(T,Lp,Sp)) then #(showAct(Th,Pr,Sp)) else #(showAct(El,Pr,Sp))".
    .doCase(Lc,G,C) where (Lp,OPr,Rp) ?= isInfixOp("in") =>
      "case #(showCanon(G,Lp,Sp)) in #(showCases(C,showAct,Sp))".
    .doWhile(_,G,B) where (OPr,Rp) ?= isPrefixOp("while") =>
      "while #(showCanon(G,Rp,Sp)) do #(showAct(B,0,Sp))".
    .doLet(Lc,Defs,_Decs,B) where Sp2.=Sp++"  " && (Lp,OPr,Rp) ?= isInfixOp("in") =>
      "let {\n#(Sp2)#(showGroup(Defs,Sp2))\n#(Sp)} in #(showAct(B,Rp,Sp2))".
    .doLetRec(Lc,Defs,Decs,B) where Sp2.=Sp++"  " &&
	(Lp,OPr,Rp) ?= isInfixOp("in") =>
      "let {.\n#(Sp2)#(showGroup(Defs,Sp2))\n#(Sp).} in #(showAct(B,Rp,Sp2))". 
    .doRetire(Lc,T,E)  where (Lp,OPr,Rp) ?= isInfixOp("retire") =>
      "#(showCanon(T,Lp,Sp)) retire #(showCanon(E,Rp,Sp))".
    .doCall(_,E) => "call #(showCanon(E,Pr,Sp))".
  }

  showActSeq(.doSeq(_,L,R),Pr,Sp) => "#(showAct(L,Pr-1,Sp));\n#(Sp)#(showActSeq(R,Pr,Sp))".
  showActSeq(A,Pr,Sp) => showAct(A,Pr,Sp).

  public implementation display[canonAction] => {
    disp(C) => showAct(C,0,"")
  }

  showCases:all x ~~ (cons[rule[x]],(x,integer,string)=>string,string)=>string.
  showCases(Cs,Shw,Sp) => "{#(showRls("",Cs,Shw,Sp))}".

  showFields(Fields,Sp) => interleave(Fields//(Fld)=>showField(Fld,Sp),".\n"++Sp)*.

  showField((Nm,Val),Sp) => "#(Nm) = #(showCanon(Val,1000,Sp))".

  showGroup:(cons[canonDef],string) => string.
  showGroup(G,Sp) => interleave(G//(D)=>showDef(D,Sp),".\n"++Sp)*.

  showDef:(canonDef,string)=>string.
  showDef(Df,Sp) => case Df in {
    .varDef(_,Nm,.lambda(_,_LamNm,Rls,_),_,Tp) =>
      "Fun: #(showRls(Nm,Rls,showCanon,Sp))".
    .varDef(_,Nm,V,_,Tp) => "Var: #(Nm) = #(showCanon(V,0,Sp))".
    .typeDef(_,Nm,T,_) => "Type: #(Nm)~>$(T)".
    .conDef(_,_,Nm,Tp) => "Contract: #(Nm) ::= $(Tp)".
    .cnsDef(_,_,Nm,Tp) => "Constructor: #(Nm):$(Tp)".
    .implDef(_,_,Nm,Exp,Cx,Tp) => "Implementation: #(Nm)\:$(Tp) = $(Cx) |: $(Exp)".
    .accDef(_,Fld,Nm,Tp) => "Access: #(Fld):$(Tp) = $(Nm)".
    .updDef(_,Fld,Nm,Tp) => "Update: #(Fld):$(Tp) = $(Nm)".
  }

  showRls:all x ~~ (string,cons[rule[x]],(x,integer,string)=>string,string) => string.
  showRls(Nm,Rls,Shw,Sp) => interleave(Rls//(Rl)=>showRl(Nm,Rl,Shw,Sp),"\n"++Sp++"| ")*.

  showRl:all x ~~ (string,rule[x],(x,integer,string)=>string,string) => string.
  showRl(Nm,.rule(_,Ptn,.none,Val),Shw,Sp) where (Lp,OPr,Rp) ?= isInfixOp("=>") =>
    "#(Nm)#(showCanon(Ptn,Lp,Sp)) => #(Shw(Val,Rp,Sp))".
  showRl(Nm,.rule(_,Ptn,.some(C),Val),Shw,Sp) where (Lp,OPr,Rp) ?= isInfixOp("=>") =>
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

  public isGoal:(canon)=>boolean.
  isGoal(Cn) => case Cn in {
    .enm(_,"star.core#true",.nomnal("star.core*boolean")) => .true.
    .enm(_,"star.core#false",.nomnal("star.core*boolean")) => .true.
    .match(_,_,_) => .true.
    .conj(_,_,_) => .true.
    .disj(_,_,_) => .true.
    .neg(_,_) => .true.
    .cond(_,_,L,R) => isGoal(L) && isGoal(R).
    _ default => .false.
  }

  public isFunDef:(canon)=>boolean.
  isFunDef(Df) => case Df in {
    .lambda(_,_,_,_) => .true.
    .letExp(_,_,_,Exp) => isFunDef(Exp).
    .letRec(_,_,_,Exp) => isFunDef(Exp).
    _ default => .false.
  }
}
