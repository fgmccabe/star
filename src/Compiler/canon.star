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
  .over(option[locn],canon,constraint) |
  .intr(option[locn],integer) |
  .bintr(option[locn],bigint) |
  .kar(option[locn],char) |
  .flt(option[locn],float) |
  .strng(option[locn],string) |
  .enm(option[locn],string,tipe) |
  .dot(option[locn],canon,string,tipe) |
  .tdot(option[locn],canon,integer,tipe) |
  .update(option[locn],canon,string,canon) |
  .csexp(option[locn],canon,cons[rule[canon]],tipe) |
  .tryC(option[locn],canon,cons[rule[canon]],tipe) |
  .thrwC(option[locn],canon,tipe,tipe) |
  .match(option[locn],canon,canon) |
  .conj(option[locn],canon,canon) |
  .disj(option[locn],canon,canon) |
  .neg(option[locn],canon) |
  .cond(option[locn],canon,canon,canon) |
  .apply(option[locn],canon,cons[canon],tipe) |
  .tapply(option[locn],canon,cons[canon],tipe,tipe) |
  .tple(option[locn],cons[canon]) |
  .lambda(option[locn],string,rule[canon],tipe) |
  .thunk(option[locn],canon,tipe) |
  .thRef(option[locn],canon,tipe) |
  .newSav(option[locn],tipe) |
  .svGet(option[locn],canon,tipe) |
  .svSet(option[locn],canon,canon) |
  .cell(option[locn],canon,tipe) |
  .get(option[locn],canon,tipe) |
  .owpen(option[locn],canon) |
  .letExp(option[locn],cons[canonDef],cons[decl],canon) |
  .letRec(option[locn],cons[canonDef],cons[decl],canon) |
  .vlof(option[locn],canonAction,tipe) |
  .susp(option[locn],canon,canon,tipe) |
  .retyre(option[locn],canon,canon,tipe) |
  .resum(option[locn],canon,canon,tipe).

  public canonAction ::= .doNop(option[locn]) |
  .doSeq(option[locn],canonAction,canonAction) |
  .doLbld(option[locn],string,canonAction) |
  .doBrk(option[locn],string) |
  .doValis(option[locn],canon) |
  .doDefn(option[locn],canon,canon) |
  .doMatch(option[locn],canon,canon) |
  .doAssign(option[locn],canon,canon) |
  .doTryCatch(option[locn],canonAction,canon,cons[rule[canonAction]]) |
  .doThrow(option[locn],canon) |
  .doIfThen(option[locn],canon,canonAction,canonAction) |
  .doCase(option[locn],canon,cons[rule[canonAction]]) |
  .doWhile(option[locn],canon,canonAction) |
  .doLet(option[locn],cons[canonDef],cons[decl],canonAction) |
  .doLetRec(option[locn],cons[canonDef],cons[decl],canonAction) |
  .doExp(option[locn],canon).

  public rule[t] ::= .rule(option[locn],canon,option[canon],t).
    
  public canonDef ::=
    .varDef(option[locn],string,string,canon,cons[constraint],tipe) |
    .funDef(option[locn],string,cons[rule[canon]],cons[constraint],tipe) |
    .typeDef(option[locn],string,tipe,typeRule) |
    .cnsDef(option[locn],string,integer,tipe) |
    .implDef(option[locn],string,string,canon,cons[constraint],tipe).


  public implementation hasType[canon] => {.
    typeOf(Cn) => case Cn in {
      | .anon(_,T) => T
      | .vr(_,_,T) => T
      | .mtd(_,_,T) => T
      | .over(_,T,_) => typeOf(T)
      | .intr(_,_) => intType
      | .bintr(_,_) => bigintType
      | .flt(_,_) => fltType
      | .kar(_,_) => chrType
      | .strng(_,_) => strType
      | .enm(_,_,Tp) => Tp
      | .csexp(_,_,_,Tp) => Tp
      | .tryC(_,_,_,Tp) => Tp
      | .thrwC(_,_,Tp,_) => Tp
      | .lambda(_,_,_,Tp) => Tp
      | .thunk(_,_,Tp) => Tp
      | .thRef(_,_,Tp) => Tp
      | .newSav(_,Tp) => Tp
      | .svGet(_,_,Tp) => Tp
      | .svSet(_,_,Vl) => typeOf(Vl)
      | .cell(_,_,Tp) => Tp
      | .get(_,_,Tp) => Tp
      | .letExp(_,_,_,E) => typeOf(E)
      | .letRec(_,_,_,E) => typeOf(E)
      | .apply(_,_,_,Tp) => Tp
      | .tapply(_,_,_,Tp,_) => Tp
      | .tple(_,Els) => .tupleType(Els//typeOf)
      | .dot(_,_,_,Tp) => Tp
      | .tdot(_,_,_,Tp) => Tp
      | .update(_,R,_,_) => typeOf(R)
      | .match(_,_,_) => boolType
      | .conj(_,_,_) => boolType
      | .disj(_,_,_) => boolType
      | .cond(_,_,L,_) => typeOf(L)
      | .vlof(_,_,Tp) => Tp
      | .susp(_,_,_,Tp) => Tp
      | .retyre(_,_,_,Tp) => Tp
      | .resum(_,_,_,Tp) => Tp
    }
  .}

  public implementation hasLoc[canon] => {
    locOf(Cn) => case Cn in {
      | .anon(Lc,_) => Lc
      | .vr(Lc,_,_) => Lc
      | .mtd(Lc,_,_) => Lc
      | .over(Lc,_,_) => Lc
      | .intr(Lc,_) => Lc
      | .bintr(Lc,_) => Lc
      | .flt(Lc,_) => Lc
      | .kar(Lc,_) => Lc
      | .strng(Lc,_) => Lc
      | .enm(Lc,_,_) => Lc
      | .dot(Lc,_,_,_) => Lc
      | .update(Lc,_,_,_) => Lc
      | .tdot(Lc,_,_,_) => Lc
      | .thunk(Lc,_,_) => Lc
      | .thRef(Lc,_,_) => Lc
      | .newSav(Lc,_) => Lc
      | .svGet(Lc,_,_) => Lc
      | .svSet(Lc,_,_) => Lc
      | .cell(Lc,_,_) => Lc
      | .get(Lc,_,_) => Lc
      | .csexp(Lc,_,_,_) => Lc
      | .tryC(Lc,_,_,_) => Lc
      | .thrwC(Lc,_,_,_) => Lc
      | .match(Lc,_,_) => Lc
      | .conj(Lc,_,_) => Lc
      | .disj(Lc,_,_) => Lc
      | .neg(Lc,_) => Lc
      | .cond(Lc,_,_,_) => Lc
      | .apply(Lc,_,_,_) => Lc
      | .tapply(Lc,_,_,_,_) => Lc
      | .tple(Lc,_) => Lc
      | .lambda(Lc,_,_,_) => Lc
      | .letExp(Lc,_,_,_) => Lc
      | .letRec(Lc,_,_,_) => Lc
      | .vlof(Lc,_,_) => Lc
      | .susp(Lc,_,_,_) => Lc
      | .retyre(Lc,_,_,_) => Lc
      | .resum(Lc,_,_,_) => Lc
    }
  }

  public implementation all x ~~ hasLoc[rule[x]] => {
    locOf(.rule(Lc,_,_,_)) => Lc.
  }

  public implementation hasLoc[canonAction] => {
    locOf(A) => case A in {
      | .doNop(Lc) => Lc
      | .doSeq(Lc,_,_) => Lc
      | .doLbld(Lc,_,_) => Lc
      | .doBrk(Lc,_) => Lc
      | .doValis(Lc,_) => Lc
      | .doDefn(Lc,_,_) => Lc
      | .doMatch(Lc,_,_) => Lc
      | .doAssign(Lc,_,_) => Lc
      | .doTryCatch(Lc,_,_,_) => Lc
      | .doThrow(Lc,_,_) => Lc
      | .doIfThen(Lc,_,_,_) => Lc
      | .doCase(Lc,_,_) => Lc
      | .doWhile(Lc,_,_) => Lc
      | .doLet(Lc,_,_,_) => Lc
      | .doLetRec(Lc,_,_,_) => Lc
      | .doExp(Lc,_) => Lc
    }
  }

  public implementation hasLoc[canonDef] => {
    locOf(Df) => case Df in {
      | .varDef(Lc,_,_,_,_,_) => Lc
      | .funDef(Lc,_,_,_,_) => Lc
      | .typeDef(Lc,_,_,_) => Lc
      | .cnsDef(Lc,_,_,_) => Lc
      | .implDef(Lc,_,_,_,_,_) => Lc
    }
  }

  public implementation reQuant[canonDef] => {
    reQuant(Qs,Df) => case Df in {
      | .varDef(Lc,Nm,LongNm,Vl,Cx,Tp) => .varDef(Lc,Nm,LongNm,Vl,Cx,reQuant(Qs,Tp))
      | .funDef(Lc,Nm,Rls,Cx,Tp) => .funDef(Lc,Nm,Rls,Cx,reQuant(Qs,Tp))
      | .typeDef(Lc,Nm,Tp,Rl) => .typeDef(Lc,Nm,reQuant(Qs,Tp),reQuant(Qs,Rl))
      | .cnsDef(Lc,Nm,Ix,Tp) => .cnsDef(Lc,Nm,Ix,reQuant(Qs,Tp))
      | .implDef(Lc,Nm,FlNm,Vl,Cx,Tp) => .implDef(Lc,Nm,FlNm,Vl,Cx,reQuant(Qs,Tp))
    }
    reQuantX(Qs,Df) => case Df in {
      | .varDef(Lc,Nm,LongNm,Vl,Cx,Tp) => .varDef(Lc,Nm,LongNm,Vl,Cx,reQuantX(Qs,Tp))
      | .funDef(Lc,Nm,Rls,Cx,Tp) => .funDef(Lc,Nm,Rls,Cx,reQuantX(Qs,Tp))
      | .typeDef(Lc,Nm,Tp,Rl) => .typeDef(Lc,Nm,reQuantX(Qs,Tp),reQuantX(Qs,Rl))
      | .cnsDef(Lc,Nm,Ix,Tp) => .cnsDef(Lc,Nm,Ix,reQuantX(Qs,Tp))
      | .implDef(Lc,Nm,FlNm,Vl,Cx,Tp) => .implDef(Lc,Nm,FlNm,Vl,Cx,reQuantX(Qs,Tp))
    }
  }

  public implementation reQuant[decl] => {
    reQuant(Qs,Dc) => case Dc in {
      | .implDec(Lc,Nm,FlNm,Tp) => .implDec(Lc,Nm,FlNm,reQuant(Qs,Tp))
      | .accDec(Lc,Tp,Nm,FlNm,ATp) => .accDec(Lc,reQuant(Qs,Tp),Nm,FlNm,reQuant(Qs,ATp))
      | .updDec(Lc,Tp,Nm,FlNm,ATp) => .updDec(Lc,reQuant(Qs,Tp),Nm,FlNm,reQuant(Qs,ATp))
      | .conDec(Lc,Nm,FlNm,TpRl) => .conDec(Lc,Nm,FlNm,reQuant(Qs,TpRl))
      | .tpeDec(Lc,Nm,Tp,TpRl) => .tpeDec(Lc,Nm,reQuant(Qs,Tp),reQuant(Qs,TpRl))
      | .varDec(Lc,Nm,FlNm,Tp) => .varDec(Lc,Nm,FlNm,reQuant(Qs,Tp))
      | .funDec(Lc,Nm,FlNm,Tp) => .funDec(Lc,Nm,FlNm,reQuant(Qs,Tp))
      | .cnsDec(Lc,Nm,FlNm,Tp) => .cnsDec(Lc,Nm,FlNm,reQuant(Qs,Tp))
    }
    reQuantX(Qs,Dc) => case Dc in {
      | .implDec(Lc,Nm,FlNm,Tp) => .implDec(Lc,Nm,FlNm,reQuantX(Qs,Tp))
      | .accDec(Lc,Tp,Nm,FlNm,ATp) => .accDec(Lc,reQuantX(Qs,Tp),Nm,FlNm,reQuantX(Qs,ATp))
      | .updDec(Lc,Tp,Nm,FlNm,ATp) => .updDec(Lc,reQuantX(Qs,Tp),Nm,FlNm,reQuantX(Qs,ATp))
      | .conDec(Lc,Nm,FlNm,TpRl) => .conDec(Lc,Nm,FlNm,reQuantX(Qs,TpRl))
      | .tpeDec(Lc,Nm,Tp,TpRl) => .tpeDec(Lc,Nm,reQuantX(Qs,Tp),reQuantX(Qs,TpRl))
      | .varDec(Lc,Nm,FlNm,Tp) => .varDec(Lc,Nm,FlNm,reQuantX(Qs,Tp))
      | .funDec(Lc,Nm,FlNm,Tp) => .funDec(Lc,Nm,FlNm,reQuantX(Qs,Tp))
      | .cnsDec(Lc,Nm,FlNm,Tp) => .cnsDec(Lc,Nm,FlNm,reQuantX(Qs,Tp))
    }
  }

  showCanon:(canon,integer,string)=>string.
  showCanon(Cn,Pr,Sp) => case Cn in {
    | .anon(_,_) => "_"
    | .vr(_,Nm,Tp) => Nm
    | .mtd(_,Fld,_) => "µ#(Fld)"
    | .over(_,V,Cx) => "$(Cx)|:#(showCanon(V,Pr,Sp))"
    | .intr(_,Lt) => disp(Lt)
    | .bintr(_,Lt) => disp(Lt)
    | .kar(_,Ch) => disp(Ch)
    | .flt(_,Lt) => disp(Lt)
    | .strng(_,Lt) => disp(Lt)
    | .enm(_,Nm,Tp) => ".#(Nm)"
    | .dot(_,R,F,Tp) => "#(showCanon(R,0,Sp))°#(F)\:$(Tp)"
    | .tdot(_,R,F,Tp) => "#(showCanon(R,0,Sp))°$(F)\:$(Tp)"
    | .update(_,L,F,R) where (Lp,OPr,Rp) ?= isInfixOp("=") =>
      "#(leftParen(OPr,Pr))#(showCanon(L,Lp,Sp)).#(F) = #(showCanon(R,Rp,Sp))#(rgtParen(OPr,Pr))"
    | .csexp(_,Exp,Cs,_) where (OPr,Rp) ?= isPrefixOp("case") =>
      "#(leftParen(OPr,Pr))case #(showCanon(Exp,Rp,Sp)) in #(showCases(Cs,showCanon,Sp))#(rgtParen(OPr,Pr))"
    | .tryC(_,Exp,H,_) where (OPr,Rp) ?= isPrefixOp("try") =>
      "#(leftParen(OPr,Pr))try #(showCanon(Exp,Rp,Sp)) catch #(showCases(H,showCanon,Sp++"  "))#(rgtParen(OPr,Pr))"
    | .thrwC(_,Exp,_,_) where (OPr,Rp) ?= isPrefixOp("throw") =>
      "#(leftParen(OPr,Pr)) throw #(showCanon(Exp,Rp,Sp))#(rgtParen(OPr,Pr))"
    | .match(_,Ptn,Gen) where (Lp,OPr,Rp) ?= isInfixOp(".=") =>
      "#(leftParen(OPr,Pr))#(showCanon(Ptn,Lp,Sp)) .= #(showCanon(Gen,Rp,Sp))#(rgtParen(OPr,Pr))"
    | .conj(_,L,R) where (Lp,OPr,Rp) ?= isInfixOp("&&") =>
      "#(leftParen(OPr,Pr))#(showCanon(L,Lp,Sp)) && #(showCanon(R,Rp,Sp))#(rgtParen(OPr,Pr))"
    | .disj(_,L,R) where (Lp,OPr,Rp) ?= isInfixOp("||") =>
      "#(leftParen(OPr,Pr))#(showCanon(L,Lp,Sp)) || #(showCanon(R,Rp,Sp))#(rgtParen(OPr,Pr))"
    | .neg(_,R)  where (OPr,Rp) ?= isPrefixOp("~") =>
      "#(leftParen(OPr,Pr))~ #(showCanon(R,Rp,Sp))#(rgtParen(OPr,Pr))"
    | .cond(_,T,L,R) where (Lp,OPr,Rp) ?= isInfixOp("??") =>
      "(#(showCanon(T,Lp,Sp)) ?? #(showCanon(L,Rp,Sp)) || #(showCanon(R,Rp,Sp)))"
    | .apply(_,L,R,_) => showApply(L,R,Pr,Sp)
    | .tapply(_,L,R,_,ETp) => "#(showApply(L,R,Pr,Sp)) throws $(ETp)"
    | .tple(_,Els) => "#(showTuple(Els,Sp))"
    | .lambda(_,Nm,Rl,_) => "(#(showRl(Nm,Rl,showCanon,Sp++"  ")))"
    | .thunk(_,E,Tp) => "$$#(showCanon(E,0,Sp))"
    | .thRef(_,E,Tp) => "#(showCanon(E,0,Sp))!!"
    | .newSav(_,Tp) => "^$(Tp)"
    | .svGet(_,S,_) => "#(showCanon(S,0,Sp))^"
    | .svSet(_,S,V) => "#(showCanon(S,0,Sp)) <- #(showCanon(V,0,Sp))"
    | .cell(_,I,_) => "ref #(showCanon(I,0,Sp))"
    | .get(_,I,_) => "#(showCanon(I,0,Sp))!"
    | .letExp(_,Defs,Dcs,Ep) where Sp2.=Sp++"  " && (Lp,OPr,Rp) ?= isInfixOp("in") =>
      "#(leftParen(OPr,Pr))let {\n#(Sp2)#(showGroup(Defs,Sp2))\n#(Sp)} in #(showCanon(Ep,Rp,Sp2))#(rgtParen(OPr,Pr))"
    | .letRec(_,Defs,Dcs,Ep) where Sp2.=Sp++"  " && (Lp,OPr,Rp) ?= isInfixOp("in") =>
      "#(leftParen(OPr,Pr))let {.\n#(Sp2)#(showGroup(Defs,Sp2))\n#(Sp)#(showDecs(Dcs,Sp2)).} in #(showCanon(Ep,Rp,Sp2))#(rgtParen(OPr,Pr))"
    | .vlof(_,A,_) where (OPr,Rp) ?= isPrefixOp("valof") =>
      "#(leftParen(OPr,Pr))valof #(showAct(A,Rp,Sp))#(rgtParen(OPr,Pr))"
    | .susp(_,T,M,_) where (Lp,OPr,Rp) ?= isInfixOp("suspend") =>
      "#(leftParen(OPr,Pr))#(showCanon(T,Lp,Sp)) suspend #(showCanon(M,Rp,Sp))#(rgtParen(OPr,Pr))"
    | .retyre(_,T,M,_) where (Lp,OPr,Rp) ?= isInfixOp("retire") =>
      "#(leftParen(OPr,Pr))#(showCanon(T,Lp,Sp)) retire #(showCanon(M,Rp,Sp))#(rgtParen(OPr,Pr))"
    | .resum(_,T,M,_) where (Lp,OPr,Rp) ?= isInfixOp("resume") =>
      "#(leftParen(OPr,Pr))#(showCanon(T,Lp,Sp)) resume #(showCanon(M,Rp,Sp))#(rgtParen(OPr,Pr))"
  }

  showApply(.vr(_,Op,_),[L,R],Pr,Sp) where (Lp,OPr,Rp) ?= isInfixOp(Op) =>
    "#(leftParen(OPr,Pr))#(showCanon(L,Lp,Sp)) #(Op) #(showCanon(R,Rp,Sp)) #(rgtParen(OPr,Pr))".
  showApply(.vr(_,Op,_),[R],Pr,Sp) where (OPr,Rp) ?= isPrefixOp(Op) =>
    "#(leftParen(OPr,Pr)) #(Op) #(showCanon(R,Rp,Sp)) #(rgtParen(OPr,Pr))".
  showApply(.vr(_,Op,_),[L],Pr,Sp) where (Lp,OPr) ?= isPostfixOp(Op) =>
    "#(leftParen(OPr,Pr))#(showCanon(L,Lp,Sp)) #(Op) #(rgtParen(OPr,Pr))".
  showApply(Op,Args,_,Sp) => "#(showCanon(Op,0,Sp))#(showTuple(Args,Sp))".

  showTuple(Els,Sp) => "(#(interleave(Els//(El)=>showCanon(El,0,Sp),",")*))".

  showAct(Ac,Pr,Sp) => case Ac in {
    | .doNop(_) => "{}"
    | .doSeq(Lc,L,R)  where (Lp,OPr,Rp) ?= isInfixOp(";") =>
      "{\n#(Sp++"  ")#(showActSeq(.doSeq(Lc,L,R),Rp,Sp++"  "))\n#(Sp)}"
    | .doLbld(_,Lb,A) => "#(Lb)\:#(showAct(A,Pr,Sp))"
    | .doBrk(_,Lb) => "break #(Lb)"
    | .doValis(_,E) where (OPr,Rp) ?= isPrefixOp("valis") =>
      "valis #(showCanon(E,Rp,Sp))"
    | .doDefn(_,L,R) where (Lp,OPr,Rp) ?= isInfixOp("=") =>
      "#(showCanon(L,Lp,Sp))\:$(typeOf(L)) = #(showCanon(R,Rp,Sp))"
    | .doMatch(_,L,R) where (Lp,OPr,Rp) ?= isInfixOp(".=") =>
      "#(showCanon(L,Lp,Sp)) .= #(showCanon(R,Rp,Sp))"
    | .doAssign(_,L,R)  where (Lp,OPr,Rp) ?= isInfixOp(":=") =>
      "#(showCanon(L,Lp,Sp)) := #(showCanon(R,Rp,Sp))"
    | .doTryCatch(_,A,T,H) =>
      "try #(showAct(A,Pr,Sp)) catch #(showCanon(T,Pr,Sp)) in {\n#(showCases(H,showAct,Sp))\n}"
    | .doThrow(_,A,_) =>
      "throw #(showCanon(A,Pr,Sp))"
    | .doIfThen(_,T,Th,El) where (Lp,OPr,Rp) ?= isInfixOp("then") =>
      "if #(showCanon(T,Lp,Sp)) then #(showAct(Th,Pr,Sp)) else #(showAct(El,Pr,Sp))"
    | .doCase(Lc,G,C) where (Lp,OPr,Rp) ?= isInfixOp("in") =>
      "case #(showCanon(G,Lp,Sp)) in #(showCases(C,showAct,Sp))"
    | .doWhile(_,G,B) where (OPr,Rp) ?= isPrefixOp("while") =>
      "while #(showCanon(G,Rp,Sp)) do #(showAct(B,0,Sp))"
    | .doLet(Lc,Defs,_Decs,B) where Sp2.=Sp++"  " && (Lp,OPr,Rp) ?= isInfixOp("in") =>
      "let {\n#(Sp2)#(showGroup(Defs,Sp2))\n#(Sp)} in #(showAct(B,Rp,Sp2))"
    | .doLetRec(Lc,Defs,Decs,B) where Sp2.=Sp++"  " &&
	(Lp,OPr,Rp) ?= isInfixOp("in") =>
      "let {.\n#(Sp2)#(showGroup(Defs,Sp2))\n#(Sp).} in #(showAct(B,Rp,Sp2))"
    | .doExp(_,E) => "do #(showCanon(E,Pr,Sp))"
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
    | .funDef(_,Nm,Rls,_,Tp) => "Fun: #(showRls(Nm,Rls,showCanon,Sp))"
    | .varDef(_,Nm,LongNm,V,_,Tp) => "Var: #(LongNm)[#(Nm)]\:$(Tp) = #(showCanon(V,0,Sp))"
    | .typeDef(_,Nm,_,Rl) => "Type: $(Rl)"
    | .cnsDef(_,Nm,Ix,Tp) => "Constructor: #(Nm)[$(Ix)]\:$(Tp)"
    | .implDef(_,_,Nm,Exp,Cx,Tp) => "Implementation: #(Nm)\:$(Tp) = $(Cx) |: $(Exp)"
  }

  showRls:all x ~~ (string,cons[rule[x]],(x,integer,string)=>string,string) => string.
  showRls(Nm,Rls,Shw,Sp) => interleave(Rls//(Rl)=>showRl(Nm,Rl,Shw,Sp),"\n"++Sp++"| ")*.

  showRl:all x ~~ (string,rule[x],(x,integer,string)=>string,string) => string.
  showRl(Nm,.rule(_,Ptn,.none,Val),Shw,Sp) where (Lp,OPr,Rp) ?= isInfixOp("=>") =>
    "#(Nm)#(showCanon(Ptn,Lp,Sp)) => #(Shw(Val,Rp,Sp))".
  showRl(Nm,.rule(_,Ptn,.some(C),Val),Shw,Sp) where (Lp,OPr,Rp) ?= isInfixOp("=>") =>
    "#(Nm)#(showCanon(Ptn,Lp,Sp)) where #(showCanon(C,Lp,Sp)) => #(Shw(Val,Rp,Sp))".

  showDecs:(cons[decl],string) => string.
  showDecs(Dcs,Sp) => interleave(Dcs//disp,"\n"++Sp)*.

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
   | .enm(_,"star.core#true",.nomnal("star.core*boolean")) => .true
   | .enm(_,"star.core#false",.nomnal("star.core*boolean")) => .true
   | .match(_,_,_) => .true
   | .conj(_,_,_) => .true
   | .disj(_,_,_) => .true
   | .neg(_,_) => .true
   | .cond(_,_,L,R) => isGoal(L) && isGoal(R)
   | _ default => .false
  }

  public isFunDef:(canon)=>boolean.
  isFunDef(Df) => case Df in {
   | .lambda(_,_,_,_) => .true
   | .letExp(_,_,_,Exp) => isFunDef(Exp)
   | .letRec(_,_,_,Exp) => isFunDef(Exp)
   | _ default => .false
  }
}
