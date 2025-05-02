star.compiler.assem{
  -- Automatically Generated Assembler -- Do NOT Edit
  import star.
  import star.multi.
  import star.sort.

  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.location.
  import star.compiler.data.
  import star.compiler.meta.
  import star.compiler.types.
  import star.compiler.types.encode.
  import star.compiler.ltipe.

  public codeSegment ::= .func(termLbl,codePolicy,ltipe,cons[(string,data)],multi[assemOp]) |
    .struct(termLbl,tipe,integer) |
    .tipe(tipe,typeRule,cons[(termLbl,tipe,integer)]).

  public assemOp ::=
    | .iHalt(integer)
    | .iNop
    | .iAbort
    | .iCall(termLbl)
    | .iOCall(integer)
    | .iEscape(string)
    | .iXCall(termLbl,assemLbl)
    | .iXOCall(integer,assemLbl)
    | .iXEscape(string,assemLbl)
    | .iTCall(termLbl)
    | .iTOCall(integer)
    | .iEntry(integer)
    | .iRet
    | .iXRet
    | .iBlock(integer,multi[assemOp])
    | .iBreak(assemLbl)
    | .iResult(integer,assemLbl)
    | .iLoop(assemLbl)
    | .iDrop
    | .iDup
    | .iRot(integer)
    | .iRst(integer)
    | .iPick(integer,integer)
    | .iFiber
    | .iSuspend
    | .iResume
    | .iRetire
    | .iUnderflow
    | .iLdV
    | .iLdC(data)
    | .iLdA(integer)
    | .iLdL(string)
    | .iStL(string)
    | .iStV(string)
    | .iTL(string)
    | .iLdG(string)
    | .iStG(string)
    | .iTG(string)
    | .iSav
    | .iLdSav(assemLbl)
    | .iTstSav
    | .iStSav
    | .iTSav
    | .iCell
    | .iGet
    | .iAssign
    | .iCLbl(termLbl,assemLbl)
    | .iCLit(data,assemLbl)
    | .iNth(integer)
    | .iStNth(integer)
    | .iIf(assemLbl)
    | .iIfNot(assemLbl)
    | .iCase(integer)
    | .iIndxJmp(integer)
    | .iIAdd
    | .iISub
    | .iIMul
    | .iIDiv(assemLbl)
    | .iIMod
    | .iIAbs
    | .iIEq
    | .iILt
    | .iIGe
    | .iICmp(assemLbl)
    | .iCEq
    | .iCLt
    | .iCGe
    | .iCCmp(assemLbl)
    | .iBAnd
    | .iBOr
    | .iBXor
    | .iBLsl
    | .iBLsr
    | .iBAsr
    | .iBNot
    | .iFAdd
    | .iFSub
    | .iFMul
    | .iFDiv
    | .iFMod
    | .iFAbs
    | .iFEq
    | .iFLt
    | .iFGe
    | .iFCmp(assemLbl)
    | .iAlloc(termLbl)
    | .iClosure(termLbl)
    | .iCmp(assemLbl)
    | .iFrame(ltipe)
    | .iDBug

    | .iLbl(string, assemOp)
    | .iLine(data).

  public assemLbl ~> string.

  public assem:(codeSegment) => data.
  assem(Df) => case Df in {
    | .func(Nm,H,Sig,Lcs,Ins) => valof{
      funSig = .strg(Sig::string);
      (Lt0,_) = findLit([],.symb(Nm));
      (Lt1,tpIx) = findLit(Lt0,funSig);
      (Code,_,Lts,Lns) = assemBlock(Ins,[],0,[],Lt1,declareLocals(Lcs),[]);
      valis mkCons("func",
          [.symb(Nm),encPolicy(H),.intgr(tpIx),.intgr(stackHwm(Ins)),
          .intgr(size(Lcs)),litTbl(Lts),mkTpl(Code::cons[data]),
           mkTpl(Lcs//(((Vnm,Spec))=>mkTpl([.strg(Vnm),Spec]))),mkTpl(Lns::cons[data])])
    }
    | .struct(Lbl,Tp,Ix) =>
      mkCons("struct",[.symb(Lbl),.strg(encodeSignature(Tp)),.intgr(Ix)])
    | .tipe(Tp,TpRl,Map) =>
      mkCons("type",[.strg(tpName(Tp)),.strg(encodeTpRlSignature(TpRl)),encodeMap(Map)])
  }.

  declareLocals:(cons[(string,data)]) => map[string,integer].
  declareLocals(Lcs) => let{.
    decl([],_) => {}.
    decl([(Vr,_),..Ls],Lc) => [Vr -> Lc,..decl(Ls,Lc+1)].
  .} in decl(Lcs,1).             -- First local is #1

  encodeMap(Entries) => mkTpl(Entries//((Lbl,_,Ix))=>mkTpl([.symb(Lbl),.intgr(Ix)])).

  encPolicy(.hardDefinition) => mkTpl([]).
  encPolicy(.softDefinition) => mkTpl([.strg("soft")]).

  lblLevel ~> option[assemLbl].

  private assemBlock:(multi[assemOp],multi[data],integer,
                      cons[lblLevel],map[data,integer],map[string,integer],set[data]) =>
                                        (multi[data],integer,map[data,integer],set[data]).
  assemBlock([],Code,Pc,Lbls,Lts,_Lcx,Lns) => (Code,Pc,Lts,Lns).
  assemBlock([I,..Ins],SoFar,Pc,Lbs,Lts,Lcx,Lns) => valof{
    (Code,Pc1,Lt0,Ln0) = mnem(I,Pc,Lbs,Lts,Lcx,Lns);
    valis assemBlock(Ins,SoFar++Code,Pc1,Lbs,Lt0,Lcx,Ln0)
  }

  private mnem:(assemOp,integer,cons[lblLevel],map[data,integer],map[string,integer],set[data]) =>
    (multi[data],integer,map[data,integer],set[data]).
  mnem(.iLbl(Lb,I),Pc,Lbls,Lts,Lcs,Lns) => mnem(I,Pc,[.some(Lb),..Lbls],Lts,Lcs,Lns).
  mnem(.iLine(Lne),Pc,Lbls,Lts,Lcs,Lns) => ([],Pc,Lts,Lns\+mkTpl([.intgr(Pc),Lne])).
  mnem(.iHalt(U),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(0),.intgr(U)],Pc+1,Lts,Lns).
  mnem(.iNop,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(1)],Pc+1,Lts,Lns).
  mnem(.iAbort,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(2)],Pc+1,Lts,Lns).
  mnem(.iCall(U),Pc,Lbls,Lts,Lcs,Lns) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) => ([.intgr(3),.intgr(LtNo)],Pc+1,Lt1,Lns).
  mnem(.iOCall(U),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(4),.intgr(U)],Pc+1,Lts,Lns).
  mnem(.iEscape(U),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(5),.strg(U)],Pc+1,Lts,Lns).
  mnem(.iXCall(U,V),Pc,Lbls,Lts,Lcs,Lns) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) && Tgt ?= findLevel(Lbls,V) => ([.intgr(6),.intgr(LtNo),.intgr(Tgt)],Pc+1,Lt1,Lns).
  mnem(.iXOCall(U,V),Pc,Lbls,Lts,Lcs,Lns) where Lvl ?= findLevel(Lbls,V) =>  ([.intgr(7),.intgr(U),.intgr(Lvl)],Pc+1,Lts,Lns).
  mnem(.iXEscape(U,V),Pc,Lbls,Lts,Lcs,Lns) where Lvl ?= findLevel(Lbls,V) => ([.intgr(8),.strg(U),.intgr(Lvl)],Pc+1,Lts,Lns).
  mnem(.iTCall(U),Pc,Lbls,Lts,Lcs,Lns) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) => ([.intgr(9),.intgr(LtNo)],Pc+1,Lt1,Lns).
  mnem(.iTOCall(U),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(10),.intgr(U)],Pc+1,Lts,Lns).
  mnem(.iEntry(U),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(11),.intgr(U)],Pc+1,Lts,Lns).
  mnem(.iRet,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(12)],Pc+1,Lts,Lns).
  mnem(.iXRet,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(13)],Pc+1,Lts,Lns).
  mnem(.iBlock(U,V),Pc,Lbls,Lts,Lcs,Lns) where (Blk,Pc1,Lts1,Lns1) .= assemBlock(V,[],Pc+1,[.none,..Lbls],Lt1,Lcs,Lns) =>
    ([.intgr(14),.intgr(U),mkTpl(Blk::cons[data])],Pc1,Lts1,Lns1).
  mnem(.iBreak(V),Pc,Lbls,Lts,Lcs,Lns) where Tgt ?= findLevel(Lbls,V) => ([.intgr(15),.intgr(Tgt)],Pc+1,Lts,Lns).
  mnem(.iResult(U,V),Pc,Lbls,Lts,Lcs,Lns) where Lvl ?= findLevel(Lbls,V) =>  ([.intgr(16),.intgr(U),.intgr(Lvl)],Pc+1,Lts,Lns).
  mnem(.iLoop(V),Pc,Lbls,Lts,Lcs,Lns) where Tgt ?= findLevel(Lbls,V) => ([.intgr(17),.intgr(Tgt)],Pc+1,Lts,Lns).
  mnem(.iDrop,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(18)],Pc+1,Lts,Lns).
  mnem(.iDup,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(19)],Pc+1,Lts,Lns).
  mnem(.iRot(U),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(20),.intgr(U)],Pc+1,Lts,Lns).
  mnem(.iRst(U),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(21),.intgr(U)],Pc+1,Lts,Lns).
  mnem(.iPick(U,V),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(22),.intgr(U),.intgr(V)],Pc+1,Lts,Lns).
  mnem(.iFiber,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(23)],Pc+1,Lts,Lns).
  mnem(.iSuspend,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(24)],Pc+1,Lts,Lns).
  mnem(.iResume,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(25)],Pc+1,Lts,Lns).
  mnem(.iRetire,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(26)],Pc+1,Lts,Lns).
  mnem(.iUnderflow,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(27)],Pc+1,Lts,Lns).
  mnem(.iLdV,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(28)],Pc+1,Lts,Lns).
  mnem(.iLdC(U),Pc,Lbls,Lts,Lcs,Lns) where (Lt1,LtNo) .= findLit(Lts,U) => ([.intgr(29),.intgr(LtNo)],Pc+1,Lt1,Lns).
  mnem(.iLdA(U),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(30),.intgr(U)],Pc+1,Lts,Lns).
  mnem(.iLdL(U),Pc,Lbls,Lts,Lcs,Lns) where Off ?= findLocal(U,Lcs) => ([.intgr(31),.intgr(Off)],Pc+1,Lts,Lns).
  mnem(.iStL(U),Pc,Lbls,Lts,Lcs,Lns) where Off ?= findLocal(U,Lcs) => ([.intgr(32),.intgr(Off)],Pc+1,Lts,Lns).
  mnem(.iStV(U),Pc,Lbls,Lts,Lcs,Lns) where Off ?= findLocal(U,Lcs) => ([.intgr(33),.intgr(Off)],Pc+1,Lts,Lns).
  mnem(.iTL(U),Pc,Lbls,Lts,Lcs,Lns) where Off ?= findLocal(U,Lcs) => ([.intgr(34),.intgr(Off)],Pc+1,Lts,Lns).
  mnem(.iLdG(U),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(35),.strg(U)],Pc+1,Lts,Lns).
  mnem(.iStG(U),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(36),.strg(U)],Pc+1,Lts,Lns).
  mnem(.iTG(U),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(37),.strg(U)],Pc+1,Lts,Lns).
  mnem(.iSav,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(38)],Pc+1,Lts,Lns).
  mnem(.iLdSav(V),Pc,Lbls,Lts,Lcs,Lns) where Tgt ?= findLevel(Lbls,V) => ([.intgr(39),.intgr(Tgt)],Pc+1,Lts,Lns).
  mnem(.iTstSav,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(40)],Pc+1,Lts,Lns).
  mnem(.iStSav,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(41)],Pc+1,Lts,Lns).
  mnem(.iTSav,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(42)],Pc+1,Lts,Lns).
  mnem(.iCell,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(43)],Pc+1,Lts,Lns).
  mnem(.iGet,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(44)],Pc+1,Lts,Lns).
  mnem(.iAssign,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(45)],Pc+1,Lts,Lns).
  mnem(.iCLbl(U,V),Pc,Lbls,Lts,Lcs,Lns) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) && Tgt ?= findLevel(Lbls,V) => ([.intgr(46),.intgr(LtNo),.intgr(Tgt)],Pc+1,Lt1,Lns).
  mnem(.iCLit(U,V),Pc,Lbls,Lts,Lcs,Lns) where (Lt1,LtNo) .= findLit(Lts,U) && Tgt ?= findLevel(Lbls,V) => ([.intgr(47),.intgr(LtNo),.intgr(Tgt)],Pc+1,Lt1,Lns).
  mnem(.iNth(U),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(48),.intgr(U)],Pc+1,Lts,Lns).
  mnem(.iStNth(U),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(49),.intgr(U)],Pc+1,Lts,Lns).
  mnem(.iIf(V),Pc,Lbls,Lts,Lcs,Lns) where Tgt ?= findLevel(Lbls,V) => ([.intgr(50),.intgr(Tgt)],Pc+1,Lts,Lns).
  mnem(.iIfNot(V),Pc,Lbls,Lts,Lcs,Lns) where Tgt ?= findLevel(Lbls,V) => ([.intgr(51),.intgr(Tgt)],Pc+1,Lts,Lns).
  mnem(.iCase(U),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(52),.intgr(U)],Pc+1,Lts,Lns).
  mnem(.iIndxJmp(U),Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(53),.intgr(U)],Pc+1,Lts,Lns).
  mnem(.iIAdd,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(54)],Pc+1,Lts,Lns).
  mnem(.iISub,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(55)],Pc+1,Lts,Lns).
  mnem(.iIMul,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(56)],Pc+1,Lts,Lns).
  mnem(.iIDiv(V),Pc,Lbls,Lts,Lcs,Lns) where Tgt ?= findLevel(Lbls,V) => ([.intgr(57),.intgr(Tgt)],Pc+1,Lts,Lns).
  mnem(.iIMod,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(58)],Pc+1,Lts,Lns).
  mnem(.iIAbs,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(59)],Pc+1,Lts,Lns).
  mnem(.iIEq,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(60)],Pc+1,Lts,Lns).
  mnem(.iILt,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(61)],Pc+1,Lts,Lns).
  mnem(.iIGe,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(62)],Pc+1,Lts,Lns).
  mnem(.iICmp(V),Pc,Lbls,Lts,Lcs,Lns) where Tgt ?= findLevel(Lbls,V) => ([.intgr(63),.intgr(Tgt)],Pc+1,Lts,Lns).
  mnem(.iCEq,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(64)],Pc+1,Lts,Lns).
  mnem(.iCLt,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(65)],Pc+1,Lts,Lns).
  mnem(.iCGe,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(66)],Pc+1,Lts,Lns).
  mnem(.iCCmp(V),Pc,Lbls,Lts,Lcs,Lns) where Tgt ?= findLevel(Lbls,V) => ([.intgr(67),.intgr(Tgt)],Pc+1,Lts,Lns).
  mnem(.iBAnd,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(68)],Pc+1,Lts,Lns).
  mnem(.iBOr,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(69)],Pc+1,Lts,Lns).
  mnem(.iBXor,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(70)],Pc+1,Lts,Lns).
  mnem(.iBLsl,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(71)],Pc+1,Lts,Lns).
  mnem(.iBLsr,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(72)],Pc+1,Lts,Lns).
  mnem(.iBAsr,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(73)],Pc+1,Lts,Lns).
  mnem(.iBNot,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(74)],Pc+1,Lts,Lns).
  mnem(.iFAdd,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(75)],Pc+1,Lts,Lns).
  mnem(.iFSub,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(76)],Pc+1,Lts,Lns).
  mnem(.iFMul,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(77)],Pc+1,Lts,Lns).
  mnem(.iFDiv,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(78)],Pc+1,Lts,Lns).
  mnem(.iFMod,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(79)],Pc+1,Lts,Lns).
  mnem(.iFAbs,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(80)],Pc+1,Lts,Lns).
  mnem(.iFEq,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(81)],Pc+1,Lts,Lns).
  mnem(.iFLt,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(82)],Pc+1,Lts,Lns).
  mnem(.iFGe,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(83)],Pc+1,Lts,Lns).
  mnem(.iFCmp(V),Pc,Lbls,Lts,Lcs,Lns) where Tgt ?= findLevel(Lbls,V) => ([.intgr(84),.intgr(Tgt)],Pc+1,Lts,Lns).
  mnem(.iAlloc(U),Pc,Lbls,Lts,Lcs,Lns) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) => ([.intgr(85),.intgr(LtNo)],Pc+1,Lt1,Lns).
  mnem(.iClosure(U),Pc,Lbls,Lts,Lcs,Lns) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) => ([.intgr(86),.intgr(LtNo)],Pc+1,Lt1,Lns).
  mnem(.iCmp(V),Pc,Lbls,Lts,Lcs,Lns) where Tgt ?= findLevel(Lbls,V) => ([.intgr(87),.intgr(Tgt)],Pc+1,Lts,Lns).
  mnem(.iFrame(U),Pc,Lbls,Lts,Lcs,Lns) where (Lt1,LtNo) .= findLit(Lts,.strg(U::string)) => ([.intgr(88),.intgr(LtNo)],Pc+1,Lt1,Lns).
  mnem(.iDBug,Pc,Lbls,Lts,Lcs,Lns) => ([.intgr(89)],Pc+1,Lts,Lns).

  mnem(I,Pc,Lbls,Lts,Lcs,Lns) => valof{
    reportTrap("Cannot assemble instruction $(I)");
    valis ([],Pc,Lts,Lns)
  }.

  private stackHwm:(multi[assemOp]) => integer.
  stackHwm(Code) where (_,HWM) .= stkHwm(Code,0,0) => HWM.

  private stkHwm:(multi[assemOp],integer,integer) => (integer,integer).
  stkHwm([],H,HWM) => (H,HWM).
  stkHwm([.iLbl(_,I),..Ins],CH,Hwm) => valof{
    (CH1,H1) = stkHwm([I],CH,Hwm);
    valis stkHwm(Ins,CH1,H1)
  }
  stkHwm([.iLine(_),..Ins],CH,Hwm) => stkHwm(Ins,CH,Hwm).
  stkHwm([.iHalt(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iNop,..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iAbort,..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iCall(_),..Ins],CH0,H0) => valof{
    CH1 = CH0+1;
    valis stkHwm(Ins,CH1,(CH1>H0??CH1||H0))
  }
  stkHwm([.iOCall(_),..Ins],CH0,H0) => valof{
    CH1 = CH0+1;
    valis stkHwm(Ins,CH1,(CH1>H0??CH1||H0))
  }
  stkHwm([.iEscape(_),..Ins],CH0,H0) => valof{
    CH1 = CH0+1;
    valis stkHwm(Ins,CH1,(CH1>H0??CH1||H0))
  }
  stkHwm([.iXCall(_,_),..Ins],CH0,H0) => valof{
    CH1 = CH0+1;
    valis stkHwm(Ins,CH1,(CH1>H0??CH1||H0))
  }
  stkHwm([.iXOCall(_,_),..Ins],CH0,H0) => valof{
    CH1 = CH0+1;
    valis stkHwm(Ins,CH1,(CH1>H0??CH1||H0))
  }
  stkHwm([.iXEscape(_,_),..Ins],CH0,H0) => valof{
    CH1 = CH0+1;
    valis stkHwm(Ins,CH1,(CH1>H0??CH1||H0))
  }
  stkHwm([.iTCall(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iTOCall(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iEntry(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iRet,..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iXRet,..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iBlock(_,W),..Ins],CH0,H0) => valof{
  (CH1,H1) = stkHwm(W,CH0,H0);
    valis stkHwm(Ins,CH1,(CH1>H1??CH1||H1))
  }
  stkHwm([.iBreak(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iResult(_,_),..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iLoop(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iDrop,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iDup,..Ins],CH0,H0) => valof{
    CH1 = CH0+1;
    valis stkHwm(Ins,CH1,(CH1>H0??CH1||H0))
  }
  stkHwm([.iRot(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iRst(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iPick(_,_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iFiber,..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iSuspend,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iResume,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iRetire,..Ins],CH0,H0) => valof{
    CH1 = CH0-2;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iUnderflow,..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iLdV,..Ins],CH0,H0) => valof{
    CH1 = CH0+1;
    valis stkHwm(Ins,CH1,(CH1>H0??CH1||H0))
  }
  stkHwm([.iLdC(_),..Ins],CH0,H0) => valof{
    CH1 = CH0+1;
    valis stkHwm(Ins,CH1,(CH1>H0??CH1||H0))
  }
  stkHwm([.iLdA(_),..Ins],CH0,H0) => valof{
    CH1 = CH0+1;
    valis stkHwm(Ins,CH1,(CH1>H0??CH1||H0))
  }
  stkHwm([.iLdL(_),..Ins],CH0,H0) => valof{
    CH1 = CH0+1;
    valis stkHwm(Ins,CH1,(CH1>H0??CH1||H0))
  }
  stkHwm([.iStL(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iStV(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iTL(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iLdG(_),..Ins],CH0,H0) => valof{
    CH1 = CH0+1;
    valis stkHwm(Ins,CH1,(CH1>H0??CH1||H0))
  }
  stkHwm([.iStG(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iTG(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iSav,..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iLdSav(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iTstSav,..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iStSav,..Ins],CH0,H0) => valof{
    CH1 = CH0-2;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iTSav,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iCell,..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iGet,..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iAssign,..Ins],CH0,H0) => valof{
    CH1 = CH0-2;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iCLbl(_,_),..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iCLit(_,_),..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iNth(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iStNth(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-2;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iIf(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iIfNot(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iCase(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iIndxJmp(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iIAdd,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iISub,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iIMul,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iIDiv(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-2;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iIMod,..Ins],CH0,H0) => valof{
    CH1 = CH0-2;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iIAbs,..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iIEq,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iILt,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iIGe,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iICmp(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-2;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iCEq,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iCLt,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iCGe,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iCCmp(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-2;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iBAnd,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iBOr,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iBXor,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iBLsl,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iBLsr,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iBAsr,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iBNot,..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iFAdd,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iFSub,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iFMul,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iFDiv,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iFMod,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iFAbs,..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iFEq,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iFLt,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iFGe,..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iFCmp(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-2;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iAlloc(_),..Ins],CH0,H0) => valof{
    CH1 = CH0+1;
    valis stkHwm(Ins,CH1,(CH1>H0??CH1||H0))
  }
  stkHwm([.iClosure(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iCmp(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-2;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iFrame(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iDBug,..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }


  findLit:(map[data,integer],data) => (map[data,integer],integer).
  findLit(Lts,T) where O ?= Lts[T] => (Lts,O).
  findLit(Lts,T) where O .= size(Lts) => (Lts[T->O],O).

  findLocal:(string,map[string,integer])=>option[integer].
  findLocal(Nm,Lcs) => Lcs[Nm].

  findLevel:(cons[lblLevel],assemLbl) => option[integer].
  findLevel(Lbs,Lb) => let{.
    findLvl([],_) => .none.
    findLvl([.some(LL),..Ls], Lvl) => (LL==Lb ?? .some(Lvl) || findLvl(Ls,Lvl)).
    findLvl([.none,..Ls],Lvl) => findLvl(Ls,Lvl+1).
  .} in findLvl(Lbs,0).

  litTbl:(map[data,integer]) => data.
  litTbl(Lts) => mkTpl(sort(Lts::cons[(data,integer)],((T1,Ix1), (T2,Ix2)) => Ix1<Ix2)//fst).

  public implementation display[codeSegment] => {
    disp(.func(Nm,_,Tp,_,Ins)) => "fun $(Nm)\:$(Tp)\n"++showMnem(Ins).
    disp(.struct(Lbl,Tp,Ix)) => "struct $(Lbl)\:$(Tp) @ $(Ix)".
    disp(.tipe(_Tp,TpRl,Map)) => "type $(TpRl), map = $(Map)".
  }

  public implementation display[assemOp] => {
    disp(Op) => showIns(Op,[]).
  }

  showMnem:(multi[assemOp]) => string.
  showMnem(Ops) => showBlock(Ops,[0]).

  showBlock:(multi[assemOp],cons[integer]) => string.
  showBlock(Ins,Pc) => interleave(showCode(Ins,[0,..Pc]),"\n")*.

  showCode:(multi[assemOp],cons[integer]) => cons[string].
  showCode([],_) => [].
  showCode([Ins,..Cde],Pc) => ["#(showPc(Pc))\: #(showIns(Ins,Pc))",..showCode(Cde,bumpPc(Pc))].

  showIns:(assemOp,cons[integer]) => string.
  showIns(.iLbl(Lb,I),Pc) => "#(Lb):  #(showIns(I,Pc))".
  showIns(.iLine(D),Pc) => "Line $(D)".
  showIns(.iHalt(U),Pc) => "Halt $(U)".
  showIns(.iNop,Pc) => "Nop".
  showIns(.iAbort,Pc) => "Abort".
  showIns(.iCall(U),Pc) => "Call $(U)".
  showIns(.iOCall(U),Pc) => "OCall $(U)".
  showIns(.iEscape(U),Pc) => "Escape $(U)".
  showIns(.iXCall(U,V),Pc) => "XCall $(U) #(V)".
  showIns(.iXOCall(U,V),Pc) => "XOCall $(U) #(V)".
  showIns(.iXEscape(U,V),Pc) => "XEscape $(U) #(V)".
  showIns(.iTCall(U),Pc) => "TCall $(U)".
  showIns(.iTOCall(U),Pc) => "TOCall $(U)".
  showIns(.iEntry(U),Pc) => "Entry $(U)".
  showIns(.iRet,Pc) => "Ret".
  showIns(.iXRet,Pc) => "XRet".
  showIns(.iBlock(U,V),Pc) => "Block $(U)\n#(showBlock(V,Pc))".
  showIns(.iBreak(V),Pc) => "Break #(V)".
  showIns(.iResult(U,V),Pc) => "Result $(U) #(V)".
  showIns(.iLoop(V),Pc) => "Loop #(V)".
  showIns(.iDrop,Pc) => "Drop".
  showIns(.iDup,Pc) => "Dup".
  showIns(.iRot(U),Pc) => "Rot $(U)".
  showIns(.iRst(U),Pc) => "Rst $(U)".
  showIns(.iPick(U,V),Pc) => "Pick $(U) $(V)".
  showIns(.iFiber,Pc) => "Fiber".
  showIns(.iSuspend,Pc) => "Suspend".
  showIns(.iResume,Pc) => "Resume".
  showIns(.iRetire,Pc) => "Retire".
  showIns(.iUnderflow,Pc) => "Underflow".
  showIns(.iLdV,Pc) => "LdV".
  showIns(.iLdC(U),Pc) => "LdC $(U)".
  showIns(.iLdA(U),Pc) => "LdA $(U)".
  showIns(.iLdL(U),Pc) => "LdL #(U)".
  showIns(.iStL(U),Pc) => "StL #(U)".
  showIns(.iStV(U),Pc) => "StV #(U)".
  showIns(.iTL(U),Pc) => "TL #(U)".
  showIns(.iLdG(U),Pc) => "LdG $(U)".
  showIns(.iStG(U),Pc) => "StG $(U)".
  showIns(.iTG(U),Pc) => "TG $(U)".
  showIns(.iSav,Pc) => "Sav".
  showIns(.iLdSav(V),Pc) => "LdSav #(V)".
  showIns(.iTstSav,Pc) => "TstSav".
  showIns(.iStSav,Pc) => "StSav".
  showIns(.iTSav,Pc) => "TSav".
  showIns(.iCell,Pc) => "Cell".
  showIns(.iGet,Pc) => "Get".
  showIns(.iAssign,Pc) => "Assign".
  showIns(.iCLbl(U,V),Pc) => "CLbl $(U) #(V)".
  showIns(.iCLit(U,V),Pc) => "CLit $(U) #(V)".
  showIns(.iNth(U),Pc) => "Nth $(U)".
  showIns(.iStNth(U),Pc) => "StNth $(U)".
  showIns(.iIf(V),Pc) => "If #(V)".
  showIns(.iIfNot(V),Pc) => "IfNot #(V)".
  showIns(.iCase(U),Pc) => "Case $(U)".
  showIns(.iIndxJmp(U),Pc) => "IndxJmp $(U)".
  showIns(.iIAdd,Pc) => "IAdd".
  showIns(.iISub,Pc) => "ISub".
  showIns(.iIMul,Pc) => "IMul".
  showIns(.iIDiv(V),Pc) => "IDiv #(V)".
  showIns(.iIMod,Pc) => "IMod".
  showIns(.iIAbs,Pc) => "IAbs".
  showIns(.iIEq,Pc) => "IEq".
  showIns(.iILt,Pc) => "ILt".
  showIns(.iIGe,Pc) => "IGe".
  showIns(.iICmp(V),Pc) => "ICmp #(V)".
  showIns(.iCEq,Pc) => "CEq".
  showIns(.iCLt,Pc) => "CLt".
  showIns(.iCGe,Pc) => "CGe".
  showIns(.iCCmp(V),Pc) => "CCmp #(V)".
  showIns(.iBAnd,Pc) => "BAnd".
  showIns(.iBOr,Pc) => "BOr".
  showIns(.iBXor,Pc) => "BXor".
  showIns(.iBLsl,Pc) => "BLsl".
  showIns(.iBLsr,Pc) => "BLsr".
  showIns(.iBAsr,Pc) => "BAsr".
  showIns(.iBNot,Pc) => "BNot".
  showIns(.iFAdd,Pc) => "FAdd".
  showIns(.iFSub,Pc) => "FSub".
  showIns(.iFMul,Pc) => "FMul".
  showIns(.iFDiv,Pc) => "FDiv".
  showIns(.iFMod,Pc) => "FMod".
  showIns(.iFAbs,Pc) => "FAbs".
  showIns(.iFEq,Pc) => "FEq".
  showIns(.iFLt,Pc) => "FLt".
  showIns(.iFGe,Pc) => "FGe".
  showIns(.iFCmp(V),Pc) => "FCmp #(V)".
  showIns(.iAlloc(U),Pc) => "Alloc $(U)".
  showIns(.iClosure(U),Pc) => "Closure $(U)".
  showIns(.iCmp(V),Pc) => "Cmp #(V)".
  showIns(.iFrame(U),Pc) => "Frame $(U)".
  showIns(.iDBug,Pc) => "dBug".


  showPc:(cons[integer]) => string.
  showPc(Pcs) => "#(spaces(size(Pcs)))#(interleave(Pcs//disp,".")*)".

  spaces:(integer)=>string.
  spaces(Ln) => let{.
    sp(0) => [].
    sp(N) => [` `,..sp(N-1)].
  .} in _implode(sp(Ln)).

  bumpPc:(cons[integer]) => cons[integer].
  bumpPc([Pc,..Rest]) => [Pc+1,..Rest].

  public opcodeHash = 2141160724337298435.
}
