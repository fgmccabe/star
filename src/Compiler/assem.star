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
  import star.compiler.encode.
  import star.compiler.ltipe.

  public codeSegment ::= .func(termLbl,codePolicy,ltipe,cons[(string,data)],multi[assemOp]) |
    .struct(termLbl,tipe,integer) |
    .tipe(tipe,typeRule,map[termLbl,integer]).

  public assemOp ::=
    | .iHalt(integer)
    | .iNop
    | .iAbort(data)
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
    | .iResult(assemLbl)
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
    | .iCInt(data,assemLbl)
    | .iCChar(data,assemLbl)
    | .iCFlt(data,assemLbl)
    | .iCLit(data,assemLbl)
    | .iNth(integer)
    | .iStNth(integer)
    | .iIf(assemLbl)
    | .iIfNot(assemLbl)
    | .iICase(integer)
    | .iCase(integer)
    | .iIxCase(integer)
    | .iIAdd
    | .iISub
    | .iIMul
    | .iIDiv(assemLbl)
    | .iIMod(assemLbl)
    | .iIAbs
    | .iIEq
    | .iILt
    | .iIGe
    | .iCEq
    | .iCLt
    | .iCGe
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
    | .iFDiv(assemLbl)
    | .iFMod(assemLbl)
    | .iFAbs
    | .iFEq
    | .iFLt
    | .iFGe
    | .iAlloc(termLbl)
    | .iClosure(termLbl)
    | .iFrame(integer)
    | .iLine(data)
    | .iDBug(data)

    | .iLbl(string, assemOp).

  public assemLbl ~> string.

  public assem:(codeSegment) => data.
  assem(Df) => case Df in {
    | .func(Nm,H,Sig,Lcs,Ins) => valof{
      funSig = .strg(Sig::string);
      (Lt0,_) = findLit([],.symb(Nm));
      (Lt1,tpIx) = findLit(Lt0,funSig);
      (Code,_,Lts) = assemBlock(Ins,[],0,[],Lt1,declareLocals(Lcs));
      valis mkCons("func",
          [.symb(Nm),encPolicy(H),.intgr(tpIx),.intgr(stackHwm(Ins)),
          .intgr(size(Lcs)),litTbl(Lts),mkTpl(Code::cons[data]),
           mkTpl(Lcs//(((Vnm,Spec))=>mkTpl([.strg(Vnm),Spec])))])
    }
    | .struct(Lbl,Tp,Ix) => mkCons("struct",[.symb(Lbl),.strg(encodeSignature(Tp)),.intgr(Ix)])
    | .tipe(Tp,TpRl,Map) => mkCons("type",[.strg(tpName(Tp)),.strg(encodeTpRlSignature(TpRl)),encodeMap(Map)])
  }.

  declareLocals:(cons[(string,data)]) => map[string,integer].
  declareLocals(Lcs) => let{.
    decl([],_) => {}.
    decl([(Vr,_),..Ls],Lc) => [Vr -> Lc,..decl(Ls,Lc+1)].
  .} in decl(Lcs,1).             -- First local is #1

  encodeMap(Entries) => mkTpl(ixRight((Lbl,Ix,Lst)=>[mkTpl([.symb(Lbl),.intgr(Ix)]),..Lst],[],Entries)).

  encPolicy(.hardDefinition) => mkTpl([]).
  encPolicy(.softDefinition) => mkTpl([.strg("soft")]).

  lblLevel ~> option[assemLbl].

  private assemBlock:(multi[assemOp],multi[data],integer,
                      cons[lblLevel],map[data,integer],map[string,integer]) =>
                                        (multi[data],integer,map[data,integer]).
  assemBlock([],Code,Pc,Lbls,Lts,_Lcx) => (Code,Pc,Lts).
  assemBlock([I,..Ins],SoFar,Pc,Lbs,Lts,Lcx) => valof{
    (Code,Pc1,Lt0) = mnem(I,Pc,Lbs,Lts,Lcx);
    valis assemBlock(Ins,SoFar++Code,Pc1,Lbs,Lt0,Lcx)
  }

  private mnem:(assemOp,integer,cons[lblLevel],map[data,integer],map[string,integer]) =>
    (multi[data],integer,map[data,integer]).
  mnem(.iLbl(Lb,I),Pc,Lbls,Lts,Lcs) => mnem(I,Pc,[.some(Lb),..Lbls],Lts,Lcs).
  mnem(.iHalt(U),Pc,Lbls,Lts,Lcs) => ([.intgr(0),.intgr(U)],Pc+1,Lts).
  mnem(.iNop,Pc,Lbls,Lts,Lcs) => ([.intgr(1)],Pc+1,Lts).
  mnem(.iAbort(U),Pc,Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,U) => ([.intgr(2),.intgr(LtNo)],Pc+1,Lt1).
  mnem(.iCall(U),Pc,Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) => ([.intgr(3),.intgr(LtNo)],Pc+1,Lt1).
  mnem(.iOCall(U),Pc,Lbls,Lts,Lcs) => ([.intgr(4),.intgr(U)],Pc+1,Lts).
  mnem(.iEscape(U),Pc,Lbls,Lts,Lcs) => ([.intgr(5),.strg(U)],Pc+1,Lts).
  mnem(.iXCall(U,V),Pc,Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) && Tgt ?= findLevel(Lbls,V) => ([.intgr(6),.intgr(LtNo),.intgr(Tgt)],Pc+1,Lt1).
  mnem(.iXOCall(U,V),Pc,Lbls,Lts,Lcs) where Lvl ?= findLevel(Lbls,V) =>  ([.intgr(7),.intgr(U),.intgr(Lvl)],Pc+1,Lts).
  mnem(.iXEscape(U,V),Pc,Lbls,Lts,Lcs) where Lvl ?= findLevel(Lbls,V) => ([.intgr(8),.strg(U),.intgr(Lvl)],Pc+1,Lts).
  mnem(.iTCall(U),Pc,Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) => ([.intgr(9),.intgr(LtNo)],Pc+1,Lt1).
  mnem(.iTOCall(U),Pc,Lbls,Lts,Lcs) => ([.intgr(10),.intgr(U)],Pc+1,Lts).
  mnem(.iEntry(U),Pc,Lbls,Lts,Lcs) => ([.intgr(11),.intgr(U)],Pc+1,Lts).
  mnem(.iRet,Pc,Lbls,Lts,Lcs) => ([.intgr(12)],Pc+1,Lts).
  mnem(.iXRet,Pc,Lbls,Lts,Lcs) => ([.intgr(13)],Pc+1,Lts).
  mnem(.iBlock(U,V),Pc,Lbls,Lts,Lcs) where (Blk,Pc1,Lts1) .= assemBlock(V,[],Pc+1,[.none,..Lbls],Lts,Lcs) =>
    ([.intgr(14),.intgr(U),mkTpl(Blk::cons[data])],Pc1,Lts1).
  mnem(.iBreak(V),Pc,Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(15),.intgr(Tgt)],Pc+1,Lts).
  mnem(.iResult(V),Pc,Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(16),.intgr(Tgt)],Pc+1,Lts).
  mnem(.iLoop(V),Pc,Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(17),.intgr(Tgt)],Pc+1,Lts).
  mnem(.iDrop,Pc,Lbls,Lts,Lcs) => ([.intgr(18)],Pc+1,Lts).
  mnem(.iDup,Pc,Lbls,Lts,Lcs) => ([.intgr(19)],Pc+1,Lts).
  mnem(.iRot(U),Pc,Lbls,Lts,Lcs) => ([.intgr(20),.intgr(U)],Pc+1,Lts).
  mnem(.iRst(U),Pc,Lbls,Lts,Lcs) => ([.intgr(21),.intgr(U)],Pc+1,Lts).
  mnem(.iPick(U,V),Pc,Lbls,Lts,Lcs) => ([.intgr(22),.intgr(U),.intgr(V)],Pc+1,Lts).
  mnem(.iFiber,Pc,Lbls,Lts,Lcs) => ([.intgr(23)],Pc+1,Lts).
  mnem(.iSuspend,Pc,Lbls,Lts,Lcs) => ([.intgr(24)],Pc+1,Lts).
  mnem(.iResume,Pc,Lbls,Lts,Lcs) => ([.intgr(25)],Pc+1,Lts).
  mnem(.iRetire,Pc,Lbls,Lts,Lcs) => ([.intgr(26)],Pc+1,Lts).
  mnem(.iUnderflow,Pc,Lbls,Lts,Lcs) => ([.intgr(27)],Pc+1,Lts).
  mnem(.iLdV,Pc,Lbls,Lts,Lcs) => ([.intgr(28)],Pc+1,Lts).
  mnem(.iLdC(U),Pc,Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,U) => ([.intgr(29),.intgr(LtNo)],Pc+1,Lt1).
  mnem(.iLdA(U),Pc,Lbls,Lts,Lcs) => ([.intgr(30),.intgr(U)],Pc+1,Lts).
  mnem(.iLdL(U),Pc,Lbls,Lts,Lcs) where Off ?= findLocal(U,Lcs) => ([.intgr(31),.intgr(Off)],Pc+1,Lts).
  mnem(.iStL(U),Pc,Lbls,Lts,Lcs) where Off ?= findLocal(U,Lcs) => ([.intgr(32),.intgr(Off)],Pc+1,Lts).
  mnem(.iStV(U),Pc,Lbls,Lts,Lcs) where Off ?= findLocal(U,Lcs) => ([.intgr(33),.intgr(Off)],Pc+1,Lts).
  mnem(.iTL(U),Pc,Lbls,Lts,Lcs) where Off ?= findLocal(U,Lcs) => ([.intgr(34),.intgr(Off)],Pc+1,Lts).
  mnem(.iLdG(U),Pc,Lbls,Lts,Lcs) => ([.intgr(35),.strg(U)],Pc+1,Lts).
  mnem(.iStG(U),Pc,Lbls,Lts,Lcs) => ([.intgr(36),.strg(U)],Pc+1,Lts).
  mnem(.iTG(U),Pc,Lbls,Lts,Lcs) => ([.intgr(37),.strg(U)],Pc+1,Lts).
  mnem(.iSav,Pc,Lbls,Lts,Lcs) => ([.intgr(38)],Pc+1,Lts).
  mnem(.iLdSav(V),Pc,Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(39),.intgr(Tgt)],Pc+1,Lts).
  mnem(.iTstSav,Pc,Lbls,Lts,Lcs) => ([.intgr(40)],Pc+1,Lts).
  mnem(.iStSav,Pc,Lbls,Lts,Lcs) => ([.intgr(41)],Pc+1,Lts).
  mnem(.iTSav,Pc,Lbls,Lts,Lcs) => ([.intgr(42)],Pc+1,Lts).
  mnem(.iCell,Pc,Lbls,Lts,Lcs) => ([.intgr(43)],Pc+1,Lts).
  mnem(.iGet,Pc,Lbls,Lts,Lcs) => ([.intgr(44)],Pc+1,Lts).
  mnem(.iAssign,Pc,Lbls,Lts,Lcs) => ([.intgr(45)],Pc+1,Lts).
  mnem(.iCLbl(U,V),Pc,Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) && Tgt ?= findLevel(Lbls,V) => ([.intgr(46),.intgr(LtNo),.intgr(Tgt)],Pc+1,Lt1).
  mnem(.iCInt(U,V),Pc,Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,U) && Tgt ?= findLevel(Lbls,V) => ([.intgr(47),.intgr(LtNo),.intgr(Tgt)],Pc+1,Lt1).
  mnem(.iCChar(U,V),Pc,Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,U) && Tgt ?= findLevel(Lbls,V) => ([.intgr(48),.intgr(LtNo),.intgr(Tgt)],Pc+1,Lt1).
  mnem(.iCFlt(U,V),Pc,Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,U) && Tgt ?= findLevel(Lbls,V) => ([.intgr(49),.intgr(LtNo),.intgr(Tgt)],Pc+1,Lt1).
  mnem(.iCLit(U,V),Pc,Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,U) && Tgt ?= findLevel(Lbls,V) => ([.intgr(50),.intgr(LtNo),.intgr(Tgt)],Pc+1,Lt1).
  mnem(.iNth(U),Pc,Lbls,Lts,Lcs) => ([.intgr(51),.intgr(U)],Pc+1,Lts).
  mnem(.iStNth(U),Pc,Lbls,Lts,Lcs) => ([.intgr(52),.intgr(U)],Pc+1,Lts).
  mnem(.iIf(V),Pc,Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(53),.intgr(Tgt)],Pc+1,Lts).
  mnem(.iIfNot(V),Pc,Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(54),.intgr(Tgt)],Pc+1,Lts).
  mnem(.iICase(U),Pc,Lbls,Lts,Lcs) => ([.intgr(55),.intgr(U)],Pc+1,Lts).
  mnem(.iCase(U),Pc,Lbls,Lts,Lcs) => ([.intgr(56),.intgr(U)],Pc+1,Lts).
  mnem(.iIxCase(U),Pc,Lbls,Lts,Lcs) => ([.intgr(57),.intgr(U)],Pc+1,Lts).
  mnem(.iIAdd,Pc,Lbls,Lts,Lcs) => ([.intgr(58)],Pc+1,Lts).
  mnem(.iISub,Pc,Lbls,Lts,Lcs) => ([.intgr(59)],Pc+1,Lts).
  mnem(.iIMul,Pc,Lbls,Lts,Lcs) => ([.intgr(60)],Pc+1,Lts).
  mnem(.iIDiv(V),Pc,Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(61),.intgr(Tgt)],Pc+1,Lts).
  mnem(.iIMod(V),Pc,Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(62),.intgr(Tgt)],Pc+1,Lts).
  mnem(.iIAbs,Pc,Lbls,Lts,Lcs) => ([.intgr(63)],Pc+1,Lts).
  mnem(.iIEq,Pc,Lbls,Lts,Lcs) => ([.intgr(64)],Pc+1,Lts).
  mnem(.iILt,Pc,Lbls,Lts,Lcs) => ([.intgr(65)],Pc+1,Lts).
  mnem(.iIGe,Pc,Lbls,Lts,Lcs) => ([.intgr(66)],Pc+1,Lts).
  mnem(.iCEq,Pc,Lbls,Lts,Lcs) => ([.intgr(67)],Pc+1,Lts).
  mnem(.iCLt,Pc,Lbls,Lts,Lcs) => ([.intgr(68)],Pc+1,Lts).
  mnem(.iCGe,Pc,Lbls,Lts,Lcs) => ([.intgr(69)],Pc+1,Lts).
  mnem(.iBAnd,Pc,Lbls,Lts,Lcs) => ([.intgr(70)],Pc+1,Lts).
  mnem(.iBOr,Pc,Lbls,Lts,Lcs) => ([.intgr(71)],Pc+1,Lts).
  mnem(.iBXor,Pc,Lbls,Lts,Lcs) => ([.intgr(72)],Pc+1,Lts).
  mnem(.iBLsl,Pc,Lbls,Lts,Lcs) => ([.intgr(73)],Pc+1,Lts).
  mnem(.iBLsr,Pc,Lbls,Lts,Lcs) => ([.intgr(74)],Pc+1,Lts).
  mnem(.iBAsr,Pc,Lbls,Lts,Lcs) => ([.intgr(75)],Pc+1,Lts).
  mnem(.iBNot,Pc,Lbls,Lts,Lcs) => ([.intgr(76)],Pc+1,Lts).
  mnem(.iFAdd,Pc,Lbls,Lts,Lcs) => ([.intgr(77)],Pc+1,Lts).
  mnem(.iFSub,Pc,Lbls,Lts,Lcs) => ([.intgr(78)],Pc+1,Lts).
  mnem(.iFMul,Pc,Lbls,Lts,Lcs) => ([.intgr(79)],Pc+1,Lts).
  mnem(.iFDiv(V),Pc,Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(80),.intgr(Tgt)],Pc+1,Lts).
  mnem(.iFMod(V),Pc,Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(81),.intgr(Tgt)],Pc+1,Lts).
  mnem(.iFAbs,Pc,Lbls,Lts,Lcs) => ([.intgr(82)],Pc+1,Lts).
  mnem(.iFEq,Pc,Lbls,Lts,Lcs) => ([.intgr(83)],Pc+1,Lts).
  mnem(.iFLt,Pc,Lbls,Lts,Lcs) => ([.intgr(84)],Pc+1,Lts).
  mnem(.iFGe,Pc,Lbls,Lts,Lcs) => ([.intgr(85)],Pc+1,Lts).
  mnem(.iAlloc(U),Pc,Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) => ([.intgr(86),.intgr(LtNo)],Pc+1,Lt1).
  mnem(.iClosure(U),Pc,Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) => ([.intgr(87),.intgr(LtNo)],Pc+1,Lt1).
  mnem(.iFrame(U),Pc,Lbls,Lts,Lcs) => ([.intgr(88),.intgr(U)],Pc+1,Lts).
  mnem(.iLine(U),Pc,Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,U) => ([.intgr(89),.intgr(LtNo)],Pc+1,Lt1).
  mnem(.iDBug(U),Pc,Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,U) => ([.intgr(90),.intgr(LtNo)],Pc+1,Lt1).

  mnem(I,Pc,Lbls,Lts,Lcs) => valof{
    reportTrap("Cannot assemble instruction $(I)");
    valis ([],Pc,Lts)
  }.

  private stackHwm:(multi[assemOp]) => integer.
  stackHwm(Code) where (_,HWM) .= stkHwm(Code,0,0) => HWM.

  private stkHwm:(multi[assemOp],integer,integer) => (integer,integer).
  stkHwm([],H,HWM) => (H,HWM).
  stkHwm([.iLbl(_,I),..Ins],CH,Hwm) => valof{
    (CH1,H1) = stkHwm([I],CH,Hwm);
    valis stkHwm(Ins,CH1,H1)
  }
  stkHwm([.iHalt(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iNop,..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iAbort(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
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
  stkHwm([.iResult(_),..Ins],CH0,H0) => valof{
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
  stkHwm([.iCInt(_,_),..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iCChar(_,_),..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iCFlt(_,_),..Ins],CH0,H0) => valof{
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
  stkHwm([.iICase(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iCase(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iIxCase(_),..Ins],CH0,H0) => valof{
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
  stkHwm([.iIMod(_),..Ins],CH0,H0) => valof{
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
  stkHwm([.iFDiv(_),..Ins],CH0,H0) => valof{
    CH1 = CH0-1;
    valis stkHwm(Ins,CH1,H0)
  }
  stkHwm([.iFMod(_),..Ins],CH0,H0) => valof{
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
  stkHwm([.iAlloc(_),..Ins],CH0,H0) => valof{
    CH1 = CH0+1;
    valis stkHwm(Ins,CH1,(CH1>H0??CH1||H0))
  }
  stkHwm([.iClosure(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iFrame(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iLine(_),..Ins],CH0,H0) => valof{
    valis stkHwm(Ins,CH0,H0)
  }
  stkHwm([.iDBug(_),..Ins],CH0,H0) => valof{
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
  showIns(.iHalt(U),Pc) => "Halt $(U)".
  showIns(.iNop,Pc) => "Nop".
  showIns(.iAbort(U),Pc) => "Abort $(U)".
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
  showIns(.iResult(V),Pc) => "Result #(V)".
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
  showIns(.iCInt(U,V),Pc) => "CInt $(U) #(V)".
  showIns(.iCChar(U,V),Pc) => "CChar $(U) #(V)".
  showIns(.iCFlt(U,V),Pc) => "CFlt $(U) #(V)".
  showIns(.iCLit(U,V),Pc) => "CLit $(U) #(V)".
  showIns(.iNth(U),Pc) => "Nth $(U)".
  showIns(.iStNth(U),Pc) => "StNth $(U)".
  showIns(.iIf(V),Pc) => "If #(V)".
  showIns(.iIfNot(V),Pc) => "IfNot #(V)".
  showIns(.iICase(U),Pc) => "ICase $(U)".
  showIns(.iCase(U),Pc) => "Case $(U)".
  showIns(.iIxCase(U),Pc) => "IxCase $(U)".
  showIns(.iIAdd,Pc) => "IAdd".
  showIns(.iISub,Pc) => "ISub".
  showIns(.iIMul,Pc) => "IMul".
  showIns(.iIDiv(V),Pc) => "IDiv #(V)".
  showIns(.iIMod(V),Pc) => "IMod #(V)".
  showIns(.iIAbs,Pc) => "IAbs".
  showIns(.iIEq,Pc) => "IEq".
  showIns(.iILt,Pc) => "ILt".
  showIns(.iIGe,Pc) => "IGe".
  showIns(.iCEq,Pc) => "CEq".
  showIns(.iCLt,Pc) => "CLt".
  showIns(.iCGe,Pc) => "CGe".
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
  showIns(.iFDiv(V),Pc) => "FDiv #(V)".
  showIns(.iFMod(V),Pc) => "FMod #(V)".
  showIns(.iFAbs,Pc) => "FAbs".
  showIns(.iFEq,Pc) => "FEq".
  showIns(.iFLt,Pc) => "FLt".
  showIns(.iFGe,Pc) => "FGe".
  showIns(.iAlloc(U),Pc) => "Alloc $(U)".
  showIns(.iClosure(U),Pc) => "Closure $(U)".
  showIns(.iFrame(U),Pc) => "Frame $(U)".
  showIns(.iLine(U),Pc) => "Line $(U)".
  showIns(.iDBug(U),Pc) => "dBug $(U)".


  showPc:(cons[integer]) => string.
  showPc(Pcs) => "#(spaces(size(Pcs)))#(interleave(Pcs//disp,".")*)".

  spaces:(integer)=>string.
  spaces(Ln) => let{.
    sp(0) => [].
    sp(N) => [` `,..sp(N-1)].
  .} in _implode(sp(Ln)).

  bumpPc:(cons[integer]) => cons[integer].
  bumpPc([Pc,..Rest]) => [Pc+1,..Rest].

  public opcodeHash = 84721529136952277.
}
