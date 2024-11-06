star.compiler.assem{
  -- Automatically Generated Assembler -- Do NOT Edit
  import star.
  import star.multi.
  import star.sort.

  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.location.
  import star.compiler.data.
  import star.compiler.types.
  import star.compiler.types.encode.
  import star.compiler.ltipe.

  public codePolicy ::= .hardDefinition | .softDefinition.

  public implementation display[codePolicy] => {
    disp(.hardDefinition) => "hard".
    disp(.softDefinition) => "soft".
  }

  public codeSegment ::= .func(termLbl,codePolicy,ltipe,integer,cons[assemOp]) |
    .global(termLbl,ltipe,integer,cons[assemOp]) |
    .struct(termLbl,tipe,integer) |
    .tipe(tipe,typeRule,cons[(termLbl,tipe,integer)]).

  public assemOp ::=
    .iHalt(integer) |
    .iNop |
    .iAbort |
    .iCall(termLbl) |
    .iOCall(integer) |
    .iEscape(string) |
    .iTCall(termLbl) |
    .iTOCall(integer) |
    .iEntry |
    .iRet |
    .iBlock(ltipe,cons[assemOp]) |
    .iBreak(assemLbl) |
    .iLoop(assemLbl) |
    .iDrop |
    .iDup |
    .iRot(integer) |
    .iRst(integer) |
    .iPick(integer,integer) |
    .iFiber |
    .iSpawn |
    .iSuspend |
    .iResume |
    .iRetire |
    .iUnderflow |
    .iTEq |
    .iTry(ltipe,cons[assemOp]) |
    .iEndTry(assemLbl) |
    .iThrow |
    .iReset |
    .iShift |
    .iInvoke |
    .iLdV |
    .iLdC(data) |
    .iLdA(integer) |
    .iLdL(integer) |
    .iStL(integer) |
    .iStV(integer) |
    .iTL(integer) |
    .iStA(integer) |
    .iLdG(string) |
    .iStG(string) |
    .iTG(string) |
    .iThunk |
    .iLdTh |
    .iStTh |
    .iTTh |
    .iCell |
    .iGet |
    .iAssign |
    .iCLbl(termLbl,assemLbl) |
    .iCLit(data,assemLbl) |
    .iNth(integer) |
    .iStNth(integer) |
    .iIf(assemLbl) |
    .iIfNot(assemLbl) |
    .iCase(integer) |
    .iIndxJmp(integer) |
    .iIAdd |
    .iISub |
    .iIMul |
    .iIDiv |
    .iIMod |
    .iIAbs |
    .iIEq |
    .iILt |
    .iIGe |
    .iICmp(assemLbl) |
    .iCEq |
    .iCLt |
    .iCGe |
    .iCCmp(assemLbl) |
    .iBAnd |
    .iBOr |
    .iBXor |
    .iBLsl |
    .iBLsr |
    .iBAsr |
    .iBNot |
    .iFAdd |
    .iFSub |
    .iFMul |
    .iFDiv |
    .iFMod |
    .iFAbs |
    .iFEq |
    .iFLt |
    .iFGe |
    .iFCmp(assemLbl) |
    .iAlloc(termLbl) |
    .iClosure(termLbl) |
    .iCmp(assemLbl) |
    .iFrame(ltipe) |
    .idBug |

    .iLbl(string, assemOp).

  public assemLbl ::= .bo(string) | .bp(string).

  public assem:(codeSegment) => data.
  assem(Df) => case Df in {
    | .func(Nm,H,Sig,Lx,Ins) => valof{
      funSig = .strg(Sig::string);
      (Lt0,_) = findLit([],.symb(Nm));
      (Lt1,tpIx) = findLit(Lt0,funSig);
      (Code,Lts,Lcs) = assemBlock(Ins,[],[],Lt1,[]);
      valis mkCons("func",
          [.symb(Nm),encPolicy(H),.intgr(tpIx),.intgr(Lx),mkTpl(Code::cons[data]),litTbl(Lts),mkTpl(Lcs::cons[data])])
    }
    | .global(Nm,Sig,Lx,Ins) => valof{
      funSig = .strg(Sig::string);
      (Lt0,_) = findLit([],.symb(Nm));
      (Lt1,tpIx) = findLit(Lt0,funSig);
      (Code,Lts,Lcs) = assemBlock(Ins,[],[],Lt1,[]);
      valis mkCons("global",
         [.symb(Nm),.intgr(tpIx),.intgr(Lx),mkTpl(Code::cons[data]),litTbl(Lts),mkTpl({Lcl|Lcl in Lcs})])
    }
    | .struct(Lbl,Tp,Ix) =>
      mkCons("struct",[.symb(Lbl),.strg(encodeSignature(Tp)),.intgr(Ix)])
    | .tipe(Tp,TpRl,Map) =>
      mkCons("type",[.strg(tpName(Tp)),.strg(encodeTpRlSignature(TpRl)),encodeMap(Map)])
  }.

  encodeMap(Entries) => mkTpl(Entries//((Lbl,_,Ix))=>mkTpl([.symb(Lbl),.intgr(Ix)])).

  encPolicy(.hardDefinition) => mkTpl([]).
  encPolicy(.softDefinition) => mkTpl([.strg("soft")]).

  private assemBlock:(cons[assemOp],multi[data],cons[string],map[data,integer],set[data]) =>
                                        (multi[data],map[data,integer],set[data]).
  assemBlock([],Code,Lbls,Lts,Lcx) => (Code,Lts,Lcx).
  assemBlock([I,..Ins],SoFar,Lbs,Lts,Lcx) => valof{
    (Code,Lt0,Lc0) = mnem(I,Lbs,Lts,Lcx);
    valis assemBlock(Ins,SoFar++Code,Lbs,Lt0,Lc0)
  }

  private mnem:(assemOp,cons[string],map[data,integer],set[data]) =>
    (multi[data],map[data,integer],set[data]).
  mnem(.iLbl(Lb,I),Lbls,Lts,Lcs) => mnem(I,[Lb,..Lbls],Lts,Lcs).
  mnem(.iHalt(U),Lbls,Lts,Lcs) => ([.intgr(0),.intgr(U)],Lts,Lcs).
  mnem(.iNop,Lbls,Lts,Lcs) => ([.intgr(1)],Lts,Lcs).
  mnem(.iAbort,Lbls,Lts,Lcs) => ([.intgr(2)],Lts,Lcs).
  mnem(.iCall(U),Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) => ([.intgr(3),.intgr(LtNo)],Lt1,Lcs).
  mnem(.iOCall(U),Lbls,Lts,Lcs) => ([.intgr(4),.intgr(U)],Lts,Lcs).
  mnem(.iEscape(U),Lbls,Lts,Lcs) => ([.intgr(5),.strg(U)],Lts,Lcs).
  mnem(.iTCall(U),Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) => ([.intgr(6),.intgr(LtNo)],Lt1,Lcs).
  mnem(.iTOCall(U),Lbls,Lts,Lcs) => ([.intgr(7),.intgr(U)],Lts,Lcs).
  mnem(.iEntry,Lbls,Lts,Lcs) => ([.intgr(8)],Lts,Lcs).
  mnem(.iRet,Lbls,Lts,Lcs) => ([.intgr(9)],Lts,Lcs).
  mnem(.iBlock(U,V),Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,.strg(U::string)) && (Blk,Lts1,Lcs1) .= assemBlock(V,[],["",..Lbls],Lt1,Lcs) => ([.intgr(10),.intgr(LtNo),mkTpl(Blk::cons[data])],Lts1,Lcs1).
  mnem(.iBreak(V),Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(11),.intgr(Tgt)],Lts,Lcs).
  mnem(.iLoop(V),Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(12),.intgr(Tgt)],Lts,Lcs).
  mnem(.iDrop,Lbls,Lts,Lcs) => ([.intgr(13)],Lts,Lcs).
  mnem(.iDup,Lbls,Lts,Lcs) => ([.intgr(14)],Lts,Lcs).
  mnem(.iRot(U),Lbls,Lts,Lcs) => ([.intgr(15),.intgr(U)],Lts,Lcs).
  mnem(.iRst(U),Lbls,Lts,Lcs) => ([.intgr(16),.intgr(U)],Lts,Lcs).
  mnem(.iPick(U,V),Lbls,Lts,Lcs) => ([.intgr(17),.intgr(U),.intgr(V)],Lts,Lcs).
  mnem(.iFiber,Lbls,Lts,Lcs) => ([.intgr(18)],Lts,Lcs).
  mnem(.iSpawn,Lbls,Lts,Lcs) => ([.intgr(19)],Lts,Lcs).
  mnem(.iSuspend,Lbls,Lts,Lcs) => ([.intgr(20)],Lts,Lcs).
  mnem(.iResume,Lbls,Lts,Lcs) => ([.intgr(21)],Lts,Lcs).
  mnem(.iRetire,Lbls,Lts,Lcs) => ([.intgr(22)],Lts,Lcs).
  mnem(.iUnderflow,Lbls,Lts,Lcs) => ([.intgr(23)],Lts,Lcs).
  mnem(.iTEq,Lbls,Lts,Lcs) => ([.intgr(24)],Lts,Lcs).
  mnem(.iTry(U,V),Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,.strg(U::string)) && (Blk,Lts1,Lcs1) .= assemBlock(V,[],["",..Lbls],Lt1,Lcs) => ([.intgr(25),.intgr(LtNo),mkTpl(Blk::cons[data])],Lts1,Lcs1).
  mnem(.iEndTry(V),Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(26),.intgr(Tgt)],Lts,Lcs).
  mnem(.iThrow,Lbls,Lts,Lcs) => ([.intgr(27)],Lts,Lcs).
  mnem(.iReset,Lbls,Lts,Lcs) => ([.intgr(28)],Lts,Lcs).
  mnem(.iShift,Lbls,Lts,Lcs) => ([.intgr(29)],Lts,Lcs).
  mnem(.iInvoke,Lbls,Lts,Lcs) => ([.intgr(30)],Lts,Lcs).
  mnem(.iLdV,Lbls,Lts,Lcs) => ([.intgr(31)],Lts,Lcs).
  mnem(.iLdC(U),Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,U) => ([.intgr(32),.intgr(LtNo)],Lt1,Lcs).
  mnem(.iLdA(U),Lbls,Lts,Lcs) => ([.intgr(33),.intgr(U)],Lts,Lcs).
  mnem(.iLdL(U),Lbls,Lts,Lcs) => ([.intgr(34),.intgr(U)],Lts,Lcs).
  mnem(.iStL(U),Lbls,Lts,Lcs) => ([.intgr(35),.intgr(U)],Lts,Lcs).
  mnem(.iStV(U),Lbls,Lts,Lcs) => ([.intgr(36),.intgr(U)],Lts,Lcs).
  mnem(.iTL(U),Lbls,Lts,Lcs) => ([.intgr(37),.intgr(U)],Lts,Lcs).
  mnem(.iStA(U),Lbls,Lts,Lcs) => ([.intgr(38),.intgr(U)],Lts,Lcs).
  mnem(.iLdG(U),Lbls,Lts,Lcs) => ([.intgr(39),.strg(U)],Lts,Lcs).
  mnem(.iStG(U),Lbls,Lts,Lcs) => ([.intgr(40),.strg(U)],Lts,Lcs).
  mnem(.iTG(U),Lbls,Lts,Lcs) => ([.intgr(41),.strg(U)],Lts,Lcs).
  mnem(.iThunk,Lbls,Lts,Lcs) => ([.intgr(42)],Lts,Lcs).
  mnem(.iLdTh,Lbls,Lts,Lcs) => ([.intgr(43)],Lts,Lcs).
  mnem(.iStTh,Lbls,Lts,Lcs) => ([.intgr(44)],Lts,Lcs).
  mnem(.iTTh,Lbls,Lts,Lcs) => ([.intgr(45)],Lts,Lcs).
  mnem(.iCell,Lbls,Lts,Lcs) => ([.intgr(46)],Lts,Lcs).
  mnem(.iGet,Lbls,Lts,Lcs) => ([.intgr(47)],Lts,Lcs).
  mnem(.iAssign,Lbls,Lts,Lcs) => ([.intgr(48)],Lts,Lcs).
  mnem(.iCLbl(U,V),Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) && Lvl ?= findLevel(Lbls,V) => ([.intgr(49),.intgr(LtNo),.intgr(Lvl)],Lt1,Lcs).
  mnem(.iCLit(U,V),Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,U) && Tgt ?= findLevel(Lbls,V) => ([.intgr(50),.intgr(LtNo),.intgr(Tgt)],Lt1,Lcs).
  mnem(.iNth(U),Lbls,Lts,Lcs) => ([.intgr(51),.intgr(U)],Lts,Lcs).
  mnem(.iStNth(U),Lbls,Lts,Lcs) => ([.intgr(52),.intgr(U)],Lts,Lcs).
  mnem(.iIf(V),Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(53),.intgr(Tgt)],Lts,Lcs).
  mnem(.iIfNot(V),Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(54),.intgr(Tgt)],Lts,Lcs).
  mnem(.iCase(U),Lbls,Lts,Lcs) => ([.intgr(55),.intgr(U)],Lts,Lcs).
  mnem(.iIndxJmp(U),Lbls,Lts,Lcs) => ([.intgr(56),.intgr(U)],Lts,Lcs).
  mnem(.iIAdd,Lbls,Lts,Lcs) => ([.intgr(57)],Lts,Lcs).
  mnem(.iISub,Lbls,Lts,Lcs) => ([.intgr(58)],Lts,Lcs).
  mnem(.iIMul,Lbls,Lts,Lcs) => ([.intgr(59)],Lts,Lcs).
  mnem(.iIDiv,Lbls,Lts,Lcs) => ([.intgr(60)],Lts,Lcs).
  mnem(.iIMod,Lbls,Lts,Lcs) => ([.intgr(61)],Lts,Lcs).
  mnem(.iIAbs,Lbls,Lts,Lcs) => ([.intgr(62)],Lts,Lcs).
  mnem(.iIEq,Lbls,Lts,Lcs) => ([.intgr(63)],Lts,Lcs).
  mnem(.iILt,Lbls,Lts,Lcs) => ([.intgr(64)],Lts,Lcs).
  mnem(.iIGe,Lbls,Lts,Lcs) => ([.intgr(65)],Lts,Lcs).
  mnem(.iICmp(V),Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(66),.intgr(Tgt)],Lts,Lcs).
  mnem(.iCEq,Lbls,Lts,Lcs) => ([.intgr(67)],Lts,Lcs).
  mnem(.iCLt,Lbls,Lts,Lcs) => ([.intgr(68)],Lts,Lcs).
  mnem(.iCGe,Lbls,Lts,Lcs) => ([.intgr(69)],Lts,Lcs).
  mnem(.iCCmp(V),Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(70),.intgr(Tgt)],Lts,Lcs).
  mnem(.iBAnd,Lbls,Lts,Lcs) => ([.intgr(71)],Lts,Lcs).
  mnem(.iBOr,Lbls,Lts,Lcs) => ([.intgr(72)],Lts,Lcs).
  mnem(.iBXor,Lbls,Lts,Lcs) => ([.intgr(73)],Lts,Lcs).
  mnem(.iBLsl,Lbls,Lts,Lcs) => ([.intgr(74)],Lts,Lcs).
  mnem(.iBLsr,Lbls,Lts,Lcs) => ([.intgr(75)],Lts,Lcs).
  mnem(.iBAsr,Lbls,Lts,Lcs) => ([.intgr(76)],Lts,Lcs).
  mnem(.iBNot,Lbls,Lts,Lcs) => ([.intgr(77)],Lts,Lcs).
  mnem(.iFAdd,Lbls,Lts,Lcs) => ([.intgr(78)],Lts,Lcs).
  mnem(.iFSub,Lbls,Lts,Lcs) => ([.intgr(79)],Lts,Lcs).
  mnem(.iFMul,Lbls,Lts,Lcs) => ([.intgr(80)],Lts,Lcs).
  mnem(.iFDiv,Lbls,Lts,Lcs) => ([.intgr(81)],Lts,Lcs).
  mnem(.iFMod,Lbls,Lts,Lcs) => ([.intgr(82)],Lts,Lcs).
  mnem(.iFAbs,Lbls,Lts,Lcs) => ([.intgr(83)],Lts,Lcs).
  mnem(.iFEq,Lbls,Lts,Lcs) => ([.intgr(84)],Lts,Lcs).
  mnem(.iFLt,Lbls,Lts,Lcs) => ([.intgr(85)],Lts,Lcs).
  mnem(.iFGe,Lbls,Lts,Lcs) => ([.intgr(86)],Lts,Lcs).
  mnem(.iFCmp(V),Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(87),.intgr(Tgt)],Lts,Lcs).
  mnem(.iAlloc(U),Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) => ([.intgr(88),.intgr(LtNo)],Lt1,Lcs).
  mnem(.iClosure(U),Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,.symb(U)) => ([.intgr(89),.intgr(LtNo)],Lt1,Lcs).
  mnem(.iCmp(V),Lbls,Lts,Lcs) where Tgt ?= findLevel(Lbls,V) => ([.intgr(90),.intgr(Tgt)],Lts,Lcs).
  mnem(.iFrame(U),Lbls,Lts,Lcs) where (Lt1,LtNo) .= findLit(Lts,.strg(U::string)) => ([.intgr(91),.intgr(LtNo)],Lt1,Lcs).
  mnem(.idBug,Lbls,Lts,Lcs) => ([.intgr(92)],Lts,Lcs).

  mnem(I,Lbls,Lts,Lcs) => valof{
    reportTrap("Cannot assemble instruction $(I)");
    valis ([],Lts,Lcs)
  }.

  findLit:(map[data,integer],data) => (map[data,integer],integer).
  findLit(Lts,T) where O ?= Lts[T] => (Lts,O).
  findLit(Lts,T) where O .= size(Lts) => (Lts[T->O],O).

  findLevel:(cons[string],assemLbl) => option[integer].
  findLevel(Lbs,Lb) => let{.
    findLvl([],_) => .none.
    findLvl([LL,.._], Lvl) where .bo(LL) .= Lb => .some(Lvl).
    findLvl([LL,.._], Lvl) where .bp(LL) .= Lb => .some(-Lvl).
    findLvl(["",..Ls],Lvl) => findLvl(Ls,Lvl+1).
    findLvl([_,..Ls],Lvl) => findLvl(Ls,Lvl).
  .} in findLvl(Lbs,0).

  litTbl:(map[data,integer]) => data.
  litTbl(Lts) => mkTpl(sort(Lts::cons[(data,integer)],((T1,Ix1), (T2,Ix2)) => Ix1<Ix2)//fst).

  public implementation display[assemLbl] => {
    disp(.bo(L)) => "→\#(L)".
    disp(.bp(L)) => "←\#(L)".
  }

  public implementation display[codeSegment] => {
    disp(.func(Nm,_,Tp,_,Ins)) => "fun $(Nm)\:$(Tp)\n"++showMnem(Ins).
    disp(.global(Nm,Tp,_,Ins)) => "glb $(Nm)\:$(Tp)\n"++showMnem(Ins).
    disp(.struct(Lbl,Tp,Ix)) => "struct $(Lbl)\:$(Tp) @ $(Ix)".
    disp(.tipe(_Tp,TpRl,Map)) => "type $(TpRl), map = $(Map)".
  }

  public implementation display[assemOp] => {
    disp(Op) => showIns(Op,[]).
  }

  showMnem:(cons[assemOp]) => string.
  showMnem(Ops) => showBlock(Ops,[0]).

  showBlock:(cons[assemOp],cons[integer]) => string.
  showBlock(Ins,Pc) => interleave(showCode(Ins,Pc),"\n")*.

  showCode([],_) => [].
  showCode([Ins,..Cde],Pc) => ["\#(showPc(Pc)\: \#(showIns(Ins,Pc))",..showCode(Cde,bumpPc(Pc))].

  showIns:(assemOp,cons[integer]) => string.
  showIns(.iLbl(Lb,I),Pc) => "#(Lb): \#(showIns(I,Pc))".
  showIns(.iHalt(U),Pc) => "Halt $(U)".
  showIns(.iNop,Pc) => "Nop".
  showIns(.iAbort,Pc) => "Abort".
  showIns(.iCall(U),Pc) => "Call $(U)".
  showIns(.iOCall(U),Pc) => "OCall $(U)".
  showIns(.iEscape(U),Pc) => "Escape $(U)".
  showIns(.iTCall(U),Pc) => "TCall $(U)".
  showIns(.iTOCall(U),Pc) => "TOCall $(U)".
  showIns(.iEntry,Pc) => "Entry".
  showIns(.iRet,Pc) => "Ret".
  showIns(.iBlock(U,V),Pc) => "Block $(U) $(V)".
  showIns(.iBreak(V),Pc) => "Break $(V)".
  showIns(.iLoop(V),Pc) => "Loop $(V)".
  showIns(.iDrop,Pc) => "Drop".
  showIns(.iDup,Pc) => "Dup".
  showIns(.iRot(U),Pc) => "Rot $(U)".
  showIns(.iRst(U),Pc) => "Rst $(U)".
  showIns(.iPick(U,V),Pc) => "Pick $(U) $(V)".
  showIns(.iFiber,Pc) => "Fiber".
  showIns(.iSpawn,Pc) => "Spawn".
  showIns(.iSuspend,Pc) => "Suspend".
  showIns(.iResume,Pc) => "Resume".
  showIns(.iRetire,Pc) => "Retire".
  showIns(.iUnderflow,Pc) => "Underflow".
  showIns(.iTEq,Pc) => "TEq".
  showIns(.iTry(U,V),Pc) => "Try $(U) $(V)".
  showIns(.iEndTry(V),Pc) => "EndTry $(V)".
  showIns(.iThrow,Pc) => "Throw".
  showIns(.iReset,Pc) => "Reset".
  showIns(.iShift,Pc) => "Shift".
  showIns(.iInvoke,Pc) => "Invoke".
  showIns(.iLdV,Pc) => "LdV".
  showIns(.iLdC(U),Pc) => "LdC $(U)".
  showIns(.iLdA(U),Pc) => "LdA $(U)".
  showIns(.iLdL(U),Pc) => "LdL $(U)".
  showIns(.iStL(U),Pc) => "StL $(U)".
  showIns(.iStV(U),Pc) => "StV $(U)".
  showIns(.iTL(U),Pc) => "TL $(U)".
  showIns(.iStA(U),Pc) => "StA $(U)".
  showIns(.iLdG(U),Pc) => "LdG $(U)".
  showIns(.iStG(U),Pc) => "StG $(U)".
  showIns(.iTG(U),Pc) => "TG $(U)".
  showIns(.iThunk,Pc) => "Thunk".
  showIns(.iLdTh,Pc) => "LdTh".
  showIns(.iStTh,Pc) => "StTh".
  showIns(.iTTh,Pc) => "TTh".
  showIns(.iCell,Pc) => "Cell".
  showIns(.iGet,Pc) => "Get".
  showIns(.iAssign,Pc) => "Assign".
  showIns(.iCLbl(U,V),Pc) => "CLbl $(U) $(V)".
  showIns(.iCLit(U,V),Pc) => "CLit $(U) $(V)".
  showIns(.iNth(U),Pc) => "Nth $(U)".
  showIns(.iStNth(U),Pc) => "StNth $(U)".
  showIns(.iIf(V),Pc) => "If $(V)".
  showIns(.iIfNot(V),Pc) => "IfNot $(V)".
  showIns(.iCase(U),Pc) => "Case $(U)".
  showIns(.iIndxJmp(U),Pc) => "IndxJmp $(U)".
  showIns(.iIAdd,Pc) => "IAdd".
  showIns(.iISub,Pc) => "ISub".
  showIns(.iIMul,Pc) => "IMul".
  showIns(.iIDiv,Pc) => "IDiv".
  showIns(.iIMod,Pc) => "IMod".
  showIns(.iIAbs,Pc) => "IAbs".
  showIns(.iIEq,Pc) => "IEq".
  showIns(.iILt,Pc) => "ILt".
  showIns(.iIGe,Pc) => "IGe".
  showIns(.iICmp(V),Pc) => "ICmp $(V)".
  showIns(.iCEq,Pc) => "CEq".
  showIns(.iCLt,Pc) => "CLt".
  showIns(.iCGe,Pc) => "CGe".
  showIns(.iCCmp(V),Pc) => "CCmp $(V)".
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
  showIns(.iFCmp(V),Pc) => "FCmp $(V)".
  showIns(.iAlloc(U),Pc) => "Alloc $(U)".
  showIns(.iClosure(U),Pc) => "Closure $(U)".
  showIns(.iCmp(V),Pc) => "Cmp $(V)".
  showIns(.iFrame(U),Pc) => "Frame $(U)".
  showIns(.idBug,Pc) => "dBug".


  showPc:(cons[integer]) => string.
  showPc(Pcs) => interleave(Pcs//disp,":")*.

  bumpPc:(cons[integer]) => cons[integer].
  bumpPc([Pc,..Rest]) => [Pc+1,..Rest].

  public opcodeHash = 855221420700215238.
}
