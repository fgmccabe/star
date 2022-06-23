star.compiler.assem{
  -- Automatically Generated Assembler -- Do NOT Edit
  import star.
  import star.multi.
  import star.sort.

  import star.compiler.escapes.
  import star.compiler.location.
  import star.compiler.terms.
  import star.compiler.types.
  import star.compiler.ltipe.

  public codePolicy ::= .hardDefinition | .softDefinition.

  public implementation display[codePolicy] => {
    disp(.hardDefinition) => "hard".
    disp(.softDefinition) => "soft".
  }

  public codeSegment ::= func(termLbl,codePolicy,tipe,cons[assemOp]) |
    global(termLbl,tipe,cons[assemOp]) |
    struct(termLbl,tipe,integer) |
    tipe(tipe,cons[(termLbl,tipe,integer)]).

  public assemOp ::=
    iHalt(integer) |
    .iNop |
    .iAbort |
    iCall(termLbl) |
    iOCall(integer) |
    iEscape(string) |
    iTCall(termLbl) |
    iTOCall(integer) |
    .iRet |
    .iRtG |
    iJmp(assemLbl) |
    .iDrop |
    .iDup |
    iRst(integer) |
    .iSwap |
    .iTask |
    iSuspend |
    iResume |
    iRetire |
    .iRelease |
    .iUnderflow |
    .iLdV |
    iLdC(term) |
    iLdA(integer) |
    iLdL(integer) |
    iStL(integer) |
    iStV(integer) |
    iTL(integer) |
    iStA(integer) |
    iLdG(string) |
    iStG(string) |
    iTG(string) |
    .iThnk |
    .iThGet |
    .iThSet |
    .iCell |
    .iGet |
    .iAssign |
    iCLbl(termLbl,assemLbl) |
    iNth(integer) |
    iStNth(integer) |
    iIf(assemLbl) |
    iIfNot(assemLbl) |
    iCase(integer) |
    iIndxJmp(integer) |
    iUnpack(termLbl,assemLbl) |
    .iIAdd |
    .iISub |
    .iIMul |
    .iIDiv |
    .iIMod |
    .iIAbs |
    .iIEq |
    .iILt |
    .iIGe |
    iICmp(assemLbl) |
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
    iFCmp(assemLbl) |
    iAlloc(termLbl) |
    iCmp(assemLbl) |
    iFrame(ltipe) |
    .idBug |

    iLbl(assemLbl) |
    iLocal(string,string,string,integer) |
    iLine(term).

  public assemLbl ::= al(string).

  public assem:(codeSegment) => term.
  assem(func(Nm,H,Sig,Ins)) where
    (Lt0,_) .= findLit([],symb(Nm)) &&
    (_,Lbls) .= genLblTbl(Ins,0,[]) &&
    (Code,Lts,Lns,Lcs,_,Max) .= assemBlock(Ins,Lbls,Lt0,[],[],0,0,[]) =>
    mkCons("func",
      [symb(Nm),encPolicy(H),strg(encodeSignature(Sig)),intgr(Max),mkTpl(Code::cons[term]),litTbl(Lts),mkTpl(Lcs::cons[term]),
            mkTpl(sortLines(Lns))]).
  assem(global(Nm,Sig,Ins)) where
    (Lt0,_) .= findLit([],symb(Nm)) &&
    (_,Lbls) .= genLblTbl(Ins,0,[]) &&
    (Code,Lts,Lns,Lcs,_,Max) .= assemBlock(Ins,Lbls,Lt0,[],[],0,0,[]) =>
    mkCons("global",
       [symb(Nm),strg(encodeSignature(Sig)),intgr(Max),mkTpl(Code::cons[term]),litTbl(Lts),mkTpl({Lcl|Lcl in Lcs}),
            mkTpl(sortLines(Lns))]).
  assem(struct(Lbl,Tp,Ix)) =>
    mkCons("struct",[symb(Lbl),strg(encodeSignature(Tp)),intgr(Ix)]).

  encPolicy(.hardDefinition) => mkTpl([]).
  encPolicy(.softDefinition) => mkTpl([strg("soft")]).

  private assemBlock:(cons[assemOp],map[string,integer],map[term,integer],map[term,integer],
                      set[term],integer,integer,cons[integer]) =>
                                        (multi[term],map[term,integer],map[term,integer],set[term],integer,integer).
  assemBlock(Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (End,_).=genLblTbl(Code,Pc,[])
    => mnem(Code,[],Lbls,Lts,Lns,Lcs,Pc,MxLcl,[End,..Ends]).

  private mnem:(cons[assemOp],multi[term],map[string,integer],map[term,integer],map[term,integer],set[term],integer,integer,cons[integer]) =>
    (multi[term],map[term,integer],map[term,integer],set[term],integer,integer).
  mnem([],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,_) => (Code,Lts,Lns,Lcs,Pc,MxLcl).
  mnem([iLbl(_),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends).
  mnem([iLocal(Nm,Frm,End,Off),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where
    F ^= Lbls[Frm] &&
    T ^= Lbls[End] =>
    mnem(Ins,Code,Lbls,Lts,Lns,Lcs\+mkTpl([strg(Nm),intgr(F),intgr(T),intgr(Off)]),Pc,MxLcl,Ends).
  mnem([iLine(Lc),..Ins],Code,Lbs,Lts,Lns,Lcs,Pc,MxLcl,Ends) =>
        mnem(Ins,Code,Lbs,Lts,Lns[mkTpl([Lc,intgr(Pc)])->Pc],Lcs,Pc,MxLcl,Ends).
  mnem([iHalt(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(0),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iNop,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(1)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iAbort,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(2)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iCall(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Code++[intgr(3),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iOCall(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(4),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iEscape(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends)  where Cd^=isEscape(U) => mnem(Ins,Code++[intgr(5),intgr(Cd)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iTCall(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Code++[intgr(6),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iTOCall(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(7),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iRet,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(8)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iRtG,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(9)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iJmp(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(10),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iDrop,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(11)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iDup,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(12)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iRst(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(13),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iSwap,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(14)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iTask,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(15)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iSuspend,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(16)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iResume,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(17)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iRetire,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(18)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iRelease,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(19)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iUnderflow,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(20)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iLdV,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(21)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iLdC(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,U) => mnem(Ins,Code++[intgr(22),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iLdA(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(23),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iLdL(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(24),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),Ends).
  mnem([iStL(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(25),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),Ends).
  mnem([iStV(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(26),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),Ends).
  mnem([iTL(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(27),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),Ends).
  mnem([iStA(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(28),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iLdG(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(29),strg(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iStG(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(30),strg(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iTG(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(31),strg(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iThnk,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(32)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iThGet,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(33)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iThSet,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(34)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iCell,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(35)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iGet,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(36)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iAssign,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(37)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iCLbl(U,al(V)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) && Tgt ^= Lbls[V] => mnem(Ins,Code++[intgr(38),intgr(LtNo),intgr(Tgt-Pc-5)],Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl,Ends).
  mnem([iNth(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(39),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iStNth(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(40),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iIf(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(41),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iIfNot(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(42),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iCase(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(43),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iIndxJmp(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(44),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iUnpack(U,al(V)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) && Tgt ^= Lbls[V] => mnem(Ins,Code++[intgr(45),intgr(LtNo),intgr(Tgt-Pc-5)],Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl,Ends).
  mnem([.iIAdd,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(46)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iISub,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(47)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIMul,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(48)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIDiv,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(49)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIMod,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(50)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIAbs,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(51)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIEq,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(52)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iILt,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(53)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iIGe,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(54)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iICmp(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(55),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.iBAnd,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(56)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBOr,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(57)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBXor,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(58)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBLsl,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(59)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBLsr,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(60)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBAsr,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(61)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iBNot,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(62)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFAdd,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(63)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFSub,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(64)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFMul,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(65)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFDiv,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(66)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFMod,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(67)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFAbs,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(68)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFEq,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(69)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFLt,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(70)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([.iFGe,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(71)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).
  mnem([iFCmp(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(72),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iAlloc(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Code++[intgr(73),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iCmp(al(U)),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(74),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([iFrame(U),..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) where (Lt1,LtNo) .= findLit(Lts,strg(U::string)) => mnem(Ins,Code++[intgr(75),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).
  mnem([.idBug,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) => mnem(Ins,Code++[intgr(76)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).


  genLblTbl:(cons[assemOp],integer,map[string,integer]) => (integer,map[string,integer]).
  genLblTbl([],Pc,Lbls) => (Pc,Lbls).
  genLblTbl([iLbl(al(Lbl)),..Ins],Pc,Lbls) =>
    genLblTbl(Ins,Pc,Lbls[Lbl->Pc]).
  genLblTbl([iLocal(_,_,_,_),..Ins],Pc,Lbls) =>
    genLblTbl(Ins,Pc,Lbls).
  genLblTbl([iLine(T),..Ins],Pc,Lbs) => genLblTbl(Ins,Pc,Lbs).
  genLblTbl([iHalt(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iNop,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iAbort,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iCall(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iOCall(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iEscape(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTCall(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTOCall(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iRet,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iRtG,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iJmp(al(A)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iDrop,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iDup,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iRst(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iSwap,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iTask,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iSuspend,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iResume,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iRetire,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iRelease,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iUnderflow,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iLdV,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iLdC(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdA(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdL(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStL(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStV(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTL(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStA(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdG(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStG(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTG(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iThnk,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iThGet,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iThSet,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iCell,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iGet,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iAssign,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iCLbl(A,al(B)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+5,Lbls).
  genLblTbl([iNth(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStNth(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iIf(al(A)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iIfNot(al(A)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCase(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iIndxJmp(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iUnpack(A,al(B)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+5,Lbls).
  genLblTbl([.iIAdd,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iISub,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIMul,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIDiv,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIMod,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIAbs,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIEq,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iILt,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIGe,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iICmp(al(A)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iBAnd,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBOr,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBXor,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBLsl,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBLsr,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBAsr,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBNot,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFAdd,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFSub,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFMul,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFDiv,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFMod,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFAbs,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFEq,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFLt,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFGe,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iFCmp(al(A)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iAlloc(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCmp(al(A)),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iFrame(A),..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.idBug,..Ins],Pc,Lbls)  => genLblTbl(Ins,Pc+1,Lbls).


  findEnd:(cons[integer],integer)=>integer.
  findEnd([E,.._],0) => E.
  findEnd([_,..Ends],Lvl) => findEnd(Ends,Lvl-1).

  findLit:(map[term,integer],term) => (map[term,integer],integer).
  findLit(Lts,T) where O ^= Lts[T] => (Lts,O).
  findLit(Lts,T) where O .= size(Lts) => (Lts[T->O],O).

  litTbl:(map[term,integer]) => term.
  litTbl(Lts) => mkTpl(sort(Lts::cons[keyval[term,integer]],((T1->Ix1), (T2->Ix2)) => Ix1<Ix2)//(K->_)=>K).

  sortLines:(map[term,integer]) => cons[term].
  sortLines(Lns) => (sort(Lns::cons[keyval[term,integer]],compLine)//(K->_)=>K).

  compLine:(keyval[term,integer],keyval[term,integer])=>boolean.
  compLine(T1->P1,T2->P2) => P1<P2.

  public implementation display[assemLbl] => {
    disp(al(L)) => L
  }

  public implementation display[codeSegment] => {
    disp(func(Nm,H,Sig,Ins)) => "$(H) $(Nm)\:$(Sig)\n"++showMnem(Ins,0).
    disp(global(Nm,Sig,Ins)) => "global $(Nm)\:$(Sig)\n"++showMnem(Ins,0).
  }

  public implementation display[assemOp] => {
    disp(Op) => showMnem([Op],0).
  }

  showMnem:(cons[assemOp],integer) => string.
  showMnem([],_) => "".
  showMnem([iLbl(al(Lb)),..Ins],Pc) => "#(Lb):\n"++showMnem(Ins,Pc).
  showMnem([iLocal(Nm,Frm,End,Off),..Ins],Pc) => "#(Nm)\::$(Frm)-$(End)\:$(Off)\n"++showMnem(Ins,Pc).
  showMnem([iHalt(U),..Ins],Pc) => "$(Pc)\: Halt $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([.iNop,..Ins],Pc) => "$(Pc)\: Nop\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iAbort,..Ins],Pc) => "$(Pc)\: Abort\n" ++ showMnem(Ins,Pc+1).
  showMnem([iCall(U),..Ins],Pc) => "$(Pc)\: Call $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iOCall(U),..Ins],Pc) => "$(Pc)\: OCall $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iEscape(U),..Ins],Pc) => "$(Pc)\: Escape $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iTCall(U),..Ins],Pc) => "$(Pc)\: TCall $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iTOCall(U),..Ins],Pc) => "$(Pc)\: TOCall $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([.iRet,..Ins],Pc) => "$(Pc)\: Ret\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iRtG,..Ins],Pc) => "$(Pc)\: RtG\n" ++ showMnem(Ins,Pc+1).
  showMnem([iJmp(al(U)),..Ins],Pc) => "$(Pc)\: Jmp $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([.iDrop,..Ins],Pc) => "$(Pc)\: Drop\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iDup,..Ins],Pc) => "$(Pc)\: Dup\n" ++ showMnem(Ins,Pc+1).
  showMnem([iRst(U),..Ins],Pc) => "$(Pc)\: Rst $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([.iSwap,..Ins],Pc) => "$(Pc)\: Swap\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iTask,..Ins],Pc) => "$(Pc)\: Task\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iSuspend,..Ins],Pc) => "$(Pc)\: Suspend\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iResume,..Ins],Pc) => "$(Pc)\: Resume\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iRetire,..Ins],Pc) => "$(Pc)\: Retire\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iRelease,..Ins],Pc) => "$(Pc)\: Release\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iUnderflow,..Ins],Pc) => "$(Pc)\: Underflow\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iLdV,..Ins],Pc) => "$(Pc)\: LdV\n" ++ showMnem(Ins,Pc+1).
  showMnem([iLdC(U),..Ins],Pc) => "$(Pc)\: LdC $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iLdA(U),..Ins],Pc) => "$(Pc)\: LdA $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iLdL(U),..Ins],Pc) => "$(Pc)\: LdL $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iStL(U),..Ins],Pc) => "$(Pc)\: StL $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iStV(U),..Ins],Pc) => "$(Pc)\: StV $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iTL(U),..Ins],Pc) => "$(Pc)\: TL $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iStA(U),..Ins],Pc) => "$(Pc)\: StA $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iLdG(U),..Ins],Pc) => "$(Pc)\: LdG $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iStG(U),..Ins],Pc) => "$(Pc)\: StG $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iTG(U),..Ins],Pc) => "$(Pc)\: TG $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([.iThnk,..Ins],Pc) => "$(Pc)\: Thnk\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iThGet,..Ins],Pc) => "$(Pc)\: ThGet\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iThSet,..Ins],Pc) => "$(Pc)\: ThSet\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iCell,..Ins],Pc) => "$(Pc)\: Cell\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iGet,..Ins],Pc) => "$(Pc)\: Get\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iAssign,..Ins],Pc) => "$(Pc)\: Assign\n" ++ showMnem(Ins,Pc+1).
  showMnem([iCLbl(U,al(V)),..Ins],Pc) => "$(Pc)\: CLbl $(U) $(V)\n" ++ showMnem(Ins,Pc+5).
  showMnem([iNth(U),..Ins],Pc) => "$(Pc)\: Nth $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iStNth(U),..Ins],Pc) => "$(Pc)\: StNth $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iIf(al(U)),..Ins],Pc) => "$(Pc)\: If $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iIfNot(al(U)),..Ins],Pc) => "$(Pc)\: IfNot $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iCase(U),..Ins],Pc) => "$(Pc)\: Case $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iIndxJmp(U),..Ins],Pc) => "$(Pc)\: IndxJmp $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iUnpack(U,al(V)),..Ins],Pc) => "$(Pc)\: Unpack $(U) $(V)\n" ++ showMnem(Ins,Pc+5).
  showMnem([.iIAdd,..Ins],Pc) => "$(Pc)\: IAdd\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iISub,..Ins],Pc) => "$(Pc)\: ISub\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iIMul,..Ins],Pc) => "$(Pc)\: IMul\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iIDiv,..Ins],Pc) => "$(Pc)\: IDiv\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iIMod,..Ins],Pc) => "$(Pc)\: IMod\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iIAbs,..Ins],Pc) => "$(Pc)\: IAbs\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iIEq,..Ins],Pc) => "$(Pc)\: IEq\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iILt,..Ins],Pc) => "$(Pc)\: ILt\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iIGe,..Ins],Pc) => "$(Pc)\: IGe\n" ++ showMnem(Ins,Pc+1).
  showMnem([iICmp(al(U)),..Ins],Pc) => "$(Pc)\: ICmp $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([.iBAnd,..Ins],Pc) => "$(Pc)\: BAnd\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iBOr,..Ins],Pc) => "$(Pc)\: BOr\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iBXor,..Ins],Pc) => "$(Pc)\: BXor\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iBLsl,..Ins],Pc) => "$(Pc)\: BLsl\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iBLsr,..Ins],Pc) => "$(Pc)\: BLsr\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iBAsr,..Ins],Pc) => "$(Pc)\: BAsr\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iBNot,..Ins],Pc) => "$(Pc)\: BNot\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iFAdd,..Ins],Pc) => "$(Pc)\: FAdd\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iFSub,..Ins],Pc) => "$(Pc)\: FSub\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iFMul,..Ins],Pc) => "$(Pc)\: FMul\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iFDiv,..Ins],Pc) => "$(Pc)\: FDiv\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iFMod,..Ins],Pc) => "$(Pc)\: FMod\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iFAbs,..Ins],Pc) => "$(Pc)\: FAbs\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iFEq,..Ins],Pc) => "$(Pc)\: FEq\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iFLt,..Ins],Pc) => "$(Pc)\: FLt\n" ++ showMnem(Ins,Pc+1).
  showMnem([.iFGe,..Ins],Pc) => "$(Pc)\: FGe\n" ++ showMnem(Ins,Pc+1).
  showMnem([iFCmp(al(U)),..Ins],Pc) => "$(Pc)\: FCmp $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iAlloc(U),..Ins],Pc) => "$(Pc)\: Alloc $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iCmp(al(U)),..Ins],Pc) => "$(Pc)\: Cmp $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([iFrame(U),..Ins],Pc) => "$(Pc)\: Frame $(U)\n" ++ showMnem(Ins,Pc+3).
  showMnem([.idBug,..Ins],Pc) => "$(Pc)\: dBug\n" ++ showMnem(Ins,Pc+1).

}
