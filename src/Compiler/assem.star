star.compiler.assem{
  -- Automatically Generated Assembler -- Do NOT Edit
  import star.
  import star.sort.

  import star.compiler.location.
  import star.compiler.terms.
  import star.compiler.types.

  public codeSegment ::= method(termLbl,tipe,cons[assemOp]) |
    global(termLbl,tipe,cons[assemOp]) |
    struct(termLbl,tipe,tipe).

  public assemOp ::=
    .iHalt |
    iCall(termLbl) |
    iOCall(integer) |
    iEscape(string) |
    iTail(termLbl) |
    iOTail(integer) |
    .iRet |
    iJmp(assemLbl) |
    .iDrop |
    .iDup |
    iRst(integer) |
    .iLdV |
    iLdG(string) |
    iLdC(term) |
    iLdA(integer) |
    iLdL(integer) |
    iStL(integer) |
    iStV(integer) |
    iTL(integer) |
    iStA(integer) |
    iStG(string) |
    iTG(string) |
    .iAM |
    .iLM |
    .iSM |
    .iTM |
    iCLbl(assemLbl) |
    iNth(integer) |
    iStNth(integer) |
    iGet(termLbl) |
    iSet(termLbl) |
    iCase(integer) |
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
    iAlTpl(termLbl) |
    iCmp(assemLbl) |
    iBf(assemLbl) |
    iBt(assemLbl) |
    iFrame(term) |
    iThrow(assemLbl) |
    iUnwind(assemLbl) |
    idLine(term) |
    .idBug |
    .idBreak |

    iLbl(assemLbl) |
    iLocal(string,string,string,integer) |
    iLine(term).

  public assemLbl ::= al(string).

  public assem:(codeSegment) => term.
  assem(method(Nm,Sig,Ins)) where
    (Lt0,_) .= findLit([],enum(Nm)) &&
    (Code,Lts,Lns,Max,Lcs) .= mnem(Ins,genLblTbl(Ins,0,[]),Lt0,[],[],0,0,[]) =>
    term(tLbl("method",7),
      [enum(Nm),strg(encodeSignature(Sig)),intgr(Max),mkTpl(Code),litTbl(Lts),mkTpl(Lcs),
            mkTpl(sortLines(Lns))]).
  assem(global(Nm,Sig,Ins)) where
    (Lt0,_) .= findLit([],enum(Nm)) &&
    (Code,Lts,Lns,Max,Lcs) .= mnem(Ins,genLblTbl(Ins,0,[]),Lt0,[],[],0,0,[]) =>
    term(tLbl("global",7),
       [enum(Nm),strg(encodeSignature(Sig)),intgr(Max),mkTpl(Code),litTbl(Lts),mkTpl(Lcs),
            mkTpl(sortLines(Lns))]).
  assem(struct(Lbl,Tp,Flds)) =>
    term(tLbl("struct",3),[enum(Lbl),strg(encodeSignature(Tp)),strg(encodeSignature(Flds))]).

  private mnem:(cons[assemOp],map[string,integer],map[term,integer],map[term,integer],set[term],integer,integer,cons[term]) =>
    (cons[term],map[term,integer],map[term,integer],integer,cons[term]).
  mnem([],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => (reverse(Code),Lts,Lns,MxLcl,{Lcl|Lcl in Lcs}).
  mnem([iLbl(_),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code).
  mnem([iLocal(Nm,Frm,End,Off),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where
    F ^= Lbls[Frm] &&
    T ^= Lbls[End] =>
    mnem(Ins,Lbls,Lts,Lns,Lcs\+mkTpl([strg(Nm),intgr(F),intgr(T),intgr(Off)]),Pc,MxLcl,Code).
  mnem([iLine(Lc),..Ins],Lbs,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem([idLine(Lc),..Ins],Lbs,Lts,Lns[mkTpl([Lc,intgr(Pc)])->Pc],Lcs,Pc,MxLcl,Code).
  mnem([.iHalt,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(0),..Code]).
  mnem([iCall(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,enum(V)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(1),..Code]).
  mnem([iOCall(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(V),intgr(2),..Code]).
  mnem([iEscape(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[strg(V),intgr(3),..Code]).
  mnem([iTail(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,enum(V)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(4),..Code]).
  mnem([iOTail(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(V),intgr(5),..Code]).
  mnem([.iRet,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(6),..Code]).
  mnem([iJmp(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(7),..Code]).
  mnem([.iDrop,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(8),..Code]).
  mnem([.iDup,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(9),..Code]).
  mnem([iRst(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(V),intgr(10),..Code]).
  mnem([.iLdV,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(11),..Code]).
  mnem([iLdG(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[strg(V),intgr(12),..Code]).
  mnem([iLdC(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,V) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(13),..Code]).
  mnem([iLdA(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(V),intgr(14),..Code]).
  mnem([iLdL(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,max(V,MxLcl),[intgr(V),intgr(15),..Code]).
  mnem([iStL(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,max(V,MxLcl),[intgr(V),intgr(16),..Code]).
  mnem([iStV(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,max(V,MxLcl),[intgr(V),intgr(17),..Code]).
  mnem([iTL(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,max(V,MxLcl),[intgr(V),intgr(18),..Code]).
  mnem([iStA(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(V),intgr(19),..Code]).
  mnem([iStG(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[strg(V),intgr(20),..Code]).
  mnem([iTG(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[strg(V),intgr(21),..Code]).
  mnem([.iAM,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(22),..Code]).
  mnem([.iLM,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(23),..Code]).
  mnem([.iSM,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(24),..Code]).
  mnem([.iTM,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(25),..Code]).
  mnem([iCLbl(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(26),..Code]).
  mnem([iNth(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(V),intgr(27),..Code]).
  mnem([iStNth(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(V),intgr(28),..Code]).
  mnem([iGet(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,enum(V)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(29),..Code]).
  mnem([iSet(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,enum(V)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(30),..Code]).
  mnem([iCase(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(V),intgr(31),..Code]).
  mnem([.iIAdd,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(32),..Code]).
  mnem([.iISub,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(33),..Code]).
  mnem([.iIMul,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(34),..Code]).
  mnem([.iIDiv,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(35),..Code]).
  mnem([.iIMod,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(36),..Code]).
  mnem([.iIAbs,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(37),..Code]).
  mnem([.iIEq,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(38),..Code]).
  mnem([.iILt,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(39),..Code]).
  mnem([.iIGe,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(40),..Code]).
  mnem([iICmp(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(41),..Code]).
  mnem([.iBAnd,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(42),..Code]).
  mnem([.iBOr,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(43),..Code]).
  mnem([.iBXor,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(44),..Code]).
  mnem([.iBLsl,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(45),..Code]).
  mnem([.iBLsr,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(46),..Code]).
  mnem([.iBAsr,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(47),..Code]).
  mnem([.iBNot,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(48),..Code]).
  mnem([.iFAdd,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(49),..Code]).
  mnem([.iFSub,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(50),..Code]).
  mnem([.iFMul,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(51),..Code]).
  mnem([.iFDiv,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(52),..Code]).
  mnem([.iFMod,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(53),..Code]).
  mnem([.iFAbs,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(54),..Code]).
  mnem([.iFEq,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(55),..Code]).
  mnem([.iFLt,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(56),..Code]).
  mnem([.iFGe,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(57),..Code]).
  mnem([iFCmp(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(58),..Code]).
  mnem([iAlloc(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,enum(V)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(59),..Code]).
  mnem([iAlTpl(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,enum(V)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(60),..Code]).
  mnem([iCmp(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(61),..Code]).
  mnem([iBf(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(62),..Code]).
  mnem([iBt(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(63),..Code]).
  mnem([iFrame(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,V) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(64),..Code]).
  mnem([iThrow(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(65),..Code]).
  mnem([iUnwind(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(66),..Code]).
  mnem([idLine(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,V) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(67),..Code]).
  mnem([.idBug,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(68),..Code]).
  mnem([.idBreak,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(69),..Code]).


  genLblTbl:(cons[assemOp],integer,map[string,integer]) => map[string,integer].
  genLblTbl([],_,Lbls) => Lbls.
  genLblTbl([iLbl(al(Lbl)),..Ins],Pc,Lbls) =>
    genLblTbl(Ins,Pc,Lbls[Lbl->Pc]).
  genLblTbl([iLocal(_,_,_,_),..Ins],Pc,Lbls) =>
    genLblTbl(Ins,Pc,Lbls).
  genLblTbl([iLine(T),..Ins],Pc,Lbs) => genLblTbl([idLine(T),..Ins],Pc,Lbs).
  genLblTbl([.iHalt,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iCall(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iOCall(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iEscape(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTail(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iOTail(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iRet,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iJmp(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iDrop,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iDup,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iRst(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iLdV,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iLdG(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdC(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdA(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdL(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStL(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStV(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTL(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStA(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStG(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTG(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iAM,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iLM,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iSM,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iTM,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iCLbl(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iNth(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStNth(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iGet(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iSet(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCase(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iIAdd,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iISub,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIMul,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIDiv,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIMod,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIAbs,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIEq,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iILt,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iIGe,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iICmp(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.iBAnd,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBOr,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBXor,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBLsl,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBLsr,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBAsr,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iBNot,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFAdd,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFSub,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFMul,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFDiv,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFMod,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFAbs,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFEq,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFLt,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.iFGe,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iFCmp(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iAlloc(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iAlTpl(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCmp(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iBf(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iBt(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iFrame(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iThrow(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iUnwind(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([idLine(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([.idBug,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([.idBreak,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).


  findLit:(map[term,integer],term) => (map[term,integer],integer).
  findLit(Lts,T) where O ^= Lts[T] => (Lts,O).
  findLit(Lts,T) where O .= size(Lts) => (Lts[T->O],O).

  litTbl:(map[term,integer]) => term.
  litTbl(Lts) => mkTpl(sort(Lts::cons[keyval[term,integer]],((T1->Ix1), (T2->Ix2)) => Ix1<Ix2)//(K->_)=>K).

  sortLines:(map[term,integer]) => cons[term].
  sortLines(Lns) => (sort(Lns::cons[keyval[term,integer]],compLine)//(K->_)=>K).

  compLine:(keyval[term,integer],keyval[term,integer])=>boolean.
  compLine(T1->P1,T2->P2) => P1<P2.

  public implementation display[assemLbl] => {.
    disp(al(L)) => ss(L)
  .}

  public implementation display[codeSegment] => {.
    disp(method(Nm,Sig,Ins)) => ssSeq([disp(Nm),ss(":"),disp(Sig),ss("\n"),..reverse(showMnem(Ins,0,[]))]).
    disp(global(Nm,Sig,Ins)) => ssSeq([ss("global "),disp(Nm),ss(":"),disp(Sig),ss("\n"),..reverse(showMnem(Ins,0,[]))]).
  .}

  public implementation display[assemOp] => {.
    disp(Op) => ssSeq(reverse(showMnem([Op],0,[]))).
  .}

  showMnem:(cons[assemOp],integer,cons[ss]) => cons[ss].
  showMnem([],_,Out) => Out.
  showMnem([iLbl(al(Lb)),..Ins],Pc,Out) => showMnem(Ins,Pc,[ssSeq([ss(Lb),ss(":\n")]),..Out]).
  showMnem([iLocal(Nm,Frm,End,_Off),..Ins],Pc,Out) => showMnem(Ins,Pc,[ssSeq([ss(Nm),ss("::"),disp(Frm),ss("-"),disp(End),ss("\n")]),..Out]).
  showMnem([.iHalt,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("Halt"),ss("\n")]),..Out]).
  showMnem([iCall(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Call"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iOCall(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("OCall"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iEscape(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Escape"),ss(" "),ss(XX),ss("\n")]),..Out]).
  showMnem([iTail(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Tail"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iOTail(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("OTail"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([.iRet,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("Ret"),ss("\n")]),..Out]).
  showMnem([iJmp(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Jmp"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([.iDrop,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("Drop"),ss("\n")]),..Out]).
  showMnem([.iDup,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("Dup"),ss("\n")]),..Out]).
  showMnem([iRst(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Rst"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([.iLdV,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("LdV"),ss("\n")]),..Out]).
  showMnem([iLdG(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("LdG"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iLdC(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("LdC"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iLdA(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("LdA"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iLdL(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("LdL"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iStL(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("StL"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iStV(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("StV"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iTL(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("TL"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iStA(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("StA"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iStG(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("StG"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iTG(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("TG"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([.iAM,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("AM"),ss("\n")]),..Out]).
  showMnem([.iLM,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("LM"),ss("\n")]),..Out]).
  showMnem([.iSM,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("SM"),ss("\n")]),..Out]).
  showMnem([.iTM,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("TM"),ss("\n")]),..Out]).
  showMnem([iCLbl(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("CLbl"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iNth(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Nth"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iStNth(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("StNth"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iGet(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Get"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iSet(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Set"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iCase(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Case"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([.iIAdd,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("IAdd"),ss("\n")]),..Out]).
  showMnem([.iISub,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("ISub"),ss("\n")]),..Out]).
  showMnem([.iIMul,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("IMul"),ss("\n")]),..Out]).
  showMnem([.iIDiv,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("IDiv"),ss("\n")]),..Out]).
  showMnem([.iIMod,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("IMod"),ss("\n")]),..Out]).
  showMnem([.iIAbs,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("IAbs"),ss("\n")]),..Out]).
  showMnem([.iIEq,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("IEq"),ss("\n")]),..Out]).
  showMnem([.iILt,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("ILt"),ss("\n")]),..Out]).
  showMnem([.iIGe,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("IGe"),ss("\n")]),..Out]).
  showMnem([iICmp(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("ICmp"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([.iBAnd,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("BAnd"),ss("\n")]),..Out]).
  showMnem([.iBOr,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("BOr"),ss("\n")]),..Out]).
  showMnem([.iBXor,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("BXor"),ss("\n")]),..Out]).
  showMnem([.iBLsl,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("BLsl"),ss("\n")]),..Out]).
  showMnem([.iBLsr,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("BLsr"),ss("\n")]),..Out]).
  showMnem([.iBAsr,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("BAsr"),ss("\n")]),..Out]).
  showMnem([.iBNot,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("BNot"),ss("\n")]),..Out]).
  showMnem([.iFAdd,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("FAdd"),ss("\n")]),..Out]).
  showMnem([.iFSub,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("FSub"),ss("\n")]),..Out]).
  showMnem([.iFMul,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("FMul"),ss("\n")]),..Out]).
  showMnem([.iFDiv,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("FDiv"),ss("\n")]),..Out]).
  showMnem([.iFMod,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("FMod"),ss("\n")]),..Out]).
  showMnem([.iFAbs,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("FAbs"),ss("\n")]),..Out]).
  showMnem([.iFEq,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("FEq"),ss("\n")]),..Out]).
  showMnem([.iFLt,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("FLt"),ss("\n")]),..Out]).
  showMnem([.iFGe,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("FGe"),ss("\n")]),..Out]).
  showMnem([iFCmp(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("FCmp"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iAlloc(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Alloc"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iAlTpl(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("AlTpl"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iCmp(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Cmp"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iBf(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Bf"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iBt(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Bt"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iFrame(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Frame"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iThrow(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Throw"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([iUnwind(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("Unwind"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([idLine(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[ssSeq([disp(Pc),ss(":"),ss("dLine"),ss(" "),disp(XX),ss("\n")]),..Out]).
  showMnem([.idBug,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("dBug"),ss("\n")]),..Out]).
  showMnem([.idBreak,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[ssSeq([disp(Pc),ss(":"),ss("dBreak"),ss("\n")]),..Out]).

}
