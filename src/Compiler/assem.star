star.compiler.assem{
  -- Automatically Generated Assembler -- Do NOT Edit
  import star.
  import star.sort.

  import star.compiler.location.
  import star.compiler.terms.
  import star.compiler.types.

  public codeSegment ::= method(termLbl,tipe,list[assemOp]) |
    global(termLbl,tipe,list[assemOp]) |
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

  mnem:(list[assemOp],map[string,integer],map[term,integer],map[term,integer],set[term],integer,integer,list[term]) =>
    (list[term],map[term,integer],map[term,integer],integer,list[term]).
  mnem([],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => (Code,Lts,Lns,MxLcl,[Lcl|Lcl in Lcs]).
  mnem([iLbl(_),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code).
  mnem([iLocal(Nm,Frm,End,Off),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where
    F ^= Lbls[Frm] &&
    T ^= Lbls[End] =>
    mnem(Ins,Lbls,Lts,Lns,_addMem(mkTpl([strg(Nm),intgr(F),intgr(T),intgr(Off)]),Lcs),Pc,MxLcl,Code).
  mnem([iLine(Lc),..Ins],Lbs,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem([idLine(Lc),..Ins],Lbs,Lts,Lns[mkTpl([Lc,intgr(Pc)])->Pc],Lcs,Pc,MxLcl,Code).
  mnem([.iHalt,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(0)]).
  mnem([iCall(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,enum(V)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(1),intgr(LtNo)]).
  mnem([iOCall(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(2),intgr(V)]).
  mnem([iEscape(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(3),strg(V)]).
  mnem([iTail(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,enum(V)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(4),intgr(LtNo)]).
  mnem([iOTail(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(5),intgr(V)]).
  mnem([.iRet,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(6)]).
  mnem([iJmp(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(7),intgr(Tgt-Pc-3)]).
  mnem([.iDrop,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(8)]).
  mnem([.iDup,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(9)]).
  mnem([iRst(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(10),intgr(V)]).
  mnem([.iLdV,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(11)]).
  mnem([iLdG(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(12),strg(V)]).
  mnem([iLdC(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,V) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(13),intgr(LtNo)]).
  mnem([iLdA(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(14),intgr(V)]).
  mnem([iLdL(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,max(V,MxLcl),[Code..,intgr(15),intgr(V)]).
  mnem([iStL(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,max(V,MxLcl),[Code..,intgr(16),intgr(V)]).
  mnem([iStV(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,max(V,MxLcl),[Code..,intgr(17),intgr(V)]).
  mnem([iTL(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,max(V,MxLcl),[Code..,intgr(18),intgr(V)]).
  mnem([iStA(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(19),intgr(V)]).
  mnem([iStG(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(20),strg(V)]).
  mnem([iTG(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(21),strg(V)]).
  mnem([iCLbl(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(22),intgr(Tgt-Pc-3)]).
  mnem([iNth(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(23),intgr(V)]).
  mnem([iStNth(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(24),intgr(V)]).
  mnem([iGet(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,enum(V)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(25),intgr(LtNo)]).
  mnem([iSet(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,enum(V)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(26),intgr(LtNo)]).
  mnem([iCase(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(27),intgr(V)]).
  mnem([.iIAdd,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(28)]).
  mnem([.iISub,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(29)]).
  mnem([.iIMul,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(30)]).
  mnem([.iIDiv,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(31)]).
  mnem([.iIMod,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(32)]).
  mnem([.iIAbs,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(33)]).
  mnem([.iIEq,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(34)]).
  mnem([.iILt,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(35)]).
  mnem([.iIGe,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(36)]).
  mnem([iICmp(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(37),intgr(Tgt-Pc-3)]).
  mnem([.iBAnd,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(38)]).
  mnem([.iBOr,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(39)]).
  mnem([.iBXor,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(40)]).
  mnem([.iBLsl,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(41)]).
  mnem([.iBLsr,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(42)]).
  mnem([.iBAsr,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(43)]).
  mnem([.iBNot,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(44)]).
  mnem([.iFAdd,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(45)]).
  mnem([.iFSub,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(46)]).
  mnem([.iFMul,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(47)]).
  mnem([.iFDiv,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(48)]).
  mnem([.iFMod,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(49)]).
  mnem([.iFAbs,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(50)]).
  mnem([.iFEq,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(51)]).
  mnem([.iFLt,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(52)]).
  mnem([.iFGe,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(53)]).
  mnem([iFCmp(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(54),intgr(Tgt-Pc-3)]).
  mnem([iAlloc(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,enum(V)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(55),intgr(LtNo)]).
  mnem([iCmp(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(56),intgr(Tgt-Pc-3)]).
  mnem([iBf(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(57),intgr(Tgt-Pc-3)]).
  mnem([iBt(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(58),intgr(Tgt-Pc-3)]).
  mnem([iFrame(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,V) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(59),intgr(LtNo)]).
  mnem([iThrow(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(60),intgr(Tgt-Pc-3)]).
  mnem([iUnwind(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(61),intgr(Tgt-Pc-3)]).
  mnem([idLine(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,V) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(62),intgr(LtNo)]).
  mnem([.idBug,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(63)]).
  mnem([.idBreak,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(64)]).


  genLblTbl:(list[assemOp],integer,map[string,integer]) => map[string,integer].
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
  litTbl(Lts) => mkTpl(sort(Lts::list[keyval[term,integer]],((T1->Ix1), (T2->Ix2)) => Ix1<Ix2)//(K->_)=>K).

  sortLines:(map[term,integer]) => list[term].
  sortLines(Lns) => (sort(Lns::list[keyval[term,integer]],compLine)//(K->_)=>K).

  compLine:(keyval[term,integer],keyval[term,integer])=>boolean.
  compLine(T1->P1,T2->P2) => P1<P2.

  public implementation display[assemLbl] => {.
    disp(al(L)) => ss(L)
  .}

  public implementation display[codeSegment] => {.
    disp(method(Nm,Sig,Ins)) => ssSeq([disp(Nm),ss(":"),disp(Sig),ss("\n"),..showMnem(Ins,0,[])]).
    disp(global(Nm,Sig,Ins)) => ssSeq([ss("global "),disp(Nm),ss(":"),disp(Sig),ss("\n"),..showMnem(Ins,0,[])]).
  .}

  implementation display[assemLbl] => {.
    disp(al(Nm)) => ss(Nm).
  .}

  public implementation display[assemOp] => {.
    disp(Op) => ssSeq(reverse(showMnem([Op],0,[]))).
  .}

  showMnem:(list[assemOp],integer,list[ss]) => list[ss].
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
