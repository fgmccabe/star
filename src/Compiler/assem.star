-- Automatically Generated Assembler -- Do NOT Edit

star.compiler.assem{
  import star.
  import star.sort.

  import star.compiler.location.
  import star.compiler.terms.
  import star.compiler.types.

  public codeSegment ::= method(term,string,integer,list[assemOp]) |
    struct(term,tipe,tipe).

  public assemOp ::=
    iHalt |
    iNop |
    iCall(term) |
    iOCall(integer) |
    iEscape(string) |
    iTail(term) |
    iOTail(integer) |
    iAbort |
    iRet |
    iJmp(string) |
    iDrop |
    iDup |
    iPull(integer) |
    iRot(integer) |
    iRst(integer) |
    iLdV |
    iLdG(string) |
    iLdC(term) |
    iLdA(integer) |
    iLdL(integer) |
    iStL(integer) |
    iStV(integer) |
    iTL(integer) |
    iStA(integer) |
    iStG(string) |
    iCLbl(string) |
    iCVd |
    iNth(integer) |
    iStNth(integer) |
    iGet(term) |
    iSet(term) |
    iCase(integer) |
    iAlloc(term) |
    iCmp(string) |
    iBf(string) |
    iBt(string) |
    iFrame(integer) |
    iThrow(string) |
    iUnwind(string) |
    idLine(term) |
    idBug |
    idBreak |

    iLbl(string) |
    iLocal(string,string,string,integer) |
    iLine(term).

  public assem:(codeSegment) => term.
  assem(method(Nm,Sig,Stx,Ins)) where
    (Lt0,_) .= findLit([],Nm) &&
    (Code,Lts,Lns,Lcs) .= mnem(Ins,genLblTbl(Ins,0,[]),Lt0,[],[],0,[]) =>
    mkTpl([Nm,strg(Sig),intgr(Stx),mkTpl(Code),litTbl(Lts),mkTpl(Lcs),mkTpl(sortLines(Lns))]).
  assem(struct(Lbl,Tp,Flds)) =>
    mkTpl([Lbl,strg(encodeSignature(Tp)),strg(encodeSignature(Flds))]).

  mnem:(list[assemOp],map[string,integer],map[term,integer],map[term,integer],set[term],integer,list[term]) =>
    (list[term],map[term,integer],map[term,integer],list[term]).
  mnem([],Lbls,Lts,Lns,Lcs,Pc,Code) => (Code,Lts,Lns,[Lcl|Lcl in Lcs]).
  mnem([iLbl(_),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc,Code).
  mnem([iLocal(Nm,Frm,End,Off),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) where
    F ^= Lbls[Frm] &&
    T ^= Lbls[End] =>
    mnem(Ins,Lbls,Lts,Lns,_addMem(mkTpl([strg(Nm),intgr(F),intgr(T),intgr(Off)]),Lcs),Pc,Code).
  mnem([iLine(Lc),..Ins],Lbs,Lts,Lns,Lcs,Pc,Code) => mnem([idLine(Lc),..Ins],Lbs,Lts,Lns[mkTpl([Lc,intgr(Pc)])->Pc],Lcs,Pc,Code).
  mnem([iHalt,..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,[Code..,intgr(0)]).
  mnem([iNop,..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,[Code..,intgr(1)]).
  mnem([iCall(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) where (Lt1,LtNo) .= findLit(Lts,V) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,[Code..,intgr(2),intgr(LtNo)]).
  mnem([iOCall(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(3),intgr(V)]).
  mnem([iEscape(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(4),strg(V)]).
  mnem([iTail(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) where (Lt1,LtNo) .= findLit(Lts,V) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,[Code..,intgr(5),intgr(LtNo)]).
  mnem([iOTail(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(6),intgr(V)]).
  mnem([iAbort,..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,[Code..,intgr(7)]).
  mnem([iRet,..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,[Code..,intgr(8)]).
  mnem([iJmp(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(9),intgr(Tgt-Pc-3)]).
  mnem([iDrop,..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,[Code..,intgr(10)]).
  mnem([iDup,..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,[Code..,intgr(11)]).
  mnem([iPull(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(12),intgr(V)]).
  mnem([iRot(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(13),intgr(V)]).
  mnem([iRst(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(14),intgr(V)]).
  mnem([iLdV,..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,[Code..,intgr(15)]).
  mnem([iLdG(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(16),strg(V)]).
  mnem([iLdC(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) where (Lt1,LtNo) .= findLit(Lts,V) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,[Code..,intgr(17),intgr(LtNo)]).
  mnem([iLdA(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(18),intgr(V)]).
  mnem([iLdL(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(19),intgr(V)]).
  mnem([iStL(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(20),intgr(V)]).
  mnem([iStV(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(21),intgr(V)]).
  mnem([iTL(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(22),intgr(V)]).
  mnem([iStA(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(23),intgr(V)]).
  mnem([iStG(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(24),strg(V)]).
  mnem([iCLbl(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(25),intgr(Tgt-Pc-3)]).
  mnem([iCVd,..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,[Code..,intgr(26)]).
  mnem([iNth(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(27),intgr(V)]).
  mnem([iStNth(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(28),intgr(V)]).
  mnem([iGet(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) where (Lt1,LtNo) .= findLit(Lts,V) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,[Code..,intgr(29),intgr(LtNo)]).
  mnem([iSet(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) where (Lt1,LtNo) .= findLit(Lts,V) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,[Code..,intgr(30),intgr(LtNo)]).
  mnem([iCase(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(31),intgr(V)]).
  mnem([iAlloc(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) where (Lt1,LtNo) .= findLit(Lts,V) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,[Code..,intgr(32),intgr(LtNo)]).
  mnem([iCmp(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(33),intgr(Tgt-Pc-3)]).
  mnem([iBf(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(34),intgr(Tgt-Pc-3)]).
  mnem([iBt(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(35),intgr(Tgt-Pc-3)]).
  mnem([iFrame(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(36),intgr(V)]).
  mnem([iThrow(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(37),intgr(Tgt-Pc-3)]).
  mnem([iUnwind(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,[Code..,intgr(38),intgr(Tgt-Pc-3)]).
  mnem([idLine(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) where (Lt1,LtNo) .= findLit(Lts,V) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,[Code..,intgr(39),intgr(LtNo)]).
  mnem([idBug,..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,[Code..,intgr(40)]).
  mnem([idBreak,..Ins],Lbls,Lts,Lns,Lcs,Pc,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,[Code..,intgr(41)]).


  genLblTbl:(list[assemOp],integer,map[string,integer]) => map[string,integer].
  genLblTbl([],_,Lbls) => Lbls.
  genLblTbl([iLbl(Lbl),..Ins],Pc,Lbls) =>
    genLblTbl(Ins,Pc,Lbls[Lbl->Pc]).
  genLblTbl([iLocal(_,_,_,_),..Ins],Pc,Lbls) =>
    genLblTbl(Ins,Pc,Lbls).
  genLblTbl([iLine(T),..Ins],Pc,Lbs) => genLblTbl([idLine(T),..Ins],Pc,Lbs).
  genLblTbl([iHalt,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iNop,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iCall(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iOCall(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iEscape(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTail(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iOTail(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iAbort,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iRet,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iJmp(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iDrop,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iDup,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iPull(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iRot(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iRst(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdV,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iLdG(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdC(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdA(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iLdL(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStL(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStV(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTL(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStA(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStG(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCLbl(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCVd,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iNth(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iStNth(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iGet(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iSet(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCase(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iAlloc(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCmp(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iBf(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iBt(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iFrame(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iThrow(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iUnwind(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([idLine(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([idBug,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([idBreak,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).


  findLit:(map[term,integer],term) => (map[term,integer],integer).
  findLit(Lts,T) where O ^= Lts[T] => (Lts,O).
  findLit(Lts,T) where O .= size(Lts) => (Lts[T->O],O).

  litTbl:(map[term,integer]) => term.
  litTbl(Lts) => mkTpl(sort(Lts::list[keyval[term,integer]],((T1->Ix1), (T2->Ix2)) => Ix1<Ix2)//(K->_)=>K).

  sortLines:(map[term,integer]) => list[term].
  sortLines(Lns) => (sort(Lns::list[keyval[term,integer]],compLine)//(K->_)=>K).

  compLine:(keyval[term,integer],keyval[term,integer])=>boolean.
  compLine(T1->P1,T2->P2) => P1<P2.


  public implementation display[codeSegment] => {.
    disp(method(Nm,Sig,_Lx,Ins)) => ssSeq([disp(Nm),ss(":"),ss(Sig),ss("\n"),..showMnem(Ins,0,[])]).
  .}

  showMnem:(list[assemOp],integer,list[ss]) => list[ss].
  showMnem([],_,Out) => Out.
  showMnem([iLbl(Lb),..Ins],Pc,Out) => showMnem(Ins,Pc,[Out..,ss(Lb),ss(":\n")]).
  showMnem([iLocal(Nm,Frm,End,_Off),..Ins],Pc,Out) => showMnem(Ins,Pc,[Out..,ss(Nm),ss("::"),disp(Frm),ss("-"),disp(End),ss("\n")]).
  showMnem([iHalt,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("Halt"),ss("\n")]).
  showMnem([iNop,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("Nop"),ss("\n")]).
  showMnem([iCall(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Call"),disp(XX),ss("\n")]).
  showMnem([iOCall(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("OCall"),disp(XX),ss("\n")]).
  showMnem([iEscape(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Escape"),ss(XX),ss("\n")]).
  showMnem([iTail(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Tail"),disp(XX),ss("\n")]).
  showMnem([iOTail(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("OTail"),disp(XX),ss("\n")]).
  showMnem([iAbort,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("Abort"),ss("\n")]).
  showMnem([iRet,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("Ret"),ss("\n")]).
  showMnem([iJmp(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Jmp"),disp(XX),ss("\n")]).
  showMnem([iDrop,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("Drop"),ss("\n")]).
  showMnem([iDup,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("Dup"),ss("\n")]).
  showMnem([iPull(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Pull"),disp(XX),ss("\n")]).
  showMnem([iRot(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Rot"),disp(XX),ss("\n")]).
  showMnem([iRst(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Rst"),disp(XX),ss("\n")]).
  showMnem([iLdV,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("LdV"),ss("\n")]).
  showMnem([iLdG(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("LdG"),disp(XX),ss("\n")]).
  showMnem([iLdC(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("LdC"),disp(XX),ss("\n")]).
  showMnem([iLdA(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("LdA"),disp(XX),ss("\n")]).
  showMnem([iLdL(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("LdL"),disp(XX),ss("\n")]).
  showMnem([iStL(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("StL"),disp(XX),ss("\n")]).
  showMnem([iStV(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("StV"),disp(XX),ss("\n")]).
  showMnem([iTL(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("TL"),disp(XX),ss("\n")]).
  showMnem([iStA(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("StA"),disp(XX),ss("\n")]).
  showMnem([iStG(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("StG"),disp(XX),ss("\n")]).
  showMnem([iCLbl(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("CLbl"),disp(XX),ss("\n")]).
  showMnem([iCVd,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("CVd"),ss("\n")]).
  showMnem([iNth(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Nth"),disp(XX),ss("\n")]).
  showMnem([iStNth(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("StNth"),disp(XX),ss("\n")]).
  showMnem([iGet(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Get"),disp(XX),ss("\n")]).
  showMnem([iSet(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Set"),disp(XX),ss("\n")]).
  showMnem([iCase(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Case"),disp(XX),ss("\n")]).
  showMnem([iAlloc(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Alloc"),disp(XX),ss("\n")]).
  showMnem([iCmp(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Cmp"),disp(XX),ss("\n")]).
  showMnem([iBf(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Bf"),disp(XX),ss("\n")]).
  showMnem([iBt(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Bt"),disp(XX),ss("\n")]).
  showMnem([iFrame(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Frame"),disp(XX),ss("\n")]).
  showMnem([iThrow(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Throw"),disp(XX),ss("\n")]).
  showMnem([iUnwind(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Unwind"),disp(XX),ss("\n")]).
  showMnem([idLine(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("dLine"),disp(XX),ss("\n")]).
  showMnem([idBug,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("dBug"),ss("\n")]).
  showMnem([idBreak,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("dBreak"),ss("\n")]).

}
