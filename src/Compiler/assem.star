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
    iHalt |
    iCall(termLbl) |
    iOCall(integer) |
    iEscape(string) |
    iTail(termLbl) |
    iOTail(integer) |
    iRet |
    iJmp(assemLbl) |
    iDrop |
    iDup |
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
    iTG(string) |
    iCLbl(assemLbl) |
    iNth(integer) |
    iStNth(integer) |
    iGet(termLbl) |
    iSet(termLbl) |
    iCase(integer) |
    iAlloc(termLbl) |
    iCmp(assemLbl) |
    iBf(assemLbl) |
    iBt(assemLbl) |
    iFrame(integer) |
    iThrow(assemLbl) |
    iUnwind(assemLbl) |
    idLine(term) |
    idBug |
    idBreak |

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
  mnem([iHalt,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(0)]).
  mnem([iCall(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,enum(V)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(1),intgr(LtNo)]).
  mnem([iOCall(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(2),intgr(V)]).
  mnem([iEscape(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(3),strg(V)]).
  mnem([iTail(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,enum(V)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(4),intgr(LtNo)]).
  mnem([iOTail(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(5),intgr(V)]).
  mnem([iRet,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(6)]).
  mnem([iJmp(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(7),intgr(Tgt-Pc-3)]).
  mnem([iDrop,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(8)]).
  mnem([iDup,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(9)]).
  mnem([iRst(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(10),intgr(V)]).
  mnem([iLdV,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(11)]).
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
  mnem([iAlloc(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,enum(V)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(28),intgr(LtNo)]).
  mnem([iCmp(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(29),intgr(Tgt-Pc-3)]).
  mnem([iBf(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(30),intgr(Tgt-Pc-3)]).
  mnem([iBt(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(31),intgr(Tgt-Pc-3)]).
  mnem([iFrame(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(32),intgr(V)]).
  mnem([iThrow(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(33),intgr(Tgt-Pc-3)]).
  mnem([iUnwind(al(V)),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(34),intgr(Tgt-Pc-3)]).
  mnem([idLine(V),..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) where (Lt1,LtNo) .= findLit(Lts,V) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[Code..,intgr(35),intgr(LtNo)]).
  mnem([idBug,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(36)]).
  mnem([idBreak,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[Code..,intgr(37)]).


  genLblTbl:(list[assemOp],integer,map[string,integer]) => map[string,integer].
  genLblTbl([],_,Lbls) => Lbls.
  genLblTbl([iLbl(al(Lbl)),..Ins],Pc,Lbls) =>
    genLblTbl(Ins,Pc,Lbls[Lbl->Pc]).
  genLblTbl([iLocal(_,_,_,_),..Ins],Pc,Lbls) =>
    genLblTbl(Ins,Pc,Lbls).
  genLblTbl([iLine(T),..Ins],Pc,Lbs) => genLblTbl([idLine(T),..Ins],Pc,Lbs).
  genLblTbl([iHalt,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iCall(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iOCall(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iEscape(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iTail(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iOTail(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iRet,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iJmp(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iDrop,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
  genLblTbl([iDup,..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+1,Lbls).
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
  genLblTbl([iTG(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
  genLblTbl([iCLbl(_),..Ins],Pc,Lbls) => genLblTbl(Ins,Pc+3,Lbls).
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
    disp(method(Nm,Sig,Ins)) => ssSeq([disp(Nm),ss(":"),disp(Sig),ss("\n"),..showMnem(Ins,0,[])]).
    disp(global(Nm,Sig,Ins)) => ssSeq([ss("global "),disp(Nm),ss(":"),disp(Sig),ss("\n"),..showMnem(Ins,0,[])]).
  .}

  implementation display[assemLbl] => {.
    disp(al(Nm)) => ss(Nm).
  .}

  showMnem:(list[assemOp],integer,list[ss]) => list[ss].
  showMnem([],_,Out) => Out.
  showMnem([iLbl(al(Lb)),..Ins],Pc,Out) => showMnem(Ins,Pc,[Out..,ss(Lb),ss(":\n")]).
  showMnem([iLocal(Nm,Frm,End,_Off),..Ins],Pc,Out) => showMnem(Ins,Pc,[Out..,ss(Nm),ss("::"),disp(Frm),ss("-"),disp(End),ss("\n")]).
  showMnem([iHalt,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("Halt"),ss("\n")]).
  showMnem([iCall(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Call"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iOCall(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("OCall"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iEscape(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Escape"),ss(" "),ss(XX),ss("\n")]).
  showMnem([iTail(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Tail"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iOTail(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("OTail"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iRet,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("Ret"),ss("\n")]).
  showMnem([iJmp(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Jmp"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iDrop,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("Drop"),ss("\n")]).
  showMnem([iDup,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("Dup"),ss("\n")]).
  showMnem([iRst(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Rst"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iLdV,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("LdV"),ss("\n")]).
  showMnem([iLdG(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("LdG"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iLdC(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("LdC"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iLdA(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("LdA"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iLdL(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("LdL"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iStL(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("StL"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iStV(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("StV"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iTL(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("TL"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iStA(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("StA"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iStG(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("StG"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iTG(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("TG"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iCLbl(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("CLbl"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iNth(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Nth"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iStNth(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("StNth"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iGet(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Get"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iSet(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Set"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iCase(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Case"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iAlloc(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Alloc"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iCmp(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Cmp"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iBf(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Bf"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iBt(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Bt"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iFrame(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Frame"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iThrow(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Throw"),ss(" "),disp(XX),ss("\n")]).
  showMnem([iUnwind(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("Unwind"),ss(" "),disp(XX),ss("\n")]).
  showMnem([idLine(XX),..Ins],Pc,Out) => showMnem(Ins,Pc+3,[Out..,disp(Pc),ss(":"),ss("dLine"),ss(" "),disp(XX),ss("\n")]).
  showMnem([idBug,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("dBug"),ss("\n")]).
  showMnem([idBreak,..Ins],Pc,Out) => showMnem(Ins,Pc+1,[Out..,disp(Pc),ss(":"),ss("dBreak"),ss("\n")]).

}
