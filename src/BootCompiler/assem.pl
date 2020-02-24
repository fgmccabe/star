/* Automatically generated, do not edit */

:- module(assemble,[assem/2, showIns/3, dispIns/1]).
:- use_module(misc).
:- use_module(lterms).
:- use_module(encode).

assem(method(Nm,Sig,Lx,Ins),MTpl) :-
    genLblTbl(Ins,0,labels{},Lbs),
    findLit([],Nm,_,Ls0),
    mnem(Ins,Lbs,Ls0,Lts,[],Lcs,[],Lines,0,Cde),
    mkInsTpl(Cde,Code),
    mkLitTpl(Lts,LtTpl),
    mkTpl(Lcs,LcsTpl),
    sortLines(Lines,SLines),
    mkTpl(SLines,LnsTpl),
    mkTpl([Nm,strg(Sig),intgr(Lx),Code,LtTpl,LcsTpl,LnsTpl],MTpl).
assem(struct(Lbl,Sig,Fields),Tpl) :-
    mkTpl(Fields,FieldSigs),
    mkTpl([Lbl,Sig,FieldSigs],Tpl).

mnem([],_,Lt,Lt,Lc,Lc,Lns,Lns,_,[]).
mnem([iLbl(_)|Ins],Lbs,Lt,Lts,Lc,Lcx,Ln,Lnx,Pc,Code) :- mnem(Ins,Lbs,Lt,Lts,Lc,Lcx,Ln,Lnx,Pc,Code).
mnem([iLocal(Nm,Frm,End,Off)|Ins],Lbs,Lt,Lts,Lc,Lcx,Ln,Lnx,Pc,Code) :-
    findLbl(Frm,Lbs,F),
    findLbl(End,Lbs,T),
    mkTpl([strg(Nm),intgr(F),intgr(T),intgr(Off)],Entry),
    (is_member(Entry,Lc)->Lc0=Lc;Lc0=[Entry|Lc]),
    mnem(Ins,Lbs,Lt,Lts,Lc0,Lcx,Ln,Lnx,Pc,Code).
mnem([iLine(Loc)|Ins],Lbs,Lt,Lts,Lc,Lcx,Lns,Lnx,Pc,Code) :-
    mkTpl([Loc,intgr(Pc)],LneEntry),
    (is_member(LneEntry,Lns) -> Lns1 = Lns; Lns1=[LneEntry|Lns]),
    mnem([iDLine(Loc)|Ins],Lbs,Lt,Lts,Lc,Lcx,Lns1,Lnx,Pc,Code).
mnem([iHalt|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[0|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[1,LtNo|M]) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iOCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[2,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iEscape(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[3,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iTail(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[4,LtNo|M]) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iOTail(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[5,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iRet|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[6|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iJmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[7,Off|M]) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iDrop|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[8|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iDup|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[9|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iRst(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[10,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iLdV|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[11|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iLdG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[12,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iLdC(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[13,LtNo|M]) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iLdA(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[14,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iLdL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[15,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iStL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[16,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iStV(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[17,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iTL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[18,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iStA(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[19,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iStG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[20,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iTG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[21,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iCLbl(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[22,Off|M]) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[23,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iStNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[24,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iGet(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[25,LtNo|M]) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iSet(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[26,LtNo|M]) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iCase(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[27,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iIAdd|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[28|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iISub|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[29|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iIMul|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[30|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iIDiv|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[31|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iIMod|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[32|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iIAbs|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[33|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iIEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[34|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iILt|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[35|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iIGe|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[36|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iIAnd|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[37|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iIOr|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[38|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iIXor|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[39|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iLsl|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[40|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iLsr|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[41|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iAsr|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[42|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iINot|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[43|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iFAdd|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[44|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iFSub|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[45|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iFMul|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[46|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iFDiv|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[47|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iFMod|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[48|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iFAbs|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[49|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iFEq|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[50|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iFLt|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[51|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iFGe|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[52|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iAlloc(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[53,LtNo|M]) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[54,Off|M]) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iBf(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[55,Off|M]) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iBt(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[56,Off|M]) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iFrame(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[57,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iThrow(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[58,Off|M]) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iUnwind(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[59,Off|M]) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iDLine(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[60,LtNo|M]) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iDBug|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[61|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).
mnem([iDBreak|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[62|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).


makeKey(Id,Key) :-
  atom_string(Key,Id).

findLbl(L,Lbs,Tgt) :-
  makeKey(L,Ky),
  get_dict(Ky,Lbs,Tgt),!.

defineLbl(Lbl,Pc,Lbls,Lblx) :-
  makeKey(Lbl,Key),
  put_dict(Key,Lbls,Pc,Lblx).

genLblTbl([],_,Lbls,Lbls).
genLblTbl([iLbl(Lbl)|Ins],Pc,Lbls,Lbx) :-
  defineLbl(Lbl,Pc,Lbls,Lbli),
  genLblTbl(Ins,Pc,Lbli,Lbx).
genLblTbl([iLocal(_,_,_,_)|Ins],Pc,Lbls,Lbx) :- genLblTbl(Ins,Pc,Lbls,Lbx).
genLblTbl([iLine(Lc)|Ins],Pc,Lbls,Lbx) :- genLblTbl([iDLine(Lc)|Ins],Pc,Lbls,Lbx).
genLblTbl([iHalt|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iCall(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iOCall(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iEscape(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iTail(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iOTail(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iRet|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iJmp(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iDrop|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iDup|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iRst(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iLdV|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iLdG(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iLdC(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iLdA(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iLdL(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iStL(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iStV(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iTL(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iStA(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iStG(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iTG(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iCLbl(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iNth(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iStNth(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iGet(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iSet(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iCase(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iIAdd|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iISub|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iIMul|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iIDiv|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iIMod|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iIAbs|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iIEq|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iILt|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iIGe|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iIAnd|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iIOr|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iIXor|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iLsl|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iLsr|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iAsr|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iINot|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iFAdd|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iFSub|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iFMul|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iFDiv|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iFMod|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iFAbs|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iFEq|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iFLt|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iFGe|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iAlloc(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iCmp(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iBf(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iBt(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iFrame(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iThrow(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iUnwind(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iDLine(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iDBug|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iDBreak|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).


pcGap(Pc,Tgt,Off) :- Off is Tgt-Pc.

findLit(Lits,V,LtNo,Lits) :- is_member((V,LtNo),Lits),!.
findLit(Lits,V,LtNo,[(V,LtNo)|Lits]) :- length(Lits,LtNo).

mkLitTpl(Lits,Tpl) :-
    reverse(Lits,RLit),
    project0(RLit,Els),
    mkTpl(Els,Tpl).

mkInsTpl(Is,Tpl) :-
    map(Is,assemble:mkIns,Ins),
    mkTpl(Ins,Tpl).

mkIns((O,A),Tpl) :-
    wrap(A,WA),
    mkTpl([intgr(O),WA],Tpl).
mkIns(O,intgr(O)) :- number(O).
mkIns(S,strg(S)) :- string(S).

sortLines(Lns,Sorted) :-
 sort(Lns,assemble:compLine,Sorted).

compLine(ctpl(_,[_,intgr(Pc1)]),ctpl(_,[_,intgr(Pc2)])) :- Pc1<Pc2.

dispIns(Prog) :-
  showIns(Prog,O,[]),
  string_chars(Txt,O),
  writeln(Txt).

showIns([method(Nm,Sig,_Lx)|Ins],O,Ox) :-
  showTerm(Nm,0,O,O1),
  appStr(":",O1,O2),
  appStr(Sig,O2,O3),
  appNl(O3,O4),
  showMnem(Ins,0,[],O4,Ox).

showMnem([],_,_,Ox,Ox).
showMnem([iLbl(Lb)|Ins],Pc,Lbs,O,Ox) :-
  appStr(Lb,O,O1),
  appStr(":",O1,O2),
  appNl(O2,O3),
  showMnem(Ins,Pc,[(Lb,Pc)|Lbs],O3,Ox).
showMnem([iLocal(Nm,Frm,End,_Off)|Ins],Pc,Lbs,O,Ox) :-
  appStr(Nm,O,O1),
  appStr("::",O1,O2),
  appStr(Frm,O2,O3),
  appStr("-",O3,O4),
  appStr(End,O4,O5),
  appNl(O5,O6),
  showMnem(Ins,Pc,Lbs,O6,Ox).
showMnem([iLine(Loc)|Ins],Pc,Lbs,O,Ox) :-
  appStr("Line: ",O,O1),
  showTerm(Loc,0,O1,O2),
  appNl(O2,O3),
  showMnem(Ins,Pc,Lbs,O3,Ox).
showMnem([iHalt|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Halt ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iCall(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Call ",O00,O1),
  showTerm(XX,0,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iOCall(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("OCall ",O00,O1),
  appInt(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iEscape(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Escape ",O00,O1),
  appStr(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iTail(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Tail ",O00,O1),
  showTerm(XX,0,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iOTail(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("OTail ",O00,O1),
  appInt(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iRet|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Ret ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iJmp(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Jmp ",O00,O1),
  appStr(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iDrop|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Drop ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iDup|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Dup ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iRst(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Rst ",O00,O1),
  appInt(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iLdV|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("LdV ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iLdG(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("LdG ",O00,O1),
  appStr(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iLdC(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("LdC ",O00,O1),
  showTerm(XX,0,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iLdA(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("LdA ",O00,O1),
  appInt(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iLdL(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("LdL ",O00,O1),
  appStr(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iStL(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("StL ",O00,O1),
  appStr(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iStV(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("StV ",O00,O1),
  appStr(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iTL(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("TL ",O00,O1),
  appStr(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iStA(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("StA ",O00,O1),
  appInt(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iStG(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("StG ",O00,O1),
  appStr(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iTG(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("TG ",O00,O1),
  appStr(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iCLbl(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("CLbl ",O00,O1),
  appStr(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iNth(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Nth ",O00,O1),
  appInt(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iStNth(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("StNth ",O00,O1),
  appInt(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iGet(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Get ",O00,O1),
  showTerm(XX,0,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iSet(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Set ",O00,O1),
  showTerm(XX,0,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iCase(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Case ",O00,O1),
  appInt(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iIAdd|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("IAdd ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iISub|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("ISub ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iIMul|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("IMul ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iIDiv|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("IDiv ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iIMod|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("IMod ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iIAbs|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("IAbs ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iIEq|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("IEq ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iILt|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("ILt ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iIGe|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("IGe ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iIAnd|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("IAnd ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iIOr|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("IOr ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iIXor|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("IXor ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iLsl|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Lsl ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iLsr|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Lsr ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iAsr|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Asr ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iINot|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("INot ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iFAdd|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("FAdd ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iFSub|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("FSub ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iFMul|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("FMul ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iFDiv|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("FDiv ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iFMod|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("FMod ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iFAbs|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("FAbs ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iFEq|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("FEq ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iFLt|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("FLt ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iFGe|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("FGe ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iAlloc(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Alloc ",O00,O1),
  showTerm(XX,0,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iCmp(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Cmp ",O00,O1),
  appStr(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iBf(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Bf ",O00,O1),
  appStr(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iBt(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Bt ",O00,O1),
  appStr(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iFrame(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Frame ",O00,O1),
  appInt(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iThrow(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Throw ",O00,O1),
  appStr(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iUnwind(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("Unwind ",O00,O1),
  appStr(XX,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iDLine(XX)|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("dLine ",O00,O1),
  showTerm(XX,0,O1,O2),
  appNl(O2,O3),
  Pc1 is Pc+3,
  showMnem(Ins,Pc1,Lbls,O3,Ox).
showMnem([iDBug|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("dBug ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).
showMnem([iDBreak|Ins],Pc,Lbls,O,Ox) :- !,
  appInt(Pc,O,O0),
  appStr(":",O0,O00),
  appStr("dBreak ",O00,O1),
  appNl(O1,O2),
  Pc1 is Pc+1,
  showMnem(Ins,Pc1,Lbls,O2,Ox).

