/* Automatically generated, do not edit */

:- module(assemble,[assem/2]).
:- use_module(misc).
:- use_module(terms).
:- use_module(encode).

assem([method(Nm,Sig,Lx)|Ins],MTpl) :-
    genLblTbl(Ins,0,[],Lbs),
    findLit([],Nm,_,Ls0),
    mnem(Ins,Lbs,Ls0,Lts,[],Lcs,0,Cde),
    mkInsTpl(Cde,Code),
    mkLitTpl(Lts,LtTpl),
    mkTpl(Lcs,LcsTpl),
    mkTpl([Nm,strg(Sig),intgr(Lx),Code,LtTpl,LcsTpl],MTpl).

mnem([],_,Lt,Lt,Lc,Lc,_,[]).
mnem([iLbl(_)|Ins],Lbs,Lt,Lts,Lc,Lcx,Pc,Code) :- mnem(Ins,Lbs,Lt,Lts,Lc,Lcx,Pc,Code).
mnem([iLocal(Nm,Frm,End,Off)|Ins],Lbs,Lt,Lts,Lc,Lcx,Pc,Code) :-
    findLbl(Frm,Lbs,F),
    findLbl(End,Lbs,T),
    mkTpl([strg(Nm),intgr(F),intgr(T),intgr(Off)],Entry),
    (is_member(Entry,Lc)->Lc0=Lc;Lc0=[Entry|Lc]),
    mnem(Ins,Lbs,Lt,Lts,Lc0,Lcx,Pc,Code).
mnem([iHalt|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[0|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[1,LtNo|M]) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Pc1,M).
mnem([iOCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[2,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iEscape(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[3,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iTail(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[4,LtNo|M]) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Pc1,M).
mnem([iOTail(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[5,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iRet|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[6|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iJmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[7,Off|M]) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iDrop|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[8|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iDup|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[9|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iPull(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[10,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iRot(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[11,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iRst(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[12,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iLdG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[13,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iLdC(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[14,LtNo|M]) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Pc1,M).
mnem([iLdA(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[15,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iLdL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[16,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iStL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[17,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iTL(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[18,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iStA(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[19,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iStG(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[20,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iCLbl(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[21,Off|M]) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[22,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iStNth(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[23,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iCase(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[24,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iAlloc(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[25,LtNo|M]) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Pc1,M).
mnem([iCmp(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[26,Off|M]) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iBf(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[27,Off|M]) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iBt(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[28,Off|M]) :- Pc1 is Pc+3,
      findLbl(V,Lbls,Tgt),
      pcGap(Pc1,Tgt,Off),
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iFrame(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[29,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iDLine(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[30,LtNo|M]) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Pc1,M).
mnem([iDCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[31,LtNo|M]) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Pc1,M).
mnem([iDOCall(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[32,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iDTail(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[33,LtNo|M]) :- Pc1 is Pc+3,
      findLit(Lt,V,LtNo,Lt1),
      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Pc1,M).
mnem([iDOTail(V)|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[34,V|M]) :- Pc1 is Pc+3,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).
mnem([iDRet|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[35|M]) :- Pc1 is Pc+1,
      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).

genLblTbl([],_,Lbls,Lbls).
genLblTbl([iLbl(Lbl)|Ins],Pc,Lbls,Lbx) :- genLblTbl(Ins,Pc,[(Lbl,Pc)|Lbls],Lbx).
genLblTbl([iLocal(_,_,_,_)|Ins],Pc,Lbls,Lbx) :- genLblTbl(Ins,Pc,Lbls,Lbx).
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
genLblTbl([iPull(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iRot(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iRst(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iLdG(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iLdC(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iLdA(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iLdL(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iStL(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iTL(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iStA(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iStG(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iCLbl(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iNth(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iStNth(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iCase(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iAlloc(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iCmp(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iBf(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iBt(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iFrame(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iDLine(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iDCall(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iDOCall(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iDTail(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iDOTail(_)|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+3,  genLblTbl(Ins,Pc1,Lbls,Lbx).
genLblTbl([iDRet|Ins],Pc,Lbls,Lbx) :- !, Pc1 is Pc+1,  genLblTbl(Ins,Pc1,Lbls,Lbx).
findLbl(L,Lbs,Tgt) :- is_member((L,Tgt),Lbs),!.

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

