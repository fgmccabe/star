star.compiler.peephole{
  import star.
  import star.pkg.

  import star.compiler.assem.
  import star.compiler.errors.
  import star.compiler.misc.

  import star.compiler.location.
  import star.compiler.data.

  implementation equality[assemLbl] => {
    .al(L1) == .al(L2) => L1==L2.
  }

  implementation hashable[assemLbl] => {
    hash(.al(L)) => hash(L).
  }

  public peepOptimize:(cons[assemOp])=>cons[assemOp].
  peepOptimize(Ins) => valof{
    InsJ = peep(pullJumps(Ins));
    valis peep(deleteUnused(.false, InsJ, findLblUsages(InsJ,{})));
  }

  labelMap ~> map[assemLbl,(boolean,integer)].

  pullJumps(Ins) =>
    pullJmps(Ins,findTgts(Ins,{})).

  pullJmps([],_) => [].
  pullJmps([.iJmp(Lbl),..Ins],Map) =>
    pullJump(Lbl,Map,Ins).
  pullJmps([I,..Ins],Map) =>
    [I,..pullJmps(Ins,Map)].

  pullJump(Lbl,Map,Ins) where TIns ^= Map[Lbl] =>
    pickupIns(TIns,Lbl,Map,Ins).
  pullJump(Lbl,Map,Ins) =>
    [.iJmp(Lbl),..pullJmps(Ins,Map)].

  pickupIns:(cons[assemOp],assemLbl,map[assemLbl,cons[assemOp]],cons[assemOp])=>cons[assemOp].
  pickupIns([.iRet,.._],_,Map,Ins) =>
    [.iRet,.iNop,.iNop,..pullJmps(Ins,Map)]. -- extra nops will be deleted
  pickupIns([.iRetX,.._],_,Map,Ins) =>
    [.iRetX,.iNop,.iNop,..pullJmps(Ins,Map)].
  pickupIns([.iRtG,.._],_,Map,Ins) =>
    [.iRtG,.iNop,.iNop,..pullJmps(Ins,Map)].
  pickupIns([.iJmp(L2),.._],_,Map,Ins) =>
    pullJmps([.iJmp(L2),..Ins],Map).
  pickupIns(_,Lbl,Map,Ins) =>
    [.iJmp(Lbl),..pullJmps(Ins,Map)].

  findLblUsages([],Lbls) => Lbls.
  findLblUsages([I,..Ins],Lbls) =>
    findLblUsages(Ins,lblUsage(I,addLbl,Lbls)).

  lblUsage:(assemOp, (assemLbl,labelMap)=>labelMap, labelMap)=>labelMap.
  lblUsage(Op,H,Lbs) => case Op in {
    .iJmp(Lbl) => H(Lbl,Lbs).
    .iCLbl(_,Lbl) => H(Lbl,Lbs).
    .iIf(Lbl) => H(Lbl,Lbs).
    .iIfNot(Lbl) => H(Lbl,Lbs).
    .iUnpack(_,Lbl) => H(Lbl,Lbs).
    .iFCmp(Lbl) => H(Lbl,Lbs).
    .iICmp(Lbl) => H(Lbl,Lbs).
    .iCall(_,Lbl) => H(Lbl,Lbs).
    .iOCall(_,Lbl) => H(Lbl,Lbs).
    .iEscape(_,Lbl) => H(Lbl,Lbs).
    .iLdG(_,Lbl) => H(Lbl,Lbs).
    .iLocal(_,St,En,_) => softAdd(En,softAdd(St,Lbs)).
    _ default => Lbs
  }

  addLbl:(assemLbl,labelMap)=>labelMap.
  addLbl(Lbl,Lbs) where (L,Cnt) ^= Lbs[Lbl] => Lbs[Lbl->(L,Cnt+1)].
  addLbl(Lbl,Lbs) => Lbs[Lbl->(.false,1)].

  softAdd(Lb,Lbs) where (_,Cnt) ^= Lbs[Lb] => Lbs[Lb->(.true,Cnt)].
  softAdd(Lb,Lbs) => Lbs[Lb->(.true,0)].

  deleteUnused:(boolean,cons[assemOp],labelMap)=>cons[assemOp].
  deleteUnused(_,[],_) => [].
  deleteUnused(F,[I,..Ins],Lbs) => case I in {
    .iLbl(Lb) => valof{
      if F then{
	case Lbs[Lb] in {
	  .some(Rslt) => {
	    if (.true,0).=Rslt then
	      valis [.iLbl(Lb),..dropUntilLbl(.true,Ins,Lbs)]
	    else
	    valis [.iLbl(Lb),..deleteUnused(.false,Ins,Lbs)]
	  }.
	  .none => valis dropUntilLbl(.true,Ins,Lbs).
	}
      }
      else if _^=Lbs[Lb] then
	valis [.iLbl(Lb),..deleteUnused(F,Ins,Lbs)]
      else
	valis deleteUnused(F,Ins,Lbs)
    }.
    .iCase(Ar) => valof{
      (Hd,Tl) = copyN(Ins,Ar);
      valis [iCase(Ar),..(Hd++deleteUnused(F,Tl,Lbs))]
    }.
    .iIndxJmp(Ar) => valof{
      (Hd,Tl) = copyN(Ins,Ar);
      valis [.iIndxJmp(Ar),..(Hd++deleteUnused(F,Tl,Lbs))]
    }.
    _ default => valof{
      if uncondJmp(I) then
	valis [I,..dropUntilLbl(.true,Ins,Lbs)]
      else
      valis [I,..deleteUnused(F,Ins,Lbs)]
    }
  }
      
  copyN:all e ~~ (cons[e],integer) => (cons[e],cons[e]).
  copyN(L,X) => let{.
    copy([],_,H) => (reverse(H),[]).
    copy(I,0,H) => (reverse(H),I).
    copy([I,..Is],Cx,H) => copy(Is,Cx-1,[I,..H]).
  .} in copy(L,X,[]).

  dropUntilLbl(_,[],_) => [].
  dropUntilLbl(F,[.iLbl(Lb),..Ins],Lbs) =>
    deleteUnused(F,[.iLbl(Lb),..Ins],Lbs).
  dropUntilLbl(.true,[I,..Ins],Lbs) =>
    dropUntilLbl(.true,Ins,lblUsage(I,dropLbl,Lbs)).
  dropUntilLbl(.false,[I,..Ins],Lbs) =>
    [I,..deleteUnused(.false,Ins,Lbs)].

  dropLbl(Lb,Lbs) where (L,Cnt) ^= Lbs[Lb] =>
    ((Cnt>0 || L==.true)?
	Lbs[Lb->(L,Cnt-1)] ||
	Lbs[~Lb]).
  dropLbl(_,Lbs) default => Lbs.

  uncondJmp(Op) => case Op in {
    .iJmp(_) => .true.
    .iRet => .true.
    .iRetX => .true.
    .iRtG => .true.
    .iAbort => .true.
    .iTCall(_) => .true.
    .iTOCall(_) => .true.
    .iCase(_) => .true.
    .iIndxJmp(_) => .true.
    _ default => .false.
  }

  findTgts:(cons[assemOp],map[assemLbl,cons[assemOp]]) => map[assemLbl,cons[assemOp]].
  findTgts([],M) => M.
  findTgts([.iLbl(Lb),..Ins],Tgts) => findTgts(Ins,Tgts[Lb->Ins]).
  findTgts([I,..Ins],Tgts) => findTgts(Ins,Tgts).
  
  -- Low-level optimizations.
  peep:(cons[assemOp])=>cons[assemOp].
  peep([]) => [].
  peep([.iLine(Lc),.iLine(_),..Ins]) => peep([.iLine(Lc),..Ins]).
  peep(Ins) where Inx ^= accessorPtn(Ins) => peep(Inx).
  peep([.iStL(Off),.iLdL(Off),..Ins]) => peep([.iTL(Off),..Ins]).
  peep([.iCall(Fn,Lbl),.iFrame(_),.iRet,..Ins]) =>
    [.iTCall(Fn),..peep(Ins)].
  peep([.iOCall(O,Lbl),.iFrame(_),.iRet,..Ins]) =>
    [.iTOCall(O),..peep(Ins)].
  peep([I,..Ins]) => [I,..peep(Ins)].

  accessorPtn([.iUnpack(Lb,Fl),..Ins]) => valof{
    if (Dx,[.iStL(Off),..Ins1]) .= dropSeq(Ins,[]) &&
	(_,[.iLdL(Off),.iRet,..Inz]) .= dropSeq(Ins1,[]) then{
	  valis some([.iUnpack(Lb,Fl),..Dx++[.iRet,..Inz]])
	}
    else
    valis .none
  }
  accessorPtn(_) default => .none.

  dropSeq([.iDrop,..Ins],Dz) => dropSeq(Ins,[.iDrop,..Dz]).
  dropSeq(Ins,Dz) => (Dz,Ins).
}
