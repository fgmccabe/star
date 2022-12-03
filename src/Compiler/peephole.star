star.compiler.peephole{
  import star.
  import star.pkg.

  import star.compiler.assem.
  import star.compiler.errors.
  import star.compiler.meta.
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
    Map = splitSegments([.iLbl(al("")),..Ins],[]);
    if traceCodegen! then
      logMsg(makeDotGraph("segs",Map));
    PMap = pullTgts(Map);
    if traceCodegen! then
      logMsg(makeDotGraph("psegs",PMap));
    PC = sequentialize([al("")],PMap);
    valis ^tail(PC)
  }

  uncondJmp(Op) => case Op in {
    .iJmp(_) => .true.
    .iRet => .true.
    .iRetX => .true.
    .iRtG => .true.
    .iAbort => .true.
    .iRetire => .true.
    .iTCall(_) => .true.
    .iTOCall(_) => .true.
    .iCase(_) => .true.
    .iIndxJmp(_) => .true.
    _ default => .false.
  }

  -- Low-level optimizations.
  peep:(cons[assemOp])=>cons[assemOp].
  peep([]) => [].
  peep([.iLine(Lc),.iLine(_),..Ins]) => peep([.iLine(Lc),..Ins]).
  peep(Ins) where Inx ?= accessorPtn(Ins) => peep(Inx).
  peep([.iStL(Off),.iLdL(Off),..Ins]) => peep([.iTL(Off),..Ins]).
  peep([.iCall(Fn,.al("$$")),.iFrame(_),.iRet,..Ins]) =>
    [.iTCall(Fn),..peep(Ins)].
  peep([.iOCall(O,.al("$$")),.iFrame(_),.iRet,..Ins]) =>
    [.iTOCall(O),..peep(Ins)].
  peep([.iRot(0),..Ins]) => peep(Ins).
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

  segment::=.segment(assemLbl,option[assemLbl],cons[assemOp],set[assemLbl]).

  implementation display[segment] => {
    disp(.segment(Lb,FLb,Ins,Exits)) => "segment $(Lb)\:$(Ins) ~> $(FLb)\:$(Exits)"
  }

  splitSegments:(cons[assemOp],map[assemLbl,segment]) => map[assemLbl,segment].
  splitSegments([],Map) => Map.
  splitSegments([.iLbl(Lb),..Code],Map) => splitSegment(Code,Lb,[],[],Map).
  splitSegments([I,..Code],Map) => valof{
    reportTrap("Expecting a label $(I) ..  $(Code)");
    valis splitSegments(Code,Map)
  }

  splitSegment:(cons[assemOp],assemLbl,cons[assemOp],set[assemLbl],map[assemLbl,segment]) => map[assemLbl,segment].
  splitSegment([],Lb,SoFar,Exits,Map) => Map[Lb->.segment(Lb,.none,reverse(SoFar),Exits)].
  splitSegment([.iLbl(XLb),..Code],Lb,SoFar,Exits,Map) =>
    splitSegment(Code,XLb,[],[],Map[Lb->.segment(Lb,?XLb,reverse(SoFar),Exits)]).
  splitSegment([Op,..Code],Lb,SoFar,Exits,Map) => valof{
    Ex = (Tgt?=opTgt(Op) ?? Exits\+Tgt || Exits);

    if (.iIndxJmp(Cnt).=Op || .iCase(Cnt).=Op) && (Front,Rest) ?= front(Code,Cnt) then{
      valis splitSegments(Rest,Map[Lb->.segment(Lb,.none,reverse(SoFar)++[Op,..Front],
	    findExits(Front,Ex))])
    } else if .iJmp(Tgt).=Op && [.iLbl(Tgt),.._].=Code then{
      valis splitSegments(Code,Map[Lb->.segment(Lb,?Tgt,reverse(SoFar),Ex)])
    }
    else if uncondJmp(Op) then
      valis splitSegments(Code,Map[Lb->.segment(Lb,.none,reverse([Op,..SoFar]),Ex)])
    else 
    valis splitSegment(Code,Lb,[Op,..SoFar],Ex,Map)
  }

  findExits:(cons[assemOp],set[assemLbl]) => set[assemLbl].
  findExits(Code,Ex) => foldLeft((O,X) => (TT?=opTgt(O)??X\+TT||X),Ex,Code).

  opTgt(.iCall(_,Lb)) => ?Lb.
  opTgt(.iOCall(_,Lb)) => ?Lb.
  opTgt(.iEscape(_,Lb)) => ?Lb.
  opTgt(.iJmp(Lb)) => ?Lb.
  opTgt(.iLdG(_,Lb)) => ?Lb.
  opTgt(.iCLbl(_,Lb)) => ?Lb.
  opTgt(.iUnpack(_,Lb)) => ?Lb.
  opTgt(.iICmp(Lb)) => ?Lb.
  opTgt(.iFCmp(Lb)) => ?Lb.
  opTgt(.iCmp(Lb)) => ?Lb.
  opTgt(.iIf(Lb)) => ?Lb.
  opTgt(.iIfNot(Lb)) => ?Lb.
  opTgt(_) default => .none.

  reTgt(.iCall(Nm,_),Lb) => .iCall(Nm,Lb).
  reTgt(.iOCall(Ar,_),Lb) => .iOCall(Ar,Lb).
  reTgt(.iEscape(Nm,_),Lb) => .iEscape(Nm,Lb).
  reTgt(.iJmp(_),Lb) => .iJmp(Lb).
  reTgt(.iLdG(G,_),Lb) => .iLdG(G,Lb).
  reTgt(.iCLbl(T,_),Lb) => .iCLbl(T,Lb).
  reTgt(.iUnpack(T,_),Lb) => .iUnpack(T,Lb).
  reTgt(.iICmp(_),Lb) => .iICmp(Lb).
  reTgt(.iFCmp(_),Lb) => .iFCmp(Lb).
  reTgt(.iCmp(_),Lb) => .iCmp(Lb).
  reTgt(.iIf(_),Lb) => .iIf(Lb).
  reTgt(.iIfNot(_),Lb) => .iIfNot(Lb).
  reTgt(O,_) default => O.

  pullTgts:(map[assemLbl,segment]) => map[assemLbl,segment].
  pullTgts(Map) => (Map///(Lbl,Seg)=>pullSegment(Seg,Map)).

  pullSegment:(segment,map[assemLbl,segment]) => segment.
  pullSegment(.segment(Lbl,Flw,Ops,Exits),Map) => valof{
    Nops = peep(pullOps(Ops,Map));
    valis .segment(Lbl,Flw,Nops,findExits(Nops,[]))
  }

  pullOps([],_) => [].
  pullOps([O,..Code],Map) => [pullTgt(O,Map),..pullOps(Code,Map)].

  pullTgt(.iJmp(Tg),Map) where .segment(_,_,[Op,.._],_) ?= Map[Tg] &&
      (.iRet.=Op||.iRetX.=Op||.iRtG.=Op) => Op.
  pullTgt(O,Map) => valof{
    if Tg?=opTgt(O) then{
      if .segment(_,_,[.iJmp(Tgt),.._],_) ?= Map[Tg] then
	valis pullTgt(reTgt(O,Tgt),Map)
      else if .segment(_,?Flw,[],_) ?= Map[Tg] then
	valis pullTgt(reTgt(O,Flw),Map)
      else
      valis O
    }
    else
    valis O
  }.

  sequentialize:(cons[assemLbl],map[assemLbl,segment]) => cons[assemOp].
  sequentialize([],_Map) => [].
  sequentialize([Lbl,..Ls],Map) => valof{
    if .segment(_,Flw,Ops,Exits) ?= Map[Lbl] then{
      if F ?= Flw then{
	if _ ?= Map[F] then
	  valis [.iLbl(Lbl),..Ops]++sequentialize([F,..Ls++(Exits::cons[assemLbl])],Map[~Lbl])
	else
	valis [.iLbl(Lbl),..Ops]++[.iJmp(F)]++sequentialize(Ls++(Exits::cons[assemLbl]),Map[~Lbl])
	
      } else{
	valis [.iLbl(Lbl),..Ops]++sequentialize(Ls++(Exits::cons[assemLbl]),Map[~Lbl])
      }
    }
    else
      valis sequentialize(Ls,Map)
  }
    
  makeDotGraph:(string,map[assemLbl,segment])=>string.
  makeDotGraph(Nm,Map) => "digraph $(Nm) {\n#(ixLeft((_,S,F)=>F++makeSegGraph(S),"",Map))}".

  makeSegGraph(.segment(Lbl,Flw,Code,Exits)) => valof{
    ExNodes = ((Exits::cons[assemLbl])//(Tgt)=>"\"$(Lbl)\" -> \"$(Tgt)\";\n")*;
    Node = "\"$(Lbl)\" [shape=box,label=$(disp(Lbl)++":"++(Code//(I)=>disp(I))*)];\n";
    Follow = (Tgt?=Flw ?? "\"$(Lbl)\" -> \"$(Tgt)\" [ style=dotted, color=red ]" || "");
    valis Node++ExNodes++Follow
  }
}
