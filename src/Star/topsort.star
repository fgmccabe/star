star.topsort{
  import star.

  public contract all d,t ~~ depends[d->>t] ::= {
    references:(d)=>list[t].
    defined:(d,t)=>boolean.
  }

  public topsort:all d,t ~~ depends[d->>t] |: (list[d]) => list[list[d]].
  topsort(Defs) => let{
    defEntry::= dE{ df:d. stackPt : integer. }

    stTuple ~> (list[d],list[defEntry],list[list[d]],integer).

    analyseDefs([],Grps) => reverse(Grps).
    analyseDefs([Def,..Dfs],Grps) where (NDefs,_,NGrps,_).=analyseDef(Def,[],Dfs,Grps) =>
      analyseDefs(NDefs,NGrps).

    analyseDef(Df,Stk,Dfs,Gps) where Pt.=size(Stk) =>
      popGroups(analyseRefs(references(Df),Dfs,pushDf(Df,Pt,Stk),Gps,Pt),Pt).

    pushDf(Df,Pt,Stk) => [dE{df=Df. stackPt=Pt},..Stk].

    analyse:(t,list[d],list[defEntry],list[list[d]],integer) => stTuple.
    analyse(Ref,Dfs,Stk,Gps,Low) where X^=inStack(Ref,Stk) => (Dfs,Stk,Gps,min(X,Low)).
    analyse(Ref,Dfs,Stk,Gps,Low) where
      (Df,RDefs) ^= pickDef(Ref,Dfs,[]) &&
      (ODefs,OStk,OGps,DfPt) .= analyseDef(Df,Stk,RDefs,Gps) => (ODefs,OStk,OGps,min(Low,DfPt)).
    analyse(_,Dfs,Stk,Gps,Low) default => (Dfs,Stk,Gps,Low).

    analyseRefs([],Dfs,Stk,Gps,Low) => (Dfs,Stk,Gps,Low).
    analyseRefs([Ref,..Refs],Dfs,Stk,Gps,Low) where
      (Df1,Stk1,Gps1,Lw1) .= analyse(Ref,Dfs,Stk,Gps,Low) =>
        analyseRefs(Refs,Df1,Stk1,Gps1,Lw1).

    pickDef:(t,list[d],list[d]) => option[(d,list[d])].
    pickDef(_,[],_) => .none.
    pickDef(Ref,[Df,..Dfs],SoFar) where defined(Df,Ref) => some((Df,SoFar++Dfs)).
    pickDef(Ref,[Df,..Dfs],SoFar) => pickDef(Ref,Dfs,[SoFar..,Df]).

    inStack:(t,list[defEntry])=>option[integer].
    inStack(_,[]) => .none.
    inStack(Ref,[E,..Stk]) where defined(E.df,Ref) => some(E.stackPt).
    inStack(Ref,[_,..Stk]) => inStack(Ref,Stk).

    popGroups((Dfs,Stk,Gps,Low),Pt) where Pt>Low => (Dfs,Stk,Gps,Low).
    popGroups((Dfs,Stk,Gps,Low),Pt) where (NStk,Grp).=popGroup(Stk,Low,[]) =>
      [].=Grp ? (Dfs,NStk,Gps,Low) || (Dfs,NStk,[Grp,..Gps],Low).

    popGroup:(list[defEntry],integer,list[d]) => (list[defEntry],list[d]).
    popGroup([E,..Stk],Low,SoFar) where E.stackPt>=Low => popGroup(Stk,Low,[E.df,..SoFar]).
    popGroup(Stk,_,Grp) => (Stk,Grp)
  } in analyseDefs(Defs,[]).
}
