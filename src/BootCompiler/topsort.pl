:-module(topsort,[topsort/3]).
:-use_module(misc).

topsort(Defs,Groups,IsDef) :-
  analyseDefs(Defs,[],Grps,IsDef),!,
  reverse(Grps,Groups).

analyseDefs([],Groups,Groups,_).
analyseDefs([Def|Defs],Groups,OGroups,IsDef) :-
  analyseDef(Def,Defs,IDefs,[],_,Groups,G0,_,IsDef),
  analyseDefs(IDefs,G0,OGroups,IsDef).

analyseDef((Defines,Refs,Lc,Df),Defs,ODefs,Stack,OStack,G,OG,Pt,IsDef) :-
  pushDef((Defines,Refs,Lc,Df),Stack,S0,SPt),
  analyseRefs(Refs,Defs,ODefs,S0,S1,G,G0,SPt,Pt,IsDef),
  popGroups(S1,OStack,G0,OG,SPt,Pt).

analyseRefs([],Defs,Defs,Stk,Stk,G,G,Low,Low,_).
analyseRefs([Ref|Refs],Defs,ODefs,Stk,OStk,G,OG,Low,Pt,IsDef) :-
  analyse(Ref,Defs,ID,Stk,S1,G,G1,Low,Low1,IsDef),
  analyseRefs(Refs,ID,ODefs,S1,OStk,G1,OG,Low1,Pt,IsDef).

analyse(Ref,Defs,Defs,Stack,Stack,G,G,Low,Pt,IsDef) :- inStack(Ref,Stack,X,IsDef), minPoint(X,Low,Pt).
analyse(Ref,Defs,ODefs,Stack,OStack,G,OG,Low,Pt,IsDef) :-
  pickDef(Ref,Defs,RDefs,Df,IsDef),
  analyseDef(Df,RDefs,ODefs,Stack,OStack,G,OG,DfPt,IsDef),
  minPoint(Low,DfPt,Pt).
analyse(_,Defs,Defs,Stack,Stack,Groups,Groups,Low,Low,_).

pushDef((Defines,Refs,Lc,Def),Stack,[(Defines,Refs,Lc,Def,Sz)|Stack],Sz) :- length(Stack,Sz).

inStack(Ref,[(Rf,_,_,_,X)|_],X,IsDef) :- call(IsDef,Rf,Ref),!.
inStack(Ref,[_|Stack],X,IsDef) :- inStack(Ref,Stack,X,IsDef).

pickDef(N,[(Nm,Refs,Lc,Df)|Defs],Defs,(Nm,Refs,Lc,Df),IsDef) :- call(IsDef,N,Nm).
pickDef(Nm,[D|Defs],[D|ODefs],Df,IsDef) :- pickDef(Nm,Defs,ODefs,Df,IsDef).

popGroups(Stack,Stack,Groups,Groups,Low,Pt) :- Pt < Low. % still adding references to lower defn
popGroups(Stack,OStack,Groups,OG,_,Pt) :-
  popGroup(Stack,OStack,Group,Pt),
  mkGroup(Group,Groups,OG).

popGroup([(Nm,_,Lc,Df,Pt)|Stack],OStack,[(Nm,Lc,Df)|Group],Low) :- Pt >= Low,!,
  popGroup(Stack,OStack,Group,Low).
popGroup(Stack,Stack,[],_).

mkGroup([],Groups,Groups).
mkGroup(Group,Groups,[Group|Groups]).

minPoint(X,Y,X) :- X =< Y.
minPoint(X,Y,Y) :- X>Y.
