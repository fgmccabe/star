
generateMain(Defs,_Env,Defs) :-
   is_member(funDef(_,"_main",_,_,_,_),Defs),!.
generateMain(Defs,Env,RDefs) :-
  \+ is_member(funDef(_,"_main",_,_,_,_),Defs),
  is_member(funDef(Lc,"main",LclNm,Tp,Cx,Eqns),Defs),
  /*
    construct driver for main, which should have a type:
    (Tps) => action[integer,()]
  */
  findType("action",Lc,Env,AcTp),
  findType("integer",Lc,Env,IntegerTp),
  ActionTp = tpExp(tpExp(AcTp,IntegerTp),tupleType([])),
  newTypeVar("_A",AT),
  (sameType(funType(AT,ActionTp),Tp,Env) ->
   
   ; reportError(Lc,"main: %s not correct type",[Tp],Lc),
   RDefs=Defs).
