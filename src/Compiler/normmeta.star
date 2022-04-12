star.compiler.normalize.meta{
  import star.

  import star.compiler.core.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.terms.
  import star.compiler.types.

  public nameMapEntry ::= moduleFun(crExp,string)
    | localFun(string,string,crVar)
    | localVar(crExp)
    | moduleCons(string,tipe)
    | localCons(string,tipe,crVar)
    | labelArg(crVar,integer)
    | memoArg(string,crVar,integer)
    | globalVar(string,tipe).

  public consEntry ~> cons[(termLbl,integer,tipe)].

  public mapLayer ::= lyr(option[crVar],map[string,nameMapEntry],map[string,consEntry]).

  public nameMap ~> cons[mapLayer].

  public implementation display[mapLayer] => {
    disp(lyr(V,Entries,ConsMap)) => "thV=$(V)\:$(Entries)\:$(ConsMap)".
  }

  public implementation display[nameMapEntry] => {
    disp(moduleFun(C,V)) => "module fun $(V), closure $(C)".
    disp(moduleCons(Nm,Tp)) => "module cons $(Nm)".
    disp(localCons(Nm,Tp,Vr)) => "local cons #(Nm)[$(Vr)]".
    disp(localFun(Nm,ClNm,V)) => "local fun #(Nm), closure $(ClNm), ThV $(V)".
    disp(localVar(Vr)) => "local var $(Vr)".
    disp(labelArg(Base,Ix)) => "label arg $(Base)[$(Ix)]".
    disp(memoArg(Nm,Base,Ix)) => "memo arg #(Nm)@$(Base)[$(Ix)]".
    disp(globalVar(Nm,Tp)) => "global #(Nm)".
  }

  public crFlow ~> (crExp,cons[crDefn]).

  public lookupVarName:(nameMap,string)=>option[nameMapEntry].
  lookupVarName(Map,Nm) => lookup(Map,Nm,anyDef).

  anyDef(D) => some(D).

  public lookupThetaVar:(nameMap,string)=>option[crVar].
  lookupThetaVar(Map,Nm) where E^=lookupVarName(Map,Nm) =>
    case E in {
      labelArg(ThV,_) => some(ThV).
      memoArg(_,ThV,_) => some(ThV).
      localFun(_,_,ThV) => some(ThV).
      _ default => .none
    }.
  lookupThetaVar(_,_) default => .none.

  public layerVar:(nameMap)=>option[crVar].
  layerVar([lyr(V,_,_),.._])=>V.
  layerVar([])=>.none.

  public lookup:all e ~~ (nameMap,string,(nameMapEntry)=>option[e])=>option[e].
  lookup([],_,_) => .none.
  lookup([lyr(_,Entries,_),..Map],Nm,P) where E ^= Entries[Nm] =>
    P(E).
  lookup([_,..Map],Nm,P) => lookup(Map,Nm,P).

  public findIndexMap:(tipe,nameMap) => option[consEntry].
  findIndexMap(Tp,Map) => let{.
    look([],_) => .none.
    look([lyr(_,_,Cons),.._],Nm) where E ^= Cons[Nm] => some(E).
    look([_,..Mp],Nm) => look(Mp,Nm).
  .} in look(Map,tpName(Tp)).
    
}
  
