star.compiler.normalize.meta{
  import star.
  import star.pkg.
  import star.sort.

  import star.compiler.canon.
  import star.compiler.term.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.data.
  import star.compiler.ltipe.
  import star.compiler.types.
  import star.compiler.location.

  public nameMapEntry ::= .moduleFun(cExp,string,ltipe)
  | .localFun(canonVar,string,string,integer,ltipe)
  | .moduleCons(string,integer,tipe)
  | .localCons(canonVar,string,tipe,integer)
  | .labelArg(canonVar,integer)
  | .localArg(canonVar,integer)
  | .thunkArg(canonVar,string,integer)
  | .globalVar(string,ltipe).

  public typeMap ~> map[string,indexMap].

  public mapLayer ::= .lyr(option[canonVar],map[string,nameMapEntry],typeMap).

  public nameMap ~> cons[mapLayer].

  public implementation display[mapLayer] => {
    disp(.lyr(V,VEntries,TEntries)) => "<thV=$(V)\:$(VEntries)\:$(TEntries)>".
  }

  public implementation display[nameMapEntry] => {
    disp(En) => case En in {
      | .moduleFun(C,V,Tp) => "module fun $(C)\:$(Tp)"
      | .moduleCons(Nm,Ix,Tp) => "module cons $(Nm) @ ($(Ix)"
      | .localCons(Vr,Nm,Tp,Ix) => "local cons #(Nm)[$(Vr)] @ $(Ix)"
      | .localFun(V,Nm,ClNm,_,_) => "local fun #(Nm), closure $(ClNm), ThV $(V)"
      | .labelArg(Base,Ix) => "label arg $(Base)[$(Ix)]"
      | .localArg(Base,Ix) => "local arg $(Base)[$(Ix)]"
      | .thunkArg(Base,Lbl,Ix) => "thunk arg $(Base)[$(Ix)], $(Lbl)"
      | .globalVar(Nm,Tp) => "global #(Nm)\:$(Tp)"
    }
  }

  public lookupVarName:(nameMap,string)=>option[nameMapEntry].
  lookupVarName(Map,Nm) => lookup(Map,Nm,anyDef).

  anyDef(D) => .some(D).

  public lookupThetaVar:(nameMap,string)=>option[canonVar].
  lookupThetaVar(Map,Nm) where E?=lookupVarName(Map,Nm) =>
    case E in {
    | .labelArg(ThV,_) => .some(ThV)
    | .thunkArg(ThV,_,_) => .some(ThV)
    | .localFun(ThV,_,_,_,_) => .some(ThV)
    | _ default => .none
    }.
  lookupThetaVar(_,_) default => .none.

  public layerVar:(nameMap)=>option[canonVar].
  layerVar([.lyr(V,_,_),.._])=>V.
  layerVar([])=>.none.

  public lookup:all e ~~ (nameMap,string,(nameMapEntry)=>option[e])=>option[e].
  lookup([],_,_) => .none.
  lookup([.lyr(_,Entries,_),..Map],Nm,P) where E ?= Entries[Nm] =>
    P(E).
  lookup([_,..Map],Nm,P) => lookup(Map,Nm,P).

  public lookupTypeMap:(string,nameMap) => option[indexMap].
  lookupTypeMap(Lbl,Map) => lookupTypeIndex(Lbl,Map).

  lookupTypeIndex(_,[]) => .none.
  lookupTypeIndex(Nm,[.lyr(_,_,Entries),..Map]) where Index  ?= Entries[Nm] =>
    .some(Index).
  lookupTypeIndex(Nm,[_,..Map]) => lookupTypeIndex(Nm,Map).

  public pkgMap:(cons[decl],nameMap) => nameMap.
  pkgMap(Decls,M) => valof{
    CMap = makeTypeMap(Decls);
    valis [.lyr(.none,foldRight((Dcl,D)=>declMdlGlobal(Dcl,D),[],Decls),CMap),..M]
  }

  -- Put an entry in the constructor map for each constructor.
  -- Each entry contains the full index map for the type of the constructor.
  public makeTypeMap:(cons[decl]) => typeMap.
  makeTypeMap(Decls) => let{.
    collectTypeMaps:(cons[decl],typeMap) => typeMap.
    collectTypeMaps([],Map) => Map.
    collectTypeMaps([.tpeDec(_,Nm,Tp,_,IxMap),..Ds],Map) =>
      collectTypeMaps(Ds,Map[Nm->IxMap]).
    collectTypeMaps([_,..Ds],Map) => collectTypeMaps(Ds,Map).
  .} in collectTypeMaps(Decls,[]).

  mkConsLbl(Nm,Tp) => .tLbl(Nm,arity(Tp)).

  declMdlGlobal(.funDec(Lc,Nm,FullNm,Tp),Map) => valof{
    Entry = .moduleFun(.cClos(Lc,closureNm(FullNm),arity(Tp)+1,crTpl(Lc,[]),Tp::ltipe),FullNm,Tp::ltipe);
    valis Map[Nm->Entry][FullNm->Entry]
  }
  declMdlGlobal(.varDec(Lc,Nm,FullNm,Tp),Map) => valof{
    Entry = .globalVar(FullNm,Tp::ltipe);
    valis Map[Nm->Entry][FullNm->Entry]
  }
  declMdlGlobal(.cnsDec(Lc,Nm,FullNm,Ix,Tp),Map) => valof{
    Entry = .moduleCons(FullNm,Ix,Tp);
    valis Map[Nm->Entry][FullNm->Entry]
  }
  declMdlGlobal(.tpeDec(_,_,_,_,_),Map) => Map.
  declMdlGlobal(.accDec(_,_,_,_,_,_),Map) => Map.
  declMdlGlobal(.updDec(_,_,_,_,_,_),Map) => Map.
  declMdlGlobal(.conDec(_,_,_,_),Map) => Map.
  declMdlGlobal(.implDec(_,_,_,_),Map) => Map.

  public crTpl:(option[locn],cons[cExp]) => cExp.
  crTpl(Lc,Args) => .cTerm(Lc,tplLbl(size(Args)),0,Args).

  public closureNm:(string)=>string.
  closureNm(Nm)=>Nm++"^".

  public varClosureNm:(string)=>string.
  varClosureNm(Nm) => Nm++"$".

  public implementation coercion[canonVar,cV->>_] => {
    _coerce(.var(V,T)) => .cV(V,T::ltipe)
  }
}

