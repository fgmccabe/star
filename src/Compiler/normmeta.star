star.compiler.normalize.meta{
  import star.
  import star.pkg.
  import star.sort.

  import star.compiler.canon.
  import star.compiler.term.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.data.
  import star.compiler.types.

  public consMap ~> cons[(termLbl,tipe,integer)].

  public nameMapEntry ::= moduleFun(crExp,string)
    | localFun(string,string,crVar)
    | localVar(crExp)
    | moduleCons(string,tipe)
    | localCons(string,tipe,crVar)
    | labelArg(crVar,integer)
    | memoArg(string,crVar,integer)
    | globalVar(string,tipe).

  public typeMapEntry ::= moduleType(string,tipe,consMap).

  public mapLayer ::= lyr(option[crVar],map[string,nameMapEntry],map[string,typeMapEntry]).

  public nameMap ~> cons[mapLayer].

  public implementation display[mapLayer] => {
    disp(lyr(V,VEntries,TEntries)) => "thV=$(V)\:$(VEntries)\:$(TEntries)".
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

  public implementation display[typeMapEntry] => {
    disp(moduleType(Nm,Tp,ConsMap)) => "type #(Nm)\:$(Tp) <- $(ConsMap)".
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

  public findIndexMap:(string,nameMap) => option[consMap].
  findIndexMap(Tp,Map) => lookupTypeMap(Map,Tp).

  lookupTypeMap([],_) => .none.
  lookupTypeMap([lyr(_,_,Entries),..Map],Nm) where moduleType(_,_,E) ^= Entries[Nm] =>
    some(E).
  lookupTypeMap([_,..Map],Nm) => lookupTypeMap(Map,Nm).

  public pkgMap:(pkgSpec,cons[decl]) => nameMap.
  pkgMap(pkgSpec(Pkg,_,_),Decls) =>
    [lyr(.none,foldRight((Dcl,D)=>declMdlGlobal(Dcl,D),{},Decls),makeConsMap(Decls))].

  public makeConsMap:(cons[decl]) => map[string,typeMapEntry].
  makeConsMap(Decls) => let{.
    collectConstructors:(cons[decl],map[string,cons[(string,tipe)]]) =>
      map[string,cons[(string,tipe)]].
    collectConstructors([],Map) => Map.
    collectConstructors([cnsDec(Lc,Nm,FullNm,Tp),..Ds],Map) where
	TpNm ^= collectibleConsType(Tp) =>
      (E ^= Map[TpNm] ?
	collectConstructors(Ds,Map[TpNm->[(FullNm,Tp),..E]]) ||
	collectConstructors(Ds,Map[TpNm->[(FullNm,Tp)]])
      ).
    collectConstructors([_,..Ds],Map) => collectConstructors(Ds,Map).

    indexConstructors:(map[string,cons[(string,tipe)]]) => map[string,consMap].
    indexConstructors(M) =>
      { TpNm -> { (mkConsLbl(CNm,CTp),CTp,Ix) | ((CNm,CTp),Ix) in indexList(sort(CMp,((N1,_),(N2,_))=>N1<N2),0) } | TpNm->CMp in M }.
	    
    indexList:all e ~~ (cons[e],integer)=>cons[(e,integer)].
    indexList([],_) => [].
    indexList([E,..Es],Ix) => [(E,Ix),..indexList(Es,Ix+1)].
    
    collectMdlTypes:(cons[decl],map[string,consMap],
      map[string,typeMapEntry]) => map[string,typeMapEntry].
    collectMdlTypes([],Cns,Map) => Map.
    collectMdlTypes([tpeDec(Lc,Nm,Tp,_),..Ds],Cns,Map) where
	TpNm .= tpName(Tp) &&
	Entry ^= Cns[TpNm] =>
      collectMdlTypes(Ds,Cns,(Map[Nm->moduleType(TpNm,Tp,Entry)])
	[TpNm->moduleType(TpNm,Tp,Entry)]).
    collectMdlTypes([_D,..Ds],Cns,Map) => collectMdlTypes(Ds,Cns,Map).
  .} in collectMdlTypes(Decls,indexConstructors(collectConstructors(Decls,[])),[]).

  collectibleConsType:(tipe) => option[string].
  collectibleConsType(Tp) where
      (_,I) .= deQuant(Tp) && (A,R) ^= isConsType(I) &&
      ~ _ ^= fieldTypes(A) => -- Leave out record constructors
    some(tpName(R)).
  collectibleConsType(_) default => .none.

  mkConsLbl(Nm,Tp) => tLbl(Nm,arity(Tp)).

  declMdlGlobal(funDec(Lc,Nm,FullNm,Tp),Map) =>
    Map[Nm->moduleFun(idnt(Lc,crId(FullNm,Tp)),FullNm)].
  declMdlGlobal(varDec(Lc,Nm,FullNm,Tp),Map) =>
    Map[Nm->globalVar(FullNm,Tp)].
  declMdlGlobal(cnsDec(Lc,Nm,FullNm,Tp),Map) =>
    Map[Nm->moduleCons(FullNm,Tp)].
  declMdlGlobal(_,Map) => Map.

  public extendFunTp:all x ~~ hasType[x] |: (tipe,option[x])=>tipe.
  extendFunTp(Tp,.none) => Tp.
  extendFunTp(Tp,Vs) where (A,B)^=isFunType(Tp) &&
      tupleType(Es).=deRef(A) =>
    funType(extendTplType(Es,Vs),B).
  extendFunTp(allType(V,B),Vs) => allType(V,extendFunTp(B,Vs)).
  extendFunTp(existType(V,B),Vs) => existType(V,extendFunTp(B,Vs)).
  extendFunTp(constrainedType(T,C),Vs) => constrainedType(extendFunTp(T,Vs),C).

  extendTplType:all x ~~ hasType[x] |: (cons[tipe],option[x])=>cons[tipe].
  extendTplType(Es,.none) => Es.
  extendTplType(Es,some(E)) => [typeOf(E),..Es].

  public findMemoIx:(string,crVar,nameMap) => option[integer].
  findMemoIx(Nm,ThV,Map) => lookup(Map,Nm,isMemoVar(ThV)).

  isMemoVar:(crVar)=>(nameMapEntry)=>option[integer].
  isMemoVar(ThV) => let{
    check(memoArg(_,ThV,Ix))=>some(Ix).
    check(_) default => .none
  } in check.
  
}
  
