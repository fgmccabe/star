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
  import star.compiler.location.

  public consMap ~> cons[(termLbl,tipe,integer)].

  public nameMapEntry ::= .moduleFun(cExp,string)
  | .localFun(string,string,integer,cId)
  | .localVar(cExp)
  | .moduleCons(string,tipe)
  | .localCons(string,tipe,cId)
  | .labelArg(cId,integer)
  | .globalVar(string,tipe).

  public typeMapEntry ::= .moduleType(string,tipe,consMap).

  public mapLayer ::= .lyr(option[cId],map[string,nameMapEntry],map[string,typeMapEntry]).

  public nameMap ~> cons[mapLayer].

  public implementation display[mapLayer] => {
    disp(.lyr(V,VEntries,TEntries)) => "<thV=$(V)\:$(VEntries)\:$(TEntries)>".
  }

  public implementation display[nameMapEntry] => {
    disp(En) => case En in {
      .moduleFun(C,V) => "module fun $(C)\:$(typeOf(C))".
      .moduleCons(Nm,Tp) => "module cons $(Nm)".
      .localCons(Nm,Tp,Vr) => "local cons #(Nm)[$(Vr)]".
      .localFun(Nm,ClNm,_,V) => "local fun #(Nm), closure $(ClNm), ThV $(V)".
      .localVar(Vr) => "local var $(Vr)".
      .labelArg(Base,Ix) => "label arg $(Base)[$(Ix)]".
      .globalVar(Nm,Tp) => "global #(Nm)".
    }
  }

  public implementation display[typeMapEntry] => {
    disp(.moduleType(Nm,Tp,ConsMap)) => "type #(Nm)\:$(Tp) <- $(ConsMap)".
  }

  public lookupVarName:(nameMap,string)=>option[nameMapEntry].
  lookupVarName(Map,Nm) => lookup(Map,Nm,anyDef).

  anyDef(D) => .some(D).

  public lookupThetaVar:(nameMap,string)=>option[cId].
  lookupThetaVar(Map,Nm) where E?=lookupVarName(Map,Nm) =>
    case E in {
      .labelArg(ThV,_) => .some(ThV).
      .localFun(_,_,_,ThV) => .some(ThV).
      _ default => .none
    }.
  lookupThetaVar(_,_) default => .none.

  public layerVar:(nameMap)=>option[cId].
  layerVar([.lyr(V,_,_),.._])=>V.
  layerVar([])=>.none.

  public lookup:all e ~~ (nameMap,string,(nameMapEntry)=>option[e])=>option[e].
  lookup([],_,_) => .none.
  lookup([.lyr(_,Entries,_),..Map],Nm,P) where E ?= Entries[Nm] =>
    P(E).
  lookup([_,..Map],Nm,P) => lookup(Map,Nm,P).

  public findIndexMap:(string,nameMap) => option[consMap].
  findIndexMap(Tp,Map) => lookupTypeMap(Map,Tp).

  lookupTypeMap([],_) => .none.
  lookupTypeMap([.lyr(_,_,Entries),..Map],Nm) where .moduleType(_,_,E) ?= Entries[Nm] =>
    .some(E).
  lookupTypeMap([_,..Map],Nm) => lookupTypeMap(Map,Nm).

  public pkgMap:(cons[decl]) => nameMap.
  pkgMap(Decls) =>
    [.lyr(.none,foldRight((Dcl,D)=>declMdlGlobal(Dcl,D),[],Decls),makeConsMap(Decls))].

  public makeConsMap:(cons[decl]) => map[string,typeMapEntry].
  makeConsMap(Decls) => let{.
    collectConstructors:(cons[decl],map[string,cons[(string,tipe)]]) =>
      map[string,cons[(string,tipe)]].
    collectConstructors([],Map) => Map.
    collectConstructors([.cnsDec(Lc,Nm,FullNm,Tp),..Ds],Map) where
	TpNm ?= collectibleConsType(Tp) =>
      (E ?= Map[TpNm] ??
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
    collectMdlTypes([.tpeDec(Lc,Nm,Tp,_),..Ds],Cns,Map) where
	TpNm .= tpName(Tp) &&
	Entry ?= Cns[TpNm] =>
      collectMdlTypes(Ds,Cns,(Map[Nm->.moduleType(TpNm,Tp,Entry)])
	[TpNm->.moduleType(TpNm,Tp,Entry)]).
    collectMdlTypes([_D,..Ds],Cns,Map) => collectMdlTypes(Ds,Cns,Map).
  .} in collectMdlTypes(Decls,indexConstructors(collectConstructors(Decls,[])),[]).

  collectibleConsType:(tipe) => option[string].
  collectibleConsType(Tp) where
      (_,I) .= deQuant(Tp) && (A,R) ?= isConsType(I) &&
      ~ _ ?= fieldTypes(A) => -- Leave out record constructors
    .some(tpName(R)).
  collectibleConsType(_) default => .none.

  mkConsLbl(Nm,Tp) => .tLbl(Nm,arity(Tp)).

  declMdlGlobal(.funDec(Lc,Nm,FullNm,Tp),Map) => valof{
    Entry = .moduleFun(.cClos(Lc,closureNm(FullNm),arity(Tp)+1,crTpl(Lc,[]),Tp),FullNm);
    valis Map[Nm->Entry][FullNm->Entry]
  }
  declMdlGlobal(.varDec(Lc,Nm,FullNm,Tp),Map) => valof{
    Entry = .globalVar(FullNm,Tp);
    valis Map[Nm->Entry][FullNm->Entry]
  }
  declMdlGlobal(.cnsDec(Lc,Nm,FullNm,Tp),Map) => valof{
    Entry = .moduleCons(FullNm,Tp);
    valis Map[Nm->Entry][FullNm->Entry]
  }
  declMdlGlobal(.tpeDec(_,_,_,_),Map) => Map.
  declMdlGlobal(.accDec(_,_,_,_,_),Map) => Map.
  declMdlGlobal(.updDec(_,_,_,_,_),Map) => Map.
  declMdlGlobal(.conDec(_,_,_,_),Map) => Map.
  declMdlGlobal(.implDec(_,_,_,_),Map) => Map.

  public crTpl:(option[locn],cons[cExp]) => cExp.
  crTpl(Lc,Args) => let{
    Tp = typeOf(Args).
    Ar = size(Args).
  } in .cTerm(Lc,tplLbl(Ar),Args,Tp).

  public closureNm:(string)=>string.
  closureNm(Nm)=>Nm++"^".

  public varClosureNm:(string)=>string.
  varClosureNm(Nm) => Nm++"$".
}
  
