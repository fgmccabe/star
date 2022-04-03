star.compiler.dict{
  import star.

  import star.compiler.canon.
  import star.compiler.escapes.
  import star.compiler.intrinsics.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.types.

  public tpDef ::= tpDefn(option[locn],string,tipe,typeRule).

  public vrEntry ::= vrEntry(option[locn],(locn,dict)=>canon,tipe,option[tipe]).

  public implEntry ::= implEntry(option[locn],string,tipe).

  public accEntry ::= accEntry(option[locn],string,tipe).

  public scope ::= scope(
    map[string,tpDef],
    map[string,vrEntry],
    cons[constraint],
    map[string,typeRule],
    map[string,implEntry],
    map[string,map[string,accEntry]],
    map[string,map[string,accEntry]]).

  public dict ~> cons[scope].

  public implementation display[scope] => {
    disp(scope(Tps,Vrs,_,Cnts,Imps,Accs,Ups)) =>
      "Types:$(Tps),\nVars:$(Vrs),\nContracts:$(Cnts),\nImplementations: $(Imps),\nAccessors: $(Accs)\nUpdaters: $(Ups)\n".
  }

  public implementation display[vrEntry] => {
    disp(vrEntry(some(Lc),Mk,Tp,_)) => "$(Mk(Lc,emptyDict))|=$(Tp)".
    disp(vrEntry(.none,_,Tp,_)) => disp(Tp).
  }

  public implementation display[tpDef] => {
    disp(tpDefn(_,_,Tmpl,Rl)) => "$(Tmpl) == $(Rl)".
  }

  public implementation display[implEntry] => {
    disp(implEntry(Lc,ImplNm,FnTp))=>"Impl:#(ImplNm)\:$(FnTp)@$(Lc)"
  }

  public implementation display[accEntry] => {
    disp(accEntry(Lc,Fn,FnTp))=>"Acc:#(Fn)\:$(FnTp)@$(Lc)"
  }

  public vrType:(vrEntry)=>tipe.
  vrType(vrEntry(_,_,Tp,_))=>Tp.

  public vrFace(vrEntry(_,_,_,Fc))=>Fc.
  
  public declareType:(string,option[locn],tipe,typeRule,dict) => dict.
  declareType(Nm,Lc,Tp,TpRl,[scope(Tps,Vrs,Cns,Cnts,Imps,Accs,Ups),..Rest]) =>
    [scope(Tps[Nm->tpDefn(Lc,Nm,Tp,TpRl)],Vrs,Cns,Cnts,Imps,Accs,Ups),..Rest].

  public findType:(dict,string) => option[(option[locn],tipe,typeRule)].
  findType([],Nm) => .none.
  findType([scope(Tps,_,_,_,_,_,_),.._],Ky) where tpDefn(Lc,_,Tp,Rl)^=Tps[Ky] =>
    some((Lc,Tp,Rl)).
  findType([_,..Rest],Ky) => findType(Rest,Ky).

  public findContract:(dict,string) => option[typeRule].
  findContract([],Nm) => .none.
  findContract([scope(_,_,_,Cns,_,_,_),.._],Ky) where Con^=Cns[Ky] => some(Con).
  findContract([_,..Rest],Ky) => findContract(Rest,Ky).

  public findImplementation:(dict,string) => option[canon].
  findImplementation([scope(_,_,_,_,Imps,_,_),.._],INm) where implEntry(Lc,Vr,Tp) ^= Imps[INm] => some(vr(Lc,Vr,Tp)).
  findImplementation([_,..Rest],INm) => findImplementation(Rest,INm).
  findImplementation([],_) => .none.

  public declareImplementation:(option[locn],string,string,tipe,dict) => dict.
  declareImplementation(Lc,ImplNm,ImplVr,Tp,
    [scope(Tps,Vrs,Cns,Cnts,Imps,Accs,Ups),..Env]) =>
    [scope(Tps,Vrs,Cns,Cnts,Imps[ImplNm->implEntry(Lc,ImplVr,Tp)],Accs,Ups),..Env].

  public undeclareImplementation:(string,dict) => dict.
  undeclareImplementation(Nm,[scope(Tps,Vrs,Cns,Cnts,Imps,Accs,Ups),..Env]) =>
    [scope(Tps,Vrs,Cns,Cnts,Imps[~Nm],Accs,Ups),..Env].

  public declareAccessor:(option[locn],tipe,string,string,tipe,dict) => dict.
  declareAccessor(Lc,Tp,Fld,AccFn,AccTp,
    [scope(Tps,Vrs,Cns,Cnts,Imps,Accs,Ups),..Env]) => valof{
    Key .= tpName(Tp);
    Entry .= accEntry(Lc,AccFn,AccTp);
--    logMsg("declare accessor for $(Tp)[$(Key)].#(Fld) |:$(AccTp)");
    if AccOrs ^= Accs[Key] then{
      valis [scope(Tps,Vrs,Cns,Cnts,Imps,Accs[Key->AccOrs[Fld->Entry]],Ups),..Env]
    } else{
      valis [scope(Tps,Vrs,Cns,Cnts,Imps,Accs[Key->{Fld->Entry}],Ups),..Env]
    }
  }

  public getFieldAccess:(tipe,string,dict)=>option[accEntry].
  getFieldAccess(Tp,Fld,Env) => getField(tpName(Tp),"."++Fld,Env).

  getField(_,_,[]) => .none.
  getField(Key,Fld,[scope(_,_,_,_,_,Accs,_),.._]) where
      AccOrs ^= Accs[Key] &&
      Acc ^= AccOrs[Fld] => some(Acc).
  getField(Key,Fld,[_,..Env]) => getField(Key,Fld,Env).

  public declareUpdater:(option[locn],tipe,string,string,tipe,dict) => dict.
  declareUpdater(Lc,Tp,Fld,UpdFn,UpdTp,
    [scope(Tps,Vrs,Cns,Cnts,Imps,Accs,Ups),..Env]) => valof{
    Key .= tpName(Tp);
    Entry .= accEntry(Lc,UpdFn,UpdTp);
--    logMsg("declare updater for $(Tp)[$(Key)].#(Fld) |:$(UpdTp)");
    if AccOrs ^= Ups[Key] then{
      valis [scope(Tps,Vrs,Cns,Cnts,Imps,Accs,Ups[Key->AccOrs[Fld->Entry]]),..Env]
    } else{
      valis [scope(Tps,Vrs,Cns,Cnts,Imps,Accs,Ups[Key->{Fld->Entry}]),..Env]
    }
  }

  public pushScope:(dict)=>dict.
  pushScope(Env) => [scope({},{},[],{},{},{},{}),..Env].

  public declareTypeVars:(cons[(string,tipe)],dict) => dict.
  declareTypeVars([],Env) => Env.
  declareTypeVars([(Nm,Tp),..Q],Env) =>
    declareTypeVars(Q,declareType(Nm,.none,Tp,typeExists(Tp,Tp),Env)).

  emptyFace = faceType([],[]).

  public emptyDict:dict.
  public emptyDict = [scope({},{},[],{},{},{},{})].

-- Standard types are predefined by the language
  public stdDict:dict.
  stdDict =
    declareType("integer",.none,intType,typeExists(intType,emptyFace),
      declareType("float",.none,fltType,typeExists(fltType,emptyFace),
	declareType("boolean",.none,boolType,typeExists(boolType,emptyFace),
	  declareType("string",.none,strType,typeExists(strType,emptyFace),
	    declareType("cons",.none,tpFun("star.core*cons",1),
	      allRule(nomnal("e"),
		typeExists(lstType(nomnal("e")),faceType([],[]))),
	      emptyDict))))).
}
