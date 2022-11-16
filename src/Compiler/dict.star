star.compiler.dict{
  import star.

  import star.compiler.canon.
  import star.compiler.escapes.
  import star.compiler.intrinsics.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.types.

  public tpDef ::= .tpDefn(option[locn],string,tipe,typeRule,map[string,tipe]).

  public vrEntry ::= .vrEntry(option[locn],(option[locn],dict)=>canon,tipe,option[tipe]).

  public implEntry ::= .implEntry(option[locn],string,tipe).

  public accEntry ::= .accEntry(option[locn],string,tipe).

  public scope ::= scope{
    types:map[string,tpDef].
    vars:map[string,vrEntry].
    contracts:map[string,typeRule].
    impls:map[string,implEntry].
    accessors:map[string,map[string,accEntry]].
    updaters:map[string,map[string,accEntry]].
    labels:map[string,(option[locn],string)]}.

  public dict ~> cons[scope].

  public implementation display[scope] => {
    disp(scope{types=Tps.
	vars=Vrs.
	contracts=Cnts.
	impls=Imps.
	accessors=Accs.
	updaters=Ups.
	labels=Lbls}) =>
      "Types:$(Tps),\nVars:$(Vrs),\nContracts:$(Cnts),\nImplementations: $(Imps),\nAccessors: $(Accs)\nUpdaters: $(Ups)\nLabels: $(Lbls)".
  }

  public implementation display[vrEntry] => {
    disp(.vrEntry(Lc,Mk,Tp,_)) => "|=$(Tp)".
  }

  public implementation display[tpDef] => {
    disp(.tpDefn(_,_,Tmpl,Rl,Mp)) => "$(Rl)\:$(Mp)".
  }

  public implementation display[implEntry] => {
    disp(.implEntry(Lc,ImplNm,FnTp))=>"Impl:#(ImplNm)\:$(FnTp)@$(Lc)"
  }

  public implementation display[accEntry] => {
    disp(.accEntry(Lc,Fn,FnTp))=>"Acc:#(Fn)\:$(FnTp)@$(Lc)"
  }

  public vrType:(vrEntry)=>tipe.
  vrType(.vrEntry(_,_,Tp,_))=>Tp.

  public vrFace(.vrEntry(_,_,_,Fc))=>Fc.
  
  public declareType:(string,option[locn],tipe,typeRule,dict) => dict.
  declareType(Nm,Lc,Tp,TpRl,[Level,..Rest]) =>
    [Level.types<<-Level.types[Nm->.tpDefn(Lc,Nm,Tp,TpRl,{})],..Rest].

  public findType:(dict,string) => option[(option[locn],tipe,typeRule,map[string,tipe])].
  findType([],Nm) => .none.
  findType([Lvl,.._],Ky) where .tpDefn(Lc,_,Tp,Rl,Cns)?=Lvl.types[Ky] =>
    .some((Lc,Tp,Rl,Cns)).
  findType([_,..Rest],Ky) => findType(Rest,Ky).

  public findContract:(dict,string) => option[typeRule].
  findContract([],Nm) => .none.
  findContract([scope{contracts=Cns},.._],Ky) where Con?=Cns[Ky] => .some(Con).
  findContract([_,..Rest],Ky) => findContract(Rest,Ky).

  public declareImplementation:(option[locn],string,string,tipe,dict) => dict.
  declareImplementation(Lc,ImplNm,ImplVr,Tp,[Scope,..Env]) =>
    [Scope.impls<<-Scope.impls[ImplNm->implEntry(Lc,ImplVr,Tp)],..Env].

  public undeclareImplementation:(string,dict) => dict.
  undeclareImplementation(Nm,[Scope,..Env]) =>
    [Scope.impls<<-Scope.impls[~Nm],..Env].

  public declareAccessor:(option[locn],tipe,string,string,tipe,dict) => dict.
  declareAccessor(Lc,Tp,Fld,AccFn,AccTp,[Scope,..Env]) => valof{
    Key = tpName(Tp);
    Entry = accEntry(Lc,AccFn,AccTp);
    Accs = Scope.accessors;

    if AccOrs ?= Accs[Key] then{
      valis [Scope.accessors<<-Accs[Key->AccOrs[Fld->Entry]],..Env]
    } else{
      valis [Scope.accessors<<-Accs[Key->{Fld->Entry}],..Env]
    }
  }

  public getFieldAccess:(tipe,string,dict)=>option[accEntry].
  getFieldAccess(Tp,Fld,Env) => getField(tpName(Tp),Fld,Env).

  getField(_,_,[]) => .none.
  getField(Key,Fld,[Scope,.._]) where
      AccOrs ?= Scope.accessors[Key] &&
      Acc ?= AccOrs[Fld] => .some(Acc).
  getField(Key,Fld,[_,..Env]) => getField(Key,Fld,Env).

  public declareUpdater:(option[locn],tipe,string,string,tipe,dict) => dict.
  declareUpdater(Lc,Tp,Fld,UpdFn,UpdTp,[Scope,..Env]) => valof{
    Key = tpName(Tp);
    Entry = accEntry(Lc,UpdFn,UpdTp);
    Ups = Scope.updaters;
    
    if AccOrs ?= Ups[Key] then{
      valis [Scope.updaters<<-Ups[Key->AccOrs[Fld->Entry]],..Env]
    } else{
      valis [Scope.updaters<<-Ups[Key->{Fld->Entry}],..Env]
    }
  }

  public getFieldUpdate:(tipe,string,dict)=>option[accEntry].
  getFieldUpdate(Tp,Fld,Env) => getUpdate(tpName(Tp),Fld,Env).

  getUpdate(_,_,[]) => .none.
  getUpdate(Key,Fld,[Scope,.._]) where
      AccOrs ?= Scope.accessors[Key] &&
      Acc ?= AccOrs[Fld] => .some(Acc).
  getUpdate(Key,Fld,[_,..Env]) => getUpdate(Key,Fld,Env).

  public declareLabel:(option[locn],string,dict) => dict.
  declareLabel(Lc,Lb,[Scope,..Env]) => [Scope.labels<<-Scope.labels[Lb->(Lc,Lb)],..Env].

  public isLabel:(string,dict) => option[(option[locn],string)].
  isLabel(Lb,[Sc,.._]) where Tgt?=Sc.labels[Lb] => .some(Tgt).
  isLabel(Lb,[_,..Env]) => isLabel(Lb,Env).
  isLabel(_,_) default => .none.

  public pushScope:(dict)=>dict.
  pushScope(Env) => [scope{
      types={}.
      vars={}.
      contracts={}.
      impls={}.
      accessors={}.
      updaters={}.
      labels=[]},..Env].

  public declareTypeVars:(cons[(string,tipe)],dict) => dict.
  declareTypeVars([],Env) => Env.
  declareTypeVars([(Nm,Tp),..Q],Env) =>
    declareTypeVars(Q,declareType(Nm,.none,Tp,typeExists(Tp,Tp),Env)).

  emptyFace = faceType([],[]).

  emptyDict:dict.
  emptyDict = pushScope([]).

-- Standard types are predefined by the language
  public stdDict:dict.
  stdDict =
    declareType("integer",.none,intType,typeExists(intType,emptyFace),
      declareType("bigint",.none,bigintType,typeExists(bigintType,emptyFace),
	declareType("float",.none,fltType,typeExists(fltType,emptyFace),
	  declareType("boolean",.none,boolType,typeExists(boolType,emptyFace),
	    declareType("char",.none,chrType,typeExists(chrType,emptyFace),
	      declareType("string",.none,strType,typeExists(strType,emptyFace),
		declareType("cons",.none,tpFun("star.core*cons",1),
		  allRule(nomnal("e"),
		    typeExists(lstType(nomnal("e")),emptyFace)),
		  emptyDict))))))).
}
