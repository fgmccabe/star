star.compiler.dict{
  import star.

  import star.compiler.canon.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.types.

  public tpDef ::= .tpDefn(option[locn],string,tipe,typeRule,map[string,tipe]).

  public vrEntry ::= .vrEntry(option[locn],(option[locn],boolean,dict)=>canon,tipe,option[tipe]).

  public implEntry ::= .implEntry(option[locn],string,tipe).

  public accEntry ::= .accEntry(option[locn],string,tipe).

  public scope ::= scope{
    types:map[string,tpDef].
    vars:map[string,vrEntry].
    contracts:map[string,typeRule].
    impls:map[string,implEntry].
    accessors:map[string,map[string,accEntry]].
    updaters:map[string,map[string,accEntry]].
    }.

  public dict::=.dict(cons[scope],ref map[string,(cons[canonDef],cons[decl])]).

  public implementation display[scope] => {
    disp(scope{types=Tps.
	vars=Vrs.
	contracts=Cnts.
	impls=Imps.
	accessors=Accs.
	updaters=Ups.
	}) =>
      "Types:$(Tps),\nVars:$(Vrs),\nContracts:$(Cnts),\nImplementations: $(Imps),\nAccessors: $(Accs)\nUpdaters: $(Ups)".
  }

  public implementation display[dict] => {
    disp(D) => showDict(D,3)
  }

  public showDict:(dict,integer)=>string.
  showDict(.dict(Scs,Br),Dp) => let{
    shLvls() where (F,_) ?= front(Scs,Dp) => interleave(F//disp,"\n")*.
    shLvls() default => interleave(Scs//disp,"\n")*.
  } in "dict: #(shLvls())\n$(Br!)".

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
  declareType(Nm,Lc,Tp,TpRl,.dict([Level,..Rest],Br)) =>
    .dict([Level.types=Level.types[Nm->.tpDefn(Lc,Nm,Tp,TpRl,[])],..Rest],Br).

  public findType:(dict,string) => option[(option[locn],tipe,typeRule,map[string,tipe])].
  findType(.dict(Scs,_),Nm) => let{.
    findTp([]) => .none.
    findTp([Lvl,.._]) where .tpDefn(Lc,_,Tp,Rl,Cns)?=Lvl.types[Nm] =>
      .some((Lc,Tp,Rl,Cns)).
    findTp([_,..Rest]) => findTp(Rest).
  .} in findTp(Scs).

  public findContract:(dict,string) => option[typeRule].
  findContract(.dict(Scs,_),Nm) => let{.
    findC([]) => .none.
    findC([scope{contracts=Cns},.._]) where Con?=Cns[Nm] => .some(Con).
    findC([_,..Rest]) => findC(Rest).
  .} in findC(Scs).

  public declareImplementation:(option[locn],string,string,tipe,dict) => dict.
  declareImplementation(Lc,ImplNm,ImplVr,Tp,.dict([Scope,..Env],Br)) =>
    .dict([Scope.impls=Scope.impls[ImplNm->.implEntry(Lc,ImplVr,Tp)],..Env],Br).

  public undeclareImplementation:(string,dict) => dict.
  undeclareImplementation(Nm,.dict([Scope,..Env],Br)) =>
    .dict([Scope.impls=Scope.impls[~Nm],..Env],Br).

  public declareAccessor:(option[locn],tipe,string,string,tipe,dict) => dict.
  declareAccessor(Lc,Tp,Fld,AccFn,AccTp,.dict([Scope,..Env],Br)) => valof{
    Key = tpName(Tp);
    Entry = .accEntry(Lc,AccFn,AccTp);
    Accs = Scope.accessors;

    if AccOrs ?= Accs[Key] then{
      valis .dict([Scope.accessors=Accs[Key->AccOrs[Fld->Entry]],..Env],Br)
    } else{
      valis .dict([Scope.accessors=Accs[Key->{Fld->Entry}],..Env],Br)
    }
  }

  public getFieldAccess:(tipe,string,dict)=>option[accEntry].
  getFieldAccess(Tp,Fld,.dict(Scs,_)) => let{.
    Ky = tpName(Tp).
    getField([]) => .none.
    getField([Scope,.._]) where
	AccOrs ?= Scope.accessors[Ky] &&
	    Acc ?= AccOrs[Fld] => .some(Acc).
    getField([_,..Env]) => getField(Env).
  .} in getField(Scs).


  public declareUpdater:(option[locn],tipe,string,string,tipe,dict) => dict.
  declareUpdater(Lc,Tp,Fld,UpdFn,UpdTp,.dict([Scope,..Env],Br)) => valof{
    Key = tpName(Tp);
    Entry = .accEntry(Lc,UpdFn,UpdTp);
    Ups = Scope.updaters;
    
    if AccOrs ?= Ups[Key] then{
      valis .dict([Scope.updaters=Ups[Key->AccOrs[Fld->Entry]],..Env],Br)
    } else{
      valis .dict([Scope.updaters=Ups[Key->{Fld->Entry}],..Env],Br)
    }
  }

  public getFieldUpdate:(tipe,string,dict)=>option[accEntry].
  getFieldUpdate(Tp,Fld,.dict(Scs,_)) => let{.
    getUpdate(_,[]) => .none.
    getUpdate(Key,[Scope,.._]) where
	AccOrs ?= Scope.updaters[Key] &&
	    Acc ?= AccOrs[Fld] => .some(Acc).
    getUpdate(Key,[_,..Env]) => getUpdate(Key,Env).
  .} in getUpdate(tpName(Tp),Scs).

  public pushScope:(dict)=>dict.
  pushScope(.dict(Scs,Br)) => .dict([scope{
	types=[].
	vars=[].
	contracts=[].
	impls=[].
	accessors=[].
	updaters=[].
      },..Scs],Br).

  public declareTypeVars:(cons[(string,tipe)],dict) => dict.
  declareTypeVars([],Env) => Env.
  declareTypeVars([(Nm,Tp),..Q],Env) =>
    declareTypeVars(Q,declareType(Nm,.none,Tp,.typeExists(Tp,Tp),Env)).

  public emptyDict:dict.
  emptyDict = .dict([scope{
	types=[].
	vars=[].
	contracts=[].
	impls=[].
	accessors=[].
	updaters=[].
      }],ref []).

  public stdTypes:cons[decl].
  stdTypes = [.tpeDec(.none,"integer",intType,.typeExists(intType,emptyFace)),
    .tpeDec(.none,"bigint",bigintType,.typeExists(bigintType,emptyFace)),
    .tpeDec(.none,"float",fltType,.typeExists(fltType,emptyFace)),
    .tpeDec(.none,"boolean",boolType,.typeExists(boolType,emptyFace)),
    .cnsDec(.none,"true","true",enumType(boolType)),
    .cnsDec(.none,"false","false",enumType(boolType)),
    .tpeDec(.none,"char",chrType,.typeExists(chrType,emptyFace)),
    .tpeDec(.none,"string",strType,.typeExists(strType,emptyFace)),
    .tpeDec(.none,"cons",.tpFun("cons",1),
      .allRule(.kVar("e"),
	.typeExists(lstType(.kVar("e")),emptyFace))),
    .cnsDec(.none,"cons","cons",
      .allType(.kVar("e"),consType(.tupleType([.kVar("e"),lstType(.kVar("e"))]),
	  lstType(.kVar("e"))))),
    .cnsDec(.none,"nil","nil",
      .allType(.kVar("e"),consType(.tupleType([]),lstType(.kVar("e"))))),
    .tpeDec(.none,"option",.tpFun("option",1),
      .allRule(.kVar("e"),
	.typeExists(optType(.kVar("e")),emptyFace))),
    .cnsDec(.none,"some","some",
      .allType(.kVar("e"),consType(.tupleType([.kVar("e")]),
	  optType(.kVar("e"))))),
    .cnsDec(.none,"none","none",
      .allType(.kVar("e"),consType(.tupleType([]),optType(.kVar("e"))))),

    .tpeDec(.none,"thunk",.tpFun("thunk",1),
      .allRule(.kVar("e"),
	.typeExists(thunkType(.kVar("e")),emptyFace))),
    .tpeDec(.none,"fiber",.tpFun("fiber",2),
      .allRule(.kVar("a"),
	.allRule(.kVar("e"),
	  .typeExists(makeTpExp("fiber",
	      [.kVar("a"),.kVar("e")]),emptyFace)))),
    .tpeDec(.none,"ioHandle",ioType,.typeExists(ioType,emptyFace)),
    .tpeDec(.none,"future",.tpFun("future",2),
      .allRule(.kVar("v"),
	.allRule(.kVar("e"),
	  .typeExists(futureType(.kVar("v"),.kVar("e")),
	    emptyFace)))),
    .tpeDec(.none,"errorCode",errorCodeType,.typeExists(errorCodeType,emptyFace)),
    .cnsDec(.none,"eINTRUPT","eINTRUPT",enumType(errorCodeType)),
    .cnsDec(.none,"eNOTDIR","eNOTDIR",enumType(errorCodeType)),
    .cnsDec(.none,"eNOFILE","eNOFILE",enumType(errorCodeType)),
    .cnsDec(.none,"eNOTFND","eNOTFND",enumType(errorCodeType)),
    .cnsDec(.none,"eINVAL","eINVAL",enumType(errorCodeType)),
    .cnsDec(.none,"eRANGE","eRANGE",enumType(errorCodeType)),
    .cnsDec(.none,"eNOPERM","eNOPERM",enumType(errorCodeType)),
    .cnsDec(.none,"eFAIL","eFAIL",enumType(errorCodeType)),
    .cnsDec(.none,"eIOERROR","eIOERROR",enumType(errorCodeType)),
    .cnsDec(.none,"eCONNECT","eCONNECT",enumType(errorCodeType)),
    .cnsDec(.none,"eDEAD","eDEAD",enumType(errorCodeType)),
    .cnsDec(.none,"divZero","divZero",enumType(errorCodeType)),
    .cnsDec(.none,"noValue","noValue",enumType(errorCodeType)),
    .cnsDec(.none,"hasValue","hasValue",enumType(errorCodeType)),
    .cnsDec(.none,"eEOF","eEOF",enumType(errorCodeType))
  ]
}
