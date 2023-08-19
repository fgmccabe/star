star.compiler.dict{
  import star.

  import star.compiler.canon.
  import star.compiler.escapes.
  import star.compiler.intrinsics.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.meta.
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

  emptyDict:dict.
  emptyDict = .dict([scope{
	types=[].
	vars=[].
	contracts=[].
	impls=[].
	accessors=[].
	updaters=[].
      }],ref []).


-- Standard types are predefined by the language
  public stdDict:dict.
  stdDict =
    declareType("integer",.none,intType,.typeExists(intType,emptyFace),
      declareType("bigint",.none,bigintType,.typeExists(bigintType,emptyFace),
	declareType("float",.none,fltType,.typeExists(fltType,emptyFace),
	  declareType("boolean",.none,boolType,.typeExists(boolType,emptyFace),
	    declareType("char",.none,chrType,.typeExists(chrType,emptyFace),
	      declareType("string",.none,strType,.typeExists(strType,emptyFace),
		declareType("cons",.none,.tpFun("star.core*cons",1),
		  .allRule(.nomnal("e"),
		    .typeExists(lstType(.nomnal("e")),emptyFace)),
		  declareType("fiber",.none,.tpFun("star.core*fiber",2),
		    .allRule(.nomnal("a"),
		      .allRule(.nomnal("e"),
			.typeExists(makeTpExp("star.core*fiber",
			    [.nomnal("a"),.nomnal("e")]),emptyFace))),
		    emptyDict)))))))).
}
