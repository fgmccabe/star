star.compiler.wasm.gentypes{
  import star.
  import star.topsort.

  import star.compiler.data.
  import star.compiler.errors.
  import star.compiler.meta.
  import star.compiler.term.
  import star.compiler.types.
  import star.compiler.wasm.types.

  public buildWasmTypeMap:(cons[cDefn]) => cons[recType].
  buildWasmTypeMap(Defs) => valof{
    Groups = sortTpDefs(Defs);
    valis (Groups//(G)=>.recType((G//bildTp)*))
  }

  constructorIndexType = .fldTp(.const,i32Type).

  generalWType = .RefType((.nonnull,.EqTp)).
  nominalType(Nm) => .RefType((.nonnull,.NamedTp(Nm))).

  bildTp(.tpDef(Lc,Tp,_,Cons)) => valof{
    Nm = tpName(Tp);
    RootDef = .wTypeDef(Nm,.notFinal,[.EqTp],.Struct([constructorIndexType]));
    ConsDefs = Cons//(C)=>bildConTp(Lc,C,.NamedTp(Nm));
    valis [RootDef,..ConsDefs]
  }

  bildConTp(Lc,(.tLbl(Nm,Ar),Tp,_),Root) => valof{
    if ATp ?= funTypeArg(Tp) && (Arity,Els) ?= isTupleType(ATp) && Ar==Arity then{
      valis .wTypeDef(Nm,.final,[Root],.Struct([constructorIndexType,..Els//wFieldType]))
    } else{
      reportError("(internal)Type signature: $(Tp) not consistent with #(Nm)/$(Ar)",Lc);
      valis .wTypeDef(Nm,.final,[Root],.Struct([constructorIndexType]))
    }
  }

  wFieldType(T) => .fldTp(.const,wValueType(T)).

  wValueType:(tipe)=>value_type.
  wValueType(Tp) => case deRef(Tp) in {
    | .nomnal(Nm) => nominalType(Nm)
    | .tpExp(O,_) => wValueType(O) -- We ignore type arguments in the conversion
    | .tupleType(_) => nominalType(tpName(Tp))
    | .allType(_,B) => wValueType(B)
    | .existType(_,B) => wValueType(B)
    | .faceType(_,_) => nominalType(tpName(Tp))
    | .constrainedType(T,_) => wValueType(T)
    | _ default => generalWType
  }
  

  -- buildWasmTypeMap(Defs) => bildMap(Defs,[]).

  -- bildMap([],Map) => Map.
  -- bildMap([Def,..Defs],Map) =>
  --   bildMap(Defs,bildEntry(Def,Map)).

  -- bildEntry(.tpDef(Lc,Tp,Rl,Cons),Map) => valof{
  --   tpNm = dlrName(tpName(Tp)); -- $tpName is the text format name of the type
  --   if _ ?= Map[tpNm] && isEmpty(Cons) then
  --     valis Map
  --   else{

  --   }
  -- }

  sortTpDefs:(cons[cDefn]) => cons[cons[cDefn]].
  sortTpDefs(Defs) => valof{
    Tps = foldRight((D,S)=>defineSpec(D,S),[],Defs);
    All = ixRight((_,D,A)=>[findRefs(D,Tps),..A],[],Tps);
    Sorted = topsort(All);
    valis (Sorted//((Gp)=>(Gp//((.tpSpec(Df,_,_))=>Df))))
  }

  typeName ~> string.

  tpSpec ::= .tpSpec(cDefn,typeName,cons[typeName]).

  implementation depends[tpSpec->>typeName] => {
    references(.tpSpec(_,_,Refs)) => Refs.
    defined(.tpSpec(_,Sp,_),Rf) => Sp==Rf.
  }

  defineSpec:(cDefn,map[typeName,tpSpec])=>map[typeName,tpSpec].
  defineSpec(.tpDef(Lc,Tp,Rl,Cons),Specs) => let{
    TpNm = tpName(Tp)
  } in Specs[TpNm->.tpSpec(.tpDef(Lc,Tp,Rl,Cons),TpNm,[])].
  defineSpec(_,Specs) => Specs.

  findRefs:(tpSpec,map[typeName,tpSpec])=>tpSpec.
  findRefs(.tpSpec(Df where .tpDef(_,Tp,_,Cons).=Df,TpNm,_),Dfs) =>
    .tpSpec(Df,TpNm,findRfsInCons(Cons,Dfs,findRfsInTp(Tp,Dfs,[]))).

  findRfsInTp(Tp,Defs,SoFar) => case Tp in {
    | .voidType => SoFar
    | .anonType => SoFar
    | .kVar(_) => SoFar
    | .kFun(_,_) => SoFar
    | .tVar(_,_) => SoFar
    | .tFun(_,_,_) => SoFar
    | .nomnal(Nm) => (_?=Defs[Nm] ?? SoFar\+Nm || SoFar)
    | .tpFun(Nm,_) => (_?=Defs[Nm] ?? SoFar\+Nm || SoFar)
    | .tpExp(O,A) => findRfsInTp(O,Defs,findRfsInTp(A,Defs,SoFar))
    | .tupleType(Els) => foldRight((E,S)=>findRfsInTp(E,Defs,S),SoFar,Els)
    | .allType(V,T) => findRfsInTp(T,Defs,SoFar) -- valid because V is kVar
    | .existType(V,T) => findRfsInTp(T,Defs,SoFar)
    | .faceType(Flds,Tps) => foldRight(((_,R),S)=>findRfsInRl(R,Defs,S),foldRight(((_,E),S)=>findRfsInTp(E,Defs,S),SoFar,Flds),Tps)
    | .constrainedType(T,C) => findRfsInTp(T,Defs,findRfsInCon(C,Defs,SoFar))
  }

  findRfsInCon(Con,Defs,SoFar) => case Con in {
    | .conTract(_,Ts,Ds) => foldRight((E,S)=>findRfsInTp(E,Defs,S),foldRight((E,S)=>findRfsInTp(E,Defs,S),SoFar,Ts),Ds)
    | .hasField(T,_,R) => findRfsInTp(T,Defs,findRfsInTp(R,Defs,SoFar))
    | .implicit(_,T) => findRfsInTp(T,Defs,SoFar)
    | .raisEs(T) => findRfsInTp(T,Defs,SoFar)
  }

  findRfsInRl(Rl,Defs,SoFar) => case Rl in {
    | .contractExists(_,_,_,B) => findRfsInTp(B,Defs,SoFar)
    | .typeLambda(L,R) => findRfsInTp(L,Defs,findRfsInTp(R,Defs,SoFar))
    | .allRule(_,Rl) => findRfsInRl(Rl,Defs,SoFar)
  }

  findRfsInCons:(cons[(termLbl,tipe,integer)],map[typeName,tpSpec],cons[typeName])=>cons[typeName].
  findRfsInCons(Cons,Dfs,SoFar) =>
    foldRight(((_,T,_),S)=>findRfsInTp(T,Dfs,S),SoFar,Cons).
  }
