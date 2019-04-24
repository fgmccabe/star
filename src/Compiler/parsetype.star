star.compiler.typeparse{
  import star.

  import star.compiler.ast.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.freshen.
  import star.compiler.location.
  import star.compiler.types.
  import star.compiler.unify.
  import star.compiler.wff.

  public parseType:(ast,dict,reports) => either[reports,tipe].

  parseType(Typ,Env,Rp) => let{
    parseT(Tp,Q) where (Lc,V,BT) ^= isQuantified(Tp) => do{
      BV <- parseBoundTpVars(V);
      In <- parseT(BT,Q++BV);
      lift reQuant(BV,In)
    }. 
    parseT(Tp,Q) where (Lc,Nm) ^= isName(Tp) =>
      parseTypeName(Lc,Nm,Q).
    parseT(Tp,Q) where (Lc,O,Args) ^= isSquareTerm(Tp) => do{
      Op <- parseT(O,Q);
      ArgTps <- parseTps(Args,Q);
      if (Qx,OOp) .= freshen(Op,[],Env) then {
	Inn <- applyTypeFun(deRef(OOp),ArgTps);
        return rebind(Qx,Inn)
      } else
        throw reportError(Rp,"Could not freshen $(Op)",Lc)
    }

    parseTps:(list[ast],list[(string,tipe)]) => either[reports,list[tipe]].
    parseTps([],_) => either([]).
    parseTps([T,..L],Q) => do{
      Tl <- parseT(T,Q);
      Tr <- parseTps(L,Q);
      lift [Tl,..Tr]
    }

    parseTypeName(_,"_",_) => either(newTypeVar("_")).
    parseTypeName(Lc,"this",_) => either(thisType).
    parseTypeName(_,Nm,Q) where (Nm,Tp) in Q => either(Tp).

    applyTypeFun(kFun(Nm,Ar),Args) where size(Args)==Ar => either(mkTypeExp(kFun(Nm,Ar),Args)).
    

    parseBoundTpVars:(list[ast])=>either[reports,list[(string,tipe)]].
    parseBoundTpVars([]) => either([]).
    parseBoundTpVars([V,..R]) =>
      parseBoundTpVars(R) >>= (L) =>
      parseBoundTpVar(V) >>= (Vr) => return [Vr,..L].
    
    parseBoundTpVar(Nm) where (_,Id) ^= isName(Nm) => either((Id,newTypeVar(Id))).
    parseBoundTpVar(FNm) where
	(_,Lhs,Rhs) ^= isBinary(FNm,"/") &&
    (_,Id) ^= isName(Lhs) &&
    (_,Ar) ^= isInt(Rhs) => either((Id,newTypeFun(Id,Ar))).
    parseBoundTpVar(O) default =>
      other(reportError(Rp,"invalid bound type variable $(O)",locOf(O))).

    rebind:(list[(string,tipe)],tipe)=>tipe.
    rebind([],T) => T.
    rebind([(Nm,TV),..L],T) where Ar ^= isUnboundFVar(TV) && sameType(TV,kFun(Nm,Ar),Env) =>
      rebind(L,allType(kFun(Nm,Ar),T)).
    rebind([(Nm,TV),..L],T) where sameType(TV,kVar(Nm),Env) =>
      rebind(L,allType(kVar(Nm),T)).
    
      
    
  } in parseT(Typ,[]).

  reQuant:(list[(string,tipe)],tipe) => tipe.
  reQuant([],Tp) => Tp.
  reQuant([(_,KV),..T],Tp) => reQuant(T,allType(KV,Tp)).



}
