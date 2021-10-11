star.compiler.core.ast{
  import star.
  import star.compiler.ast.
  import star.compiler.core.
  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.wff.
  import star.pkg.

  -- parse an ast as a core expression
  -- A definition takes one of a few forms:
  -- function:
  -- name(P:T,..,P:T)=>E:T
  -- global variable definition:
  -- name:T = E
  
  public parseCrDef:(ast,map[string,tipe],reports) => either[reports,crDefn].
  parseCrDef(A,D,Rp) where (Lc,L,R) ^= isBinary(A,"=>") &&
      (_,E,T) ^= isBinary(R,":") => do{
	(Nm,Args,ArgTps,D1) <- parseHead(A,D,Rp);
	Tp <- astType(T,D,Rp);
	Rslt <- parseExp(E,some(Tp),D1,Rp);
	valis fnDef(Lc,Nm,funType(ArgTps,Tp),Args,Rslt)
      }.
  parseCrDef(A,D,Rp) where (Lc,L,R) ^= isBinary(A,"=") &&
      (_,N,T) ^= isBinary(L,":") && (_,Id)^=isNme(N) => do{
	Tp <- astType(T,D,Rp);
	Vl <- parseExp(R,some(Tp),D[Id->Tp],Rp);
	valis glbDef(Lc,Id,Tp,Vl)
      }.
  parseCrDef(A,D,Rp) where (Lc,L,R) ^= isBinary(A,"<=>") &&
      (_,Op,Els) ^= isBrTerm(R) && (_,Id) ^= isNme(Op) => do{
	Tp <- astType(L,D,Rp);
	Flds <- parseFields(Els,[],[],D,Rp);
	valis rcDef(Lc,Id,Tp,Flds)
      }.
  parseCrDef(A,D,Rp) where (Lc,L,R) ^= isBinary(A,"<~") => do{
    HdTp <- astType(L,D,Rp);
    Face <- astType(R,D,Rp);
    valis tpDef(Lc,HdTp,Face)
  }
  parseCrDef(A,D,Rp) where (Lc,Q,I) ^= isQuantified(A) => do{
    Qs <- seqmap((V)=>astType(V,D,Rp),Q);
    Df <- parseCrDef(I,D,Rp);
    valis reQuantifyDef(Lc,Qs,Df)
  }
  parseCrDef(A,_,Rp) => other(reportError(Rp,"Cannot parse $(A)",locOf(A))).

  reQuantifyDef(Lc,Q,tpDef(_,Hd,Fc)) =>
    tpDef(Lc,reQuant(Q,Hd),reQuant(Q,typeExists(Hd,Fc))).
  reQuantifyDef(Lc,Q,rcDef(_,Id,Tp,Flds)) =>
    rcDef(Lc,Id,reQuant(Q,Tp),Flds).
  

  parseHead:(ast,map[string,tipe],reports)=>either[reports,(string,cons[crVar],cons[tipe],map[string,tipe])].
  parseHead(A,D,Rp) where (Lc,N,Args) ^= isRoundTerm(A)  && (_,Nm)^=isNme(N) => do{
    Ags <- seqmap((Ag)=>parseArg(Ag,D,Rp),Args);
    Tps .= (Ags//typeOf);
    D1 .= declareArgs(Ags,D);
    valis (Nm,Ags,Tps,D1)
  }

  parseArg:(ast,map[string,tipe],reports) => either[reports,crVar].
  parseArg(A,D,Rp) where (_,N,T)^=isBinary(A,":") && (_,Id) ^= isNme(N) =>do{
    Tp <- astType(T,D,Rp);
    valis crId(Id,Tp)
  }

  declareArgs:(cons[crVar],map[string,tipe])=>map[string,tipe].
  declareArgs([],D)=>D.
  declareArgs([crId(Nm,Tp),..Ags],D) => declareArgs(Ags,D[Nm->Tp]).

  parseExp:(ast,option[tipe],map[string,tipe],reports)=>either[reports,crExp].
  parseExp(int(Lc,Ix),_,_,_) => either(crInt(Lc,Ix)).
  parseExp(num(Lc,Dx),_,_,_) => either(crFlot(Lc,Dx)).
  parseExp(str(Lc,Sx),_,_,_) => either(crStrg(Lc,Sx)).
  parseExp(nme(Lc,Id),.none,D,_) where Tp^=D[Id] => either(crVar(Lc,crId(Id,Tp))).
  parseExp(nme(Lc,Id),some(Tp),D,_) => either(crVar(Lc,crId(Id,Tp))).
  parseExp(nme(Lc,Id),.none,D,Rp) => other(reportError(Rp,"$(Id) not declared",Lc)).
  parseExp(qnm(Lc,"$void"),some(Tp),_,_) => either(crVoid(Lc,Tp)).
  parseExp(qnm(Lc,Id),.none,D,_) where Tp^=D[Id] => either(crVar(Lc,crId(Id,Tp))).
  parseExp(qnm(Lc,Id),some(Tp),D,_) => either(crVar(Lc,crId(Id,Tp))).
  parseExp(qnm(Lc,Id),.none,D,Rp) => other(reportError(Rp,"$(Id) not declared",Lc)).
  
  parseExp(A,Tp,D,Rp) where (_,[E])^=isTuple(A) => parseExp(E,Tp,D,Rp).
  parseExp(A,some(Tp),_,_) where (Lc,nme(_,Id))^=isUnary(A,".") =>
    either(crLbl(Lc,Id,Tp)).
  parseExp(A,some(Tp),D,Rp) where (Lc,R,F) ^= isBinary(A,".") && (_,Ix)^=isInt(F) => do{
    Rc <- parseExp(R,.none,D,Rp);
    valis  crTplOff(Lc,Rc,Ix,Tp)
  }
  parseExp(A,some(Tp),D,Rp) where (Lc,R,F) ^= isBinary(A,".") &&
      (_,Nm)^=isNme(F) => do{
    Rc <- parseExp(R,.none,D,Rp);
    valis crDot(Lc,Rc,Nm,Tp)
  }.
  parseExp(A,Tp,D,Rp) where (Lc,L,R) ^= isBinary(A,"&&") => do{
    LL <- parseExp(L,some(boolType),D,Rp);
    RR <- parseExp(R,some(boolType),D,Rp);
    valis crCnj(Lc,LL,RR)
  }
  parseExp(A,Tp,D,Rp) where (Lc,L,R) ^= isBinary(A,"||") => do{
    LL <- parseExp(L,some(boolType),D,Rp);
    RR <- parseExp(R,some(boolType),D,Rp);
    valis crDsj(Lc,LL,RR)
  }
  parseExp(A,Tp,D,Rp) where (Lc,R) ^= isUnary(A,"~") => do{
    RR <- parseExp(R,some(boolType),D,Rp);
    valis crNeg(Lc,RR)
  }
  parseExp(A,Tp,D,Rp) where (Lc,T,L,R) ^= isConditional(A) => do{
    TT <- parseExp(T,some(boolType),D,Rp);
    LL <- parseExp(L,Tp,D,Rp);
    RR <- parseExp(R,Tp,D,Rp);
    valis crCnd(Lc,TT,LL,RR)
  }
  parseExp(A,Tp,D,Rp) where (Lc,L,R) ^= isWhere(A) => do{
    LL <- parseExp(L,Tp,D,Rp);
    RR <- parseExp(R,some(boolType),D,Rp);
    valis crWhere(Lc,LL,RR)
  }
  parseExp(A,Tp,D,Rp) where (Lc,L,R) ^= isMatch(A) => do{
    LL <- parseExp(L,Tp,D,Rp);
    RR <- parseExp(R,Tp,D,Rp);
    valis crMatch(Lc,LL,RR)
  }
  parseExp(A,_,D,Rp) where (Lc,L,R) ^= isBinary(A,":") => do{
    T <- astType(R,D,Rp);
    parseExp(L,some(T),D,Rp)
  }
  parseExp(A,Tp,D,Rp) where (Lc,L,R) ^= isBinary(A,"in") &&
      (_,N,B) ^= isBinary(L,"=") => do{
	crVar(Lc,Vr) <- parseExp(N,.none,D,Rp);
	EB <- parseExp(B,some(typeOf(Vr)),D,Rp);
	ER <- parseExp(R,Tp,D[crName(Vr)->typeOf(Vr)],Rp);
	valis crLtt(Lc,Vr,EB,ER)
      }.
  parseExp(app(Lc,Op,tpl(_,"(||)",Els)),some(Tp),D,Rp) where (_,Id)^=isNme(Op) => do{
    Args <- seqmap((E)=>parseExp(E,.none,D,Rp),Els);
    valis crTerm(Lc,Id,Args,Tp)
  }
  parseExp(app(Lc,Op,tpl(_,"()",Els)),some(Tp),D,Rp) where
      (_,Id)^=isNme(Op) && isEscape(Id) =>do{
	Args <- seqmap((E)=>parseExp(E,.none,D,Rp),Els);
	valis crECall(Lc,Id,Args,Tp)
      }.
  parseExp(app(Lc,Op,tpl(_,"()",Els)),some(Tp),D,Rp) where
      (_,Id)^=isNme(Op) => do{
	Args <- seqmap((E)=>parseExp(E,.none,D,Rp),Els);
	valis crCall(Lc,Id,Args,Tp)
      }.
  parseExp(app(Lc,Op,tpl(_,"{}",Ss)),some(Tp),D,Rp) where
      (_,Id)^=isNme(Op) => do{
	Els <- seqmap((E)=>parseRecordField(E,D,Rp),Ss);
	valis crRecord(Lc,Id,Els,Tp)
      }
  parseExp(A,some(Tp),D,Rp) where (Lc,L,R) ^= isBinary(A,".") &&
      (_,Els)^=isTuple(R) => do{
	Args <- seqmap((E)=>parseExp(E,.none,D,Rp),Els);
	Ob <- parseExp(L,.none,D,Rp);
	valis crOCall(Lc,Ob,Args,Tp)
      }.

  parseRecordField(A,D,Rp) where (Lc,N,E) ^= isBinary(A,"=") &&
      (_,Id)^=isNme(N) => do{
	Exp <- parseExp(E,.none,D,Rp);
	valis (Id,Exp)
      }.
  
  astType:(ast,map[string,tipe],reports)=>either[reports,tipe].
  astType(nme(_,Id),D,_) where Tp ^=D[Id] => either(Tp).
  astType(qnm(_,Id),D,_) where Tp ^=D[Id] => either(Tp).
  astType(N,D,_) where (_,Id)^=isNme(N) => either(nomnal(Id)).
  astType(A,D,Rp) where (_,O,[E]) ^= isSquareTerm(A) &&
      (L,R) ^= isDepends(E) => do{
	Op <- astType(O,D,Rp);
	ArgTps <- typeArgs(L,D,Rp);
	DepTps <- typeArgs(R,D,Rp);
	valis funDeps(mkTypeExp(deRef(Op),ArgTps),DepTps)
      }.
  astType(A,D,Rp) where (_,O,L) ^= isSquareTerm(A) => do{
    Op <- astType(O,D,Rp);
    ArgTps <- typeArgs(L,D,Rp);
    valis mkTypeExp(deRef(Op),ArgTps)
  }.
  astType(A,D,Rp) where (_,V,BT) ^= isQuantified(A) => do{
    (BV,D1) <- boundTypes(V,D,Rp);
    In <- astType(BT,D1,Rp);
    valis reQuant(BV,In)
  }
  astType(A,D,Rp) where (_,V,BT) ^= isXQuantified(A) => do{
    (BV,D1) <- boundTypes(V,D,Rp);
    In <- astType(BT,D1,Rp);
    valis reQuantX(BV,In)
  }

  boundTypes([],D,Rp) => either(([],D)).
  boundTypes([V,..R],D,Rp) => do{
    (L,D1) <- boundTypes(R,D,Rp);
    (Vr,D2) <- boundType(V,D1,Rp);
    valis ([Vr,..L],D2)
  }

  boundType(N,D,_) where (_,Id)^=isNme(N) =>
    either((nomnal(Id),D[Id->nomnal(Id)])).
  boundType(V,D,_) where (_,L,R) ^= isBinary(V,"/") &&
      (_,Id) ^= isNme(L) &&
      (_,Ar) ^= isInt(R) => do{
	Tp.=kFun(Id,Ar);
	valis (Tp,D[Id->Tp])
      }
	
    

  typeArgs(L,D,Rp) => seqmap((T)=>astType(T,D,Rp),L).

  parseFields:(cons[ast],cons[(string,tipe)],cons[(string,tipe)],
    map[string,tipe],reports)=>either[reports,tipe].
  parseFields([],Flds,Tps,_,_) => either(faceType(Flds,Tps)).
  parseFields([E,..Es],Flds,Tps,D,Rp) => do{
    if (_,R) ^= isUnary(E,"type") &&
	(_,N,T)^=isBinary(R,":") && (_,Id)^=isNme(N) then{
	  Tp <- astType(T,D,Rp);
	  parseFields(Es,Flds,[(Id,Tp),..Tps],D,Rp)
	}
    else if (_,N,T)^=isBinary(E,":") && (_,Id)^=isNme(N) then{
      Tp <- astType(T,D,Rp);
      parseFields(Es,[(Id,Tp),..Flds],Tps,D,Rp)
    } else
    raise reportError(Rp,"invalid field $(E)",locOf(E))
  }

  exprAst:(crExp)=>ast.
  exprAst(crVar(Lc,crId(Id,_))) => exprId(Lc,Id).
  exprAst(crInt(Lc,Ix)) => int(Lc,Ix).
  exprAst(crFlot(Lc,Dx)) => num(Lc,Dx).
  exprAst(crStrg(Lc,Sx)) => str(Lc,Sx).
  exprAst(crVoid(Lc,Tp)) => typedAst(Lc,qnm(Lc,"$void"),Tp).
  exprAst(crLbl(Lc,Nm,Tp)) => binary(Lc,":",
    unary(Lc,".",exprId(Lc,Nm)),tpeAst(Lc,Tp)).
  exprAst(crTerm(Lc,Nm,Els,Tp)) =>
    typedAst(Lc,app(Lc,exprId(Lc,Nm),tpl(Lc,"(||)",
	  Els//exprAst)),Tp).
  exprAst(crCall(Lc,Nm,Els,Tp)) =>
    typedAst(Lc,app(Lc,exprId(Lc,Nm),tpl(Lc,"()",
	  Els//exprAst)),Tp).
  exprAst(crECall(Lc,Nm,Els,Tp)) =>
    typedAst(Lc,app(Lc,exprId(Lc,Nm),tpl(Lc,"()",
	  Els//exprAst)),Tp).
  exprAst(crOCall(Lc,Op,Els,Tp)) =>
    typedAst(Lc,binary(Lc,".",exprAst(Op),tpl(Lc,"()",
	  Els//exprAst)),Tp).
    


  exprId(Lc,Id) => (isIdentifier(Id) ? nme(Lc,Id) || qnm(Lc,Id)).  

  isIdentifier:(string)=>boolean.
  isIdentifier(Id) => Ch in (Id::cons[integer]) *> isAlphaNum(Ch).

  typedAst:(locn,ast,tipe) => ast.
  typedAst(Lc,E,T) => binary(Lc,":",E,tpeAst(Lc,T)).

  tpeAst:(locn,tipe)=>ast.
  tpeAst(Lc,nomnal(Nm)) where isIdentifier(Nm) => nme(Lc,Nm).
  tpeAst(Lc,nomnal(Nm)) => qnm(Lc,Nm).
  
  
}
