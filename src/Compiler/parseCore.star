star.compiler.core.parse{
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
  
  parseDef:(ast,map[string,tipe],reports) => either[reports,crDefn].
  parseDef(A,D,Rp) where (Lc,L,R) ^= isBinary(A,"=>") &&
      (_,E,T) ^= isBinary(R,":") => do{
	(Nm,Args,ArgTps,D1) <- parseHead(A,D,Rp);
	RTp <- parseType(T,D,Rp);
	Rslt <- parseExp(E,some(RTp),D1,Rp);
	valis fnDef(Lc,Nm,funType(ArgTps,RTp),Args,Rslt)
      }.

  parseHead:(ast,map[string,tipe],reports)=>either[reports,(string,cons[crVar],cons[tipe],map[string,tipe])].
  parseHead(A,D,Rp) where (Lc,N,Args) ^= isRoundTerm(A)  && (_,Nm)^=isNme(N) => do{
    Ags <- seqmap((Ag)=>parseArg(Ag,D,Rp),Args);
    Tps .= (Ags//typeOf);
    D1 .= declareArgs(Ags,D);
    valis (Nm,Ags,Tps,D1)
  }

  parseArg:(ast,map[string,tipe],reports) => either[reports,crVar].
  parseArg(A,D,Rp) where (_,N,T)^=isBinary(A,":") && (_,Id) ^= isNme(N) =>do{
    Tp <- parseType(T,D,Rp);
    valis crId(Id,Tp)
  }

  declareArgs:(cons[crVar],map[string,tipe])=>map[string,tipe].
  declareArgs([],D)=>D.
  declareArgs([crId(Nm,Tp),..Ags],D) => declareArgs(Ags,D[Nm->Tp]).
  

  parseExp:(ast,option[tipe],map[string,tipe],reports)=>either[reports,crExp].
  parseExp(int(Lc,Ix),_,_,_) => either(crInt(Lc,Ix)).
  parseExp(num(Lc,Dx),_,_,_) => either(crFlot(Lc,Dx)).
  parseExp(str(Lc,Sx),_,_,_) => either(crStrg(Lc,Sx)).
  parseExp(nme(Lc,"_void"),some(Tp),_,_) => either(crVoid(Lc,Tp)).
  parseExp(nme(Lc,Id),.none,D,_) where Tp^=D[Id] => either(crVar(Lc,crId(Id,Tp))).
  parseExp(nme(Lc,Id),some(Tp),D,_) => either(crVar(Lc,crId(Id,Tp))).
  parseExp(nme(Lc,Id),.none,D,Rp) => other(reportError(Rp,"$(Id) not declared",Lc)).
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
    T <- parseType(R,D,Rp);
    parseExp(L,some(T),D,Rp)
  }
  parseExp(A,Tp,D,Rp) where (Lc,L,R) ^= isBinary(A,"in") &&
      (_,N,B) ^= isBinary(L,"=") => do{
	crVar(Lc,Vr) <- parseExp(N,.none,D,Rp);
	EB <- parseExp(B,some(typeOf(Vr)),D,Rp);
	ER <- parseExp(R,Tp,D[crName(Vr)->typeOf(Vr)],Rp);
	valis crLtt(Lc,Vr,EB,ER)
      }.
  parseExp(app(Lc,nme(_,Op),tpl(_,"(||)",Els)),some(Tp),D,Rp) => do{
    Args <- seqmap((E)=>parseExp(E,.none,D,Rp),Els);
    valis crTerm(Lc,Op,Args,Tp)
  }
  parseExp(app(Lc,nme(_,Op),tpl(_,"()",Els)),some(Tp),D,Rp) where isEscape(Op) =>do{
    Args <- seqmap((E)=>parseExp(E,.none,D,Rp),Els);
    valis crECall(Lc,Op,Args,Tp)
  }
  parseExp(app(Lc,nme(_,Op),tpl(_,"()",Els)),some(Tp),D,Rp) => do{
    Args <- seqmap((E)=>parseExp(E,.none,D,Rp),Els);
    valis crCall(Lc,Op,Args,Tp)
  }
  parseExp(A,some(Tp),D,Rp) where (Lc,L,R) ^= isBinary(A,".") &&
      (_,Els)^=isTuple(R) => do{
	Args <- seqmap((E)=>parseExp(E,.none,D,Rp),Els);
	Ob <- parseExp(L,.none,D,Rp);
	valis crOCall(Lc,Ob,Args,Tp)
      }
  
  parseType:(ast,map[string,tipe],reports)=>either[reports,tipe].
  parseType(nme(_,Id),D,_) where Tp ^=D[Id] => either(Tp).
  
  
}
