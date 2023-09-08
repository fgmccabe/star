star.compiler.grammar{
  import star.
  import star.location.
  import star.pkg.
  import star.uri.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.operators.
  import star.compiler.parser.
  import star.compiler.wff.

  /*
  * implement grammar notation, using rules of the form:

  NT(Ptns) >> Val --> [Term], NT(Exp)>>Ptn, {Test}, || ... || [] ||...

  The type of such a rule looks like:

  all s,t ~~ stream[s->>t],hasLoc[t] |: (s,Ptypes) => (s,option[Valtype])
  */

  public grRule ::= grRule{
    lc : option[locn].
    name:ast.
    args:cons[ast].
    cond:option[ast].
    isDefault:boolean.
    value:ast.
    body:grBody}.

  public grSymbol ::=
    .term(ast) |
    .nonTerm(option[locn],ast,cons[ast]).

  public grBody ::=
    .seq(option[locn],grBody,grBody) |
    .rep(option[locn],grBody) |
    .sep(option[locn],grBody,grBody) |
    .dis(option[locn],grBody,grBody) |
    .neg(option[locn],grBody) |
    .single(option[locn],grSymbol) |
    .test(option[locn],ast) |
    .prod(option[locn],grBody,ast) |
    .epsilon(option[locn]).

  implementation equality[grSymbol] => {
    .term(A1) == .term(A2) => A1==A2.
    .nonTerm(_,S1,A1) == .nonTerm(_,S2,A2) => S1==S2 && A1==A2.
  }

  implementation hashable[grSymbol] => {
    hash(.term(S)) => hash(S).
    hash(.nonTerm(_,S,A)) => hash(S)*37+hash(A)
  }

  parseHead:(ast) => option[(option[locn],ast,cons[ast],option[ast],ast,boolean)].
  parseHead(A) => parseHd(A,.none,unit(locOf(A)),.false).

  parseHd(A,Cond,_Val,Deflt) where (Lc,L,R) ?= isBinary(A,">>") =>
    parseHd(L,Cond,R,Deflt).
  parseHd(A,_Cond,Val,Deflt) where (_,L,R) ?= isWhere(A) =>
    parseHd(L,.some(R),Val,Deflt).
  parseHd(A,Cond,Val,_Deflt) where (_,L) ?= isDefault(A) =>
    parseHd(L,Cond,Val,.true).
  parseHd(A,Cond,Val,Deflt) where (Lc,Op,Args) ?= isRoundTerm(A) =>
    .some((Lc,Op,Args,Cond,Val,Deflt)).
  parseHd(A,Cond,Val,Deflt) where (Lc,_) ?= isName(A) =>
    .some((Lc,A,[],Cond,Val,Deflt)).
  parseHd(A,_,_,_) => valof{
    reportError("cannot parse $(A) as a head of a grammar rule",locOf(A));
    valis .none
  }
  
  parseTerminal(A) => .some(.single(locOf(A),.term(A))).

  parseNonTerminal(A) where (Lc,Nm,Args) ?= isRoundTerm(A) =>
    .some(.single(Lc,.nonTerm(Lc,Nm,Args))).
  parseNonTerminal(A) where (Lc,_) ?= isName(A) =>
    .some(.single(Lc,.nonTerm(Lc,A,[]))).
  parseNonTerminal(A) default => valof{
    reportError("invalid non-terminal $(A)",locOf(A));
    valis .none
  }

  parseBody:(ast) => option[grBody].
  parseBody(A) where (Lc,L,R) ?= isBinary(A,"|") => valof{
    if Left?=parseBody(L) && Right?=parseBody(R) then
      valis .some(.dis(Lc,Left,Right))
    else{
      reportError("cannot parse disjunction $(A)",Lc);
      valis .none
    }
  }
  parseBody(A) where (Lc,L,R) ?= isComma(A) => valof{
    if Left?=parseBody(L) && Right?=parseBody(R) then
      valis .some(.seq(Lc,Left,Right))
    else{
      reportError("cannot parse sequence $(A)",Lc);
      valis .none
    }
  }
  parseBody(A) where (Lc,R) ?= isNegation(A) => valof{
    if Right?=parseBody(R) then
      valis .some(.neg(Lc,Right))
    else{
      reportError("cannot parse negated sequence $(A)",Lc);
      valis .none
    }
  }
  parseBody(A) where (Lc,Els) ?= isSqTuple(A) => valof{
    if isEmpty(Els) then
      valis .some(.epsilon(Lc))
    else{
      valis foldLeft((G,X)=>pairUp(parseTerminal(G),X,Lc),.none,Els)
    }
  }
  parseBody(A) where (_,[E]) ?= isTuple(A) => parseBody(E).
  parseBody(A) where (Lc,L) ?= isUnary(A,"*") => valof{
    if LL?=parseBody(L) then
      valis .some(.rep(Lc,LL))
    else
    valis .none
  }
  parseBody(A) where (Lc,L,R) ?= isBinary(A,"*") => valof{
    if LL?=parseBody(L) && RR?=parseBody(R) then
      valis .some(.sep(Lc,LL,RR))
    else
    valis .none
  }
  parseBody(A) where (Lc,L,R) ?= isBinary(A,">>") => valof{
    if Lhs?=parseBody(L) then
      valis .some(.prod(Lc,Lhs,R))
    else{
      valis .none
    }
  }
  parseBody(A) where (Lc,[El]) ?= isBrTuple(A) => .some(.test(Lc,El)).
  parseBody(A) => parseNonTerminal(A).

  pairUp:(option[grBody],option[grBody],option[locn]) => option[grBody].
  pairUp(B,.none,_) => B.
  pairUp(.none,B,_) => B.
  pairUp(.some(L),.some(R),Lc) => .some(.seq(Lc,L,R)).

  implementation iter[grBody->>grSymbol] => {.
    _iter(.epsilon(_),St,_) => St.
    _iter(.seq(_,L,R),St,Fn) => _iter(L,_iter(R,St,Fn),Fn).
    _iter(.dis(_,L,R),St,Fn) => _iter(L,_iter(R,St,Fn),Fn).
    _iter(.single(_,S),St,Fn) => Fn(S,St).
    _iter(.test(_,_),St,_) => St.
  .}

  parseRule(A) where (Lc,L,R) ?= isBinary(A,"-->") => valof{
    if (_,Nm,Args,Cond,Val,Deflt) ?= parseHead(L) then{
      if Body?=parseBody(R) then{
	valis .some(grRule{lc=Lc. name=Nm. args=Args. cond=Cond. isDefault=Deflt. value=Val. body=Body})
      } else{
	reportError("can't parse body of rule: $(R)",Lc);
	valis .none
      }
    };
    valis .none
  }

  public implementation display[grRule] => {
    disp(Rl) => "$(Rl.name)$(Rl.args) #(C?=Rl.cond??" where $(C)"||"") >> $(Rl.value) --> $(Rl.body)".
  }

  public implementation display[grBody] => let{.
    dB(.seq(_,L,R),Pr) where (Lp,P,Rp) ?= isInfixOp(",") =>
      "#(leftPar(P,Pr))#(dB(L,Lp)) , #(dB(R,Rp))#(rightPar(P,Pr))".
    dB(.dis(_,L,R),Pr) where (Lp,P,Rp) ?= isInfixOp("|") =>
      "#(leftPar(P,Pr))#(dB(L,Lp)) | #(dB(R,Rp))#(rightPar(P,Pr))".
    dB(.neg(_,R),Pr) where (P,Rp) ?= isPrefixOp("~") =>
      "#(leftPar(P,Pr)) ~ #(dB(R,Rp))#(rightPar(P,Pr))".
    dB(.rep(_,L),Pr) where (Lp,P) ?= isPostfixOp("*") =>
      "#(leftPar(P,Pr)) #(dB(L,Lp)) * #(rightPar(P,Pr))".
    dB(.sep(_,L,R),Pr) where (Lp,P,Rp) ?= isInfixOp("*") =>
      "#(leftPar(P,Pr)) #(dB(L,Lp)) * #(dB(R,Rp)) #(rightPar(P,Pr))".
    dB(.single(_,L),Pr) =>dispSymbol(L,Pr).
    dB(.prod(_,L,R),Pr) where (Lp,P,Rp) ?= isInfixOp(">>") =>
      "#(leftPar(P,Pr)) #(dB(L,Lp)) >> #(dispAst(R,Rp,"")) #(rightPar(P,Pr))".
    dB(.test(_,T),_) => "{ #(dispAst(T,2000,"")) }".
    dB(.epsilon(_),_) => "[]".
  .} in {
    disp(B) => dB(B,1259)
  }

  dispSymbol(.term(T),Pr) => "[#(dispAst(T,1000,""))]".
  dispSymbol(.nonTerm(_,N,A),Pr) => "$(N)$(A)".
  
  public implementation display[grSymbol] => {
    disp(S) => dispSymbol(S,0)
  }

  compBody:(grBody,ast,ast,option[ast]) => ast.
  compBody(.epsilon(Lc),Str,Nxt,.none) => mkMatch(Lc,Nxt,Str). -- Nxt.=Str
  compBody(.epsilon(Lc),Str,Nxt,.some(V)) => valof{
    reportError("cannot produce $(V) from []",Lc);
    valis mkMatch(Lc,Nxt,Str)
  }
  compBody(.single(Lc,T),Str,Nxt,V) => compSymbol(Lc,T,Str,Nxt,V).
  compBody(.prod(_,B,P),Str,Nxt,.none) =>
    compBody(B,Str,Nxt,.some(P)).
  compBody(.prod(Lc,B,P),Str,Nxt,.some(V)) => valof{
    reportError("conflicting production: $(P) cannot override $(V)",Lc);
    valis compBody(B,Str,Nxt,.some(V))
  }
  compBody(.test(Lc,T),Str,Nxt,.none) =>
    mkConjunct(Lc,T,mkMatch(Lc,Nxt,Str)). --  Test && Nxt.=Str
  compBody(.test(Lc,T),Str,Nxt,.some(V)) => valof{
    reportError("cannot produce $(V) from $(T)",Lc);
    valis mkConjunct(Lc,T,mkMatch(Lc,Nxt,Str)).
  }
  compBody(.seq(Lc,L,R),Str,Nxt,V) => valof{
    Vr = genName(Lc,"V");
    valis mkConjunct(Lc,compBody(L,Str,Vr,.none),compBody(R,Vr,Nxt,V))
  }
  compBody(.dis(Lc,L,R),Str,Nxt,V) => 
    mkDisjunct(Lc,compBody(L,Str,Nxt,V), compBody(R,Str,Nxt,V)).
  compBody(.neg(Lc,R),Str,Nxt,.none) => valof{
    Vr = genName(Lc,"V");
    valis mkConditional(Lc,mkMatch(Lc,enum(Lc,"none"),compBody(R,Str,Vr,.none)),
      mkMatch(Lc,Nxt,Str),
      enum(Lc,"false"))
  }

  compSymbol(Lc,.term(T),Str,Nxt,.none) =>
    mkOptionMatch(Lc,rndTuple(Lc,[T,Nxt]),unary(Lc,"_hdtl",Str)).
  compSymbol(Lc,.term(T),Str,Nxt,.some(V)) =>
    mkConjunct(Lc,
      mkOptionMatch(Lc,rndTuple(Lc,[T,Nxt]),unary(Lc,"_hdtl",Str)),
      mkMatch(Lc,V,T)).
  compSymbol(Lc,.nonTerm(Lc,N,Args),Str,Nxt,.none) =>
    mkOptionMatch(Lc,rndTuple(Lc,[mkAnon(Lc),Nxt]),roundTerm(Lc,N,[Str,..Args])).
  compSymbol(Lc,.nonTerm(Lc,N,Args),Str,Nxt,.some(V)) =>
    mkOptionMatch(Lc,rndTuple(Lc,[V,Nxt]),roundTerm(Lc,N,[Str,..Args])).

  compRule:(grRule) => ast.
  compRule(Rl) => valof{
    Lc = Rl.lc;
    Rst = genName(Lc,"R");
    Str = genName(Lc,"S");
    B = compBody(Rl.body,Str,Rst,.none);
    valis mkEquation(Lc,.some(Rl.name),Rl.isDefault,rndTuple(Lc,[Str,..Rl.args]),Rl.cond,
      mkOption(Lc,rndTuple(Lc,[Rl.value,Rst])))
  }

  public main:(uri,pkg)=>().
  main(U,P) => valof{
    WD=_optval(parseUri("file:"++_cwd()));
    if R ?= resolveUri(WD,U) then{
      Src = parseSrc(R,P);

      logMsg("source text $(Src)");
    };

    valis ()
  }
}	
