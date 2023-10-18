star.compiler.macro.grammar{
  import star.
  import star.location.
  import star.pkg.
  import star.uri.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.macro.grtypes.
  import star.compiler.macro.infra.
  import star.compiler.operators.
  import star.compiler.parser.
  import star.compiler.wff.

  /*
  * implement grammar notation, using rules of the form:

  NT(Ptns) >> Val --> [Term], NT(Exp)>>Ptn, {Test}, || ... || [] ||...

  The type of such a rule looks like:

  all s,t ~~ stream[s->>t],hasLoc[t] |: (s,Ptypes) => (s,option[Valtype])
  */


  parseHead:(ast) => option[(option[locn],string,cons[ast],option[ast],ast,boolean)].
  parseHead(A) => parseHd(A,.none,unit(locOf(A)),.false).

  parseHd(A,Cond,_Val,Deflt) where (Lc,L,R) ?= isBinary(A,">>") =>
    parseHd(L,Cond,R,Deflt).
  parseHd(A,_Cond,Val,Deflt) where (_,L,R) ?= isWhere(A) =>
    parseHd(L,.some(R),Val,Deflt).
  parseHd(A,Cond,Val,_Deflt) where (_,L) ?= isDefault(A) =>
    parseHd(L,Cond,Val,.true).
  parseHd(A,Cond,Val,Deflt) where (Lc,Op,Args) ?= isRoundTerm(A) && (_,Id)?=isName(Op)=>
    .some((Lc,Id,Args,Cond,Val,Deflt)).
  parseHd(A,Cond,Val,Deflt) where (Lc,Id) ?= isName(A) =>
    .some((Lc,Id,[],Cond,Val,Deflt)).
  parseHd(A,_,_,_) => valof{
    reportError("cannot parse $(A) as a head of a grammar rule",locOf(A));
    valis .none
  }
  
  parseTerminal(A) => .some(.term(locOf(A),A)).

  parseNonTerminal(A) where (Lc,Nm,Args) ?= isRoundTerm(A) =>
    .some(.nonTerm(Lc,Nm,Args)).
  parseNonTerminal(A) where (Lc,_) ?= isName(A) =>
    .some(.nonTerm(Lc,A,[])).
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
  parseBody(A) where (Lc,Txt) ?= isStr(A) => valof{
    Els = Txt::cons[char];
    valis .some(foldLeft((C,X)=>.seq(Lc,.term(Lc,.chr(Lc,C)),X),.epsilon(Lc),Els))
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
  parseBody(A) where (Lc,R) ?= isUnary(A,"*>") => valof{
    if Rhs?=parseBody(R) then
      valis .some(.skip(Lc,Rhs))
    else{
      valis .none
    }
  }
  parseBody(A) where (Lc,[El]) ?= isBrTuple(A) => .some(.test(Lc,El)).
  parseBody(A) where (Lc,"fail") ?= isName(A) => .some(.block(Lc)).
  parseBody(A) where (Lc,"end") ?= isName(A) => .some(.end(Lc)).
  parseBody(A) => parseNonTerminal(A).

  pairUp:(option[grBody],option[grBody],option[locn]) => option[grBody].
  pairUp(B,.none,_) => B.
  pairUp(.none,B,_) => B.
  pairUp(.some(L),.some(R),Lc) => .some(.seq(Lc,L,R)).

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
  parseRule(_) default => .none.

  makeBody:(grBody,ast,ast,option[ast]) => ast.
  makeBody(.epsilon(Lc),Str,Nxt,.none) => mkMatch(Lc,Nxt,Str). -- Nxt.=Str
  makeBody(.epsilon(Lc),Str,Nxt,.some(V)) => valof{
    reportError("cannot produce $(V) from []",Lc);
    valis mkMatch(Lc,Nxt,Str)
  }
  makeBody(.term(Lc,T),Str,Nxt,.none) => hdtl(Lc,T,Nxt,Str).
  makeBody(.term(Lc,T),Str,Nxt,.some(V)) =>
    mkConjunct(Lc,
      hdtl(Lc,T,Nxt,Str),
      mkMatch(Lc,V,unary(Lc,"_value",T))).
  makeBody(.nonTerm(Lc,N,Args),Str,Nxt,.none) =>
    mkOptionMatch(Lc,rndTuple(Lc,[mkAnon(Lc),Nxt]),roundTerm(Lc,N,[Str,..Args])).
  makeBody(.nonTerm(Lc,N,Args),Str,Nxt,.some(V)) =>
    mkOptionMatch(Lc,rndTuple(Lc,[V,Nxt]),roundTerm(Lc,N,[Str,..Args])).
  makeBody(.prod(_,B,P),Str,Nxt,.none) =>
    makeBody(B,Str,Nxt,.some(P)).
  makeBody(.prod(Lc,B,P),Str,Nxt,.some(V)) => valof{
    reportError("conflicting production: $(P) cannot override $(V)",Lc);
    valis makeBody(B,Str,Nxt,.some(V))
  }
  makeBody(.test(Lc,T),Str,Nxt,.none) =>
    mkConjunct(Lc,T,mkMatch(Lc,Nxt,Str)). --  Test && Nxt.=Str
  makeBody(.test(Lc,T),Str,Nxt,.some(V)) => valof{
    reportError("cannot produce $(V) from $(T)",Lc);
    valis mkConjunct(Lc,T,mkMatch(Lc,Nxt,Str)).
  }
  makeBody(.seq(Lc,L,R),Str,Nxt,V) => valof{
    Vr = genName(Lc,"V");
    valis mkConjunct(Lc,makeBody(L,Str,Vr,.none),makeBody(R,Vr,Nxt,V))
  }
  makeBody(.dis(Lc,L,R),Str,Nxt,V) => 
    mkDisjunct(Lc,makeBody(L,Str,Nxt,V), makeBody(R,Str,Nxt,V)).
  makeBody(.rep(Lc,L),Str,Nxt,V) => valof{
    /*
    let{.
    s(S0,SoF) where (X,S2)?=lft(S0) => s(S2,[X,..SoF]).
    s(S0,SoF) => .some((reverse(SoF),S0))
    .} in (V,Nxt) ?= s(S0,[]))
    */
    s = genName(Lc,"s");
    S0 = genName(Lc,"S0");
    S1 = genName(Lc,"S1");
    S2 = genName(Lc,"S2");
    F = genName(Lc,"F");
    SoF = genName(Lc,"SoF");
    Hd = roundTerm(Lc,s,[S0,SoF]);
    X = genName(Lc,"X");
    Eq1 = mkEquation(Lc,.some(s),.false,rndTuple(Lc,[S0,SoF]),
      .some(makeBody(L,S0,S1,.some(X))),
      roundTerm(Lc,s,[S1,mkCon(Lc,"cons",[X,SoF])]));
    Eq2 = mkEquation(Lc,.some(s),.true,rndTuple(Lc,[S0,SoF]),.none,
      mkOption(Lc,rndTuple(Lc,[unary(Lc,"reverse",SoF),S0])));
    Val = (VV ?= V ?? VV || mkAnon(Lc));
    valis mkOptionMatch(Lc,rndTuple(Lc,[Val,Nxt]),
      mkLetRecDef(Lc,[Eq1,Eq2],roundTerm(Lc,s,[Str,sqTuple(Lc,[])])))
  }
  makeBody(.sep(Lc,L,R),Str,Nxt,V) => valof{
    /*
    let{.
      f(S0) => <lft>(S0).
      f(S0) default => .none.
      s(S0,SoF) where (_,S1) ?= <right>(S0) && (X,S2)?=f(S1) => s(S2,[X,..SoF]).
      s(S0,SoF) => .some((reverse(SoF),S0))
      p(S0) where (Si,F)?=f(S0) => s(Si,[F])
      p(_) default => .none
    .} in && (V,Ntx) ?= p(Str)
    */
    s = genName(Lc,"s");
    f = genName(Lc,"f");
    c = genName(Lc,"c");
    S0 = genName(Lc,"S0");
    S1 = genName(Lc,"S1");
    S2 = genName(Lc,"S2");
    Si = genName(Lc,"Si");
    SoF = genName(Lc,"SoF");
    X = genName(Lc,"X");
    Fr = genName(Lc,"Fr");

    /* Build equations that implement left hand side of L*R */
    Eqf1 = makeEqnFromBody(Lc,f,L,.false);
    Eqf2 = makeEqnFromBody(Lc,f,.block(Lc),.true);

    /* Build equations for combined lhs&rhs of L*R) */
    Rgt = makeBody(R,S0,S1,.none);
    Lft = mkOptionMatch(Lc,rndTuple(Lc,[X,S2]),roundTerm(Lc,f,[S1]));
    Val = roundTerm(Lc,s,[S2,mkCon(Lc,"cons",[X,SoF])]);
    
    Tst = mkConjunct(Lc,Rgt,Lft);
    Eq1 = mkEquation(Lc,.some(s),.false,rndTuple(Lc,[S0,SoF]),.some(Tst),Val);
    Eq2 = mkEquation(Lc,.some(s),.true,rndTuple(Lc,[S0,SoF]),.none,
      mkOption(Lc,rndTuple(Lc,[unary(Lc,"reverse",SoF),S0])));

    /* build equations for overall production */
    AA = mkOptionMatch(Lc,rndTuple(Lc,[Fr,Si]),roundTerm(Lc,f,[S0]));
    Eqb1 = mkEquation(Lc,.some(c),.false,rndTuple(Lc,[S0]),.some(AA),roundTerm(Lc,s,[Si,mkCon(Lc,"cons",[Fr,enum(Lc,"nil")])]));
    Eqb2 = mkEquation(Lc,.some(c),.true,rndTuple(Lc,[S0]),.none,enum(Lc,"none"));

    BB = mkLetRecDef(Lc,[Eqf1,Eqf2,Eq1,Eq2,Eqb1,Eqb2],roundTerm(Lc,c,[Str]));
    Val = (VV ?= V ?? VV || mkAnon(Lc));
    valis mkOptionMatch(Lc,rndTuple(Lc,[Val,Nxt]),BB);
  }
  makeBody(.skip(Lc,R),Str,Nxt,V) => valof{
    /*
    (_,Nxt) ?= let{.
      sk(S0) where (_,_)?=lft(S0) => .some(((),S0)).
      sk(S0) where (_,S1) ?= hdtl(S0) => sk(S1)
    .} in s(S0)
    */
    sk = genName(Lc,"sk");
    S0 = genName(Lc,"S0");
    S1 = genName(Lc,"S1");
    An = mkAnon(Lc);

    Rgt = makeBody(R,S0,An,.none);
    Eq1 = mkEquation(Lc,.some(sk),.false,rndTuple(Lc,[S0]),
      .some(Rgt),
      mkOption(Lc,rndTuple(Lc,[unit(Lc),S0])));
    Eq2 = mkEquation(Lc,.some(sk),.false,rndTuple(Lc,[S0]),
      .some(hdtl(Lc,An,S1,S0)),
      roundTerm(Lc,sk,[S1]));
    Rh = mkLetRecDef(Lc,[Eq1,Eq2],roundTerm(Lc,sk,[Str]));
    valis mkOptionMatch(Lc,rndTuple(Lc,[An,Nxt]),Rh)
  }
  makeBody(.neg(Lc,R),Str,Nxt,.none) => valof{
    Vr = genName(Lc,"V");
    valis mkConjunct(Lc,negated(Lc,makeBody(R,Str,Vr,.none)),
      mkMatch(Lc,Nxt,Str))
  }
  makeBody(.block(Lc),Str,_,_) => enum(Lc,"false"). -- should not ever use this
  makeBody(.end(Lc),Str,Nxt,.none) =>               -- end of the stream
    mkConjunct(Lc,unary(Lc,"_eof",Str),mkMatch(Lc,Nxt,Str)).

  /* Build equations that implement a grammar body */
  makeEqnFromBody(Lc,Nm,B,Dflt) => valof{
    S0 = genName(Lc,"S0");
    S1 = genName(Lc,"S1");

    (Cond,Val) = splitCond(Lc,makeBody(B,S0,S1,.none));
    valis mkEquation(Lc,.some(Nm),Dflt,rndTuple(Lc,[S0]),Cond,Val);
  }

  splitCond(Lc,C) => Cond ?= findOptionMatch(C) ?? Cond || (.none,enum(Lc,"none")).

  findOptionMatch(C) where (Lc,L,R) ?= isConjunct(C) && (Rgt,V) ?= findOptionMatch(R) =>
    .some((mergeCond(.some(L),Rgt),V)).
  findOptionMatch(C) default => (_,L,R) ?= isOptionMatch(C) ?? .some((.none,R)) || .none.

  hdtl(Lc,T,Nxt,Str) =>
    mkOptionMatch(Lc,rndTuple(Lc,[T,Nxt]),unary(Lc,"_hdtl",Str)).

  makeRule:(grRule) => ast.
  makeRule(Rl) => valof{
    Lc = Rl.lc;
    Rst = genName(Lc,"R");
    Str = genName(Lc,"S");
    
    if ~.block(_).=Rl.body then{
      B = makeBody(Rl.body,Str,Rst,.none);
      valis mkEquation(Lc,.some(.nme(Lc,Rl.name)),Rl.isDefault,rndTuple(Lc,[Str,..Rl.args]),
	mergeCond(Rl.cond,.some(B)),
	mkOption(Lc,rndTuple(Lc,[Rl.value,Rst])))
    } else
    valis mkEquation(Lc,.some(.nme(Lc,Rl.name)),Rl.isDefault,rndTuple(Lc,[Str,..Rl.args]),.none,enum(Lc,"none")).
  }

  public makeGrammar:(cons[ast]) => cons[ast].
  makeGrammar(Ss) where (NonRl,Mp) .= collectRules(Ss) =>
    NonRl++ixLeft((Nm,Rs,SoF) => makeGr(Rs)++SoF,[],Mp).

  collectRules:(cons[ast]) => (cons[ast],map[string,cons[grRule]]).
  collectRules(Rls) => let{.
    coll([],SoF,Mp) => (reverse(SoF),Mp).
    coll([S,..Ss],SoF,Mp) where _ ?= isBinary(S,"-->") =>
      coll(Ss,SoF,addRule(parseRule(S),Mp)).
    coll([S,..Ss],SoF,Mp) => coll(Ss,[S,..SoF],Mp)
  .} in coll(Rls,[],{}).

  parseRules:(cons[ast]) => map[string,cons[grRule]].
  parseRules(Ss) => foldLeft((A,M) => addRule(parseRule(A),M),{},Ss).

  addRule(.none,M) => M.
  addRule(.some(Rl),M) => valof{
    if Rs?=M[Rl.name] then
      valis M[Rl.name->Rs++[Rl]]
    else
    valis M[Rl.name->[Rl]]
  }.

  makeGr:(cons[grRule]) => cons[ast].
  makeGr(Gs) where D?=defaultGr(Gs) => makeGrRules(Gs).
  makeGr(Gs) => makeGrRules(Gs++defaultRl(head(Gs))).

  makeGrRules:(cons[grRule]) => cons[ast].
  makeGrRules([G,..Gs]) =>
    [makeRule(G),..makeGrRules(Gs)].
  makeGrRules([]) => [].

  defaultGr:(cons[grRule]) => option[grRule].
  defaultGr([]) => .none.
  defaultGr([G,.._]) where G.isDefault => .some(G).
  defaultGr([_,..Gs]) => defaultGr(Gs).

  defaultRl:(option[grRule])=>cons[grRule].
  defaultRl(.none) => [].
  defaultRl(.some(Rl)) => [grRule{lc=Rl.lc.
      name=Rl.name.
      args=(Rl.args//(A)=>mkAnon(locOf(A))).
      cond = .none.
      isDefault = .true.
      value = enum(Rl.lc,"none").
      body = .block(Rl.lc)
    }].

  public grammarMacro:(ast,macroContext) => macroState.
  grammarMacro(A,.statement) where Rl ?= parseRule(A) =>
    .active(makeRule(Rl)).
  grammarMacro(_,_) default => .inactive.

 -- (Args)>>P --> S, as a type, becomes
 -- (S,..Args) => option[(P,S)]

  public grammarTypeMacro:(ast,macroContext) => macroState.
  grammarTypeMacro(A,.typeterm) where (Lc,L,R) ?= isBinary(A,"-->") && (_,G,P) ?= isBinary(L,">>") && (_,Els) ?= isTuple(G) => valof{
    Ft = mkFunctionType(Lc,rndTuple(Lc,[R,..Els]),
      squareApply(Lc,"option",[rndTuple(Lc,[P,R])]));

    valis .active(Ft)
  }
  grammarTypeMacro(_,_) default => .inactive.
}	
