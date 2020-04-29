star.compiler.macro{
  import star.

  import star.compiler.ast.
  import star.compiler.ast.disp.
  import star.compiler.errors.
  import star.compiler.misc.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.wff.

  public macroSquarePtn:(locn,cons[ast]) => ast.
  macroSquarePtn(Lc,Els) =>
    macroListEntries(Lc,Els,(Lx)=>mkWhere(Lx,"_eof"),
      (Lx,H,T) => mkWherePtn(Lx,tpl(Lx,"()",[H,T]),nme(Lx,"_hdtl"))).

  public macroSquareExp:(locn,cons[ast]) => ast.
  macroSquareExp(Lc,Els) =>
    macroListEntries(Lc,Els,(Lx)=>nme(Lx,"_nil"),
      (Lx,H,T) => binary(Lx,"_cons",H,T)).

  macroListEntries:(locn,cons[ast],(locn)=>ast,(locn,ast,ast)=>ast) => ast.
  macroListEntries(Lc,[],End,_) => End(Lc).
  macroListEntries(_,[Cns],_,Hed) where (Lc,H,T) ^= isCons(Cns) =>
    Hed(Lc,H,T).
  macroListEntries(Lc,[El,..Rest],Eof,Hed) =>
    Hed(Lc,El,macroListEntries(Lc,Rest,Eof,Hed)).

  public reconstructDisp:(ast)=>ast.
  reconstructDisp(C) where (Lc,Ex,Tp) ^= isCoerce(C) && (_,"string") ^= isName(Tp) => let{
    flat:(ast,cons[string])=>cons[string].
    flat(SS,So) where (_,Tx) ^= isUnary(SS,"ss") && (_,Txt) ^= isStr(Tx) => [Txt,..So].
    flat(E,So) where (_,Sq) ^= isUnary(E,"ssSeq") && (_,L) ^= isSqTuple(Sq) => fltList(L,So).
    flat(E,So) where (_,D) ^= isUnary(E,"disp") => ["\$",D::string,..So].

    fltList(L,So) => foldRight((E,X)=>flat(E,X),So,L).
    
  } in str(Lc,_str_multicat(flat(Ex,[]))).
  reconstructDisp(A) where Lc.=locOf(A) => str(Lc,A::string).

  public buildMain:(cons[ast])=>cons[ast].
  buildMain(Els) where (Lc,Tp) ^= head(lookForSignature(Els,"main")) &&
      !_^=head(lookForSignature(Els,"_main")) =>
    synthesizeMain(Lc,Tp,Els).
  buildMain(Els) default => Els.

  lookForSignature:(cons[ast],string)=>cons[(locn,ast)].
  lookForSignature(Els,Nm) => [(Lc,Tp) | El in Els && (Lc,N,Tp)^=isTypeAnnot(El) && (_,Nm)^=isName(N)].

  isTypeAnnot(A) where Ptn ^= isBinary(A,":") => some(Ptn).
  isTypeAnnot(_) default => .none.
  isTypeAnnot(A) where (_,I) ^= isPublic(A) => isTypeAnnot(I).
  isTypeAnnot(A) where (_,I) ^= isPrivate(A) => isTypeAnnot(I).

  synthesizeMain:(locn,ast,cons[ast])=>cons[ast].
  synthesizeMain(Lc,Tp,Defs) where (_,Lhs,Rhs) ^= isFunctionType(Tp) && (_,ElTps)^=isTuple(Lhs) => valof action{
    (Vs,Cs) .= synthesizeCoercions(ElTps,Lc,[],[]);
    Arg .= sqTuple(Lc,Vs);
    MLhs .= roundTerm(Lc,nme(Lc,"_main"),[Arg]);
    MRhs .= roundTerm(Lc,nme(Lc,"main"),Cs);
    Main .= equation(Lc,MLhs,unary(Lc,"valof",MRhs));
    Annot .= binary(Lc,":",nme(Lc,"_main"),equation(Lc,rndTuple(Lc,[squareTerm(Lc,nme(Lc,"cons"),[nme(Lc,"string")])]),rndTuple(Lc,[])));
--    logMsg(" _main: $(Annot)\n$(Main)");
    valis [unary(Lc,"public",Annot),Main,..Defs].
  }

  synthesizeCoercions:(cons[ast],locn,cons[ast],cons[ast])=> (cons[ast],cons[ast]).
  synthesizeCoercions([],_,Vs,Cs) => (reverse(Vs),reverse(Cs)).
  synthesizeCoercions([T,..Ts],Lc,Vs,Cs) where Nm .= genName(Lc,"X") =>
    synthesizeCoercions(Ts,Lc,[Nm,..Vs],[binary(Lc,"::",Nm,T),..Cs]).

  -- Temporary
  public isSimpleAction:(ast)=>boolean.
  isSimpleAction(A) where (_,L,R) ^= isActionSeq(A) =>
    isSimpleAction(L) && isSimpleAction(R).
  isSimpleAction(A) where _ ^= isValis(A) => .true.
  isSimpleAction(A) where _ ^= isThrow(A) => .true.
  isSimpleAction(A) where (_,[St]) ^= isBrTuple(A) => isSimpleAction(St).
  isSimpleAction(A) where (Lc,L,R) ^= isBind(A) => .true.
  isSimpleAction(A) where (Lc,L,R) ^= isMatch(A) => .true.
  isSimpleAction(A) where (Lc,L,R) ^= isOptionMatch(A) => .true.
  isSimpleAction(A) where (_,B,H) ^= isTryCatch(A) =>
    isSimpleAction(B) && ((_,[St])^=isBrTuple(H) ? isSimpleAction(St) || .true).
  isSimpleAction(A) where _ ^= isIntegrity(A) => .true.
  isSimpleAction(A) where _ ^= isShow(A) => .true.
  isSimpleAction(A) where (_,T,L,R) ^= isIfThenElse(A) =>
    isSimpleAction(L) && isSimpleAction(R).
  isSimpleAction(A) where (_,T,L) ^= isIfThen(A) =>
    isSimpleAction(L).
  isSimpleAction(A) where (_,_,B) ^= isWhileDo(A) =>
    isSimpleAction(B).
  isSimpleAction(A) where (_,_,B) ^= isForDo(A) =>
    isSimpleAction(B).
  isSimpleAction(A) where _ ^= isRoundTerm(A) => .true.
  isSimpleAction(_) default => .false.

  isIterable:(ast) => boolean.
  isIterable(A) where _ ^= isSearch(A) => .true.
  isIterable(A) where (_,L,R) ^= isConjunct(A) =>
    isIterable(L) || isIterable(R).
  isIterable(A) where (_,L,R) ^= isDisjunct(A) =>
    isIterable(L) || isIterable(R).
  isIterable(A) where (_,L,R) ^= isImplies(A) =>
    isIterable(L) || isIterable(R).
  isIterable(_) default => .false.
  
  public makeAction:(ast,option[(locn,ast)],reports) => either[reports,ast].
  makeAction(A,Cont,Rp) where (_,[St]) ^= isBrTuple(A) =>
    makeAction(St,Cont,Rp).
  makeAction(A,Cont,Rp) where (Lc,L,R) ^= isActionSeq(A) => do{
    RR <- makeAction(R,Cont,Rp);
    makeAction(L,some((Lc,RR)),Rp)
  }
  makeAction(A,.none,Rp) where (Lc,R) ^= isValis(A) =>
    either(makeReturn(Lc,R)).
  makeAction(A,some((_,CC)),Rp) where (Lc,R) ^= isValis(A) =>
    other(reportError(Rp,"$(A) must be the last action\n$(CC) follows valis",Lc)).
  makeAction(A,_,Rp) where (Lc,R) ^= isThrow(A) =>
    makeThrow(Lc,R,Rp).
  makeAction(A,.none,Rp) where (Lc,L,R) ^= isBind(A) =>
    other(reportError(Rp,"$(A) may not be the last action",Lc)).
  makeAction(A,some((CLc,Cont)),Rp) where (Lc,L,R) ^= isBind(A) => do{
    Lam .= equation(Lc,rndTuple(Lc,[L]),Cont);
    valis binary(CLc,"_sequence",R,Lam)
  }
  makeAction(A,.none,Rp) where (Lc,L,R) ^= isMatch(A) =>
    other(reportError(Rp,"$(A) may not be the last action",Lc)).
  makeAction(A,some((CLc,Cont)),Rp) where (Lc,L,R) ^= isMatch(A) => do{
    Lam .= equation(Lc,rndTuple(Lc,[L]),Cont);
    valis roundTerm(CLc,Lam,[R])
  }
  makeAction(A,.none,Rp) where (Lc,L,R) ^= isOptionMatch(A) =>
    other(reportError(Rp,"$(A) may not be the last action",Lc)).
  makeAction(A,some((CLc,Cont)),Rp) where (Lc,L,R) ^= isOptionMatch(A) => do{
    Lam .= equation(Lc,rndTuple(Lc,[unary(Lc,"some",L)]),Cont);
    valis roundTerm(CLc,Lam,[R])
  }
  makeAction(A,Cont,Rp) where (Lc,B,H) ^= isTryCatch(A) => do{
--    logMsg("macro try catch $(A)");
    NB <- makeAction(B,.none,Rp);
    NH <- makeHandler(H,Rp);
    
    valis combine(binary(Lc,"_handle",NB,NH),Cont)
  }
  /*
    assert C 
  becomes
  try{
  assrt(()=>C,"failed: C",Loc)
  } catch(Err) => action{
    logMsg(Err)
    throw ()
  }
*/
  makeAction(A,Cont,Rp) where (Lc,C) ^= isIntegrity(A) => do{
    Unit .= tpl(Lc,"()",[]);
    Lam .= equation(Lc,Unit,C);
    Assert .= ternary(Lc,"assrt",Lam,str(Lc,C::string),Lc::ast);
    B .= brTuple(Lc,[Assert]);
--    logMsg("catch body $(B)");
    Err .= genName(Lc,"E");
    Thrw <- makeThrow(Lc,Unit,Rp);
    EH .= combine(unary(Lc,"logMsg",Err),some((Lc,Thrw)));
    ELam .= equation(Lc,rndTuple(Lc,[Err]),EH);
      
    valis combine(binary(Lc,"_handle",Assert,ELam),Cont)
  }
  /*
  show E becomes shwMsg(()=>E,"E",Lc)
*/
   makeAction(A,Cont,Rp) where (Lc,E) ^= isShow(A) => do{
    Lam .= equation(Lc,rndTuple(Lc,[]),E);

    valis combine(ternary(Lc,"shwMsg",Lam,reconstructDisp(E),Lc::ast),Cont)
  }
  makeAction(A,Cont,Rp) where (Lc,T,Th,El) ^= isIfThenElse(A) => do{
    Then <- makeAction(Th,.none,Rp);
    Else <- makeAction(El,.none,Rp);
    if isIterable(T) then {
      Itr <- makeIterableGoal(T,Rp);
      valis combine(conditional(Lc,Itr,Then,Else),Cont)
    } else{
      valis combine(conditional(Lc,T,Then,Else),Cont)
    }
  }
  makeAction(A,Cont,Rp) where (Lc,T,Th) ^= isIfThen(A) => do{
    Then <- makeAction(Th,.none,Rp);
    Unit .= rndTuple(Lc,[]);
    if isIterable(T) then {
      Itr <- makeIterableGoal(T,Rp);
      valis combine(conditional(Lc,Itr,Then,makeReturn(Lc,Unit)),Cont)
    } else{
      valis combine(conditional(Lc,T,Then,makeReturn(Lc,Unit)),Cont)
    }
  }
  makeAction(A,Cont,Rp) where (Lc,"nothing") ^= isEnumSymb(A) => do{
    if (_,CC) ^= Cont then
      valis CC
    else
    valis makeReturn(Lc, rndTuple(Lc,[]))
  }
  makeAction(A,Cont,Rp) where (Lc,[]) ^= isBrTuple(A) => do{
    if (_,CC) ^= Cont then
      valis CC
    else
    valis makeReturn(Lc, rndTuple(Lc,[]))
  }
  /* Construct a local iterator function:
   let{
  loop() => do{ if C then { B; loop() }}
   } in loop()
  */
  makeAction(A,Cont,Rp) where (Lc,Tst,Body) ^= isWhileDo(A) => do{
    Unit .= rndTuple(Lc,[]);
    FnCall .= zeroary(Lc,genSym("loop"));

    Then <- makeAction(mkIfThen(Lc,Tst,actionSeq(Lc,Body,FnCall)),.none,Rp);
    Lam .= equation(Lc,FnCall,Then);
    valis combine(mkLetDef(Lc,[Lam],FnCall),Cont)
  }
    /*
   for C do {A}
  becomes:
  
  <iterator>( do{return ()}, (Lcls,St) => do {A; return ()})
*/
  makeAction(A,Cont,Rp) where (Lc,C,B) ^= isForDo(A) => do{
    Unit .= rndTuple(Lc,[]);
    Zed .= makeReturn(Lc,Unit);
    IterBody <- makeAction(B,some((Lc,Zed)),Rp);
    Loop <- makeCondition(C,genRtn,
      (St,X,Rs) => binary(Lc,"_sequence",genRtn(X),
	equation(Lc,rndTuple(Lc,[St]),Rs)),
      (St)=>IterBody,
      lyfted(Zed),Rp);
    valis combine(Loop,Cont)
  }
    
  makeAction(A,Cont,_) where _ ^= isRoundTerm(A) =>
    either(combine(A,Cont)).

  anon(Lc) => nme(Lc,"_").

  makeIterableGoal:(ast,reports) => either[reports,ast].
  makeIterableGoal(A,Rp) => do{
    Lc .= locOf(A);
    Vrs .= (goalVars(A) // (Nm)=>nme(Lc,Nm));
    VTpl .= rndTuple(Lc,Vrs);
    Unit .= unary(Lc,"either",enum(Lc,"none"));
    Zed .= unary(Lc,"_valis",Unit);
    Ptn .= unary(Lc,"either",unary(Lc,"some",VTpl));
    Seq <- makeCondition(A,(lyfted(X))=>unary(Lc,"_valis",X),
      (St,X,Rs) => binary(Lc,"_sequence",genRtn(X),
	equation(Lc,rndTuple(Lc,[St]),Rs)),
      (_)=>unary(Lc,"_valis",Ptn),
      lyfted(Zed),Rp);
    valis mkMatch(Lc,Ptn,unary(Lc,"_perform",Seq))
  }

  makeHandler(H,Rp) where (Lc,[St]) ^= isBrTuple(H) => do{
    NH <- makeAction(St,.none,Rp);
    valis equation(Lc,rndTuple(Lc,[nme(Lc,"_")]),NH)
  }
  makeHandler(H,_) => either(H). 

  combine(A,.none) => A.
  combine(A,some((Lc,Cont))) => 
    binary(Lc,"_sequence",A,equation(Lc,rndTuple(Lc,[anon(Lc)]),Cont)).

  genRtn(lyfted(Exp)) => Exp.
  genRtn(grounded(Exp)) => unary(locOf(Exp),"_valis",Exp).

  makeReturn(Lc,A) => unary(Lc,"_valis",A).

  makeThrow(Lc,A,Rp) => either(unary(Lc,"_raise",A)).

  lyfted[a] ::= lyfted(a) | grounded(a).

  

  /*
  * Ptn in Src
  * becomes
  * let{.
  *  sF(Ptn,St) => AddEl(X,St).
  *  sF(_,St) default => do { return St}.
  * .} in _iter(Src,Initial,sF)
  *
  * where AddEl, InitState are parameters to the conversion
  */
  makeCondition:(ast,(lyfted[ast])=>ast,
    (ast,lyfted[ast],ast)=>ast, (lyfted[ast])=>ast,
    lyfted[ast],reports) => either[reports,ast].
  makeCondition(A,Lift,Seq,Succ,Zed,Rp) where (Lc,Ptn,Src) ^= isSearch(A) => do{
    sF .= genName(Lc,"sF");
    St .= genName(Lc,"St");

    Eq1 .= equation(Lc,roundTerm(Lc,sF,[Ptn,St]),Succ(grounded(Ptn)));
    Eq2 .= equation(Lc,roundTerm(Lc,sF,[anon(Lc),St]),Lift(grounded(St)));
    
    FF .= letDef(Lc,[Eq1,Eq2],sF);
    valis ternary(Lc,"_iter",Src,Lift(Zed),FF)
  }
  makeCondition(A,Lift,Seq,Succ,Zed,Rp) where (Lc,L,R) ^= isConjunct(A) =>
    makeCondition(A,Lift,Seq,(Lf) => valof makeCondition(R,Lift,Seq,Succ,Lf,Rp),Zed,Rp).
  makeCondition(A,Lift,Seq,Succ,Zed,Rp) where (Lc,L,R) ^= isDisjunct(A) => do{
    E1<-makeCondition(L,Lift,Seq,Succ,Zed,Rp);
    makeCondition(R,Lift,Seq,Succ,lyfted(E1),Rp)
  }
  makeCondition(A,Lift,Seq,Succ,Zed,Rp) where (Lc,R) ^= isNegation(A) => do{
    Negated <- makeCondition(A,Lift,Seq,(_)=>Lift(grounded(enum(Lc,"true"))),
      grounded(enum(Lc,"false")),Rp);
    St .= anon(Lc);
    SuccCase .= Succ(Zed);
    FalseCase .= Lift(Zed);
    valis Seq(St,lyfted(Negated),conditional(Lc,St,FalseCase,SuccCase))
  }
  makeCondition(A,Lift,Seq,Succ,Zed,Rp) where (Lc,L,R) ^= isImplies(A) =>
    makeCondition(negated(Lc,conjunction(Lc,L,negated(Lc,R))),Lift,Seq,Succ,Zed,Rp).
  makeCondition(Other,Lift,Seq,Succ,Zed,Rp) => do{
    Lc .= locOf(Other);
    AddToSucc .= Succ(Zed);
    St .= anon(Lc);
    Init .= Lift(Zed);
    valis Seq(St,Zed,conditional(Lc,Other,AddToSucc,Init))
  }

  goalVars:(ast)=>cons[string].
  goalVars(Cond) => glVars(Cond,[],[])::cons[string].

  glVars:(ast,set[string],set[string]) => set[string].
  glVars(A,Excl,Vrs) where (_,L,R) ^= isWhere(A) =>
    glVars(L,Excl,glVars(R,Excl,Vrs)).
  glVars(A,Excl,Vrs) where (_,L,R) ^= isConjunct(A) =>
    glVars(L,Excl,glVars(R,Excl,Vrs)).
  glVars(A,Excl,Vrs) where (_,L,R) ^= isDisjunct(A) =>
    glVars(L,Excl,Vrs)/\glVars(R,Excl,Vrs).
  glVars(A,Excl,Vrs) where (_,T,L,R) ^= isConditional(A) =>
    glVars(L,Excl,glVars(T,Excl,Vrs))/\glVars(R,Excl,Vrs).
  glVars(A,Excl,Vrs) where (_,Els) ^= isTuple(A) =>
    foldRight((E,F)=>glVars(E,Excl,F),Vrs,Els).
  glVars(A,Excl,Vrs) where (_,P,C) ^= isSearch(A) => ptnVars(P,Excl,Vrs).
  glVars(A,Excl,Vrs) where (_,P,C) ^= isMatch(A) => ptnVars(P,Excl,Vrs).
  glVars(A,Excl,Vrs) where (_,P,C) ^= isOptionMatch(A) => ptnVars(P,Excl,Vrs).
  glVars(_,_,Vrs) default => Vrs.

  ptnVars:(ast,set[string],set[string]) => set[string].
  ptnVars(A,Excl,Vrs) where (_,Nm) ^= isName(A) =>
    (Nm in Excl ? Vrs || _addMem(Nm,Vrs)).
  ptnVars(A,Excl,Vrs) where _ ^= isInt(A) => Vrs.
  ptnVars(A,Excl,Vrs) where _ ^= isFlt(A) => Vrs.
  ptnVars(A,Excl,Vrs) where _ ^= isStr(A) => Vrs.
  ptnVars(A,Excl,Vrs) where _ ^= isStr(A) => Vrs.
  ptnVars(A,Excl,Vrs) where _ ^= isEnum(A) => Vrs.
  ptnVars(A,Excl,Vrs) where (_,L,R) ^= isWhere(A) =>
    glVars(R,Excl,ptnVars(L,Excl,Vrs)).
  ptnVars(A,Excl,Vrs) where (_,_,Els) ^= isRoundTerm(A) =>
    foldRight((E,F)=>ptnVars(E,Excl,F),Vrs,Els).
  ptnVars(A,Excl,Vrs) where (_,Els) ^= isTuple(A) =>
    foldRight((E,F)=>ptnVars(E,Excl,F),Vrs,Els).
  ptnVars(A,Excl,Vrs) where (_,Els) ^= isSqTuple(A) =>
    foldRight((E,F)=>ptnVars(E,Excl,F),Vrs,Els).
  ptnVars(_,_,Vrs) default => Vrs.
}
