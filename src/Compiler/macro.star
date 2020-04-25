star.compiler.macro{
  import star.

  import star.compiler.ast.
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
    Arg .= sqTuple(Lc,([V,..Vrs].=Vs ? [reComma(Vrs,V)] || []));
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
  isSimpleAction(A) where _ ^= isTryCatch(A) => .true.
  isSimpleAction(A) where _ ^= isIntegrity(A) => .true.
  isSimpleAction(A) where _ ^= isShow(A) => .true.
  isSimpleAction(A) where _ ^= isRoundTerm(A) => .true.
  isSimpleAction(_) default => .false.

  public makeAction:(ast,option[(locn,ast)],reports) => either[reports,ast].
  makeAction(A,Cont,Rp) where (_,[St]) ^= isBrTuple(A) =>
    makeAction(St,Cont,Rp).
  makeAction(A,Cont,Rp) where (Lc,L,R) ^= isActionSeq(A) => do{
    RR <- makeAction(R,Cont,Rp);
    makeAction(L,some((Lc,RR)),Rp)
  }
  makeAction(A,.none,Rp) where (Lc,R) ^= isValis(A) =>
    makeReturn(Lc,R,Rp).
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
  makeAction(A,Cont,_) where _ ^= isRoundTerm(A) =>
    either(combine(A,Cont)).

  makeHandler(H,Rp) where (Lc,[St]) ^= isBrTuple(H) => do{
    NH <- makeAction(St,.none,Rp);
    valis equation(Lc,rndTuple(Lc,[nme(Lc,"_")]),NH)
  }
  makeHandler(H,_) => either(H). 

  combine(A,.none) => A.
  combine(A,some((Lc,Cont))) => 
     binary(Lc,"_sequence",A,equation(Lc,rndTuple(Lc,[nme(Lc,"_")]),Cont)).

  makeReturn(Lc,A,Rp) => either(unary(Lc,"_valis",A)).

  makeThrow(Lc,A,Rp) => either(unary(Lc,"_raise",A)).
      
}
