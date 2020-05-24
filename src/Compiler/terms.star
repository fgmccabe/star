star.compiler.terms{
  import star.

  import star.pkg.
  import star.sort.

  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.types.

  public termLbl ::= tLbl(string,integer) |
    tRec(string,cons[(string,tipe)]).

  public term ::= intgr(integer)
    | flot(float)
    | strg(string)
    | term(termLbl,cons[term])
    | enum(termLbl).

  public implementation display[termLbl] => {.
    disp(tLbl(Nm,Ar)) => ssSeq([ss(Nm),ss("/"),disp(Ar)]).
    disp(tRec(Nm,Fields)) =>
      ssSeq([ss(Nm),ss("{"),ssSeq(interleave(Fields//((FN,FTp))=>ssSeq([ss(FN),ss(":"),disp(FTp)]),ss(","))),ss("}")]).
  .}

  public implementation sizeable[termLbl] => {.
    size(tLbl(_,Ar))=>Ar.
    size(tRec(_,F))=>size(F).

    isEmpty(tLbl(_,Ar))=>Ar==0.
    isEmpty(tRec(_,F))=>F==[].
  .}

  public implementation display[term] => let{
    dispT(intgr(Ix)) => disp(Ix).
    dispT(flot(Dx)) => disp(Dx).
    dispT(strg(Sx)) => disp(Sx).
    dispT(term(tLbl(T,_),Args)) where isTupleLbl(T) => ssSeq([ss("("),ssSeq(dispTs(Args)),ss(")")]).
    dispT(term(tLbl(Op,_),Args)) => ssSeq([ss(Op),ss("("),ssSeq(dispTs(Args)),ss(")")]).
    dispT(term(tRec(Nm,Flds),Args)) => ssSeq([ss(Nm),ss("{"),ssSeq(dispFs(Flds,Args,"")),ss("}")]).
    dispT(enum(Sx)) => ssSeq([ss("'"),disp(Sx),ss("'")]).

    dispTs(Els) => interleave(Els//dispT,ss(",")).

    dispFs([],[],_) => [].
    dispFs([(F,_),..Fs],[A,..As],Sep) =>
      [ss(Sep),ss(F),ss("="),dispT(A),..dispFs(Fs,As,",")].

    isTupleLbl(T) where [0c(,0c),.._] .= T::cons[integer] => .true.
    isTupleLbl(_) default => .false.
  } in {.
    disp(T) => dispT(T)
  .}

  public implementation hash[termLbl] => {.
    hash(tLbl(Nm,Ar))=>hash(Nm)*37+Ar.
    hash(tRec(Nm,Fs))=>foldRight(((FN,_),H)=>H*37+hash(FN),hash(Nm),Fs).
  .}

  public implementation hash[term] => let{
    hsh(intgr(X)) => X.
    hsh(flot(X)) => hash(X).
    hsh(strg(S)) => hash(S).
    hsh(enum(S)) => hash(S).
    hsh(term(Op,Args)) =>
      foldRight((T,H)=>H*37+hsh(T),hash(Op)*37,Args).
  } in {
    hash(T) => hsh(T)
  }

  public implementation equality[termLbl] => {.
    tLbl(N1,A1)==tLbl(N2,A2) => N1==N2 && A1==A2.
    tRec(N1,F1)==tRec(N2,F2) => N1==N2 && F1==F2
  .}

  public implementation equality[term] => let{
    eq(intgr(X),intgr(Y)) => X==Y.
    eq(flot(X),flot(Y)) => X==Y.
    eq(strg(X),strg(Y)) => X==Y.
    eq(enum(X),enum(Y)) => X==Y.
    eq(term(O1,A1),term(O2,A2)) => O1==O2 && eqList(A1,A2).
    eq(_,_) default => .false.

    eqList(.nil,.nil)=>.true.
    eqList(cons(E1,L1),cons(E2,L2)) => eq(E1,E2) && eqList(L1,L2).
    eqList(_,_) default => .false.
  } in {.
    X==Y => eq(X,Y).
  .}

  public mkTpl:(cons[term]) => term.
  mkTpl(A) where L.=size(A) => term(tLbl(tplLbl(L),L),A).

  public tplLbl:(integer)=>string.
  tplLbl(Ar) => "()$(Ar)".

  public isTplLbl:(string)=>boolean.
  isTplLbl(Nm) where [0c(,0c),..Ds].=(Nm::cons[integer]) => .true.
  isTplLbl(_) default => .false.

  public mkLst:(cons[term]) => term.
  mkLst(Els) => term(tLbl("[]",size(Els)),Els).

  public mkCons:(string,cons[term])=>term.
  mkCons(Nm,Args) => term(tLbl(Nm,size(Args)),Args).

  public isScalar:(term)=>boolean.
  isScalar(intgr(_)) => .true.
  isScalar(flot(_)) => .true.
  isScalar(strg(_)) => .true.
  isScalar(enum(_)) => .true.
  isScalar(_) default => .false.

  public implementation coercion[term,string] => {.
    _coerce(T) => _implode(reverse(encodeT(T,[]))).
  .}

  -- Written in this way to maximize potential for tail recursion

  encodeT:(term,cons[integer])=>cons[integer].
  encodeT(intgr(Ix),Chs) => encodeInt(Ix,[0cx,..Chs]).
  encodeT(flot(Dx),Chs) => encodeText(Dx::string,[0cd,..Chs]).
  encodeT(strg(Tx),Chs) => encodeText(Tx,[0cs,..Chs]).
  encodeT(enum(tLbl(Nm,0)),Chs) => encodeText(Nm,[0ce,..Chs]).
  encodeT(enum(Sym),Chs) => encodeL(Sym,Chs).
  encodeT(term(tLbl("[]",Ar),Els),Chs) => encodeTerms(Els,encodeNat(Ar,[0cl,..Chs])).
  encodeT(term(Op,Args),Chs) =>
    encodeTerms(Args,encodeL(Op,encodeNat(size(Args),[0cn,..Chs]))).

  encodeL:(termLbl,cons[integer])=>cons[integer].
  encodeL(tLbl(Nm,Ar),Chs) => encodeText(Nm,encodeNat(Ar,[0co,..Chs])).
  encodeL(tRec(Nm,Flds),Chs) =>
    encodeFldTypes(Flds,encodeText(Nm,[0cO,..Chs])).

  encodeTerms([],Chs) => Chs.
  encodeTerms([T,..Ts],Chs) => encodeTerms(Ts,encodeT(T,Chs)).

  public implementation coercion[string,term] => {.
    _coerce(S) => valof do{
      L.=S::cons[integer];
--      logMsg("decoding $(S)");
      (T,_) <- decodeTerm(S::cons[integer]);
      valis T
    }
  .}
  
  public decodeTerm:(cons[integer])=>either[(),(term,cons[integer])].
  decodeTerm([0cx,..Ls]) => do{
--    logMsg("decoding int");
    (Ix,L0) <- decodeInt(Ls);
--    logMsg("int is $(Ix)");
    valis (intgr(Ix),L0)
  }.
  decodeTerm([0cd,..Ls]) => do{
--    logMsg("decoding float");
    (Txt,Lx) <- decodeText(Ls);
    valis (flot(Txt::float),Lx)
  }
  decodeTerm([0ce,..Ls]) => do{
--    logMsg("decoding enum");
    (Sym,Lx) <- decodeText(Ls);
--    logMsg("enum is $(enum(tLbl(Sym,0)))");
    valis (enum(tLbl(Sym,0)),Lx)
  }
  decodeTerm([0co,..Ls]) => do{
--    logMsg("decoding label");
    (Sym,Lx) <- decodeLabel([0co,..Ls]);
--    logMsg("label is $(enum(Sym))");
    valis (enum(Sym),Lx)
  }
  decodeTerm([0cs,..Ls]) => do{
--    logMsg("decoding string");
    (Txt,Lx) <- decodeText(Ls);
--    logMsg("string is $(Txt)");
    valis (strg(Txt),Lx)
  }
  decodeTerm([0cn,..Ls]) => do{
--    logMsg("decoding term");
    (Ax,L0) <- decodeNat(Ls,0);
    (Op,LL1) <- decodeLabel(L0);
    (Args,Lx) <- decodeTerms(LL1,Ax,[]);
--    logMsg("term is $(term(Op,Args))");
    valis (term(Op,Args),Lx)
  }
  decodeTerm([0cl,..Ls]) => do{
--    logMsg("decoding list");
    (Ax,L0) <- decodeNat(Ls,0);
--    logMsg("$(Ax) elements");
    (Els,Lx) <- decodeTerms(L0,Ax,[]);
--    logMsg("list is $(mkLst(Els))");
    valis (mkLst(Els),Lx)
  }

  decodeTerms:(cons[integer],integer,cons[term]) => either[(),(cons[term],cons[integer])].
  decodeTerms(L,0,Args) => either((reverse(Args),L)).
  decodeTerms(L,Ix,Args) => do{
--    logMsg("decode $(Ix) terms");
    (Arg,L0) <- decodeTerm(L);
    decodeTerms(L0,Ix-1,[Arg,..Args])
  }

  decodeLabel:(cons[integer])=>either[(),(termLbl,cons[integer])].
  decodeLabel([0co,..Ls]) => do{
--    logMsg("decoding label");
    (Ar,L0) <- decodeNat(Ls,0);
    (Nm,Lx) <- decodeText(L0);
--    logMsg("label is $(tLbl(Nm,Ar))");
    valis (tLbl(Nm,Ar),Lx)
  }
  decodeLabel([0cO,..Ls]) => do{
--    logMsg("decoding record lbl");
    (Nm,L0) <- decodeText(Ls);
    (Fs,Lx) <- decodeFields(L0);
--    logMsg("label is $(tRec(Nm,Fs))");
    valis (tRec(Nm,Fs),Lx)
  }
    
  decodeInt:(cons[integer])=>either[(),(integer,cons[integer])].
  decodeInt([0c-,..L]) => do{
    (Px,Lx) <- decodeNat(L,0);
    valis (-Px,Lx)
  }
  decodeInt(L) default => decodeNat(L,0).
  
  decodeNat:(cons[integer],integer) => either[(),(integer,cons[integer])].
  decodeNat([Cx,..Ls],Ix) where isDigit(Cx) => decodeNat(Ls,Ix*10+digitVal(Cx)).
  decodeNat(Ls,Ix) default => either((Ix,Ls)).

  decodeText:(cons[integer]) => either[(),(string,cons[integer])].
  decodeText([C,..L]) => do{
    (Q,Cs) <- collectQuoted(L,[],C);
    valis (reverse(Q)::string,Cs)
  }

  collectQuoted:(cons[integer],cons[integer],integer) => either[(),(cons[integer],cons[integer])].
  collectQuoted([S,..Lx],SoF,S) => either((SoF,Lx)).
  collectQuoted([0c\\,X,..L],SoF,S) => collectQuoted(L,[X,..SoF],S).
  collectQuoted([X,..L],SoF,S) => collectQuoted(L,[X,..SoF],S).

  public decodeSignature:(string) => either[(),tipe].
  decodeSignature(St) => do{
    (Tp,_) <- decodeType(St::cons[integer]);
    valis Tp
  }

  decodeType:(cons[integer]) => either[(),(tipe,cons[integer])].
  decodeType([0ci,..Ts]) => either((nomnal("star.core*integer"),Ts)).
  decodeType([0cf,..Ts]) => either((nomnal("star.core*float"),Ts)).
  decodeType([0cS,..Ts]) => either((nomnal("star.core*string"),Ts)).
  decodeType([0cl,..Ts]) => either((nomnal("star.core*boolean"),Ts)).
  decodeType([0c_,..Ts]) => either((newTypeVar("_"),Ts)).
  decodeType([0ck,..Ts]) => do {
    (Nm,T1) <- decodeText(Ts);
    valis (nomnal(Nm),T1)
  }
  decodeType([0cK,..Ts]) => do {
    (Ar,T0) <- decodeNat(Ts,0);
    (Nm,T1) <- decodeText(T0);
    valis (kFun(Nm,Ar),T1)
  }
  decodeType([0ct,..Ts]) => do {
    (Nm,T1) <- decodeText(Ts);
    valis (nomnal(Nm),T1)
  }
  decodeType([0cz,..Ts]) => do {
    (Ar,T0) <- decodeNat(Ts,0);
    (Nm,T1) <- decodeText(T0);
    valis (tpFun(Nm,Ar),T1)
  }
  decodeType([0cL,..Ts]) => do {
    (ElTp,T0) <- decodeType(Ts);
    valis (lstType(ElTp),T0)
  }
  decodeType([0cU,..Ts]) => do {
    (OpTp,T0) <- decodeType(Ts);
    (ElTp,T1) <- decodeType(T0);
    valis (tpExp(OpTp,ElTp),T1)
  }
  decodeType([0cd,..Ts]) => do{
    (OpTp,T0) <- decodeType(Ts);
    (tupleType(Els),T1) <- decodeType(T0);
    valis (funDeps(OpTp,Els),T1)
  }
  decodeType([0cr,..Ts]) => do {
    (ElTp,T0) <- decodeType(Ts);
    valis (refType(ElTp),T0)
  }
  decodeType([0x28,..Ts]) => do {       -- 0x28 == (
    (Tps,T0) <- decodeTypes(Ts);
    valis (tupleType(Tps),T0)
  }
  decodeType([0c:,..Ts]) => do{
    (V,T0) <- decodeType(Ts);
    (B,T1) <- decodeType(T0);
    valis (allType(V,B),T1)
  }
  decodeType([0ce,..Ts]) => do{
    (V,T0) <- decodeType(Ts);
    (B,T1) <- decodeType(T0);
    valis (existType(V,B),T1)
  }
  decodeType([0c|,..Ts]) => do{
    (V,T0) <- decodeType(Ts);
    (B,T1) <- decodeConstraint(T0);
    valis (constrainedType(V,B),T1)
  }
  decodeType([0cI,..Ts]) => do{
    (F1,T0) <- decodeFields(Ts);
    (F2,T1) <- decodeFields(T0);
    valis (faceType(F1,F2),T1)
  }
  decodeType([0cF,..Ts]) => do{
    (A,T0) <- decodeType(Ts);
    (R,T1) <- decodeType(T0);
    valis (fnType(A,R),T1)
  }
  decodeType([0cC,..Ts]) => do{
    (A,T0) <- decodeType(Ts);
    (R,T1) <- decodeType(T0);
    valis (consType(A,R),T1)
  }
  decodeType([0cY,..Ts]) => do{
    (A,T0) <- decodeType(Ts);
    (R,T1) <- decodeType(T0);
    valis (typeExists(A,R),T1)
  }
  decodeType([0cy,..Ts]) => do{
    (A,T0) <- decodeType(Ts);
    (R,T1) <- decodeType(T0);
    valis (typeLambda(A,R),T1)
  }

  decodeTypes([0x29,..Ts]) => either(([],Ts)). -- 0x29 == )
  decodeTypes(Ts) => do{
    (ElTp,T0) <- decodeType(Ts);
    (Tps,T1) <- decodeTypes(T0);
    valis ([ElTp,..Tps],T1)
  }

  decodeFields([0x7b,..Ts]) => decodeFlds(Ts,[]). -- 0x7b == {

  decodeFlds([0x7d,..Ts],Flds) => either((reverse(Flds),Ts)). -- 0x7d == }
  decodeFlds(Ts,Flds) => do{
    (Nm,T0) <- decodeText(Ts);
    (Tp,T1) <- decodeType(T0);
    decodeFlds(T1,[(Nm,Tp),..Flds])
  }
  decodeConstraint([0cc,..T]) => do{
    (Con,T1) <- decodeType(T);
    valis (contractConstraint(Con),T1)
  }
  decodeConstraint([0ca,..T]) => do{
    (BT,T0) <- decodeType(T);
    (FT,T1) <- decodeType(T0);
    valis (fieldConstraint(BT,FT),T1)
  }

  public encodeSignature:(tipe) => string.
  encodeSignature(Tp) => reverse(encodeType(deRef(Tp),[]))::string.

  encodeType:(tipe,cons[integer]) => cons[integer].
  encodeType(V,Chs) where isUnbound(V) => [0c_,..Chs].
  encodeType(nomnal("star.core*integer"),Chs) => [0ci,..Chs].
  encodeType(nomnal("star.core*float"),Chs) => [0cf,..Chs].
  encodeType(nomnal("star.core*string"),Chs) => [0cS,..Chs].
  encodeType(nomnal("star.core*boolean"),Chs) => [0cl,..Chs].
  encodeType(nomnal(Nm),Chs) => encodeText(Nm,[0ct,..Chs]).
  encodeType(kFun(Nm,Ar),Chs) => encodeText(Nm,encodeNat(Ar,[0cK,..Chs])).
  encodeType(tpFun(Nm,Ar),Chs) => encodeText(Nm,encodeNat(Ar,[0cz,..Chs])).
  encodeType(tpExp(tpFun("star.core*cons",1),El),Chs) =>
    encodeType(deRef(El),[0cL,..Chs]).
  encodeType(tpExp(tpFun("star.core*ref",1),El),Chs) =>
    encodeType(deRef(El),[0cr,..Chs]).
  encodeType(tpExp(tpExp(tpFun("=>",2),A),R),Chs) =>
    encodeType(deRef(R),encodeType(deRef(A),[0cF,..Chs])).
  encodeType(tpExp(tpExp(tpFun("<=>",2),A),R),Chs) =>
    encodeType(deRef(R),encodeType(deRef(A),[0cC,..Chs])).
  encodeType(tpExp(Op,A),Chs) =>
    encodeType(deRef(A),encodeType(deRef(Op),[0cU,..Chs])).
  encodeType(tupleType(Els),Chs) => encodeTypes(Els,[0c\(,..Chs]).
  encodeType(allType(V,T),Chs) =>
    encodeType(deRef(T),encodeType(deRef(V),[0c:,..Chs])).
  encodeType(existType(V,T),Chs) =>
    encodeType(deRef(T),encodeType(deRef(V),[0ce,..Chs])).
  encodeType(constrainedType(T,C),Chs) =>
    encodeConstraint(C,encodeType(deRef(T),[0c|,..Chs])).
  encodeType(funDeps(V,Ds),Chs) =>
    encodeType(tupleType(Ds),encodeType(deRef(V),[0cd,..Chs])).
  encodeType(faceType(Flds,Tps),Chs) =>
    encodeFldTypes(Tps,encodeFldTypes(Flds,[0cI,..Chs])).
  encodeType(typeExists(H,I),Chs) =>
    encodeType(deRef(I),encodeType(deRef(H),[0cY,..Chs])).
  encodeType(typeLambda(Hd,I),Chs) =>
    encodeType(deRef(I),encodeType(deRef(Hd),[0cy,..Chs])).

  encodeTypes:(cons[tipe],cons[integer])=>cons[integer].
  encodeTypes([],Chs) => [0x29,..Chs].
  encodeTypes([T,..Tps],Chs) =>
    encodeTypes(Tps,encodeType(deRef(T),Chs)).

  encodeFldTypes:(cons[(string,tipe)],cons[integer])=>cons[integer].
  encodeFldTypes(Flds,Chs) => encodeFlds(sort(Flds,((N1,_),(N2,_))=>N1<N2),[0x7b,..Chs]). -- 0x7b == {
  
  encodeFlds:(cons[(string,tipe)],cons[integer])=>cons[integer].
  encodeFlds([],Chs) => [0x7d,..Chs].   -- 0x7d == }
  encodeFlds([(Nm,T),..Tps],Chs) =>
    encodeFlds(Tps,encodeType(deRef(T),encodeText(Nm,Chs))).

  encodeConstraint(contractConstraint(T),Chs) =>
    encodeType(deRef(T),[0cc,..Chs]).
  encodeConstraint(fieldConstraint(V,T),Chs) =>
    encodeType(deRef(T),encodeType(deRef(V),[0ca,..Chs])).
  
  encodeText:(string,cons[integer]) => cons[integer].
  encodeText(Txt,Chs) where Chrs .= Txt::cons[integer] &&
      D.=findDelim(Chrs,[0c|,0c/,0c%]) =>
    encodeQuoted(Chrs,D,[D,..Chs]).

  findDelim:(cons[integer],cons[integer])=>integer.
  findDelim(Chrs,[]) => 0x22. -- == "
  findDelim(Chrs,[D,..Ds]) where D in Chrs => findDelim(Chrs,Ds).
  findDelim(Chrs,[D,.._]) => D.

  encodeQuoted([],D,Chs) => [D,..Chs].
  encodeQuoted([D,..Cs],D,Chs) => encodeQuoted(Cs,D,[D,0c\\,..Chs]).
  encodeQuoted([0c\\,..Cs],D,Chrs) => encodeQuoted(Cs,D,[0c\\,0c\\,..Chrs]).
  encodeQuoted([C,..Cs],D,Chrs) => encodeQuoted(Cs,D,[C,..Chrs]).

  encodeNat:(integer,cons[integer]) => cons[integer].
  encodeNat(Dx,Chs) where Dx>=0 && Dx=<9 =>
    [Dx+0c0,..Chs].
  encodeNat(Dx,Chs) => [(Dx%10)+0c0,..encodeNat(Dx/10,Chs)].

  encodeInt:(integer,cons[integer])=>cons[integer].
  encodeInt(Ix,Chs) where Ix<0 => encodeNat(-Ix,[0c-,..Chs]).
  encodeInt(Ix,Chs) => encodeNat(Ix,Chs).

  public implementation coercion[locn,term]=>{
    _coerce(locn(Pkg,Line,Col,Off,Ln))=>mkTpl([strg(Pkg),intgr(Line),intgr(Col),intgr(Off),intgr(Ln)]).
  }

  public pkgTerm:(pkg)=>term.
  pkgTerm(pkg(Pk,Ver))=>mkCons("pkg",[strg(Pk),versTerm(Ver)]).

  public versTerm:(version)=>term.
  versTerm(.defltVersion) => enum(tLbl("*",0)).
  versTerm(vers(V)) => strg(V).
}
