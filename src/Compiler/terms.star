star.compiler.terms{
  import star.

  import star.pkg.
  import star.sort.

  import star.compiler.location.
  import star.compiler.multi.
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
    Dispt(term(tLbl(T,_),Args)) where isTupleLbl(T) => ssSeq([ss("‹"),ssSeq(dispTs(Args)),ss("›")]).

    dispT(term(tLbl(Op,_),Args)) => ssSeq([disp(Op),ss("‹"),ssSeq(dispTs(Args)),ss("›")]).

    dispT(term(tRec(Nm,Flds),Args)) => ssSeq([ss(Nm),ss("{"),ssSeq(dispFs(Flds,Args,"")),ss("}")]).
    dispT(enum(Sx)) => ssSeq([ss("'"),disp(Sx),ss("'")]).

    dispTs(Els) => interleave(Els//dispT,ss(",")).

    dispFs([],[],_) => [].
    dispFs([(F,_),..Fs],[A,..As],Sep) =>
      [ss(Sep),ss(F),ss("="),disp(A),..dispFs(Fs,As,",")].

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
    eq(term(O1,A1),term(O2,A2)) => O1==O2 && A1==A2.
    eq(_,_) default => .false.
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

  public implementation coercion[term,string] => {
    _coerce(T) => _implode(encodeT(T)).
  }

  encodeT:(term)=>multi[integer].
  encodeT(intgr(Ix)) => [0cx,..encodeInt(Ix)].
  encodeT(flot(Dx)) => [0cd,..encodeText(Dx::string)].
  encodeT(strg(Tx)) => [0cs,..encodeText(Tx)].
  encodeT(enum(tLbl(Nm,0))) => [0ce,..encodeText(Nm)].
  encodeT(enum(Sym)) => encodeL(Sym).
  encodeT(term(tLbl("[]",Ar),Els)) => [0cl,..encodeNat(Ar)]++encodeTerms(Els).
  encodeT(term(Op,Args)) =>
    [0cn,..encodeNat(size(Args))]++encodeL(Op)++encodeTerms(Args).


  encodeL:(termLbl)=>multi[integer].
  encodeL(tLbl(Nm,Ar)) => [0co,..encodeNat(Ar)]++encodeText(Nm).
  encodeL(tRec(Nm,Flds)) =>
    [0cO,..encodeText(Nm)]++encodeFldTypes(Flds).

  encodeTerms([]) => [].
  encodeTerms([T,..Ts]) => encodeT(T)++encodeTerms(Ts).

  public implementation coercion[string,term] => {.
    _coerce(S) => valof do{
      L.=S::cons[integer];
      (T,_) <- decodeTerm(S::cons[integer]);
      valis T
    }
  .}
  
  public decodeTerm:(cons[integer])=>either[(),(term,cons[integer])].
  decodeTerm([0cx,..Ls]) => do{
    (Ix,L0) <- decodeInt(Ls);
    valis (intgr(Ix),L0)
  }.
  decodeTerm([0cd,..Ls]) => do{
    (Txt,Lx) <- decodeText(Ls);
    valis (flot(Txt::float),Lx)
  }
  decodeTerm([0ce,..Ls]) => do{
    (Sym,Lx) <- decodeText(Ls);
    valis (enum(tLbl(Sym,0)),Lx)
  }
  decodeTerm([0co,..Ls]) => do{
    (Sym,Lx) <- decodeLabel([0co,..Ls]);
    valis (enum(Sym),Lx)
  }
  decodeTerm([0cs,..Ls]) => do{
    (Nm,Lx) <- decodeText(Ls);
    valis (strg(Nm),Lx)
  }
  decodeTerm([0cn,..Ls]) => do{
    (Ax,L0) <- decodeNat(Ls,0);
    (Op,LL1) <- decodeLabel(L0);
    (Args,Lx) <- decodeTerms(LL1,Ax,[]);
    valis (term(Op,Args),Lx)
  }
  decodeTerm([0cl,..Ls]) => do{
    (Ax,L0) <- decodeNat(Ls,0);
    (Els,Lx) <- decodeTerms(L0,Ax,[]);
    valis (mkLst(Els),Lx)
  }

  decodeTerms:(cons[integer],integer,cons[term]) => either[(),(cons[term],cons[integer])].
  decodeTerms(L,0,Args) => either((reverse(Args),L)).
  decodeTerms(L,Ix,Args) => do{
    (Arg,L0) <- decodeTerm(L);
    decodeTerms(L0,Ix-1,[Arg,..Args])
  }

  decodeLabel:(cons[integer])=>either[(),(termLbl,cons[integer])].
  decodeLabel([0co,..Ls]) => do{
    (Ar,L0) <- decodeNat(Ls,0);
    (Nm,Lx) <- decodeText(L0);
    valis (tLbl(Nm,Ar),Lx)
  }
  decodeLabel([0cO,..Ls]) => do{
    (Nm,L0) <- decodeText(Ls);
    (Fs,Lx) <- decodeFields(L0);
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
  decodeType([0x28,..Ts]) => do {	-- 0x28 == (
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
    valis (typeConstraint(Con),T1)
  }
  decodeConstraint([0ca,..T]) => do{
    (BT,T0) <- decodeType(T);
    (FT,T1) <- decodeType(T0);
    valis (fieldConstraint(BT,FT),T1)
  }

  public encodeSignature:(tipe) => string.
  encodeSignature(Tp) => encodeType(deRef(Tp))::string.

  encodeType:(tipe) => multi[integer].
  encodeType(V) where isUnbound(V) => [0c_].
  encodeType(nomnal("star.core*integer")) => [0ci].
  encodeType(nomnal("star.core*float")) => [0cf].
  encodeType(nomnal("star.core*string")) => [0cS].
  encodeType(nomnal("star.core*boolean")) => [0cl].
  encodeType(nomnal(Nm)) => [0ct,..encodeText(Nm)].
  encodeType(kFun(Nm,Ar)) => [0cK,..encodeNat(Ar)]++encodeText(Nm).
  encodeType(tpFun(Nm,Ar)) => [0cz,..encodeNat(Ar)]++encodeText(Nm).
  encodeType(tpExp(tpFun("star.core*cons",1),El)) =>
    [0cL,..encodeType(deRef(El))].
  encodeType(tpExp(tpFun("star.core*ref",1),El)) =>
    [0cr,..encodeType(deRef(El))].
  encodeType(tpExp(tpExp(tpFun("=>",2),A),R)) =>
    [0cF,..encodeType(deRef(A))]++encodeType(deRef(R)).
  encodeType(tpExp(tpExp(tpFun("<=>",2),A),R)) =>
    [0cC,..encodeType(deRef(A))]++encodeType(deRef(R)).
  encodeType(tpExp(Op,A)) =>
    [0cU,..encodeType(deRef(Op))]++encodeType(deRef(A)).
  encodeType(tupleType(Els)) =>
    [0c\(,..encodeTypes(Els)].
  encodeType(allType(V,T)) =>
    [0c:,..encodeType(deRef(V))]++encodeType(deRef(T)).
  encodeType(existType(V,T)) =>
    [0ce,..encodeType(deRef(V))]++encodeType(deRef(T)).
  encodeType(constrainedType(T,C)) =>
    [0c|,..encodeType(deRef(T))]++encodeConstraint(C).
  encodeType(funDeps(V,Ds)) =>
    [0cd,..encodeType(deRef(V))]++encodeType(tupleType(Ds)).
  encodeType(faceType(Flds,Tps)) =>
    [0cI,..encodeFldTypes(Flds)]++encodeFldTypes(Tps).
  encodeType(typeExists(H,I)) =>
    [0cY,..encodeType(deRef(H))]++encodeType(deRef(I)).
  encodeType(typeLambda(Hd,I)) =>
    [0cy,..encodeType(deRef(Hd))]++encodeType(deRef(I)).

  encodeTypes:(cons[tipe])=>multi[integer].
  encodeTypes([]) => [0x29].
  encodeTypes([T,..Tps]) =>
    encodeType(deRef(T))++encodeTypes(Tps).

  encodeFldTypes:(cons[(string,tipe)])=>multi[integer].
  encodeFldTypes(Flds) => [0x7b,..encodeFlds(sort(Flds,((N1,_),(N2,_))=>N1<N2))]. -- 0x7b == {
  
  encodeFlds:(cons[(string,tipe)])=>multi[integer].
  encodeFlds([]) => [0x7d].	-- 0x7d == }
  encodeFlds([(Nm,T),..Tps]) =>
    encodeText(Nm)++encodeType(deRef(T))++encodeFlds(Tps).

  encodeConstraint(typeConstraint(T)) =>
    [0cc,..encodeType(deRef(T))].
  encodeConstraint(fieldConstraint(V,T)) =>
    [0ca,..encodeType(deRef(V))]++encodeType(deRef(T)).
  
  encodeText:(string) => multi[integer].
  encodeText(Txt) where Chrs .= Txt::cons[integer] &&
      D.=findDelim(Chrs,[0c|,0c/,0c%]) =>
    [D,..encodeQuoted(Chrs,D)].

  findDelim:(cons[integer],cons[integer])=>integer.
  findDelim(Chrs,[]) => 0x22. -- == "
  findDelim(Chrs,[D,..Ds]) where D in Chrs => findDelim(Chrs,Ds).
  findDelim(Chrs,[D,.._]) => D.

  encodeQuoted([],D) => [].
  encodeQuoted([D,..Cs],D) => [0c\\,D,..encodeQuoted(Cs,D)].
  encodeQuoted([0c\\,..Cs],D) => [0c\\,0c\\,..encodeQuoted(Cs,D)].
  encodeQuoted([C,..Cs],D) => [C,..encodeQuoted(Cs,D)].

  encodeNat:(integer) => multi[integer].
  encodeNat(Dx) where Dx>=0 && Dx=<9 =>
    [Dx+0c0].
  encodeNat(Dx) => encodeNat(Dx/10)++[(Dx%10)+0c0].

  encodeInt:(integer)=>multi[integer].
  encodeInt(Ix) where Ix<0 => [0c-,..encodeNat(-Ix)].
  encodeInt(Ix) => encodeNat(Ix).

  public implementation coercion[locn,term]=>{
    _coerce(locn(Pkg,Line,Col,Off,Ln))=>mkTpl([strg(Pkg),intgr(Line),intgr(Col),intgr(Off),intgr(Ln)]).
  }

  public pkgTerm:(pkg)=>term.
  pkgTerm(pkg(Pk,Ver))=>mkCons("pkg",[strg(Pk),versTerm(Ver)]).

  public versTerm:(version)=>term.
  versTerm(.defltVersion) => enum(tLbl("*",0)).
  versTerm(vers(V)) => strg(V).
}
