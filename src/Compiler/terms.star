star.compiler.terms{
  import star.

  import star.pkg.

  import star.compiler.location.
  import star.compiler.types.

  public termLbl ::= tLbl(string,integer) |
    tRec(string,list[(string,tipe)]).

  public term ::= intgr(integer)
    | flot(float)
    | strg(string)
    | term(termLbl,list[term])
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
    dispT(term(tLbl(T,_),Args)) where isTupleLbl(T) => ssSeq([ss("‹"),ssSeq(dispTs(Args)),ss("›")]).

    dispT(term(tLbl(Op,_),Args)) => ssSeq([disp(Op),ss("‹"),ssSeq(dispTs(Args)),ss("›")]).

    dispT(term(tRec(Nm,Flds),Args)) => ssSeq([ss(Nm),ss("{"),ssSeq(dispFs(Flds,Args,"")),ss("}")]).
    dispT(enum(Sx)) => ssSeq([ss("'"),disp(Sx),ss("'")]).

    dispTs(Els) => interleave(Els//dispT,ss(",")).

    dispFs([],[],_) => [].
    dispFs([(F,_),..Fs],[A,..As],Sep) =>
      [ss(Sep),ss(F),ss("="),disp(A),..dispFs(Fs,As,",")].

    isTupleLbl(T) where [0c(,0c),.._] .= T::list[integer] => .true.
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

  public mkTpl:(list[term]) => term.
  mkTpl(A) where L.=size(A) => term(tLbl(tplLbl(L),L),A).

  public tplLbl:(integer)=>string.
  tplLbl(Ar) => "()$(Ar)".

  public isTplLbl:(string)=>boolean.
  isTplLbl(Nm) where [0c(,0c),..Ds].=(Nm::list[integer]) => .true.
  isTplLbl(_) default => .false.

  public mkLst:(list[term]) => term.
  mkLst(Els) => term(tLbl("[]",size(Els)),Els).

  public mkCons:(string,list[term])=>term.
  mkCons(Nm,Args) => term(tLbl(Nm,size(Args)),Args).

  public isScalar:(term)=>boolean.
  isScalar(intgr(_)) => .true.
  isScalar(flot(_)) => .true.
  isScalar(strg(_)) => .true.
  isScalar(enum(_)) => .true.
  isScalar(_) default => .false.

  public implementation coercion[term,string] => {
    _coerce(T) => _implode(encodeTerm(T)).
  }

  public encodeTerm:(term)=>list[integer].
  encodeTerm(T) => encodeT(T,[]).

  encodeT:(term,list[integer])=>list[integer].
  encodeT(intgr(Ix),Cs) => encodeInt(Ix,[Cs..,0cx]).
  encodeT(flot(Dx),Cs) => encodeText(Dx::string,[Cs..,0cd]).
  encodeT(strg(Tx),Cs) => encodeText(Tx,[Cs..,0cs]).
  encodeT(enum(tLbl(Nm,0)),Cs) => encodeText(Nm,[Cs..,0ce]).
  encodeT(enum(Sym),Cs) => encodeL(Sym,Cs).
  encodeT(term(tLbl("[]",Ar),Els),Cs) => encodeTerms(Els,encodeNat(Ar,[Cs..,0cl])).
  encodeT(term(Op,Args),Cs) =>
    encodeTerms(Args,encodeL(Op,encodeNat(size(Args),[Cs..,0cn]))).

  public encodeLbl:(termLbl)=>string.
  encodeLbl(Lb)=>encodeL(Lb,[])::string.
  
  encodeL:(termLbl,list[integer])=>list[integer].
  encodeL(tLbl(Nm,Ar),Cs) => encodeText(Nm,encodeNat(Ar,[Cs..,0co])).
  encodeL(tRec(Nm,Flds),Cs) =>
    encodeT(mkLst(ixRight((Ix,(FNm,Tp),Fs)=>[Fs..,mkField(FNm,Ix,Tp)],[],Flds)), encodeText(Nm,[Cs..,0cO])).

  mkField:(string,integer,tipe)=>term.
  mkField(Nm,Ix,Tp)=>mkTpl([enum(tLbl("."++Nm,0)),strg(encodeSignature(Tp)),intgr(Ix),intgr(1)]).

  encodeTerms([],Cs) => Cs.
  encodeTerms([T,..Ts],Cs) => encodeTerms(Ts,encodeT(T,Cs)).

  public implementation coercion[string,term] => {.
    _coerce(S) => valof do{
      (T,_) <- decodeTerm(S::list[integer]);
      valis T
    }
  .}
  
  public decodeTerm:(list[integer])=>either[(),(term,list[integer])].
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

  decodeTerms:(list[integer],integer,list[term]) => either[(),(list[term],list[integer])].
  decodeTerms(L,0,Args) => either((Args,L)).
  decodeTerms(L,Ix,Args) => do{
    (Arg,L0) <- decodeTerm(L);
    decodeTerms(L0,Ix-1,[Args..,Arg])
  }

  decodeLabel:(list[integer])=>either[(),(termLbl,list[integer])].
  decodeLabel([0co,..Ls]) => do{
    (Ar,L0) <- decodeNat(Ls,0);
    (Nm,Lx) <- decodeText(L0);
    valis (tLbl(Nm,Ar),Lx)
  }
  decodeLabel([0cO,..Ls]) => do{
    (Nm,L0) <- decodeText(Ls);
    (faceType(Fs,_),Lx)<-decodeType(L0);
    valis (tRec(Nm,Fs),Lx)
  }
    
  decodeInt:(list[integer])=>either[(),(integer,list[integer])].
  decodeInt([0c-,..L]) => do{
    (Px,Lx) <- decodeNat(L,0);
    valis (-Px,Lx)
  }
  decodeInt(L) default => decodeNat(L,0).
  
  decodeNat:(list[integer],integer) => either[(),(integer,list[integer])].
  decodeNat([Cx,..Ls],Ix) where isDigit(Cx) => decodeNat(Ls,Ix*10+digitVal(Cx)).
  decodeNat(Ls,Ix) default => either((Ix,Ls)).

  decodeText:(list[integer]) => either[(),(string,list[integer])].
  decodeText([C,..L]) => collectQuoted(L,[],C).

  collectQuoted:(list[integer],list[integer],integer) => either[(),(string,list[integer])].
  collectQuoted([S,..Lx],SoF,S) => either((SoF::string,Lx)).
  collectQuoted([0c\\,X,..L],SoF,S) => collectQuoted(L,[SoF..,X],S).
  collectQuoted([X,..L],SoF,S) => collectQuoted(L,[SoF..,X],S).

  public decodeSignature:(string) => either[(),tipe].
  decodeSignature(St) => do{
    (Tp,_) <- decodeType(St::list[integer]);
    valis Tp
  }

  decodeType:(list[integer]) => either[(),(tipe,list[integer])].
  decodeType([0ci,..Ts]) => either((nomnal("star.core*integer"),Ts)).
  decodeType([0cf,..Ts]) => either((nomnal("star.core*float"),Ts)).
  decodeType([0cS,..Ts]) => either((nomnal("star.core*string"),Ts)).
  decodeType([0cl,..Ts]) => either((nomnal("star.core*boolean"),Ts)).
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
    (Tps,T0) <- decodeTypes(Ts,[]);
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

  decodeTypes([0x29,..Ts],Tps) => either((Tps,Ts)). -- 0x29 == )
  decodeTypes(Ts,Tps) => do{
    (ElTp,T0) <- decodeType(Ts);
    decodeTypes(T0,[Tps..,ElTp])
  }

  decodeFields([0x7b,..Ts]) => decodeFlds(Ts,[]). -- 0x7b == {

  decodeFlds([0x7d,..Ts],Flds) => either((Flds,Ts)). -- 0x7d == }
  decodeFlds(Ts,Flds) => do{
    (Nm,T0) <- decodeText(Ts);
    (Tp,T1) <- decodeType(T0);
    decodeFlds(T1,[Flds..,(Nm,Tp)])
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
  encodeSignature(Tp) => encodeType(deRef(Tp),[])::string.

  encodeType:(tipe,list[integer]) => list[integer].
  encodeType(nomnal("star.core*integer"),Ts) => [Ts..,0ci].
  encodeType(nomnal("star.core*float"),Ts) => [Ts..,0cf].
  encodeType(nomnal("star.core*string"),Ts) => [Ts..,0cS].
  encodeType(nomnal("star.core*boolean"),Ts) => [Ts..,0cl].
  encodeType(nomnal(Nm),Ts) => encodeText(Nm,[Ts..,0ct]).
  encodeType(kFun(Nm,Ar),Ts) => encodeText(Nm,encodeNat(Ar,[Ts..,0cK])).
  encodeType(tpFun(Nm,Ar),Ts) => encodeText(Nm,encodeNat(Ar,[Ts..,0cz])).
  encodeType(tpExp(tpFun("star.core*list",1),El),Ts) =>
    encodeType(deRef(El),[Ts..,0cL]).
  encodeType(tpExp(tpFun("star.core*ref",1),El),Ts) =>
    encodeType(deRef(El),[Ts..,0cr]).
  encodeType(tpExp(tpExp(tpFun("=>",2),A),R),Ts) =>
    encodeType(deRef(R),encodeType(deRef(A),[Ts..,0cF])).
  encodeType(tpExp(tpExp(tpFun("<=>",2),A),R),Ts) =>
    encodeType(deRef(R),encodeType(deRef(A),[Ts..,0cC])).
  encodeType(tpExp(Op,A),Ts) =>
    encodeType(deRef(A),encodeType(deRef(Op),[Ts..,0cU])).
  encodeType(tupleType(Els),Ts) =>
    encodeTypes(Els,[Ts..,0c(]).
  encodeType(allType(V,T),Ts) =>
    encodeType(deRef(T),encodeType(deRef(V),[Ts..,0c:])).
  encodeType(existType(V,T),Ts) =>
    encodeType(deRef(T),encodeType(deRef(V),[Ts..,0ce])).
  encodeType(constrainedType(T,C),Ts) =>
    encodeConstraint(C,encodeType(deRef(T),[Ts..,0c|])).
  encodeType(funDeps(V,Ds),Ts) =>
    encodeType(tupleType(Ds),encodeType(deRef(V),[Ts..,0cd])).
  encodeType(faceType(Flds,Tps),Ts) =>
    encodeFields(Tps,encodeFields(Flds,[Ts..,0cI])).
  encodeType(typeExists(H,I),Ts) =>
    encodeType(deRef(I),encodeType(deRef(H),[Ts..,0cY])).
  encodeType(typeLambda(Hd,I),Ts) =>
    encodeType(deRef(I),encodeType(deRef(Hd),[Ts..,0cy])).

  encodeTypes:(list[tipe],list[integer])=>list[integer].
  encodeTypes([],Ts) => [Ts..,0x29].
  encodeTypes([T,..Tps],Ts) =>
    encodeTypes(Tps,encodeType(deRef(T),Ts)).
  
  encodeFields:(list[(string,tipe)],list[integer])=>list[integer].
  encodeFields(Flds,Ts) => encodeFlds(Flds,[Ts..,0x7b]). -- 0x7b == {
  
  encodeFlds:(list[(string,tipe)],list[integer])=>list[integer].
  encodeFlds([],Ts) => [Ts..,0x7d].	-- 0x7d == }
  encodeFlds([(Nm,T),..Tps],Ts) =>
    encodeFlds(Tps,encodeType(deRef(T),encodeText(Nm,Ts))).

  encodeConstraint(typeConstraint(T),Ts) =>
    encodeType(deRef(T),[Ts..,0cc]).
  encodeConstraint(fieldConstraint(V,T),Ts) =>
    encodeType(deRef(T),encodeType(deRef(V),[Ts..,0ca])).
  

  encodeText:(string,list[integer]) => list[integer].
  encodeText(Txt,Ts) where Chrs .= Txt::list[integer] &&
      D.=findDelim(Chrs,[0c|,0c/,0c%]) =>
    encodeQuoted(Chrs,D,[Ts..,D]).

  findDelim:(list[integer],list[integer])=>integer.
  findDelim(Chrs,[]) => 0x22. -- == "
  findDelim(Chrs,[D,..Ds]) where D in Chrs => findDelim(Chrs,Ds).
  findDelim(Chrs,[D,.._]) => D.

  encodeQuoted([],D,Ts) => [Ts..,D].
  encodeQuoted([D,..Cs],D,Ts) => encodeQuoted(Cs,D,[Ts..,0c\\,D]).
  encodeQuoted([C,..Cs],D,Ts) => encodeQuoted(Cs,D,[Ts..,C]).

  encodeNat:(integer,list[integer]) => list[integer].
  encodeNat(Dx,Ts) where Dx>=0 && Dx=<9 =>
    [Ts..,Dx+0c0].
  encodeNat(Dx,Ts) => [encodeNat(Dx/10,Ts)..,(Dx%10)+0c0].

  encodeInt:(integer,list[integer])=>list[integer].
  encodeInt(Ix,Ts) where Ix<0 => encodeNat(-Ix,[Ts..,0c-]).
  encodeInt(Ix,Ts) => encodeNat(Ix,Ts).

  public implementation coercion[locn,term]=>{
    _coerce(locn(Pkg,Line,Col,Off,Ln))=>mkTpl([strg(Pkg),intgr(Line),intgr(Col),intgr(Off),intgr(Ln)]).
  }

  public pkgTerm:(pkg)=>term.
  pkgTerm(pkg(Pk,Ver))=>mkCons("pkg",[strg(Pk),versTerm(Ver)]).

  public versTerm:(version)=>term.
  versTerm(.defltVersion) => enum(tLbl("*",0)).
  versTerm(vers(V)) => strg(V).
}
