star.compiler.terms{
  import star.
  import star.parse.

  import star.compiler.location.
  import star.compiler.types.

  public data ::= intgr(integer)
    | flot(float)
    | strg(string)
    | term(data,list[data])
    | lbl(string,integer).

  public implementation display[data] => let{
    dispT(intgr(Ix)) => disp(Ix).
    dispT(flot(Dx)) => disp(Dx).
    dispT(strg(Sx)) => disp(Sx).
    dispT(term(lbl("[]",_),Args)) => ssSeq([ss("["),ssSeq(dispTs(Args)),ss("]")]).
    dispT(term(lbl(T,_),Args)) where isTupleLbl(T) => ssSeq([ss("("),ssSeq(dispTs(Args)),ss(")")]).
    dispT(term(Op,Args)) => ssSeq([disp(Op),ss("("),ssSeq(dispTs(Args)),ss(")")]).
    dispT(lbl(Nm,Ar)) => ssSeq([ss(Nm),ss("/"),disp(Ar)]).

    dispTs(Els) => interleave(Els//dispT,ss(",")).

    isTupleLbl(T) where [0c(,0c),.._] .= T::list[integer] => true.
    isTupleLbl(_) default => false.
  } in {.
    disp(T) => dispT(T)
  .}

  public implementation hash[data] => let{
    hsh(intgr(X)) => X.
    hsh(flot(X)) => hash(X).
    hsh(strg(S)) => hash(S).
    hsh(term(Op,Args)) =>
      foldRight((T,H)=>H*37+hsh(T),hash(Op)*37,Args).
    hsh(lbl(N,A)) => hash(N)*37+A
  } in {
    hash(T) => hsh(T)
  }

  public implementation equality[data] => let{
    eq(intgr(X),intgr(Y)) => X==Y.
    eq(flot(X),flot(Y)) => X==Y.
    eq(strg(X),strg(Y)) => X==Y.
    eq(term(O1,A1),term(O2,A2)) => O1==O2 && A1==A2.
    eq(lbl(N1,A1),lbl(N2,A2)) => N1==N2 && A1==A2.
    eq(_,_) default => false.
  } in {.
    X==Y => eq(X,Y).
  .}

  public mkTpl:(list[data]) => data.
  mkTpl(A) where L.=size(A) => term(lbl(tplLbl(L),L),A).

  public tplLbl:(integer)=>string.
  tplLbl(Ar) => "()$(Ar)".

  public isTplLbl:(string)=>boolean.
  isTplLbl(Nm) where [0c(,0c),..Ds].=(Nm::list[integer]) => true.
  isTplLbl(_) default => false.

  public isScalar:(data)=>boolean.
  isScalar(intgr(_)) => true.
  isScalar(flot(_)) => true.
  isScalar(strg(_)) => true.
  isScalar(lbl(_,_)) => true.
  isScalar(_) default => false.

  public implementation coercion[data,string] => {
    _coerce(T) => _implode(encodeTerm(T)).
  }

  public encodeTerm:(data)=>list[integer].
  encodeTerm(T) => encodeT(T,[]).

  encodeT:(data,list[integer])=>list[integer].
  encodeT(intgr(Ix),Cs) => encodeInt(Ix,[Cs..,0cx]).
  encodeT(flot(Dx),Cs) => encodeText(Dx::string,[Cs..,0cd]).
  encodeT(strg(Tx),Cs) => encodeText(Tx,[Cs..,0cs]).
  encodeT(lbl(Nm,Ar),Cs) => encodeText(Nm,encodeNat(Ar,[Cs..,0co])).
  encodeT(term(Op,Args),Cs) =>
    encodeTerms(Args,encodeT(Op,encodeNat(size(Args),[Cs..,0cn]))).

  encodeTerms([],Cs) => Cs.
  encodeTerms([T,..Ts],Cs) => encodeTerms(Ts,encodeT(T,Cs)).
  
  public decodeTerm:(list[integer])=>either[(),(data,list[integer])].
  decodeTerm([0cx,..Ls]) => do{
    (Ix,L0) <- decodeInt(Ls);
    valis (intgr(Ix),L0)
  }.
  decodeTerm([0cd,..Ls]) => do{
    (Txt,Lx) <- decodeText(Ls);
    valis (flot(Txt::float),Lx)
  }
  decodeTerm([0ce,..Ls]) => do{
    (Nm,Lx) <- decodeText(Ls);
    valis (lbl(Nm,0),Lx)
  }
  decodeTerm([0cs,..Ls]) => do{
    (Nm,Lx) <- decodeText(Ls);
    valis (strg(Nm),Lx)
  }
  decodeTerm([0co,..Ls]) => do{
    (Ar,L0) <- decodeNat(Ls,0);
    (Nm,Lx) <- decodeText(L0);
    valis (lbl(Nm,Ar),Lx)
  }
  decodeTerm([0cn,..Ls]) => do{
    (Ax,L0) <- decodeNat(Ls,0);
    (Op,LL1) <- decodeTerm(L0);
    (Args,Lx) <- decodeTerms(LL1,Ax,[]);
    valis (term(Op,Args),Lx)
  }
  decodeTerm([0cl,..Ls]) => do{
    (Ax,L0) <- decodeNat(Ls,0);
    (Els,Lx) <- decodeTerms(L0,Ax,[]);
    valis (term(lbl("[]",size(Els)),Els),Lx)
  }

  decodeTerms:(list[integer],integer,list[data]) => either[(),(list[data],list[integer])].
  decodeTerms(L,0,Args) => either((Args,L)).
  decodeTerms(L,Ix,Args) => do{
    (Arg,L0) <- decodeTerm(L);
    decodeTerms(L0,Ix-1,[Args..,Arg])
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
  decodeSignature(St) => let{
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
    decodeType([0x28,..Ts]) => do {  -- 0x28 == (
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
      valis (funType(A,R),T1)
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
  } in do{
    (Tp,_) <- decodeType(St::list[integer]);
    valis Tp
  }

  public encodeSignature:(tipe) => string.
  encodeSignature(Tp) => let{
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
    encodeFlds([],Ts) => [Ts..,0x7d]. -- 0x7d == }
    encodeFlds([(Nm,T),..Tps],Ts) =>
      encodeFlds(Tps,encodeType(deRef(T),encodeText(Nm,Ts))).

    encodeConstraint(typeConstraint(T),Ts) =>
      encodeType(deRef(T),[Ts..,0cc]).
    encodeConstraint(fieldConstraint(V,T),Ts) =>
      encodeType(deRef(T),encodeType(deRef(V),[Ts..,0ca])).
  } in encodeType(deRef(Tp),[])::string.

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
}
