star.compiler.data{
  import star.

  import star.multi.
  import star.pkg.
  import star.sort.

  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.ltipe.

  public termLbl ::= .tLbl(string,integer).

  public data ::= .intgr(integer)
    | .bigi(bigint)
    | .flot(float)
    | .chr(char)
    | .strg(string)
    | .term(termLbl,cons[data])
    | .symb(termLbl).

  public implementation display[termLbl] => {
    disp(.tLbl(Nm,Ar)) => "#(Nm)/$(Ar)".
  }

  public implementation sizeable[termLbl] => {
    size(.tLbl(_,Ar))=>Ar.

    isEmpty(.tLbl(_,Ar))=>Ar==0.
  }

  public implementation display[data] => let{.
    dispT(.intgr(Ix)) => "$(Ix)".
    dispT(.bigi(Ix)) => "$(Ix)".
    dispT(.flot(Dx)) => disp(Dx).
    dispT(.chr(Cx)) => disp(Cx).
    dispT(.strg(Sx)) => disp(Sx).
    dispT(.term(.tLbl(T,_),Args)) where isTupleLbl(T) => "(#(dispTs(Args)))".
    dispT(.term(.tLbl(Op,_),Args)) => "#(Op)(#(dispTs(Args)))".
    dispT(.symb(Sx)) => "'$(Sx)'".

    dispTs(Els) => interleave(Els//dispT,",")*.

    isTupleLbl(T) where [`(`,`)`,.._] .= T::cons[char] => .true.
    isTupleLbl(_) default => .false.
  .} in {
    disp(T) => dispT(T)
  }

  public implementation hashable[termLbl] => {
    hash(.tLbl(Nm,Ar))=>hash(Nm)*37+Ar.
  }

  public implementation hashable[data] => let{.
    hsh(.intgr(X)) => X.
    hsh(.bigi(X)) => hash(X).
    hsh(.flot(X)) => hash(X).
    hsh(.chr(C)) => hash(C).
    hsh(.strg(S)) => hash(S).
    hsh(.symb(S)) => hash(S).
    hsh(.term(Op,Args)) =>
      foldRight((T,H)=>H*37+hsh(T),hash(Op)*37,Args).
  .} in {
    hash(T) => hsh(T)
  }

  public implementation equality[termLbl] => {
    .tLbl(N1,A1)==.tLbl(N2,A2) => N1==N2 && A1==A2.
  }

  public implementation equality[data] => let{.
    eq(.intgr(X),.intgr(Y)) => X==Y.
    eq(.bigi(X),.bigi(Y)) => X==Y.
    eq(.flot(X),.flot(Y)) => X==Y.
    eq(.chr(X),.chr(Y)) => X==Y.
    eq(.strg(X),.strg(Y)) => X==Y.
    eq(.symb(X),.symb(Y)) => X==Y.
    eq(.term(O1,A1),.term(O2,A2)) => O1==O2 && eqList(A1,A2).
    eq(_,_) default => .false.

    eqList(.nil,.nil)=>.true.
    eqList(.cons(E1,L1),.cons(E2,L2)) => eq(E1,E2) && eqList(L1,L2).
    eqList(_,_) default => .false.
  .} in {
    X==Y => eq(X,Y).
  }

  public mkTpl:(cons[data]) => data.
  mkTpl(A) where L.=size(A) => .term(.tLbl(tplLbl(L),L),A).

  public mkLst:(cons[data]) => data.
  mkLst(Els) => .term(.tLbl("[]",size(Els)),Els).

  public mkCons:(string,cons[data])=>data.
  mkCons(Nm,Args) => .term(.tLbl(Nm,size(Args)),Args).

  public isScalar:(data)=>boolean.
  isScalar(.intgr(_)) => .true.
  isScalar(.bigi(_)) => .true.
  isScalar(.flot(_)) => .true.
  isScalar(.chr(_)) => .true.
  isScalar(.strg(_)) => .true.
  isScalar(.symb(_)) => .true.
  isScalar(_) default => .false.

  public implementation coercion[data,string] => {
    _coerce(T) => some(_implode(reverse(encodeT(T,[])))).
  }

  public trueEnum:data.
  trueEnum = .symb(.tLbl("star.core*true",0)).

  public falseEnum:data.
  falseEnum = .symb(.tLbl("star.core*false",0)).

  -- Written in this way to maximize potential for tail recursion

  encodeT:(data,cons[char])=>cons[char].
  encodeT(.intgr(Ix),Chs) => encodeInt(Ix,[`x`,..Chs]).
  encodeT(.bigi(Ix),Chs) => encodeBig(Ix,[`b`,..Chs]).
  encodeT(.flot(Dx),Chs) => encodeText(Dx::string,[`d`,..Chs]).
  encodeT(.chr(Cx),Chs) => encodeChar(Cx,[`c`,..Chs]).
  encodeT(.strg(Tx),Chs) => encodeText(Tx,[`s`,..Chs]).
  encodeT(.symb(Sym),Chs) => encodeL(Sym,Chs).
  encodeT(.term(.tLbl("[]",Ar),Els),Chs) => encodeTerms(Els,encodeNat(Ar,[`l`,..Chs])).
  encodeT(.term(Op,Args),Chs) =>
    encodeTerms(Args,encodeL(Op,encodeNat(size(Args),[`n`,..Chs]))).

  encodeL:(termLbl,cons[char])=>cons[char].
  encodeL(.tLbl(Nm,Ar),Chs) => encodeText(Nm,encodeNat(Ar,[`o`,..Chs])).

  encodeTerms([],Chs) => Chs.
  encodeTerms([T,..Ts],Chs) => encodeTerms(Ts,encodeT(T,Chs)).

  encodeBig(Bx,Chs) => encodeText(Bx::string,Chs).

  public implementation coercion[string,data] => {
    _coerce(S) => valof{
      L=S::cons[char];
      (T,_) = decodeTerm(S::cons[char]);
      valis some(T)
    }
  }
  
  public decodeTerm:(cons[char])=>(data,cons[char]).
  decodeTerm([`x`,..Ls]) => valof{
    (Ix,L0) = decodeInt(Ls);
    valis (intgr(Ix),L0)
  }.
  decodeTerm([`d`,..Ls]) => valof{
    (Txt,Lx) = decodeText(Ls);
    valis (flot(Txt::float),Lx)
  }
  decodeTerm([`e`,..Ls]) => valof{
    (Sym,Lx) = decodeText(Ls);
    valis (symb(tLbl(Sym,0)),Lx)
  }
  decodeTerm([`o`,..Ls]) => valof{
    (Sym,Lx) = decodeLabel([`o`,..Ls]);
    valis (symb(Sym),Lx)
  }
  decodeTerm([`c`,..Ls]) => valof{
    (Ch,Lx) = decodeChar(Ls);
    valis (chr(Ch),Lx)
  }
  decodeTerm([`s`,..Ls]) => valof{
    (Txt,Lx) = decodeText(Ls);
    valis (strg(Txt),Lx)
  }
  decodeTerm([`n`,..Ls]) => valof{
    (Ax,L0) = decodeNat(Ls,0);
    (Op,LL1) = decodeLabel(L0);
    (Args,Lx) = decodeTerms(LL1,Ax,[]);
    valis (term(Op,Args),Lx)
  }
  decodeTerm([`l`,..Ls]) => valof{
    (Ax,L0) = decodeNat(Ls,0);
    (Els,Lx) = decodeTerms(L0,Ax,[]);
    valis (mkLst(Els),Lx)
  }

  decodeTerms:(cons[char],integer,cons[data]) => (cons[data],cons[char]).
  decodeTerms(L,0,Args) => (reverse(Args),L).
  decodeTerms(L,Ix,Args) => valof{
    (Arg,L0) = decodeTerm(L);
    valis decodeTerms(L0,Ix-1,[Arg,..Args])
  }

  decodeLabel:(cons[char])=>(termLbl,cons[char]).
  decodeLabel([`o`,..Ls]) => valof{
    (Ar,L0) = decodeNat(Ls,0);
    (Nm,Lx) = decodeText(L0);
    valis (tLbl(Nm,Ar),Lx)
  }
    
  decodeInt:(cons[char])=>(integer,cons[char]).
  decodeInt([`-`,..L]) => valof{
    (Px,Lx) = decodeNat(L,0);
    valis (-Px,Lx)
  }
  decodeInt(L) default => decodeNat(L,0).
  
  decodeNat:(cons[char],integer) => (integer,cons[char]).
  decodeNat([Cx,..Ls],Ix) where isDigit(Cx) => decodeNat(Ls,Ix*10+digitVal(Cx)).
  decodeNat(Ls,Ix) default => (Ix,Ls).

  decodeChar:(cons[char]) => (char,cons[char]).
  decodeChar([`\\`,X,..L]) => (X,L).
  decodeChar([X,..L]) => (X,L).

  decodeText:(cons[char]) => (string,cons[char]).
  decodeText([C,..L]) => valof{
    (Q,Cs) = collectQuoted(L,[],C);
    valis (reverse(Q)::string,Cs)
  }

  collectQuoted:(cons[char],cons[char],char) => (cons[char],cons[char]).
  collectQuoted([S,..Lx],SoF,S) => (SoF,Lx).
  collectQuoted([`\\`,X,..L],SoF,S) => collectQuoted(L,[X,..SoF],S).
  collectQuoted([X,..L],SoF,S) => collectQuoted(L,[X,..SoF],S).

  public decodeSignature:(string) => tipe throws ().
  decodeSignature(St) => valof{
    (Tp,_) = decodeType(St::cons[char]);
    valis Tp
  }

  decodeType:(cons[char]) => (tipe,cons[char]) throws ().
  decodeType([`i`,..Ts]) => (nomnal("star.core*integer"),Ts).
  decodeType([`b`,..Ts]) => (nomnal("star.core*bigint"),Ts).
  decodeType([`f`,..Ts]) => (nomnal("star.core*float"),Ts).
  decodeType([`c`,..Ts]) => (nomnal("star.core*char"),Ts).
  decodeType([`s`,..Ts]) => (nomnal("star.core*string"),Ts).
  decodeType([`l`,..Ts]) => (nomnal("star.core*boolean"),Ts).
  decodeType([`_`,..Ts]) => (newTypeVar("_"),Ts).
  decodeType([`k`,..Ts]) => valof{
    (Nm,T1) = decodeText(Ts);
    valis (nomnal(Nm),T1)
  }
  decodeType([`K`,..Ts]) => valof{
    (Ar,T0) = decodeNat(Ts,0);
    (Nm,T1) = decodeText(T0);
    valis (kFun(Nm,Ar),T1)
  }
  decodeType([`t`,..Ts]) => valof{
    (Nm,T1) = decodeText(Ts);
    valis (nomnal(Nm),T1)
  }
  decodeType([`z`,..Ts]) => valof{
    (Ar,T0) = decodeNat(Ts,0);
    (Nm,T1) = decodeText(T0);
    valis (tpFun(Nm,Ar),T1)
  }
  decodeType([`L`,..Ts]) => valof{
    (ElTp,T0) = decodeType(Ts);
    valis (lstType(ElTp),T0)
  }
  decodeType([`U`,..Ts]) => valof{
    (OpTp,T0) = decodeType(Ts);
    (ElTp,T1) = decodeType(T0);
    valis (tpExp(OpTp,ElTp),T1)
  }
  decodeType([`r`,..Ts]) => valof{
    (ElTp,T0) = decodeType(Ts);
    valis (refType(ElTp),T0)
  }
  decodeType([`(`,..Ts]) => valof{
    (Tps,T0) = decodeTypes(Ts);
    valis (tupleType(Tps),T0)
  }
  decodeType([`:`,..Ts]) => valof{
    (V,T0) = decodeType(Ts);
    (B,T1) = decodeType(T0);
    valis (allType(V,B),T1)
  }
  decodeType([`e`,..Ts]) => valof{
    (V,T0) = decodeType(Ts);
    (B,T1) = decodeType(T0);
    valis (existType(V,B),T1)
  }
  decodeType([`|`,..Ts]) => valof{
    (V,T0) = decodeType(Ts);
    (B,T1) = decodeConstraint(T0);
    valis (constrainedType(V,B),T1)
  }
  decodeType([`I`,..Ts]) => valof{
    (F1,T0) = decodeFields(Ts);
    (F2,T1) = decodeFields(T0);
    valis (faceType(F1,F2),T1)
  }
  decodeType([`F`,..Ts]) => valof{
    (A,T0) = decodeType(Ts);
    (R,T1) = decodeType(T0);
    valis (fnType(A,R),T1)
  }
  decodeType([`C`,..Ts]) => valof{
    (A,T0) = decodeType(Ts);
    (R,T1) = decodeType(T0);
    valis (consType(A,R),T1)
  }
  decodeType(_) default => throw ().

  decodeTypes:(cons[char])=> (cons[tipe],cons[char]) throws ().
  decodeTypes([`)`,..Ts]) => ([],Ts). 
  decodeTypes(Ts) => valof{
    (ElTp,T0) = decodeType(Ts);
    (Tps,T1) = decodeTypes(T0);
    valis ([ElTp,..Tps],T1)
  }

  public decodeTypeRuleSignature:(string) => typeRule throws ().
  decodeTypeRuleSignature(St) => valof{
    (Tp,_) = decodeTypeRule(St::cons[char]);
    valis Tp
  }

  decodeTypeRule:(cons[char])=>(typeRule,cons[char]) throws ().
  decodeTypeRule([`:`,..Ts]) => valof{
    (V,T0) = decodeType(Ts);
    (R,T1) = decodeTypeRule(T0);
    valis (allRule(V,R),T1)
  }  
  decodeTypeRule([`Y`,..Ts]) => valof{
    (A,T0) = decodeType(Ts);
    (R,T1) = decodeType(T0);
    valis (typeExists(A,R),T1)
  }
  decodeTypeRule([`Z`,..Ts]) => valof{
    (.conTract(N,T,D),T0) = decodeConstraint(Ts);
    (R,T1) = decodeType(T0);
    valis (.contractExists(N,T,D,R),T1)
  }
  decodeTypeRule([`y`,..Ts]) => valof{
    (A,T0) = decodeType(Ts);
    (R,T1) = decodeType(T0);
    valis (.typeLambda(A,R),T1)
  }

  decodeFields:(cons[char])=>(cons[(string,tipe)],cons[char]) throws ().
  decodeFields([`{`,..Ts]) => decodeFlds(Ts,[]).

  decodeFlds:(cons[char],cons[(string,tipe)])=>
    (cons[(string,tipe)],cons[char]) throws ().
  decodeFlds([`}`,..Ts],Flds) => (reverse(Flds),Ts).
  decodeFlds(Ts,Flds) => valof{
    (Nm,T0) = decodeText(Ts);
    (Tp,T1) = decodeType(T0);
    valis decodeFlds(T1,[(Nm,Tp),..Flds])
  }

  decodeConstraint:(cons[char])=>(constraint,cons[char]) throws ().
  decodeConstraint([`c`,..T]) => valof{
    (Nm,T0) = decodeText(T);
    (.tupleType(Tps),T1) = decodeType(T0);
    (.tupleType(Dps),T2) = decodeType(T1);
    valis (.conTract(Nm,Tps,Dps),T2)
  }
  decodeConstraint([`a`,..T]) => valof{
    (BT,T0) = decodeType(T);
    (.faceType([(Fld,FT)],_),T1) = decodeType(T0);
    valis (.fieldConstraint(BT,Fld,FT),T1)
  }

  public encodeSignature:(tipe) => string.
  encodeSignature(Tp) => reverse(encodeType(deRef(Tp),[]))::string.

  encodeType:(tipe,cons[char]) => cons[char].
  encodeType(V,Chs) where isUnbound(V) => [`_`,..Chs].
  encodeType(.nomnal("star.core*integer"),Chs) => [`i`,..Chs].
  encodeType(.nomnal("star.core*float"),Chs) => [`f`,..Chs].
  encodeType(.nomnal("star.core*string"),Chs) => [`s`,..Chs].
  encodeType(.nomnal("star.core*boolean"),Chs) => [`l`,..Chs].
  encodeType(.nomnal(Nm),Chs) => encodeText(Nm,[`t`,..Chs]).
  encodeType(.kFun(Nm,Ar),Chs) => encodeText(Nm,encodeNat(Ar,[`K`,..Chs])).
  encodeType(.tpFun(Nm,Ar),Chs) => encodeText(Nm,encodeNat(Ar,[`z`,..Chs])).
  encodeType(.tpExp(.tpFun("star.core*cons",1),El),Chs) =>
    encodeType(deRef(El),[`L`,..Chs]).
  encodeType(.tpExp(.tpFun("star.core*ref",1),El),Chs) =>
    encodeType(deRef(El),[`r`,..Chs]).
  encodeType(.tpExp(.tpExp(.tpFun("=>",2),A),R),Chs) =>
    encodeType(deRef(R),encodeType(deRef(A),[`F`,..Chs])).
  encodeType(.tpExp(.tpExp(.tpFun("<=>",2),A),R),Chs) =>
    encodeType(deRef(R),encodeType(deRef(A),[`C`,..Chs])).
  encodeType(.tpExp(Op,A),Chs) =>
    encodeType(deRef(A),encodeType(deRef(Op),[`U`,..Chs])).
  encodeType(.tupleType(Els),Chs) => encodeTypes(Els,[`(`,..Chs]).
  encodeType(.allType(V,T),Chs) =>
    encodeType(deRef(T),encodeType(deRef(V),[`:`,..Chs])).
  encodeType(.existType(V,T),Chs) =>
    encodeType(deRef(T),encodeType(deRef(V),[`e`,..Chs])).
  encodeType(.constrainedType(T,C),Chs) =>
    encodeConstraint(C,encodeType(deRef(T),[`|`,..Chs])).
  encodeType(.faceType(Flds,Tps),Chs) =>
    encodeFldTypes(Tps,encodeFldTypes(Flds,[`I`,..Chs])).

  encodeTypes:(cons[tipe],cons[char])=>cons[char].
  encodeTypes([],Chs) => [`)`,..Chs].
  encodeTypes([T,..Tps],Chs) =>
    encodeTypes(Tps,encodeType(deRef(T),Chs)).

  encodeFldTypes:(cons[(string,tipe)],cons[char])=>cons[char].
  encodeFldTypes(Flds,Chs) =>
    encodeFlds(sort(Flds,((N1,_),(N2,_))=>N1<N2),[`{`,..Chs]).
  
  encodeFlds:(cons[(string,tipe)],cons[char])=>cons[char].
  encodeFlds([],Chs) => [`}`,..Chs].
  encodeFlds([(Nm,T),..Tps],Chs) =>
    encodeFlds(Tps,encodeType(deRef(T),encodeText(Nm,Chs))).

  encodeConstraint(.conTract(Nm,Ts,Ds),Chs) =>
    encodeType(.tupleType(Ds),
      encodeType(.tupleType(Ts),
	encodeText(Nm,[`c`,..Chs]))).
  encodeConstraint(.fieldConstraint(V,F,T),Chs) =>
    encodeType(.faceType([(F,deRef(T))],[]),encodeType(deRef(V),[`a`,..Chs])).

  public encodeTypeRule:(typeRule,cons[char])=>cons[char].
  encodeTypeRule(.allRule(V,R),Chs) =>
    encodeTypeRule(R,encodeType(deRef(V),[`:`,..Chs])).
  encodeTypeRule(.typeExists(H,I),Chs) =>
    encodeType(deRef(I),encodeType(deRef(H),[`Y`,..Chs])).
  encodeTypeRule(.contractExists(N,T,D,I),Chs) =>
    encodeType(deRef(I),encodeConstraint(conTract(N,T,D),[`Z`,..Chs])).
  encodeTypeRule(.typeLambda(Hd,I),Chs) =>
    encodeType(deRef(I),encodeType(deRef(Hd),[`y`,..Chs])).

  public encodeTpRlSignature:(typeRule) => string.
  encodeTpRlSignature(Rl) => reverse(encodeTypeRule(Rl,[]))::string.

  encodeText:(string,cons[char]) => cons[char].
  encodeText(Txt,Chs) where Chrs .= Txt::cons[char] &&
      D.=findDelim(Chrs,[`|`,`/`,`%`]) =>
    encodeQuoted(Chrs,D,[D,..Chs]).

  encodeChar:(char,cons[char]) => cons[char].
  encodeChar(`\\`,Chs) => [`\\`,`\\`,..Chs].
  encodeChar(Ch,Chs) => [Ch,..Chs].

  findDelim:(cons[char],cons[char])=>char.
  findDelim(Chrs,[]) => `'`. 
  findDelim(Chrs,[D,..Ds]) where {? D in Chrs ?} => findDelim(Chrs,Ds).
  findDelim(Chrs,[D,.._]) => D.

  encodeQuoted([],D,Chs) => [D,..Chs].
  encodeQuoted([D,..Cs],D,Chs) => encodeQuoted(Cs,D,[D,`\\`,..Chs]).
  encodeQuoted([`\\`,..Cs],D,Chrs) => encodeQuoted(Cs,D,[`\\`,`\\`,..Chrs]).
  encodeQuoted([C,..Cs],D,Chrs) => encodeQuoted(Cs,D,[C,..Chrs]).

  encodeNat:(integer,cons[char]) => cons[char].
  encodeNat(Dx,Chs) where Dx>=0 && Dx=<9 =>
    [digitChar(Dx),..Chs].
  encodeNat(Dx,Chs) => [digitChar(Dx%10),..encodeNat(Dx/10,Chs)].

  encodeInt:(integer,cons[char])=>cons[char].
  encodeInt(Ix,Chs) where Ix<0 => encodeNat(-Ix,[`-`,..Chs]).
  encodeInt(Ix,Chs) => encodeNat(Ix,Chs).

  public implementation coercion[locn,data]=>{
    _coerce(.locn(Pkg,Line,Col,Off,Ln))=>some(mkTpl([.strg(Pkg),.intgr(Line),.intgr(Col),.intgr(Off),.intgr(Ln)])).
  }

  public pkgTerm:(pkg)=>data.
  pkgTerm(.pkg(Pk,Ver))=>mkCons("pkg",[.strg(Pk),versTerm(Ver)]).

  public versTerm:(version)=>data.
  versTerm(.defltVersion) => .symb(tLbl("*",0)).
  versTerm(.vers(V)) => .strg(V).
}
