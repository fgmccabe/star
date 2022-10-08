star.compiler.data{
  import star.

  import star.multi.
  import star.pkg.
  import star.sort.

  import star.compiler.location.
  import star.compiler.meta.
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
    dispT(D) => case D in {
      .intgr(Ix) => "$(Ix)".
      .bigi(Ix) => "$(Ix)".
      .flot(Dx) => disp(Dx).
      .chr(Cx) => disp(Cx).
      .strg(Sx) => disp(Sx).
      .term(.tLbl(T,_),Args) where isTupleLbl(T) => "(#(dispTs(Args)))".
      .term(.tLbl(Op,_),Args) => "#(Op)(#(dispTs(Args)))".
      .symb(Sx) => "'$(Sx)'".
    }

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
    hsh(D) => case D in {
      .intgr(X) => X.
      .bigi(X) => hash(X).
      .flot(X) => hash(X).
      .chr(C) => hash(C).
      .strg(S) => hash(S).
      .symb(S) => hash(S).
      .term(Op,Args) =>
	foldRight((T,H)=>H*37+hsh(T),hash(Op)*37,Args).
    }
  .} in {
    hash(T) => hsh(T)
  }

  public implementation equality[termLbl] => {
    .tLbl(N1,A1)==.tLbl(N2,A2) => N1==N2 && A1==A2.
  }

  public implementation equality[data] => let{.
    eq(D1,D2) => case D1 in {
      .intgr(X) => .intgr(Y).=D2 && X==Y.
      .bigi(X) => .bigi(Y).=D2 && X==Y.
      .flot(X) => .flot(Y).=D2 && X==Y.
      .chr(X) => .chr(Y).=D2 && X==Y.
      .strg(X) => .strg(Y).=D2 && X==Y.
      .symb(X) => .symb(Y).=D2 && X==Y.
      .term(O1,A1) => .term(O2,A2).=D2 && O1==O2 && eqList(A1,A2).
      _ default => .false.
    }

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
  isScalar(D) => case D in {
    .intgr(_) => .true.
    .bigi(_) => .true.
    .flot(_) => .true.
    .chr(_) => .true.
    .strg(_) => .true.
    .symb(_) => .true.
    _ default => .false.
  }

  public implementation coercion[data,string] => {
    _coerce(T) => some(_implode(reverse(encodeT(T,[])))).
  }

  public trueEnum:data.
  trueEnum = .symb(.tLbl("star.core*true",0)).

  public falseEnum:data.
  falseEnum = .symb(.tLbl("star.core*false",0)).

  -- Written in this way to maximize potential for tail recursion

  encodeT:(data,cons[char])=>cons[char].
  encodeT(D,Chs) => case D in {
    .intgr(Ix) => encodeInt(Ix,[`x`,..Chs]).
    .bigi(Ix) => encodeBig(Ix,[`b`,..Chs]).
    .flot(Dx) => encodeText(Dx::string,[`d`,..Chs]).
    .chr(Cx) => encodeChar(Cx,[`c`,..Chs]).
    .strg(Tx) => encodeText(Tx,[`s`,..Chs]).
    .symb(Sym) => encodeL(Sym,Chs).
    .term(.tLbl("[]",Ar),Els) => encodeTerms(Els,encodeNat(Ar,[`l`,..Chs])).
    .term(Op,Args) =>
      encodeTerms(Args,encodeL(Op,encodeNat(size(Args),[`n`,..Chs]))).
  }

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
  decodeTerm([Ch,..Ls]) => case Ch in {
    `x` => valof{
      (Ix,L0) = decodeInt(Ls);
      valis (intgr(Ix),L0)
    }
    `d` => valof{
      (Txt,Lx) = decodeText(Ls);
      valis (flot(Txt::float),Lx)
    }
    `e` => valof{
      (Sym,Lx) = decodeText(Ls);
      valis (symb(tLbl(Sym,0)),Lx)
    }
    `o` => valof{
      (Sym,Lx) = decodeLabel([`o`,..Ls]);
      valis (symb(Sym),Lx)
    }
    `c` => valof{
      (Ch,Lx) = decodeChar(Ls);
      valis (chr(Ch),Lx)
    }
    `s` => valof{
      (Txt,Lx) = decodeText(Ls);
      valis (strg(Txt),Lx)
    }
    `n` => valof{
      (Ax,L0) = decodeNat(Ls,0);
      (Op,LL1) = decodeLabel(L0);
      (Args,Lx) = decodeTerms(LL1,Ax,[]);
      valis (term(Op,Args),Lx)
    }
    `l` => valof{
      (Ax,L0) = decodeNat(Ls,0);
      (Els,Lx) = decodeTerms(L0,Ax,[]);
      valis (mkLst(Els),Lx)
    }
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
  decodeType([Ch,..Ts]) => case Ch in {
    `i` => (nomnal("star.core*integer"),Ts).
    `b` => (nomnal("star.core*bigint"),Ts).
    `f` => (nomnal("star.core*float"),Ts).
    `c` => (nomnal("star.core*char"),Ts).
    `s` => (nomnal("star.core*string"),Ts).
    `l` => (nomnal("star.core*boolean"),Ts).
    `_` => (newTypeVar("_"),Ts).
    `k` => valof{
      (Nm,T1) = decodeText(Ts);
      valis (nomnal(Nm),T1)
    }
    `K` => valof{
      (Ar,T0) = decodeNat(Ts,0);
      (Nm,T1) = decodeText(T0);
      valis (kFun(Nm,Ar),T1)
    }
    `t` => valof{
      (Nm,T1) = decodeText(Ts);
      valis (nomnal(Nm),T1)
    }
    `z` => valof{
      (Ar,T0) = decodeNat(Ts,0);
      (Nm,T1) = decodeText(T0);
      valis (tpFun(Nm,Ar),T1)
    }
    `L` => valof{
      (ElTp,T0) = decodeType(Ts);
      valis (lstType(ElTp),T0)
    }
    `U` => valof{
      (OpTp,T0) = decodeType(Ts);
      (ElTp,T1) = decodeType(T0);
      valis (tpExp(OpTp,ElTp),T1)
    }
    `r` => valof{
      (ElTp,T0) = decodeType(Ts);
      valis (refType(ElTp),T0)
    }
    `(` => valof{
      (Tps,T0) = decodeTypes(Ts);
      valis (tupleType(Tps),T0)
    }
    `:` => valof{
      (V,T0) = decodeType(Ts);
      (B,T1) = decodeType(T0);
      valis (allType(V,B),T1)
    }
    `e` => valof{
      (V,T0) = decodeType(Ts);
      (B,T1) = decodeType(T0);
      valis (existType(V,B),T1)
    }
    `|` => valof{
      (V,T0) = decodeType(Ts);
      (B,T1) = decodeConstraint(T0);
      valis (constrainedType(V,B),T1)
    }
    `I` => valof{
      (F1,T0) = decodeFields(Ts);
      (F2,T1) = decodeFields(T0);
      valis (faceType(F1,F2),T1)
    }
    `F` => valof{
      (A,T0) = decodeType(Ts);
      (R,T1) = decodeType(T0);
      valis (fnType(A,R),T1)
    }
    `C` => valof{
      (A,T0) = decodeType(Ts);
      (R,T1) = decodeType(T0);
      valis (consType(A,R),T1)
    }
  }

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
  encodeType(Tp,Chs) => isUnbound(Tp) ?
    [`_`,..Chs] ||
    case Tp in {
      .nomnal("star.core*integer") => [`i`,..Chs].
      .nomnal("star.core*float") => [`f`,..Chs].
      .nomnal("star.core*string") => [`s`,..Chs].
      .nomnal("star.core*boolean") => [`l`,..Chs].
      .nomnal(Nm) => encodeText(Nm,[`t`,..Chs]).
      .kFun(Nm,Ar) => encodeText(Nm,encodeNat(Ar,[`K`,..Chs])).
      .tpFun(Nm,Ar) => encodeText(Nm,encodeNat(Ar,[`z`,..Chs])).
      .tpExp(.tpFun("star.core*cons",1),El) =>
	encodeType(deRef(El),[`L`,..Chs]).
      .tpExp(.tpFun("star.core*ref",1),El) =>
	encodeType(deRef(El),[`r`,..Chs]).
      .tpExp(.tpExp(.tpFun("=>",2),A),R) =>
	encodeType(deRef(R),encodeType(deRef(A),[`F`,..Chs])).
      .tpExp(.tpExp(.tpFun("<=>",2),A),R) =>
	encodeType(deRef(R),encodeType(deRef(A),[`C`,..Chs])).
      .tpExp(Op,A) =>
	encodeType(deRef(A),encodeType(deRef(Op),[`U`,..Chs])).
      .tupleType(Els) => encodeTypes(Els,[`(`,..Chs]).
      .allType(V,T) =>
	encodeType(deRef(T),encodeType(deRef(V),[`:`,..Chs])).
      .existType(V,T) =>
	encodeType(deRef(T),encodeType(deRef(V),[`e`,..Chs])).
      .constrainedType(T,C) =>
	encodeConstraint(C,encodeType(deRef(T),[`|`,..Chs])).
      .faceType(Flds,Tps) =>
	encodeFldTypes(Tps,encodeFldTypes(Flds,[`I`,..Chs])).
    }.

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

  encodeSig:(tipe)=>data.
  encodeSig(Tp) => strg(encodeSignature(Tp)).

  public implementation coercion[decl,data] => {
    _coerce(D) => .some(case D in {
	.implDec(_,ConNm,ImplNm,Tp) =>
	  mkCons("imp",[strg(ConNm),strg(ImplNm),encodeSig(Tp)]).
	.accDec(_,Tp,Fld,Fn,AccTp) =>
	  mkCons("acc",[encodeSig(Tp),strg(Fld),strg(Fn),encodeSig(AccTp)]).
	.updDec(_,Tp,Fld,Fn,AccTp) =>
	  mkCons("upd",[encodeSig(Tp),strg(Fld),strg(Fn),encodeSig(AccTp)]).
	.conDec(_,Nm,FullNm,TpRl) =>
	  mkCons("con",[strg(Nm),strg(FullNm),strg(encodeTpRlSignature(TpRl))]).
	.tpeDec(_,Nm,Tp,TpRl) =>
	  mkCons("tpe",[strg(Nm),encodeSig(Tp),strg(encodeTpRlSignature(TpRl))]).
	.cnsDec(_,Nm,FullNm,Tp) =>
	  mkCons("cns",[strg(Nm),strg(FullNm),encodeSig(Tp)]).
	.varDec(_,Nm,FullNm,Tp) =>
	  mkCons("var",[strg(Nm),strg(FullNm),encodeSig(Tp)]).
	.funDec(_,Nm,FullNm,Tp) =>
	  mkCons("fun",[strg(Nm),strg(FullNm),encodeSig(Tp)]).
      }).
  }

  public pkgTerm:(pkg)=>data.
  pkgTerm(.pkg(Pk,Ver))=>mkCons("pkg",[.strg(Pk),versTerm(Ver)]).

  public versTerm:(version)=>data.
  versTerm(.defltVersion) => .symb(tLbl("*",0)).
  versTerm(.vers(V)) => .strg(V).
}
