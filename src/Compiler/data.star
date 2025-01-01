star.compiler.data{
  import star.

  import star.multi.
  import star.pkg.
  import star.sort.

  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.types.encode.
  import star.compiler.ltipe.

  public termLbl ::= .tLbl(string,integer).

  public data ::= .intgr(integer)
  | .bigi(bigint)
  | .flot(float)
  | .chr(char)
  | .strg(string)
  | .term(string,cons[data])
  | .symb(termLbl)
  | .clos(termLbl,data,tipe).

  public implementation display[termLbl] => {
    disp(.tLbl(Nm,Ar)) => "#(Nm)/$(Ar)".
  }

  public implementation sizeable[termLbl] => {
    size(.tLbl(_,Ar))=>Ar.

    isEmpty(.tLbl(_,Ar))=>Ar==0.
  }

  public implementation display[data] => let{.
    dispT(D) => case D in {
      | .intgr(Ix) => "$(Ix)"
      | .bigi(Ix) => "$(Ix)"
      | .flot(Dx) => disp(Dx)
      | .chr(Cx) => disp(Cx)
      | .strg(Sx) => disp(Sx)
      | .term(T,Args) where isTupleLbl(T) => "(#(dispTs(Args)))"
      | .term(Op,Args) => "#(Op)(#(dispTs(Args)))"
      | .symb(Sx) => "'$(Sx)'"
      | .clos(Lb,Fr,_) => "<$(Lb)\:#(dispT(Fr))>"
    }

    dispTs(Els) => interleave(Els//dispT,",")*.

    isTupleLbl(T) =>_str_start("()",T).
  .} in {
    disp(T) => dispT(T)
  }

  public implementation hashable[termLbl] => {
    hash(.tLbl(Nm,Ar))=>hash(Nm)*37+Ar.
  }

  public implementation hashable[data] => let{.
    hsh(D) => case D in {
      | .intgr(X) => X
      | .bigi(X) => hash(X)
      | .flot(X) => hash(X)
      | .chr(C) => hash(C)
      | .strg(S) => hash(S)
      | .symb(S) => hash(S)
      | .clos(Lb,Fr,_) => hash(Lb)*37+hash(Fr)
      | .term(Op,Args) =>
	foldRight((T,H)=>H*37+hsh(T),hash(Op)*37,Args)
    }
  .} in {
    hash(T) => hsh(T)
  }

  public implementation equality[termLbl] => {
    .tLbl(N1,A1)==.tLbl(N2,A2) => N1==N2 && A1==A2.
  }

  public implementation equality[data] => let{.
    eq(D1,D2) => case D1 in {
      | .intgr(X) => .intgr(Y).=D2 && X==Y
      | .bigi(X) => .bigi(Y).=D2 && X==Y
      | .flot(X) => .flot(Y).=D2 && X==Y
      | .chr(X) => .chr(Y).=D2 && X==Y
      | .strg(X) => .strg(Y).=D2 && X==Y
      | .symb(X) => .symb(Y).=D2 && X==Y
      | .clos(L1,F1,_) => .clos(L2,F2,_).=D2 && L1==L2 && eq(F1,F2)
      | .term(O1,A1) => .term(O2,A2).=D2 && O1==O2 && eqList(A1,A2)
      | _ default => .false
    }

    eqList(.nil,.nil)=>.true.
    eqList(.cons(E1,L1),.cons(E2,L2)) => eq(E1,E2) && eqList(L1,L2).
    eqList(_,_) default => .false.
  .} in {
    X==Y => eq(X,Y).
  }

  public mkTpl:(cons[data]) => data.
  mkTpl(A) where L.=size(A) => .term(tplLbl(L),A).

  public mkLst:(cons[data]) => data.
  mkLst(Els) => .term("[]",Els).

  public mkCons:(string,cons[data])=>data.
  mkCons(Nm,Args) => .term(Nm,Args).

  public isScalar:(data)=>boolean.
  isScalar(D) => case D in {
    | .intgr(_) => .true
    | .bigi(_) => .true
    | .flot(_) => .true
    | .chr(_) => .true
    | .strg(_) => .true
    | .symb(_) => .true
    | _ default => .false
  }

  public implementation coercion[data,string] => {
    _coerce(T) => .some(_implode(reverse(encodeT(T,[])))).
  }

  public trueEnum:data.
  trueEnum = .term("true",[]).

  public falseEnum:data.
  falseEnum = .term("false",[]).

  -- Written in this way to maximize potential for tail recursion
  encodeT:(data,cons[char])=>cons[char].
  encodeT(D,Chs) => case D in {
    | .intgr(Ix) => encodeInt(Ix,[`x`,..Chs])
    | .bigi(Ix) => encodeBig(Ix,[`b`,..Chs])
    | .flot(Dx) => encodeText(Dx::string,[`d`,..Chs])
    | .chr(Cx) => encodeChar(Cx,[`c`,..Chs])
    | .strg(Tx) => encodeText(Tx,[`s`,..Chs])
    | .symb(Sym) => encodeL(Sym,Chs)
    | .clos(Lb,F,T) => encodeT(encodeSig(T),encodeT(F,encodeL(Lb,[`p`,..Chs])))
    | .term("[]",Els) => encodeTerms(Els,encodeNat(size(Els),[`l`,..Chs]))
    | .term(Op,Args) =>
      encodeTerms(Args,encodeL(.tLbl(Op,size(Args)),encodeNat(size(Args),[`n`,..Chs])))
  }

  encodeL:(termLbl,cons[char])=>cons[char].
  encodeL(.tLbl(Nm,Ar),Chs) => encodeText(Nm,encodeNat(Ar,[`o`,..Chs])).

  encodeTerms([],Chs) => Chs.
  encodeTerms([T,..Ts],Chs) => encodeTerms(Ts,encodeT(T,Chs)).

  encodeBig(Bx,Chs) => encodeText(Bx::string,Chs).

  public implementation coercion[string,data] => {
    _coerce(S) => valof{
      (T,_) = decodeTerm(S::cons[char]);
      valis .some(T)
    }
  }
  
  public decodeTerm:(cons[char])=>(data,cons[char]).
  decodeTerm([Ch,..Ls]) => case Ch in {
    | `x` => valof{
      (Ix,L0) = decodeInt(Ls);
      valis (.intgr(Ix),L0)
    }
    | `d` => valof{
      (Txt,Lx) = decodeText(Ls);
      valis (.flot(Txt::float),Lx)
    }
    | `e` => valof{
      (Sym,Lx) = decodeText(Ls);
      valis (.term(Sym,[]),Lx)
    }
    | `o` => valof{
      (Sym,Lx) = decodeLabel([`o`,..Ls]);
      valis (.symb(Sym),Lx)
    }
    | `c` => valof{
      (Chr,Lx) = decodeChar(Ls);
      valis (.chr(Chr),Lx)
    }
    | `s` => valof{
      (Txt,Lx) = decodeText(Ls);
      valis (.strg(Txt),Lx)
    }
    | `p` => valof{
      (Op,L0) = decodeLabel(Ls);
      (Fr,L1) = decodeTerm(L0);
      (Tp,Lx) = decodeType(L1);
      valis (.clos(Op,Fr,Tp),Lx)
    }
    | `n` => valof{
      (Ax,L0) = decodeNat(Ls,0);
      (.tLbl(Op,_),LL1) = decodeLabel(L0);
      (Args,Lx) = decodeTerms(LL1,Ax,[]);
      valis (.term(Op,Args),Lx)
    }
    | `l` => valof{
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
    valis (.tLbl(Nm,Ar),Lx)
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

  public decodeSig:(data) => tipe.
  decodeSig(.strg(Sig)) => decodeSignature(Sig).

  encodeText:(string,cons[char]) => cons[char].
  encodeText(Txt,Chs) where Chrs .= Txt::cons[char] &&
      D.=findDelim(Chrs,[`|`,`/`,`%`]) =>
    encodeQuoted(Chrs,D,[D,..Chs]).

  encodeChar:(char,cons[char]) => cons[char].
  encodeChar(`\\`,Chs) => [`\\`,`\\`,..Chs].
  encodeChar(`|`,Chs) => [`|`,`\\`,..Chs]. -- quote delimiter chars
  encodeChar(`/`,Chs) => [`/`,`\\`,..Chs].
  encodeChar(`%`,Chs) => [`%`,`\\`,..Chs].
  encodeChar(`'`,Chs) => [`'`,`\\`,..Chs].
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
  encodeNat(D,Cs) => (try
    let{.
      encNat(Dx,Chs) where Dx>=0 && Dx=<9 =>
	[digitChar(Dx),..Chs].
      encNat(Dx,Chs) => [digitChar(Dx%10),..encNat(Dx/10,Chs)]
    .} in encNat(D,Cs)
    catch exception in { _ => Cs}
  ).

  encodeInt:(integer,cons[char])=>cons[char].
  encodeInt(Ix,Chs) where Ix<0 => encodeNat(-Ix,[`-`,..Chs]).
  encodeInt(Ix,Chs) => encodeNat(Ix,Chs).

  public implementation coercion[locn,data]=>{
    _coerce(.locn(Pkg,Line,Col,Off,Ln))=>.some(mkTpl([.strg(Pkg),.intgr(Line),.intgr(Col),.intgr(Off),.intgr(Ln)])).
  }

  public implementation coercion[data,locn]=>{
    _coerce(.term("()5",[.strg(P),.intgr(L),.intgr(C),.intgr(O),.intgr(N)])) =>
      .some(.locn(P,L,C,O,N)).
  }

  public implementation all e ~~ coercion[e,data] |: coercion[option[e],data] => {
    _coerce(.none) => .some(.symb(.tLbl("none",0))).
    _coerce(.some(E)) => .some(mkCons("some",[E::data]))
  }

  public implementation all e ~~ coercion[data,e] |: coercion[data,option[e]] => {
    _coerce(.symb(.tLbl("none",0))) => .some(.none).
    _coerce(.term("some",[T])) => .some(.some(T::e))
  }

  public encodeSig:(tipe)=>data.
  encodeSig(Tp) => .strg(encodeSignature(Tp)).

  public implementation coercion[tipe,data] => {
    _coerce(T) => .some(encodeSig(T))
  }

  public implementation coercion[decl,data] => {
    _coerce(D) => .some(case D in {
	| .implDec(_,ConNm,ImplNm,Tp) =>
	  mkCons("imp",[.strg(ConNm),.strg(ImplNm),encodeSig(Tp)])
	| .accDec(_,Tp,Fld,Fn,AccTp) =>
	  mkCons("acc",[encodeSig(Tp),.strg(Fld),.strg(Fn),encodeSig(AccTp)])
	| .updDec(_,Tp,Fld,Fn,AccTp) =>
	  mkCons("upd",[encodeSig(Tp),.strg(Fld),.strg(Fn),encodeSig(AccTp)])
	| .conDec(_,Nm,FullNm,TpRl) =>
	  mkCons("con",[.strg(Nm),.strg(FullNm),.strg(encodeTpRlSignature(TpRl))])
	| .tpeDec(_,Nm,Tp,TpRl) =>
	  mkCons("tpe",[.strg(Nm),encodeSig(Tp),.strg(encodeTpRlSignature(TpRl))])
	| .cnsDec(_,Nm,FullNm,Tp) =>
	  mkCons("cns",[.strg(Nm),.strg(FullNm),encodeSig(Tp)])
	| .varDec(_,Nm,FullNm,Tp) =>
	  mkCons("var",[.strg(Nm),.strg(FullNm),encodeSig(Tp)])
	| .funDec(_,Nm,FullNm,Tp) =>
	  mkCons("fun",[.strg(Nm),.strg(FullNm),encodeSig(Tp)])
      }).
  }

  public pkgTerm:(pkg)=>data.
  pkgTerm(.pkg(Pk,Ver))=>mkCons("pkg",[.strg(Pk),versTerm(Ver)]).

  public versTerm:(version)=>data.
  versTerm(.defltVersion) => .symb(.tLbl("*",0)).
  versTerm(.vers("*")) => .symb(.tLbl("*",0)).
  versTerm(.vers(V)) => .strg(V).
}
