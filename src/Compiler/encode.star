star.compiler.encode{
  import star.
  import star.multi.
  import star.sort.

  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.data.
  import star.pkg.
  import star.compiler.types.

  public implementation coercion[data,string] => {
    _coerce(T) => .some((encData(T)::cons[char])::string).
  }

  encData:(data)=>multi[char].
  encData(D) => case D in {
    | .intgr(Ix) => [`x`,..encodeInt(Ix)]
    | .bigi(Bx) => [`b`,..encodeBig(Bx)]
    | .flot(Dx) => [`d`,..encodeText(Dx::string)]
    | .chr(Cx) => [`c`,..encodeChar(Cx,`\\`)]
    | .strg(Tx) => [`s`,..encodeText(Tx)]
    | .symb(Sym) => encodeL(Sym)
    | .clos(Lb,F,T) => [`p`]++encodeL(Lb)++encData(F)++encData(encodeSig(T))
    | .term("[]",Els) => [`l`]++encodeNat(size(Els))++encodeTerms(Els)
    | .term(Op,Args) => [`n`]++encodeNat(size(Args))++encodeL(.tLbl(Op,size(Args)))++encodeTerms(Args)
  }

  encodeL:(termLbl)=>multi[char].
  encodeL(.tLbl(Nm,Ar)) => [`o`]++encodeNat(Ar)++encodeText(Nm).

  encodeTerms([]) => [].
  encodeTerms([T,..Ts]) => encData(T)++encodeTerms(Ts).

  encodeBig(Bx) => encodeText(Bx::string).

  public encodeSignature:(tipe) => string.
  encodeSignature(Tp) => (encodeType(deRef(Tp))::cons[char])::string.
  
  public encodeTpRlSignature:(typeRule) => string.
  encodeTpRlSignature(Rl) => (encodeTypeRule(Rl)::cons[char])::string.

  encodeType:(tipe) => multi[char].
  encodeType(Tp) => isUnbound(Tp) ?? [`_`] ||
  case deRef(Tp) in {
    | .voidType => [`v`]
    | .anonType => [`_`]
    | .nomnal("integer") => [`i`]
    | .nomnal("bigint") => [`b`]
    | .nomnal("float") => [`f`]
    | .nomnal("string") => [`s`]
    | .nomnal("boolean") => [`l`]
    | .nomnal(Nm) => [`t`]++encodeText(Nm)
    | .kVar(Nm) => [`k`]++encodeText(Nm)
    | .kFun(Nm,Ar) => [`K`]++encodeNat(Ar)++encodeText(Nm)
    | .tpFun(Nm,Ar) => [`z`]++encodeNat(Ar)++encodeText(Nm)
    | .tpExp(.tpFun("cons",1),El) => [`L`]++encodeType(El)
    | .tpExp(.tpFun("ref",1),El) => [`r`]++encodeType(El)
    | .tpExp(.tpExp(.tpFun("=>",2),A),R) => [`F`]++encodeType(A)++encodeType(R)
    | .tpExp(.tpExp(.tpExp(.tpFun("=>",3),A),R),E) => [`T`]++encodeType(A)++encodeType(R)++encodeType(E)
    | .tpExp(.tpExp(.tpFun("<=>",2),A),R) => [`C`]++encodeType(A)++encodeType(R)
    | .tpExp(Op,A) => [`U`]++encodeType(Op)++encodeType(A)
    | .tupleType(Els) => [`(`]++encodeTypes(Els)++[`)`]
    | .allType(V,T) => [`:`]++encodeType(V)++encodeType(T)
    | .existType(V,T) => [`e`]++encodeType(V)++encodeType(T)
    | .constrainedType(T,C) => [`|`]++encodeType(T)++encodeConstraint(C)
    | .faceType(Flds,Tps) => [`I`]++encodeFldTypes(Flds)++encodeTypeRules(Tps)
  }.

  encodeTypes:(cons[tipe])=>multi[char].
  encodeTypes([]) => [].
  encodeTypes([T,..Tps]) =>
    encodeType(T)++encodeTypes(Tps).

  encodeFldTypes:(cons[(string,tipe)])=>multi[char].
  encodeFldTypes(Flds) =>
    [`{`]++encodeFlds(sort(Flds,((N1,_),(N2,_))=>N1<N2))++[`}`].
  
  encodeFlds:(cons[(string,tipe)])=>multi[char].
  encodeFlds([]) => [].
  encodeFlds([(Nm,T),..Tps]) =>
    encodeText(Nm)++encodeType(T)++encodeFlds(Tps).

  encodeConstraint(.conTract(Nm,Ts,Ds)) =>
    [`c`]++encodeText(Nm)++encodeType(.tupleType(Ts))++
    encodeType(.tupleType(Ds)).
  encodeConstraint(.hasField(V,F,T)) =>
    [`a`]++encodeType(V)++encodeType(.faceType([(F,deRef(T))],[])).
  encodeConstraint(.implicit(Nm,T)) =>
    [`d`]++encodeText(Nm)++encodeType(T).

  encodeTypeRule:(typeRule)=>multi[char].
  encodeTypeRule(.allRule(V,R)) =>
    [`:`]++encodeType(V)++encodeTypeRule(R).
  encodeTypeRule(.typeExists(H,I)) =>
    [`Y`]++encodeType(H)++encodeType(I).
  encodeTypeRule(.contractExists(N,T,D,I)) =>
    [`Z`]++encodeConstraint(.conTract(N,T,D))++encodeType(I).
  encodeTypeRule(.typeLambda(Hd,I)) =>
    [`y`]++encodeType(Hd)++encodeType(I).

  encodeTypeRules(Rls) => [`[`,..encodeRls(Rls)]++[`]`].

  encodeRls([]) => [].
  encodeRls([(Id,Rl),..Rls]) =>
    encodeText(Id)++encodeTypeRule(Rl)++encodeRls(Rls).
  
  encodeText:(string) => multi[char].
  encodeText(Txt) where Chrs .= Txt::cons[char] &&
      D.=findDelim(Chrs,[`|`,`/`,`%`,`'`]) =>
    [D]++encodeQuoted(Chrs,D)++[D].

  findDelim:(cons[char],cons[char])=>char.
  findDelim(Chrs,[]) => `'`. 
  findDelim(Chrs,[D,..Ds]) where {? D in Chrs ?} => findDelim(Chrs,Ds).
  findDelim(Chrs,[D,.._]) => D.

  encodeQuoted:(cons[char],char)=>multi[char].
  encodeQuoted([],D) => [].
  encodeQuoted([D,..Cs],D) => [`\\`,D,..encodeQuoted(Cs,D)].
  encodeQuoted([`\\`,..Cs],D) => [`\\`,`\\`,..encodeQuoted(Cs,D)].
  encodeQuoted([C,..Cs],D) => [C,..encodeQuoted(Cs,D)].

  encodeChar:(char,char)=>multi[char].
  encodeChar(`\\`,_) => [`\\`,`\\`].
  encodeChar(D,D) => [`\\`,D].
  encodeChar(C,_) => [C].

  public encodeInt:(integer)=>multi[char].
  encodeInt(Ix) where Ix<0 => [`-`,..encodeNat(-Ix)].
  encodeInt(Ix) => encodeNat(Ix).

  public encodeNat:(integer) => multi[char].
  encodeNat(D) => (try
    let{.
      encNat:(integer) => multi[char] throws exception.
      encNat(Dx) where Dx>=0 && Dx=<9 =>
	[digitChar(Dx)].
      encNat(Dx) => encNat(Dx/10)++[digitChar(Dx%10)]
    .} in encNat(D)
    catch { _ => []}
  ).

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
	| .tpeDec(_,Nm,Tp,TpRl,IxMap) =>
	  mkCons("tpe",[.strg(Nm),encodeSig(Tp),.strg(encodeTpRlSignature(TpRl)),encodeMap(IxMap)])
	| .cnsDec(_,Nm,FullNm,Tp) =>
	  mkCons("cns",[.strg(Nm),.strg(FullNm),encodeSig(Tp)])
	| .varDec(_,Nm,FullNm,Tp) =>
	  mkCons("var",[.strg(Nm),.strg(FullNm),encodeSig(Tp)])
	| .funDec(_,Nm,FullNm,Tp) =>
	  mkCons("fun",[.strg(Nm),.strg(FullNm),encodeSig(Tp)])
      }).
  }

  encodeMap:(indexMap)=>data.
  encodeMap(Map) => mkTpl(ixRight((Lbl,Ix,Mp)=>
	[mkTpl([.symb(Lbl),.intgr(Ix)]),..Mp],[],Map)).

  public pkgTerm:(pkg)=>data.
  pkgTerm(.pkg(Pk,Ver))=>mkCons("pkg",[.strg(Pk),versTerm(Ver)]).

  public versTerm:(version)=>data.
  versTerm(.defltVersion) => .symb(.tLbl("*",0)).
  versTerm(.vers("*")) => .symb(.tLbl("*",0)).
  versTerm(.vers(V)) => .strg(V).  
}
