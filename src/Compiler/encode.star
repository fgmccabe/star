star.compiler.types.encode{
  import star.
  import star.multi.
  import star.sort.

  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.types.

  public encodeSignature:(tipe) => string.
  encodeSignature(Tp) => (encodeType(deRef(Tp))::cons[char])::string.
  
  public encodeTpRlSignature:(typeRule) => string.
  encodeTpRlSignature(Rl) => (encodeTypeRule(Rl)::cons[char])::string.

  encodeType:(tipe) => multi[char].
  encodeType(Tp) => isUnbound(Tp) ?? [`_`] ||
  case deRef(Tp) in {
    | .voidType => [`v`]
    | .anonType => [`_`]
    | .nomnal("star.core*integer") => [`i`]
    | .nomnal("star.core*bigint") => [`b`]
    | .nomnal("star.core*float") => [`f`]
    | .nomnal("star.core*string") => [`s`]
    | .nomnal("star.core*boolean") => [`l`]
    | .nomnal(Nm) => [`t`]++encodeText(Nm)
    | .kFun(Nm,Ar) => [`K`]++encodeNat(Ar)++encodeText(Nm)
    | .tpFun(Nm,Ar) => [`z`]++encodeNat(Ar)++encodeText(Nm)
    | .tpExp(.tpFun("star.core*cons",1),El) => [`L`]++encodeType(El)
    | .tpExp(.tpFun("ref",1),El) => [`r`]++encodeType(El)
    | .tpExp(.tpExp(.tpFun("=>",2),A),R) => [`F`]++encodeType(A)++encodeType(R)
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
  encodeConstraint(.raisEs(T)) =>
    [`r`]++encodeType(T).

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
      D.=findDelim(Chrs,[`|`,`/`,`%`,`+`]) =>
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

  encodeNat:(integer) => multi[char].
  encodeNat(D) => (try
    let{.
      encNat(Dx) where Dx>=0 && Dx=<9 =>
	[digitChar(Dx)].
      encNat(Dx) => encNat(Dx/10)++[digitChar(Dx%10)]
    .} in encNat(D)
    catch exception in { _ => []}
  ).
}
