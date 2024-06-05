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
    | .kVar(Nm) => [`k`]++encodeText(Nm)
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

  public encodeNat:(integer) => multi[char].
  encodeNat(D) => (try
    let{.
      encNat(Dx) where Dx>=0 && Dx=<9 =>
	[digitChar(Dx)].
      encNat(Dx) => encNat(Dx/10)++[digitChar(Dx%10)]
    .} in encNat(D)
    catch exception in { _ => []}
  ).

  public decodeSignature:(string) => tipe.
  decodeSignature(St) => valof{
    (Tp,_) = decodeType(St::cons[char]);
    valis Tp
  }

  decodeType:(cons[char]) => (tipe,cons[char]).
  decodeType([Ch,..Ts]) => case Ch in {
    | `i` => (.nomnal("star.core*integer"),Ts)
    | `b` => (.nomnal("star.core*bigint"),Ts)
    | `f` => (.nomnal("star.core*float"),Ts)
    | `c` => (.nomnal("star.core*char"),Ts)
    | `s` => (.nomnal("star.core*string"),Ts)
    | `l` => (.nomnal("star.core*boolean"),Ts)
    | `_` => (.anonType,Ts)
    | `k` => valof{
      (Nm,T1) = decodeText(Ts);
      valis (.kVar(Nm),T1)
    }
    | `K` => valof{
      (Ar,T0) = decodeNat(Ts,0);
      (Nm,T1) = decodeText(T0);
      valis (.kFun(Nm,Ar),T1)
    }
    | `t` => valof{
      (Nm,T1) = decodeText(Ts);
      valis (.nomnal(Nm),T1)
    }
    | `z` => valof{
      (Ar,T0) = decodeNat(Ts,0);
      (Nm,T1) = decodeText(T0);
      valis (.tpFun(Nm,Ar),T1)
    }
    | `L` => valof{
      (ElTp,T0) = decodeType(Ts);
      valis (lstType(ElTp),T0)
    }
    | `U` => valof{
      (OpTp,T0) = decodeType(Ts);
      (ElTp,T1) = decodeType(T0);
      valis (.tpExp(OpTp,ElTp),T1)
    }
    | `r` => valof{
      (ElTp,T0) = decodeType(Ts);
      valis (refType(ElTp),T0)
    }
    | `(` => valof{
      (Tps,T0) = decodeTypes(Ts);
      valis (.tupleType(Tps),T0)
    }
    | `:` => valof{
      (V,T0) = decodeType(Ts);
      (B,T1) = decodeType(T0);
      valis (.allType(V,B),T1)
    }
    | `e` => valof{
      (V,T0) = decodeType(Ts);
      (B,T1) = decodeType(T0);
      valis (.existType(V,B),T1)
    }
    | `|` => valof{
      (V,T0) = decodeType(Ts);
      (B,T1) = decodeConstraint(T0);
      valis (.constrainedType(V,B),T1)
    }
    | `I` => valof{
      (F1,T0) = decodeFields(Ts);
      (F2,T1) = decodeTypeRules(T0);
      valis (.faceType(F1,F2),T1)
    }
    | `F` => valof{
      (A,T0) = decodeType(Ts);
      (R,T1) = decodeType(T0);
      valis (fnType(A,R),T1)
    }
    | `C` => valof{
      (A,T0) = decodeType(Ts);
      (R,T1) = decodeType(T0);
      valis (consType(A,R),T1)
    }
  }

  decodeTypes:(cons[char])=> (cons[tipe],cons[char]).
  decodeTypes([`)`,..Ts]) => ([],Ts). 
  decodeTypes(Ts) => valof{
    (ElTp,T0) = decodeType(Ts);
    (Tps,T1) = decodeTypes(T0);
    valis ([ElTp,..Tps],T1)
  }

  public decodeTypeRuleSignature:(string) => typeRule.
  decodeTypeRuleSignature(St) => valof{
    (Tp,_) = decodeTypeRule(St::cons[char]);
    valis Tp
  }

  decodeTypeRule:(cons[char])=>(typeRule,cons[char]).
  decodeTypeRule([`:`,..Ts]) => valof{
    (V,T0) = decodeType(Ts);
    (R,T1) = decodeTypeRule(T0);
    valis (.allRule(V,R),T1)
  }  
  decodeTypeRule([`Y`,..Ts]) => valof{
    (A,T0) = decodeType(Ts);
    (R,T1) = decodeType(T0);
    valis (.typeExists(A,R),T1)
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

  decodeTypeRules:(cons[char])=>(cons[(string,typeRule)],cons[char]).
  decodeTypeRules([`[`,..Ts]) => decodeRls(Ts,[]).

  decodeRls:(cons[char],cons[(string,typeRule)])=>
    (cons[(string,typeRule)],cons[char]).
  decodeRls([`]`,..Ts],Rls) => (reverse(Rls),Ts).
  decodeRls(Ts,Rls) => valof{
    (Nm,T0) = decodeText(Ts);
    (Rl,T1) = decodeTypeRule(T0);
    valis decodeRls(T1,[(Nm,Rl),..Rls])
  }

  decodeFields:(cons[char])=>(cons[(string,tipe)],cons[char]).
  decodeFields([`{`,..Ts]) => decodeFlds(Ts,[]).

  decodeFlds:(cons[char],cons[(string,tipe)])=>
    (cons[(string,tipe)],cons[char]).
  decodeFlds([`}`,..Ts],Flds) => (reverse(Flds),Ts).
  decodeFlds(Ts,Flds) => valof{
    (Nm,T0) = decodeText(Ts);
    (Tp,T1) = decodeType(T0);
    valis decodeFlds(T1,[(Nm,Tp),..Flds])
  }

  decodeConstraint:(cons[char])=>(constraint,cons[char]).
  decodeConstraint([`c`,..T]) => valof{
    (Nm,T0) = decodeText(T);
    (.tupleType(Tps),T1) = decodeType(T0);
    (.tupleType(Dps),T2) = decodeType(T1);
    valis (.conTract(Nm,Tps,Dps),T2)
  }
  decodeConstraint([`a`,..T]) => valof{
    (BT,T0) = decodeType(T);
    if (.faceType([(Fld,FT)],_),T1) .= decodeType(T0) then
      valis (.hasField(BT,Fld,FT),T1)
    else
    valis (.hasField(BT,"",.voidType),T0)
  }
  decodeConstraint([`d`,..T]) => valof{
    (Nm,T0) = decodeText(T);
    (FT,T1) = decodeType(T0);
    valis (.implicit(Nm,FT),T1)
  }
  decodeConstraint([`r`,..T]) => valof{
    (FT,T1) = decodeType(T);
    valis (.raisEs(FT),T1)
  }

  decodeText:(cons[char]) => (string,cons[char]).
  decodeText([C,..L]) => valof{
    (Q,Cs) = collectQuoted(L,[],C);
    valis (reverse(Q)::string,Cs)
  }

  collectQuoted:(cons[char],cons[char],char) => (cons[char],cons[char]).
  collectQuoted([S,..Lx],SoF,S) => (SoF,Lx).
  collectQuoted([`\\`,X,..L],SoF,S) => collectQuoted(L,[X,..SoF],S).
  collectQuoted([X,..L],SoF,S) => collectQuoted(L,[X,..SoF],S).

  decodeNat:(cons[char],integer) => (integer,cons[char]).
  decodeNat([Cx,..Ls],Ix) where isDigit(Cx) => decodeNat(Ls,Ix*10+digitVal(Cx)).
  decodeNat(Ls,Ix) default => (Ix,Ls).
}
