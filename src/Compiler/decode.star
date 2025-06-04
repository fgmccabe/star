star.compiler.decode{
  import star.
  
  import star.compiler.types.
  import star.compiler.data.
  import star.compiler.meta.

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

  public decodeSig:(data) => tipe.
  decodeSig(.strg(Sig)) => decodeSignature(Sig).

  public decodeSignature:(string) => tipe.
  decodeSignature(St) => valof{
    (Tp,_) = decodeType(St::cons[char]);
    valis Tp
  }

  public decodeType:(cons[char]) => (tipe,cons[char]).
  decodeType([Ch,..Ts]) => case Ch in {
    | `i` => (.nomnal("integer"),Ts)
    | `b` => (.nomnal("bigint"),Ts)
    | `f` => (.nomnal("float"),Ts)
    | `c` => (.nomnal("char"),Ts)
    | `s` => (.nomnal("string"),Ts)
    | `l` => (.nomnal("boolean"),Ts)
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
    | `T` => valof{
      (A,T0) = decodeType(Ts);
      (R,T1) = decodeType(T0);
      (E,T2) = decodeType(T1);
      valis (throwingType(A,R,E),T2)
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

  decodeText:(cons[char]) => (string,cons[char]).
  decodeText([C,..L]) => valof{
    (Q,Cs) = collectQuoted(L,[],C);
    valis (reverse(Q)::string,Cs)
  }

  collectQuoted:(cons[char],cons[char],char) => (cons[char],cons[char]).
  collectQuoted([S,..Lx],SoF,S) => (SoF,Lx).
  collectQuoted([`\\`,X,..L],SoF,S) => collectQuoted(L,[X,..SoF],S).
  collectQuoted([X,..L],SoF,S) => collectQuoted(L,[X,..SoF],S).
}  
