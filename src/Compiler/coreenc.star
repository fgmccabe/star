star.compiler.term.enc{
  import star.

  import star.compiler.term.
  import star.compiler.location.
  import star.compiler.data.
  import star.compiler.ltipe.

  import star.multi.

  public implementation coercion[crDefn,multi[integer]] => {
    _coerce(D) => some(encDef(D))
  }

  encDef:(crDefn) => multi[integer].
  encDef(fnDef(Lc,Nm,Tp,Args,Rep)) =>
    [0cF]++encText(Nm)++((Tp::ltipe)::multi[integer])++
    encExp(mkCrTpl(Lc,Args//(V)=>idnt(Lc,V)))++encExp(Rep).
  encDef(vrDef(Lc,Nm,Tp,Rep)) =>
    [0cG]++encText(Nm)++((Tp::ltipe)::multi[integer])++
    encExp(Rep).
  encDef(rcDef(Lc,Nm,Tp,Fs)) =>
    [0cR]++encText(Nm)++(Tp::ltipe)::multi[integer]++
    encInt(size(Fs))++
    multi((Fs//((Nm,FTp))=>encText(Nm)++encTp(FTp))).

  encText:(string) => multi[integer].
  encText(Txt) where Chrs .= Txt::cons[integer] =>
    encInt(size(Chrs))++multi(Chrs//(Ch)=>single(Ch)).

  encNat:(integer) => multi[integer].
  encNat(Dx) where Dx>=0 && Dx=<127 =>
    [Dx].
  encNat(Dx) => [(Dx%128).|.128]++encNat(Dx.>>.7).

  encInt:(integer)=>multi[integer].
  encInt(Ix) where Ix<0 => [0c-]++encNat(-Ix).
  encInt(Ix) => encNat(Ix).

  public implementation coercion[crExp,multi[integer]] => {
    _coerce(E) => some(encExp(E))
  }

  encExp:(crExp) => multi[integer].
  encExp(idnt(_,crId(Nm,Tp))) => [0cv]++encText(Nm)++Tp::multi[integer].
  encExp(intgr(_,Ix)) => [0ci]++encInt(Ix).
  encExp(chr(_,Cx)) => [0cc]++encInt(Cx::integer).
  encExp(flot(_,Dx)) => [0cf]++encInt(_float_bits(Dx)).
  encExp(bigx(_,Bx)) where Sx.=disp(Bx) => [0cb]++Sx.
  encExp(strg(_,Sx)) => [0cs]++encText(Sx).
  encExp(crVoid(_,Tp)) => [0cV]++encTp(Tp).
  encExp(crTerm(_,Op,As)) =>
    [0cT]++encText(Op)++encInt(size(As))++multi(As//encExp).
  encExp(crCall(_,Op,As,Tp)) =>
    [0cC]++encText(Op)++encExp(mkCrTpl(As))++encTp(Tp).
  encExp(crECall(_,Op,As,Tp)) =>
    [0cE]++encText(Op)++encExp(mkCrTpl(As))++encTp(Tp).
  encExp(crOCall(_,Op,As,Tp)) =>
    [0cO]++encExp(Op)++encExp(mkCrTpl(As))++encTp(Tp).
  encExp(crTplOff(_,O,Ix,Tp)) => [0cx]++encExp(O)++encInt(Ix)++encTp(Tp).
  encExp(crTplUpdate(_,O,Ix,E)) =>
    [0cU]++encExp(O)++encInt(Ix)++encExp(E).
  encExp(crLtt(Lc,V,T,D,I)) =>
    [0cL]++encLc(Lc)++encExp(idnt(V,T))++encExp(D)++encExp(I).
  encExp(crLtRec(Lc,V,T,D,I)) =>
    [0cR]++encLc(Lc)++encExp(idnt(V,T))++encExp(D)++encExp(I).
  encExp(crCase(Lc,E,Cs,Dflt,Tp)) =>
    [0cC]++encLc(Lc)++encExp(E)++encCases(Cs)++encExp(Dflt)++encTp(Tp).
  encExp(crMatch(Lc,P,E)) => [0c=]++encLc(Lc)++encExp(P)++encExp(E).
  encExp(crWhere(Lc,T,C)) => [0cw]++encLc(Lc)++encExp(T)++encExp(C).
  encExp(crCnj(Lc,L,R)) => [0c&]++encLc(Lc)++encExp(L)++encExp(R).
  encExp(crDsj(Lc,L,R)) => [0c|]++encLc(Lc)++encExp(L)++encExp(R).
  encExp(crCnd(Lc,T,L,R)) =>[0c?]++encLc(Lc)++encExp(T)++encExp(L)++encExp(R).
  encExp(crNeg(Lc,R)) => [0c~]++encLc(Lc)++encExp(R).
  encExp(crAbort(Lc,Msg,Tp)) => [0cA]++encLc(Lc)++encText(Msg)++encTp(Tp).

  encTpl:(cons[crExp])=>multi[integer].
  encTpl(Els)=>encInt(size(Els))++multi(Els//encExp).

  encLc:(locn)=>multi[integer].
  encLc(Lc) => encExp(Lc::crExp).

  encCases(Cs) => encInt(size(Cs))++
  multi(Cs//((Lc,P,V))=>
      encLc(Lc)++encExp(P)++encExp(V)).

  encFields(Fs) =>
    encInt(size(Fs))++
    multi((Fs//((Nm,Vl))=>encText(Nm)++encExp(Vl))).


  decTp:(cons[integer])=>option[(ltipe,cons[integer])].
  decTp([0ci,..Cs]) => some((.int64,Cs)).
  decTp([0cf,..Cs]) => some((.flt64,Cs)).
  decTp([0cl,..Cs]) => some((.bool,Cs)).
  decTp([0cp,..Cs]) => some((.ptr,Cs)).
  decTp([0cT,..Cs]) where (Ar,C0)^=decInt(Cs) => let{.
    lp(0,Chs,So) => some((tplTipe(reverse(So)),Chs)).
    lp(Cx,Chs,So) where (El,C0)^=decTp(Chs) =>
      lp(Cx-1,C0,[El,..So])
  } in lp(Ar,C0,[]).
  decTp([0cF,..Cs]) where (tplTipe(As),C0)^=decTp(Cs) && (Rt,Cx) ^= decTp(C0) =>
    some((funTipe(As,Rt),Cx)).

  decInt:(cons[integer])=>option[(integer,cons[integer])].
  decInt([0c-,..Chrs]) where (N,Rst)^=decNat(Chrs,0) =>
    some((-N,Rst)).
  decInt(Chrs) => decNat(Chrs,0).

  decNat([Ch,..Rst],Acc) where Ch>=0 && Ch=<127 =>
    some(((Acc.<<.7.|.Ch),Rst)).
  decNat([Ch,..Rst],Acc) where Ch>=128 =>
    decNat(Rst,Acc.<<.7.|.(Ch.&.127)).
  decNat(_,_) default => .none.

  decText:all e ~~ (cons[integer],(string)=>e)=>option[(e,cons[integer])].
  decText(Cs,F) where (Lx,C0)^=decInt(Cs) &&
      (Nm,Cx)^=front(C0,Lx) =>
    some((F(Nm::string),Cx)).
  decText(_,_) default => .none.

  decExp:(cons[integer])=>option[(crExp,cons[integer])].
  decExp([0cv,..Cs]) where (Nm,C0)^=decText(Cs,id) && (Tp,Cx)^=decTp(C0) =>
    some((idnt(Nm,Tp),Cx)).
  decExp([0ci,..Cs]) where (Ix,Cx)^=decInt(Cs) => some((intgr(Ix),Cx)).
  decExp([0cf,..Cs]) where (Bx,Cx)^=decInt(Cs) =>
    some((flot(_bits_float(Bx)),Cx)).
  decExp([0cs,..Cs]) where (Sx,Cx)^=decText(Cs,(S)=>strg(S)) => some((Sx,Cx)).
  decExp([0cV,..Cs]) where (Tp,Cx)^=decTp(Cs) => some((crVoid(Tp),Cx)).
  decExp([0cE,..Cs]) where (Lc,C0)^=decLc(Cs) &&
      (Op,C1)^=decText(C0,id) &&
      (crTerm(_,As),Cx)^=decExp(C1) &&
      (Tp,Cx) ^= decTp(C1) => some((crECall(Lc,Op,As,Tp),Cx)).
  decExp([0cO,..Cs]) where (Lc,C0)^=decLc(Cs) &&
      (Op,C1)^=decExp(C0) &&
      (crTerm(_,As),Cx)^=decExp(C1) &&
      (Tp,Cx) ^= decTp(C1) => some((crOCall(Lc,Op,As,Tp),Cx)).
  decExp([0cC,..Cs]) where (Lc,C0)^=decLc(Cs) &&
      (Op,C1)^=decText(C0,id) &&
      (crTerm(_,As),Cx)^=decExp(C1) &&
      (Tp,Cx) ^= decTp(C1) => some((crCall(Lc,Op,As,Tp),Cx)).
  decExp([0cT,..Cs]) where
      (Op,C0)^=decText(Cs,id) &&
      (Ar,C1)^=decInt(C0) &&
      (As,Cx)^=decExps(Ar,[],C1) =>some((crTerm(Op,As),Cx)).
  decExp([0c.,..Cs]) where
      (O,C0)^=decExp(Cs)&&
      (Fld,C1)^=decText(C0,id) &&
      (Tp,Cx) ^= decTp(C1) => some((crDot(O,Fld,Tp),Cx)).
  decExp([0cx,..Cs]) where
      (O,C0)^=decExp(Cs)&&
      (Ix,C1)^=decInt(C0) &&
      (Tp,Cx) ^= decTp(C1) => some((crTplOff(O,Ix,Tp),Cx)).
  decExp([0cU,..Cs]) where
      (Lc,C0) ^= decLc(Cs) &&
      (O,C1)^=decExp(C0)&&
      (Ix,C2)^=decInt(C0) &&
      (E,Cx) ^= decExp(C2) => some((crTplUpdate(Lc,O,Ix,E),Cx)).
  decExp([0cr,..Cs]) where 
      (Nm,C0)^=decText(Cs,id) &&
      (Ar,C1)^=decInt(C0) &&
      (Fs,Cx)^=decFields(Ar,[],C1) =>some((crRecord(Nm,Fs),Cx)).


  decExps(0,So,Cx) => some((reverse(So),Cx)).
  decExps(Ar,So,Cs) where (A,C0)^=decExp(Cs) => decExps(Ar-1,[A,..So],C0).

  decFields(0,So,Cx) => some((So,Cx)).
  decFields(Ar,So,Cs) where (F,C0) ^= decText(Cs,id) && (A,C1)^=decExp(C0) =>
    decFields(Ar-1,[(F,A),..So],C1).

  decLc:(cons[integer])=>option[(locn,cons[integer])].
  decLc(Cs) where (E,Cx)^=decExp(Cs) => some((E::locn,Cx)).
}

