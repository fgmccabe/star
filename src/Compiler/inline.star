star.compiler.inline{
  import star.
  import star.sort.

  import star.compiler.core.
  import star.compiler.freevars.
  import star.compiler.misc.
  import star.compiler.types.

  import star.compiler.location.

  public simplifyDefs:(cons[crDefn]) => cons[crDefn].
  simplifyDefs(Dfs) where Prog .= foldLeft(pickupDefn,[],Dfs) => (Dfs//(D)=>simplifyDefn(D,Prog)).

  pickupDefn:(crDefn,map[string,crDefn])=>map[string,crDefn].
  pickupDefn(fnDef(Lc,Nm,Tp,Args,Val),Map) => Map[Nm->fnDef(Lc,Nm,Tp,Args,Val)].
  pickupDefn(vrDef(Lc,Nm,Tp,Val),Map) => Map[Nm->vrDef(Lc,Nm,Tp,Val)].
  pickupDefn(tpDef(_,_,_,_),Map) => Map.
  pickupDefn(lblDef(_,_,_,_),Map) => Map.

  simplifyDefn:(crDefn,map[string,crDefn])=>crDefn.
  simplifyDefn(fnDef(Lc,Nm,Tp,Args,Val),Prog) =>
    fnDef(Lc,Nm,Tp,Args,simplifyExp(Val,Prog,3)).
  simplifyDefn(vrDef(Lc,Nm,Tp,Val),Prog) =>
    vrDef(Lc,Nm,Tp,simplifyExp(Val,Prog,3)).
  simplifyDefn(D,_) default => D.

  -- There are three possibilities of a match ...
  match[e] ::= .noMatch | .insufficient | matching(e).

  -- ptnMatch tries to match an actual value with a pattern
  
  ptnMatch:(crExp,crExp,map[string,crExp]) => match[map[string,crExp]].
  ptnMatch(E,crVar(Lc,crId(V,_)),Env) =>
    (T^=Env[V] ? ptnMatch(E,T,Env) || matching(Env[V->E])).
  ptnMatch(crVar(_,V1),_,_) => .insufficient.  -- variables on left only match vars on right
  ptnMatch(crInt(_,Ix),crInt(_,Ix),Env) => matching(Env).
  ptnMatch(crBig(_,Bx),crBig(_,Bx),Env) => matching(Env).
  ptnMatch(crFlot(_,Dx),crFlot(_,Dx),Env) => matching(Env).
  ptnMatch(crChr(_,Cx),crChr(_,Cx),Env) => matching(Env).
  ptnMatch(crStrg(_,Sx),crStrg(_,Sx),Env) => matching(Env).
  ptnMatch(crTerm(_,N,A1,_),crTerm(_,N,A2,_),Env) => ptnMatchArgs(A1,A2,Env).
  ptnMatch(crVoid(_,_),_,_) => .insufficient.  -- void on left does not match anything
  ptnMatch(_,crVoid(_,_),_) => .insufficient.  -- void on right does not match anything
  ptnMatch(_,_,_) default => .noMatch.

  ptnMatchArgs([],[],Env) => matching(Env).
  ptnMatchArgs([E1,..L1],[E2,..L2],Env) => case ptnMatch(E1,E2,Env) in {
    .noMatch => .noMatch.
    .insufficient => .insufficient.
    matching(Ev) => ptnMatchArgs(L1,L2,Ev)
  }
  ptnMatchArgs(_,_,_) default => .noMatch.

  simplifyExp:(crExp,map[string,crDefn],integer) => crExp.
  simplifyExp(E,P,D) => (D>0 ? simExp(E,P,D-1) || E).
  
  simExp(crVar(Lc,V),Map,Depth) => inlineVar(Lc,V,Map,Depth).
  simExp(crInt(Lc,Ix),_,_) => crInt(Lc,Ix).
  simExp(crBig(Lc,Bx),_,_) => crBig(Lc,Bx).
  simExp(crChr(Lc,Ix),_,_) => crChr(Lc,Ix).
  simExp(crFlot(Lc,Dx),_,_) => crFlot(Lc,Dx).
  simExp(crStrg(Lc,Sx),_,_) => crStrg(Lc,Sx).
  simExp(crVoid(Lc,Tp),_,_) => crVoid(Lc,Tp).
  simExp(crTerm(Lc,Fn,Args,Tp),Prog,Depth) =>
    crTerm(Lc,Fn,Args//(A)=>simplifyExp(A,Prog,Depth),Tp).
  simExp(crCall(Lc,Fn,Args,Tp),Prog,Depth) =>
    inlineCall(Lc,Fn,Args//(A)=>simplifyExp(A,Prog,Depth),Tp,Prog,Depth).
  simExp(crECall(Lc,Fn,Args,Tp),Prog,Depth) =>
    inlineECall(Lc,Fn,Args//(A)=>simplifyExp(A,Prog,Depth),Tp,Depth).
  simExp(crOCall(Lc,Op,Args,Tp),Prog,Depth) =>
    inlineOCall(Lc,simplifyExp(Op,Prog,Depth),
      Args//(A)=>simplifyExp(A,Prog,Depth),Tp,Prog,Depth).
  simExp(crTplOff(Lc,T,Ix,Tp),Prog,Depth) =>
    inlineTplOff(Lc,simplifyExp(T,Prog,Depth),Ix,Tp).
  simExp(crTplUpdate(Lc,T,Ix,Vl),Prog,Depth) =>
    applyTplUpdate(Lc,simplifyExp(T,Prog,Depth),Ix,simplifyExp(Vl,Prog,Depth)).
  simExp(crSeq(Lc,L,R),Prog,Depth) =>
    crSeq(Lc,simplifyExp(L,Prog,Depth),simplifyExp(R,Prog,Depth)).
  simExp(crCnj(Lc,L,R),Prog,Depth) =>
    applyCnj(Lc,simplifyExp(L,Prog,Depth),simplifyExp(R,Prog,Depth)).
  simExp(crDsj(Lc,L,R),Prog,Depth) =>
    applyDsj(Lc,simplifyExp(L,Prog,Depth),simplifyExp(R,Prog,Depth)).
  simExp(crNeg(Lc,R),Prog,Depth) =>
    applyNeg(Lc,simplifyExp(R,Prog,Depth)).
  simExp(crCnd(Lc,T,L,R),Prog,Depth) =>
    applyCnd(Lc,simplifyExp(T,Prog,Depth),
      simplifyExp(L,Prog,Depth),simplifyExp(R,Prog,Depth)).
  simExp(crLtt(Lc,Vr,Bnd,Exp),Prog,Depth) =>
    inlineLtt(Lc,Vr,simplifyExp(Bnd,Prog,Depth),Exp,Prog,Depth).
  simExp(crUnpack(Lc,Gov,Cases,Tp),Prog,Depth) =>
    inlineUnpack(Lc,simplifyExp(Gov,Prog,Depth),Cases,Tp,Prog,Depth).
  simExp(crCase(Lc,Gov,Cases,Deflt,Tp),Prog,Depth) =>
    inlineCase(Lc,simplifyExp(Gov,Prog,Depth),Cases,
      simplifyExp(Deflt,Prog,Depth),Tp,Prog,Depth).
  simExp(crWhere(Lc,Ptn,Exp),Prog,Depth) =>
    applyWhere(Lc,Ptn,simplifyExp(Exp,Prog,Depth)).
  simExp(crMatch(Lc,Ptn,Exp),Prog,Depth) =>
    applyMatch(Lc,Ptn,simplifyExp(Exp,Prog,Depth)).
  simExp(crVarNames(Lc,Vrs,Exp),Prog,Depth) =>
    crVarNames(Lc,Vrs,simplifyExp(Exp,Prog,Depth)).
  simExp(crAbort(Lc,Txt,Tp),_,_) => crAbort(Lc,Txt,Tp).

  inlineVar(Lc,crId(Id,Tp),Map,_Depth) where
      vrDef(_,_,_,Vl) ^= Map[Id] && isGround(Vl) => Vl.
  inlineVar(Lc,V,_,_) => crVar(Lc,V).

  applyWhere(Lc,Ptn,crTerm(_,"star.core#true",[],_)) => Ptn.
  applyWhere(Lc,Ptn,Exp) => crWhere(Lc,Ptn,Exp).

  applyCnj(_,crTerm(_,"star.core#true",[],_),R) => R.
  applyCnj(_,crTerm(Lc,"star.core#false",[],Tp),R) => crTerm(Lc,"star.core#false",[],Tp).
  applyCnj(Lc,L,R) => crCnj(Lc,L,R).

  applyDsj(_,crTerm(_,"star.core#false",[],_),R) => R.
  applyDsj(_,crTerm(Lc,"star.core#true",[],Tp),R) => 
    crTerm(Lc,"star.core#false",[],Tp).
  applyDsj(Lc,L,R) => crDsj(Lc,L,R).

  applyNeg(_,crTerm(Lc,"star.core#false",[],Tp)) => crTerm(Lc,"star.core#true",[],Tp).
  applyNeg(_,crTerm(Lc,"star.core#true",[],Tp)) => crTerm(Lc,"star.core#false",[],Tp).
  applyNeg(Lc,Inner) => crNeg(Lc,Inner).

  applyCnd(_,crTerm(_,"star.core#false",[],_),L,R) => R.
  applyCnd(_,crTerm(Lc,"star.core#true",[],Tp),L,_) => L.
  applyCnd(Lc,T,L,R) => crCnd(Lc,T,L,R).

  inlineTplOff(_,crTerm(_,_,Els,_),Ix,Tp) where E^=Els[Ix] => E.
  inlineTplOff(Lc,T,Ix,Tp) default => crTplOff(Lc,T,Ix,Tp).
  
  applyTplUpdate(_,crTerm(Lc,Nm,Args,Tp),Ix,E) =>
    crTerm(Lc,Nm,Args[Ix->E],Tp).
  applyTplUpdate(Lc,T,Ix,E) =>
    crTplUpdate(Lc,T,Ix,E).

  applyMatch(Lc,Ptn,Exp) => crMatch(Lc,Ptn,Exp).

  inlineCase(Lc,Gov,Cases,Deflt,Tp,Prog,Depth) where 
      matching(Exp) .= matchingCase(Gov,Cases,Prog,Depth) => Exp.
  inlineCase(Lc,Gov,Cases,Deflt,Tp,Prog,Depth) =>
    crCase(Lc,Gov,Cases,Deflt,Tp).

  inlineUnpack(Lc,Gov,Cases,Tp,Prog,Depth) where
      matching(Exp) .= matchingCase(Gov,Cases,Prog,Depth) => Exp.
  inlineUnpack(Lc,Gov,Cases,Tp,_,_) => crUnpack(Lc,Gov,Cases,Tp).

  matchingCase:(crExp,cons[crCase],map[string,crDefn],integer) => match[crExp].
  matchingCase(_,[],_,_) => .noMatch.
  matchingCase(Gov,[C,..Cs],Prog,Depth) => case candidate(Gov,C) in {
    .insufficient => .insufficient.
    .noMatch => matchingCase(Gov,Cs,Prog,Depth).
    matching(Rep) => matching(simplifyExp(Rep,Prog,Depth))
  }

  candidate:(crExp,crCase) => match[crExp].
  candidate(E,(_,Ptn,Rep)) => case ptnMatch(E,Ptn,[]) in {
    matching(Theta) => matching(rewriteTerm(Rep,Theta)).
    .noMatch => .noMatch.
    .insufficient => .insufficient
  }

  inlineLtt(Lc,crId(Vr,Tp),Bnd,Exp,Prog,Depth) where crVar(_,VV).=Bnd =>
    simplifyExp(Exp,Prog[Vr->vrDef(Lc,Vr,Tp,Bnd)],Depth).
  inlineLtt(Lc,crId(Vr,Tp),Bnd,Exp,Prog,Depth) where isGround(Bnd) =>
    simplifyExp(Exp,Prog[Vr->vrDef(Lc,Vr,Tp,Bnd)],Depth).
  inlineLtt(Lc,Vr,Bnd,Exp,Prog,Depth) =>
    crLtt(Lc,Vr,Bnd,simplifyExp(Exp,Prog,Depth)).

  inlineCall:(option[locn],string,cons[crExp],tipe,map[string,crDefn],integer) => crExp.
  inlineCall(_,Nm,Args,_,Prog,Depth) where Depth>0 &&
      fnDef(Lc,_,_,Vrs,Rep) ^= Prog[Nm] =>
    simplifyExp(rewriteTerm(Rep,zip(Vrs//crName,Args)::map[string,crExp]),Prog[~Nm],Depth-1).
  inlineCall(Lc,Nm,Args,Tp,_,_) default => crCall(Lc,Nm,Args,Tp).

  inlineECall:(option[locn],string,cons[crExp],tipe,integer) => crExp.
  inlineECall(Lc,Nm,Args,Tp,Depth) where Depth>0 && {? A in Args *> isGround(A) ?} =>
    rewriteECall(Lc,Nm,Args,Tp).
  inlineECall(Lc,Nm,Args,Tp,_) default => crECall(Lc,Nm,Args,Tp).

  inlineOCall(Lc,Op,Args,Tp,Prog,Depth) =>
    crOCall(Lc,Op,Args,Tp).
  
  rewriteECall(Lc,"_int_plus",[crInt(_,A),crInt(_,B)],_) => crInt(Lc,A+B).
  rewriteECall(Lc,"_int_minus",[crInt(_,A),crInt(_,B)],_) => crInt(Lc,A-B).
  rewriteECall(Lc,"_int_times",[crInt(_,A),crInt(_,B)],_) => crInt(Lc,A*B).
  rewriteECall(Lc,Op,Args,Tp) default => crECall(Lc,Op,Args,Tp).
  
}
