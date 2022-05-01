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
  pickupDefn(glbDef(Lc,Nm,Tp,Val),Map) => Map[Nm->glbDef(Lc,Nm,Tp,Val)].

  simplifyDefn:(crDefn,map[string,crDefn])=>crDefn.
  simplifyDefn(fnDef(Lc,Nm,Tp,Args,Val),Prog) =>
    fnDef(Lc,Nm,Tp,Args,simplifyExp(Val,Prog,3)).
  simplifyDefn(glbDef(Lc,Nm,Tp,Val),Prog) =>
    glbDef(Lc,Nm,Tp,simplifyExp(Val,Prog,3)).
  simplifyDef(D) default => D.

  -- There are three possibilities of a match ...
  match[e] ::= .noMatch | .insufficient | matching(e).

  -- ptnMatch tries to match an actual value with a pattern
  
  ptnMatch:(crExp,crExp,map[string,crExp]) => match[map[string,crExp]].
  ptnMatch(E,crVar(Lc,crId(V,_)),Env) where T^=Env[V] => ptnMatch(E,T,Env).
  ptnMatch(E,crVar(_,crId(V,_)),Env) => matching(Env[V->E]).
  ptnMatch(crVar(_,V1),_,_) => .insufficient.  -- variables on left only match vars on right
  ptnMatch(crInt(_,Ix),crInt(_,Ix),Env) => matching(Env).
  ptnMatch(crFlot(_,Dx),crFlot(_,Dx),Env) => matching(Env).
  ptnMatch(crStrg(_,Sx),crStrg(_,Sx),Env) => matching(Env).
  ptnMatch(crVoid(_,_),_,_) => .insufficient.  -- void on left does not match anything
  ptnMatch(_,crVoid(_,_),_) => .insufficient.  -- void on right does not match anything
  ptnMatch(crTerm(_,N,A1,_),crTerm(_,N,A2,_),Env) => ptnMatchArgs(A1,A2,Env).
  ptnMatch(crRecord(_,N,F1,_),crRecord(_,N,F2,_),Env) => ptnMatchFields(F1,F2,Env).
  ptnMatch(_,_,_) default => .noMatch.

  ptnMatchArgs([],[],Env) => matching(Env).
  ptnMatchArgs([E1,..L1],[E2,..L2],Env) => case ptnMatch(E1,E2,Env) in {
    .noMatch => .noMatch.
    .insufficient => .insufficient.
    matching(Ev) => ptnMatchArgs(L1,L2,Ev)
  }
  ptnMatchArgs(_,_,_) default => .noMatch.

  ptnMatchFields(F1,[],Env) => matching(Env).
  ptnMatchFields(F1,[(N,P2),..F2],Env) where (N,E1) in F1 =>
    case ptnMatch(E1,P2,Env) in {
      .noMatch => .noMatch.
      .insufficient => .insufficient.
      matching(Ev) =>  ptnMatchFields(F1,F2,Ev)
    }
  ptnMatchFields(_,_,_) default => .noMatch.

  simplifyExp:(crExp,map[string,crDefn],integer) => crExp.
  simplifyExp(crCase(Lc,Gov,Cases,Deflt,Tp),Prog,Depth) =>
    applyCase(Lc,simplifyExp(Gov,Prog,Depth),Cases,simplifyExp(Deflt,Prog,Depth),Tp,Prog,Depth).
  simplifyExp(crDot(Lc,Rc,Fld,Tp),Prog,Depth) =>
    applyDot(Lc,simplifyExp(Rc,Prog,Depth),Fld,Tp).
  simplifyExp(crTplOff(_,crTerm(_,_,Args,_),Ix,_),Prog,Depth) where Exp^=Args[Ix] =>
    simplifyExp(Exp,Prog,Depth).
  simplifyExp(crCall(Lc,Fn,Args,Tp),Prog,Depth) where Depth>0 =>
    inlineCall(Lc,Fn,Args//(A)=>simplifyExp(A,Prog,Depth),Tp,Prog,Depth).
  simplifyExp(crECall(Lc,Fn,Args,Tp),Prog,Depth) where Depth>0 =>
    inlineECall(Lc,Fn,Args//(A)=>simplifyExp(A,Prog,Depth),Tp,Depth).
  simplifyExp(crOCall(Lc,Op,Args,Tp),Prog,Depth) where Depth>0 =>
    inlineOCall(Lc,simplifyExp(Op,Prog,Depth),Args//(A)=>simplifyExp(A,Prog,Depth),Tp,Prog,Depth).
  simplifyExp(crTerm(Lc,Fn,Args,Tp),Prog,Depth) =>
    crTerm(Lc,Fn,Args//(A)=>simplifyExp(A,Prog,Depth),Tp).
  simplifyExp(crCnj(Lc,L,R),Prog,Depth) =>
    applyCnj(Lc,simplifyExp(L,Prog,Depth),simplifyExp(R,Prog,Depth)).
  simplifyExp(crDsj(Lc,L,R),Prog,Depth) =>
    applyDsj(Lc,simplifyExp(L,Prog,Depth),simplifyExp(R,Prog,Depth)).
  simplifyExp(crNeg(Lc,R),Prog,Depth) =>
    applyNeg(Lc,simplifyExp(R,Prog,Depth)).
  simplifyExp(crCnd(Lc,T,L,R),Prog,Depth) =>
    applyCnd(Lc,simplifyExp(T,Prog,Depth),simplifyExp(L,Prog,Depth),simplifyExp(R,Prog,Depth)).
  simplifyExp(crWhere(Lc,Ptn,Exp),Prog,Depth) =>
    applyWhere(Lc,Ptn,simplifyExp(Exp,Prog,Depth)).
  simplifyExp(crTplOff(Lc,E,Ix,Tp),Prog,Depth) =>
    applyTplOff(Lc,simplifyExp(E,Prog,Depth),Ix,Tp).
  simplifyExp(crTplUpdate(Lc,T,Ix,E),Prog,Depth) =>
    applyTplUpdate(Lc,simplifyExp(T,Prog,Depth),Ix,simplifyExp(E,Prog,Depth)).
  simplifyExp(crLtt(Lc,Vr,Bnd,Exp),Prog,Depth) =>
    applyLtt(Lc,Vr,simplifyExp(Bnd,Prog,Depth),Exp,Prog,Depth).
  simplifyExp(crMatch(Lc,Ptn,Exp),Prog,Depth) =>
    applyMatch(Lc,Ptn,simplifyExp(Exp,Prog,Depth)).
  simplifyExp(Exp,_,_) default => Exp.

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

  applyTplOff(_,crTerm(_,_,Els,_),Ix,Tp) where E^=Els[Ix] => E.
  applyTplOff(Lc,T,Ix,Tp) default => crTplOff(Lc,T,Ix,Tp).
  
  applyTplUpdate(_,crTerm(Lc,Nm,Args,Tp),Ix,E) =>
    crTerm(Lc,Nm,Args[Ix->E],Tp).
  applyTplUpdate(Lc,T,Ix,E) =>
    crTplUpdate(Lc,T,Ix,E).

  applyMatch(Lc,Ptn,Exp) => crMatch(Lc,Ptn,Exp).

  applyDot(_,crRecord(_,_,Flds,_),Fld,_) where (Fld,Exp) in Flds => Exp.
  applyDot(Lc,Rc,Fld,Tp) => crDot(Lc,Rc,Fld,Tp).

  applyCase(Lc,Gov,Cases,Deflt,Tp,Prog,Depth) where 
      matching(Exp) .= matchingCase(Gov,Cases,Prog,Depth) => Exp.
  applyCase(Lc,Gov,Cases,Deflt,Tp,Prog,Depth) =>
    crCase(Lc,Gov,Cases,Deflt,Tp).

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

  applyLtt(Lc,Vr,Bnd,Exp,Prog,Depth) where crVar(_,VV).=Bnd =>
    simplifyExp(rewriteTerm(Exp,[crName(Vr)->Bnd]),Prog,Depth).
  applyLtt(Lc,Vr,Bnd,Exp,Prog,Depth) =>
    crLtt(Lc,Vr,Bnd,simplifyExp(Exp,Prog,Depth)).

  inlineCall:(locn,string,cons[crExp],tipe,map[string,crDefn],integer) => crExp.
  inlineCall(_,Nm,Args,_,Prog,Depth) where Depth>0 &&
      fnDef(Lc,_,_,Vrs,Rep) ^= Prog[Nm] =>
    simplifyExp(rewriteTerm(Rep,zip(Vrs//crName,Args)::map[string,crExp]),Prog[~Nm],Depth-1).
  inlineCall(Lc,Nm,Args,Tp,_,_) default => crCall(Lc,Nm,Args,Tp).

  inlineECall:(locn,string,cons[crExp],tipe,integer) => crExp.
  inlineECall(Lc,Nm,Args,Tp,Depth) where Depth>0 && A in Args *> isGround(A) =>
    rewriteECall(Lc,Nm,Args,Tp).
  inlineECall(Lc,Nm,Args,Tp,_) default => crECall(Lc,Nm,Args,Tp).

  inlineOCall(Lc,Op,Args,Tp,Prog,Depth) =>
    crOCall(Lc,Op,Args,Tp).
  
  rewriteECall(Lc,"_int_plus",[crInt(_,A),crInt(_,B)],_) => crInt(Lc,A+B).
  rewriteECall(Lc,"_int_minus",[crInt(_,A),crInt(_,B)],_) => crInt(Lc,A-B).
  rewriteECall(Lc,"_int_times",[crInt(_,A),crInt(_,B)],_) => crInt(Lc,A*B).
  rewriteECall(Lc,Op,Args,Tp) default => crECall(Lc,Op,Args,Tp).
  
}
