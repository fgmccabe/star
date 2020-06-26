star.compiler.inline{
  import star.
  import star.sort.

  import star.compiler.core.
  import star.compiler.errors.
  import star.compiler.freevars.
  import star.compiler.misc.
  import star.compiler.types.

  import star.compiler.location.

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
  ptnMatch(crLbl(_,N,_),crLbl(_,N,_),Env) => matching(Env).
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

  candidate:(crExp,crCase) => match[crExp].
  candidate(E,(_,Ptn,Rep)) => case ptnMatch(E,Ptn,[]) in {
    matching(Theta) => matching(rewriteTerm(Rep,Theta)).
    .noMatch => .noMatch.
    .insufficient => .insufficient
  }

  simplifyExp:(crExp,map[string,crDefn],integer) => crExp.
  simplifyExp(crCase(_,Gov,Cases,_,_),Prog,Depth) where 
      matching(Exp) .= matchingCase(Gov,Cases,Prog,Depth) => Exp.
  simplifyExp(crDot(_,crRecord(_,_,Flds,_),Fld,_),Prog,Depth) where (Fld,Exp) in Flds =>
    simplifyExp(Exp,Prog,Depth).
  simplifyExp(crTplOff(_,crTerm(_,_,Args,_),Ix,_),Prog,Depth) where Exp^=Args[Ix] =>
    simplifyExp(Exp,Prog,Depth).
  simplifyExp(crCall(_,Fn,Args,_),Prog,Depth) where Depth>0 &&
      fnDef(Lc,_,_,Vrs,Rep) ^= Prog[Fn] =>
    simplifyExp(rewriteTerm(Rep,zip(Vrs//crName,Args)::map[string,crExp]),Prog,Depth-1).
  simplifyExp(crCnj(_,crLbl(_,"star.core#true",_),R),Prog,Depth) =>
    simplifyExp(R,Prog,Depth).
  simplifyExp(crCnj(_,crLbl(Lc,"star.core#false",Tp),R),Prog,Depth) =>
    crLbl(Lc,"star.core#false",Tp).
  simplifyExp(crDsj(_,crLbl(_,"star.core#false",_),R),Prog,Depth) =>
    simplifyExp(R,Prog,Depth).
  simplifyExp(crCnj(_,crLbl(Lc,"star.core#true",Tp),R),Prog,Depth) =>
    crLbl(Lc,"star.core#true",Tp).
  simplifyExp(Exp,_,_) default => Exp.

  matchingCase:(crExp,cons[crCase],map[string,crDefn],integer) => match[crExp].
  matchingCase(_,[],_,_) => .noMatch.
  matchingCase(Gov,[C,..Cs],Prog,Depth) => case candidate(Gov,C) in {
    .insufficient => .insufficient.
    .noMatch => matchingCase(Gov,Cs,Prog,Depth).
    matching(Rep) => matching(simplifyExp(Rep,Prog,Depth))
  }
}
