test.opgr{
  import star.
  import star.assert.

  infix(`+`) => .some((720,720,719,.plus)).
  infix(`-`) => .some((720,720,719,.sub)).
  infix(`*`) => .some((700,700,699,.times)).
  infix(`/`) => .some((700,700,699,.div)).
  infix(_) default => .none.

  prefix(`-`) => .some((300,299,.uminus)).
  prefix(_) default => .none.

  op ::= .plus | .times | .sub | .div | .uminus.
  
  ast ::= .ix(integer) | .vr(string) | .app(op,cons[ast]).

  implementation display[op] => {
    disp(.plus) => "+".
    disp(.times) => "*".
    disp(.sub) => "-".
    disp(.div) => "/".
    disp(.uminus) => "-"
  }

  chOf(.plus) => `+`.
  chOf(.times) => `*`.
  chOf(.sub) => `-`.
  chOf(.div) => `/`.
  chOf(.uminus) => `-`.

  eval:(ast,map[string,integer])=>option[integer].
  eval(Ex,Mp) => let{.
    ev(.ix(Ix)) => .some(Ix).
    ev(.vr(Nm)) =>Mp[Nm].
    ev(.app(Op,As)) where Es?=evL(As,[]) => apply(Op,Es).
    ev(_) default => .none.

    evL:(cons[ast],cons[integer])=>option[cons[integer]].
    evL([],As) => .some(reverse(As)).
    evL([E,..Es],So) where V?=ev(E) => evL(Es,[V,..So]).
    evL(_,_) => .none.

    apply:(op,cons[integer]) => option[integer].
    apply(.plus,[L,R]) => .some(L+R).
    apply(.times,[L,R]) => .some(L*R).
    apply(.sub,[L,R]) => .some(L-R).
    apply(.div,[L,R]) => (try .some(L/R) catch exception in { _ => .none}).
    apply(.uminus,[R]) => .some(-R).
  .} in ev(Ex).

  implementation display[ast] => let{.
    dAst(.ix(Ix),_) => disp(Ix).
    dAst(.vr(N),_) => N.
    dAst(.app(Op,[L,R]),Pr) where (Lft,OPr,Rgt,_) ?= infix(chOf(Op)) =>
      "#(lpar(OPr,Pr)) #(dAst(L,Lft)) $(Op) #(dAst(R,Rgt)) #(rpar(OPr,Pr))".
    dAst(.app(Op,[R]),Pr) where (OPr,Rgt,_) ?= prefix(chOf(Op)) =>
      "#(lpar(OPr,Pr)) $(Op) #(dAst(R,Rgt)) #(rpar(OPr,Pr))".
    dAst(.app(Op,Args),_) => "$(Op)(#(interleave(Args//(A)=>dAst(A,1000),", ")*))".

    lpar(OPr,RPr) where OPr>RPr => "(".
    lpar(_,_) default => "".
    rpar(OPr,RPr) where OPr>RPr => ")".
    rpar(_,_) default => "".
  .} in {
    disp(A) => dAst(A,1000)
  }

  term:(integer) >> (ast,integer) --> cons[char].
  term(Pr) >> T --> termLeft(Pr) >> (L,LPr), termRight(L,LPr,Pr)>>T.

  termLeft(Pr) >> (.app(Op,[A]),OPr) --> [O], {(OPr,RP,Op)?=prefix(O) && OPr=<Pr}, term(RP)>>(A,_).
  termLeft(_) >> (U,0) --> term0 >> U.

  termRight(Lft,LftPr,Pr) >> T --> [O], {(LPr,OPr,RPr,Op)?=infix(O) && LPr >= LftPr && OPr=< Pr},
  term(RPr) >> (Rgt,RgtPr), termRight(.app(Op,[Lft,Rgt]),RgtPr,Pr) >> T.
  termRight(Lft,LftPr,_) default >> (Lft,LftPr) --> [].

  term0 >> .ix(Ix) --> [D], {isDigit(D)}, int(digitVal(D)) >> Ix.
  term0 >> .vr(Nm) --> ident >> Nm.
  term0 >> T --> [`(`], term(2000)>>(T,_), [`)`].

  ident >> [F,..S]::string --> letter >> F, letterDigit* >>S.

  letter >> L --> [L], {isLetter(L)}.

  letterDigit:() >> char --> cons[char].
  letterDigit >> L --> [L], {isLetter(L)||isDigit(L)}.

  digit >> D --> [D],{isDigit(D)}.

  int(X) >> Ix --> digit>>D, int(X*10+digitVal(D))>>Ix.
  int(X) >> X --> ~ digit.

  msg(M) => valof{
    logMsg(M);
    valis .true
  }

  evalStr:(string,map[string,integer]) => option[integer].
  evalStr(Txt,Mp) => valof{
    if ((Exp,_),_) ?= term(Txt::cons[char],2000) then
      valis eval(Exp,Mp)
    else
    valis .none
  }

  main:()=>().
  main() => valof{
    show term("alpha*1+alpha"::cons[char],2000);

    show evalStr("alpha*1+alpha",{"alpha"->10});
    show term("alpha*(beta+gamma)"::cons[char],2000);
    show evalStr("alpha*(beta+gamma)",{"alpha"->2, "beta"->3, "gamma"->4});

    assert 14?=evalStr("alpha*(beta+gamma)",{"alpha"->2, "beta"->3, "gamma"->4});

    valis ()
  }
  
}  
  
  
