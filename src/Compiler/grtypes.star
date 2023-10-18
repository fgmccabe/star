star.compiler.macro.grtypes{
  import star.
  import star.location.

  import star.compiler.ast.
  import star.compiler.operators.

  public grRule ::= grRule{
    lc : option[locn].
    name:string.
    args:cons[ast].
    cond:option[ast].
    isDefault:boolean.
    value:ast.
    body:grBody}.

  public grBody ::=
    .seq(option[locn],grBody,grBody) |
    .rep(option[locn],grBody) |
    .sep(option[locn],grBody,grBody) |
    .dis(option[locn],grBody,grBody) |
    .neg(option[locn],grBody) |
    .term(option[locn],ast) |
    .nonTerm(option[locn],ast,cons[ast]) |
    .test(option[locn],ast) |
    .prod(option[locn],grBody,ast) |
    .epsilon(option[locn]) |
    .skip(option[locn],grBody) |
    .block(option[locn]) |
    .end(option[locn]).

  public implementation display[grRule] => {
    disp(Rl) => "$(Rl.name)$(Rl.args) #(C?=Rl.cond??" where $(C)"||"") >> $(Rl.value) --> $(Rl.body)".
  }

  public implementation display[grBody] => let{.
    dB(.seq(_,L,R),Pr) where (Lp,P,Rp) ?= isInfixOp(",") =>
      "#(leftPar(P,Pr))#(dB(L,Lp)) , #(dB(R,Rp))#(rightPar(P,Pr))".
    dB(.dis(_,L,R),Pr) where (Lp,P,Rp) ?= isInfixOp("|") =>
      "#(leftPar(P,Pr))#(dB(L,Lp)) | #(dB(R,Rp))#(rightPar(P,Pr))".
    dB(.neg(_,R),Pr) where (P,Rp) ?= isPrefixOp("~") =>
      "#(leftPar(P,Pr)) ~ #(dB(R,Rp))#(rightPar(P,Pr))".
    dB(.rep(_,L),Pr) where (Lp,P) ?= isPostfixOp("*") =>
      "#(leftPar(P,Pr)) #(dB(L,Lp)) * #(rightPar(P,Pr))".
    dB(.sep(_,L,R),Pr) where (Lp,P,Rp) ?= isInfixOp("*") =>
      "#(leftPar(P,Pr)) #(dB(L,Lp)) * #(dB(R,Rp)) #(rightPar(P,Pr))".
    dB(.term(_,T),Pr) => "[#(dispAst(T,1000,""))]".
    dB(.nonTerm(_,N,A),Pr) => "$(N)$(A)".
    dB(.prod(_,L,R),Pr) where (Lp,P,Rp) ?= isInfixOp(">>") =>
      "#(leftPar(P,Pr)) #(dB(L,Lp)) >> #(dispAst(R,Rp,"")) #(rightPar(P,Pr))".
    dB(.skip(_,R),Pr) where (P,Rp) ?= isPrefixOp(">>") =>
      "#(leftPar(P,Pr)) *> #(dB(R,Rp))#(rightPar(P,Pr))".
    dB(.block(_),_) => "fail".
    dB(.end(_),_) => "end".
    dB(.test(_,T),_) => "{ #(dispAst(T,2000,"")) }".
    dB(.epsilon(_),_) => "[]".
  .} in {
    disp(B) => dB(B,1259)
  }
}
