star.compiler.macro.infra{
  import star.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.location.
  import star.compiler.wff.

  -- infrastructure for macro processing

  public macroState ::= .inactive | .active(ast) .

  public implementation display[macroState] => {
    disp(.inactive) => "inactive".
    disp(.active(A)) => "active $(A)".
  }

  public macroContext ::= .package
  | .statement
  | .expression
  | .actn
  | .pattern
  | .typeterm
  | .typevar
  | .constructor
  | .constraint
  | .rule.

  public implementation equality[macroContext] => {
    .package == .package => .true.
    .statement == .statement => .true.
    .rule == .rule => .true.
    .expression == .expression => .true.
    .actn == .actn => .true.
    .pattern == .pattern => .true.
    .typeterm == .typeterm => .true.
    .typevar == .typevar => .true.
    .constructor == .constructor => .true.
    .constraint == .constraint => .true.
    _ == _ default => .false
  }

  public implementation display[macroContext] => {
    disp(.package) => "package".
    disp(.statement) => "statement".
    disp(.rule) => "rule".
    disp(.expression) => "expression".
    disp(.actn) => "action".
    disp(.pattern) => "pattern".
    disp(.typeterm) => "type".
    disp(.constraint) => "constraint".
    disp(.typevar) => "type var".
    disp(.constructor) => "constructor".
  }

  public macroKey:(ast)=>string.
  macroKey(A) => case A in {
    | .nme(_,Id) => Id
    | .qnm(_,Id) => Id
    | .int(_,_) => "$integer"
    | .big(_,_) => "$bigint"
    | .num(_,_) => "$number"
    | .str(_,_) => "$string"
    | .chr(_,_) => "$char"
    | .tpl(_,"()",[.tpl(Lc,Lb,I)]) => macroKey(.tpl(Lc,Lb,I))
    | .tpl(_,Op,_) => Op
    | .app(_,O,.tpl(_,"()",_)) => macroKey(O)
    | .app(_,O,.tpl(_,"[]",_)) => "\$[]"
    | .app(_,O,.tpl(_,"{}",_)) => macroKey(O)++"\${}"
    | .app(_,O,.tpl(_,"{..}",_)) => macroKey(O)++"\${}"
  }

  public reveal:(ast,visibility) => ast.
  reveal(A,.priVate) => unary(locOf(A),"private",A).
  reveal(A,.pUblic) => unary(locOf(A),"public",A).
  reveal(A,_) default => A.

  -- Action walk only recurses into composite actions

  public foldOverAction:(cons[ast],(ast)=>macroState)=>cons[ast].
  foldOverAction(Acts,Op) => let{.
    actionFold(A) => (.active(Rx) .= Op(A) ?? Rx || actionWalk(A)).

    actionWalk(A) where (Lc,L,R) ?= isActionSeq(A) =>
      actionSeq(Lc,actionFold(L),actionFold(R)).
    actionWalk(A) where (Lc,L) ?= isUnary(A,";") =>
      actionFold(L).
    actionWalk(A) where (Lc,[As]) ?= isBrTuple(A) =>
      brTuple(Lc,[actionFold(As)]).
    actionWalk(A) where (_,[]) ?= isBrTuple(A) => A.
    actionWalk(A) where (Lc,L,R) ?= isLbldAction(A) =>
      mkLbldAction(Lc,L,R//actionFold).
    actionWalk(A) where (Lc,T,L,R) ?= isIfThenElse(A) =>
      mkIfThenElse(Lc,T,actionFold(L),actionFold(R)).
    actionWalk(A) where (Lc,T,L) ?= isIfThen(A) =>
      mkIfThen(Lc,T,actionFold(L)).
    actionWalk(A) where (Lc,B,Hs) ?= isTry(A) =>
      mkTry(Lc,actionFold(B),Hs//actionCaseFold).
    actionWalk(A) where (Lc,B,H) ?= isTryCatch(A) =>
      mkTryCatch(Lc,actionFold(B),H).
    actionWalk(A) where (Lc,C,B) ?= isWhileDo(A) =>
      mkWhileDo(Lc,C,actionFold(B)).
    actionWalk(A) where (Lc,El,C,B) ?= isForIn(A) =>
      mkForIn(Lc,El,C,actionFold(B)).
    actionWalk(A) where (Lc,El,C,B) ?= isForDo(A) =>
      mkForDo(Lc,El,C,actionFold(B)).
    actionWalk(A) where (Lc,D,B) ?= isLetDef(A) =>
      mkLetDef(Lc,D,actionFold(B)).
    actionWalk(A) where (Lc,D,B) ?= isLetRecDef(A) =>
      mkLetRecDef(Lc,D,actionFold(B)).
    actionWalk(A) where (Lc,G,Cs) ?= isCase(A) =>
      mkCaseExp(Lc,G,Cs//actionCaseFold).
    actionWalk(A) default => A.

    actionCaseFold(A) where (Lc,Dflt,L,C,R) ?= isLambda(A) =>
      mkLambda(Lc,Dflt,L,C,actionFold(R)).
  .} in (Acts//actionFold).
}
