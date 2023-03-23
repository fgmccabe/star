star.compiler.macro.infra{
  import star.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.location.

  -- infrastructure for macro processing
  
  public macroContext ::= .package
    | .statement
    | .expression
    | .actn
    | .pattern
    | .typeterm
    | .constraint.

  public implementation equality[macroContext] => {
    .package == .package => .true.
    .statement == .statement => .true.
    .expression == .expression => .true.
    .actn == .actn => .true.
    .pattern == .pattern => .true.
    .typeterm == .typeterm => .true.
    .constraint == .constraint => .true.
    _ == _ default => .false
  }

  public implementation display[macroContext] => {
    disp(.package) => "package".
    disp(.statement) => "statement".
    disp(.expression) => "expression".
    disp(.actn) => "action".
    disp(.pattern) => "pattern".
    disp(.typeterm) => "type".
    disp(.constraint) => "constraint".
  }

  public macroKey:(ast)=>string.
  macroKey(A) => case A in {
    .nme(_,Id) => Id.
    .qnm(_,Id) => Id.
    .int(_,_) => "$integer".
    .big(_,_) => "$bigint".
    .num(_,_) => "$number".
    .str(_,_) => "$string".
    .chr(_,_) => "$char".
    .tpl(_,"()",[.tpl(Lc,Lb,I)]) => macroKey(.tpl(Lc,Lb,I)).
    .tpl(_,Op,_) => Op.
    .app(_,O,.tpl(_,"()",_)) => macroKey(O).
    .app(_,O,.tpl(_,"[]",_)) => "\$[]".
    .app(_,O,.tpl(_,"{}",_)) => macroKey(O)++"\${}".
    .app(_,O,.tpl(_,"{..}",_)) => macroKey(O)++"\${}".
  }

  public reveal:(ast,visibility) => ast.
  reveal(A,.priVate) => unary(locOf(A),"private",A).
  reveal(A,.pUblic) => unary(locOf(A),"public",A).
  reveal(A,_) default => A.
}
