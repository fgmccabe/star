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

  public macroRule ~> (ast,macroContext,reports) => result[reports,macroState].

  public macroState ::= .inactive | active(ast) .

  public implementation display[macroState] => {
    disp(.inactive) => "inactive".
    disp(active(A)) => "active $(A)".
  }

  public macroKey:(ast)=>string.
  macroKey(nme(_,Id)) => Id.
  macroKey(qnm(_,Id)) => Id.
  macroKey(int(_,_)) => "$integer".
  macroKey(big(_,_)) => "$bigint".
  macroKey(num(_,_)) => "$number".
  macroKey(str(_,_)) => "$string".
  macroKey(chr(_,_)) => "$char".
  macroKey(tpl(_,"()",[tpl(Lc,Lb,I)])) => macroKey(tpl(Lc,Lb,I)).
  macroKey(tpl(_,"()",_)) => "\$()".
  macroKey(tpl(_,"[]",_)) => "\$[]".
  macroKey(tpl(_,"{}",_)) => "\${}".
  macroKey(app(_,O,_)) => macroKey(O).

  public reveal:(ast,visibility) => ast.
  reveal(A,.priVate) => unary(locOf(A),"private",A).
  reveal(A,.pUblic) => unary(locOf(A),"public",A).
  reveal(A,_) default => A.


}
