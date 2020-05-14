star.compiler.macro.infra{
  import star.

  import star.compiler.ast.
  import star.compiler.ast.disp.
  import star.compiler.errors.
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

  public implementation equality[macroContext] => {.
    .package == .package => .true.
    .statement == .statement => .true.
    .expression == .expression => .true.
    .actn == .actn => .true.
    .pattern == .pattern => .true.
    .typeterm == .typeterm => .true.
    .constraint == .constraint => .true.
    _ == _ default => .false
  .}

  implementation display[macroContext] => {.
    disp(.package) => ss("package").
    disp(.statement) => ss("statement").
    disp(.expression) => ss("expression").
    disp(.actn) => ss("action").
    disp(.pattern) => ss("pattern").
    disp(.typeterm) => ss("type").
    disp(.constraint) => ss("constraint").
  .}

  public macroRule ~> (ast,macroContext,reports) => either[reports,macroState].

  public macroState ::= .inactive | active(ast).

  public implementation display[macroState] => {.
    disp(.inactive) => ss("inactive").
    disp(active(A)) => ssSeq([ss("active "),disp(A)]).
  .}

  public macroKey:(ast)=>string.
  macroKey(nme(_,Id)) => Id.
  macroKey(qnm(_,Id)) => Id.
  macroKey(int(_,_)) => "$integer".
  macroKey(num(_,_)) => "$number".
  macroKey(str(_,_)) => "$string".
  macroKey(tpl(_,_,[tpl(Lc,Lb,I)])) => macroKey(tpl(Lc,Lb,I)).
  macroKey(tpl(_,K,_)) => K.
  macroKey(app(_,O,_)) => macroKey(O).
  

}
