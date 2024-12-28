test.qv{
  import star.
  import star.assert.

  -- Test type inference

  tipe ::=
  | .breakfast
  | .lunch
  | .dinner.

  implementation coercion[tipe,string] => {
    _coerce(T) => .some(case T in {
	| .breakfast => "petit dejeuner"
	| .lunch => "昼食"
	| .dinner => "dinner"
      })
  }

  ctx ::= ctx{
    meals : map[string,(tipe,integer)].
  }

  info:(ctx) => cons[(string,string)].
  info(Ctx) => { (Nm, Tp::string) | (Nm -> (Tp,_)) in Ctx.meals }.
--  info(Ctx) => { (Nm, Tp::string) | (Nm -> ((Tp:tipe),_)) in Ctx.meals }.
  -- info(Ctx) =>
  --   _iter(Ctx.meals, .nil, (((El:keyval[string,(tipe,integer)]),St) => (.kv(Nm,(Tp,_)).=El ??
  -- 	.cons((Nm, Tp::string),St) || St))).

  menu = ctx{
    meals = ["monday" -> (.breakfast,1),
      "tuesday" -> (.lunch,2),
      "wednesday" -> (.dinner,3)]
  }.

  main:()=>().
  main() => valof{
    show info(menu);
    valis ()
  }
}
    
