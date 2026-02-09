test.qv{
  import star.
  import star.assert.

  -- Test type inference

  tipe ::=
  | .breakfast
  | .lunch
  | .dinner.

  implementation coercion[tipe,string->>_] => {
    _coerce(T) => (case T in {
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

  menu = ctx{
    meals = ["monday" -> (.breakfast,1),
      "tuesday" -> (.lunch,2),
      "wednesday" -> (.dinner,3)]
  }.

  main:(){}.
  main(){
    show info(menu);
  }
}

