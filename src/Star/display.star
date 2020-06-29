star.display{
  import star.core.
  import star.coerce.
  import star.cons.
  import star.strings.

  public implementation coercion[ss,string] => {
    _coerce(S) => some(_str_flatten(S)).
  }
}
