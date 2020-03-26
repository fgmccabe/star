star.display{
  import star.core.
  import star.coerce.
  import star.cons.
  import star.strings.

  public implementation coercion[ss,string] => {
    _coerce(S) => _str_flatten(S).
  }
}
