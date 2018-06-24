star.repo{
  -- Main interface for accessing repositories
  import star.
  import star.uri.

  public pkg ::= pkg(string,version).

  public version ::= defltVersion | vers(string).

  public contract all r ~~ repo[r] ::= {
    baseUri:(r)=>uri.
    hasResource:(r,pkg,string) => option[string].
  }

  public implementation equality[version] => {.
    defltVersion == defltVersion => true.
    vers(V) == vers(W) => V==W.
    _ == _ => false.
  .}

  public implementation hash[version] => {.
    hash(defltVersion) => 0.
    hash(vers(V)) => hash(V).
  .}

  public implementation coercion[string,version] => {
    _coerce("*") => defltVersion.
    _coerce(V) => vers(V).
  }

  public compatibleVersion:(version,version)=>boolean.
  compatibleVersion(defltVersion,_)=>true.
  compatibleVersion(_,defltVersion)=>true.
  compatibleVersion(vers(V1),vers(V2))=>V1==V2.
  compatibleVersion(_,_) => false.
}
