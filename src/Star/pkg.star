star.pkg{
  import star.

  public pkg ::= pkg(string,version).

  public version ::= defltVersion | vers(string).

  public implementation display[pkg] => {.
    disp(pkg(P,V)) => ssSeq([ss(P),ss("#"),disp(V)]).
  .}

  public implementation equality[pkg] => {.
    pkg(P1,V1) == pkg(P2,V2) => P1==P2 && V1==V2.
  .}

  public implementation display[version] => {.
    disp(defltVersion) => ss("*").
    disp(vers(V)) => ss(V).
  .}

  public implementation equality[version] => {.
    defltVersion == defltVersion => true.
    vers(V) == vers(W) => V==W.
    _ == _ => false.
  .}

  public implementation hash[version] => {.
    hash(defltVersion) => 0.
    hash(vers(V)) => hash(V).
  .}

  public implementation hash[pkg] => {.
    hash(pkg(P,V)) => hash(P)*37+hash(V).
  .}

  public implementation coercion[string,version] => {
    _coerce("*") => defltVersion.
    _coerce(V) => vers(V).
  }

  public implementation coercion[version,string] => {
    _coerce(defltVersion) => "*".
    _coerce(vers(V)) => V.
  }

  public compatibleVersion:(version,version)=>boolean.
  compatibleVersion(defltVersion,_)=>true.
  compatibleVersion(_,defltVersion)=>true.
  compatibleVersion(vers(V1),vers(V2))=>V1==V2.
  compatibleVersion(_,_) => false.
}
