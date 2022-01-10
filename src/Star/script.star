star.script{
  import star.
  public import star.location.

  public scriptMsg ::= scriptError(locn,string).

  public implementation coercion[scriptMsg,string] => {
    _coerce(scriptError(Lc,Msg)) => some("Failed: #(Msg) at $(Lc)")
  }

  public assrt:all m/2,e ~~ execution[m] |:
    (()=>boolean,string,locn) => m[e,()].
  assrt(Tst,Msg,Lc) => do{
    if Tst() then{
      logMsg("assert #(Msg) at $(Lc)")
    }
    else{
      logMsg("failed assert #(Msg) at $(Lc)");
      _ .= _exit(1);
      valis ()
    }
  }

  public shwMsg:all m/2,e,t ~~ execution[m], display[t] |:
    (()=>t,string,locn) => m[e,()].
  shwMsg(Tst,Msg,Lc) => do{
    R .= Tst();
    showMsg("#(Msg) = $(R) at $(Lc)")
  }
}
