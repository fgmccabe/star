star.script{
  import star.
  public import star.location.

  public scriptMsg ::= scriptError(locn,string).

  public implementation coercion[scriptMsg,string] => {
    _coerce(scriptError(Lc,Msg)) => some("Failed: #(Msg) at $(Lc)")
  }


  public assrt:(boolean,string,locn) => ().
  assrt(Tst,Msg,Lc) => valof{
    if Tst then{
      logMsg("assert #(Msg) at $(Lc)")
    }
    else{
      logMsg("failed assert #(Msg) at $(Lc)");
      _ .= _exit(1);
    };
    valis ()
  }

  public shwMsg:all t ~~ display[t] |: (t,string,locn) => ().
  shwMsg(Vl,Msg,Lc) => valof{
    showMsg("#(Msg) = $(Vl) at $(Lc)");
    valis ()
  }
}
