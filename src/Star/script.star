star.script{
  import star.
  public import star.location.

  public scriptMsg ::= scriptError(locn,string).

  public implementation coercion[scriptMsg,string] => {.
    _coerce(scriptError(Lc,Msg)) => some("Failed: #(Msg) at $(Lc)")
  .}

/*  public implementation coercion[scriptMsg,()] => {.
    _coerce(_) => some(())
  .}
*/
  public assrt:all e ~~ (()=>boolean,string,locn) => result[e,()].
  assrt(Tst,Msg,Lc) => do{
    if Tst() then{
      logMsg("assert #(Msg) at $(Lc)")
    }
    else{
      logMsg("failed assert #(Msg) at $(Lc)");
      ignore _exit(1);
      valis ()
    }
  }

  public shwMsg:all e,t ~~ display[t] |: (()=>t,string,locn) => result[e,()].
  shwMsg(Tst,Msg,Lc) => do{
    R .= Tst();
    logMsg("#(Msg) = $(R) at $(Lc)")
  }
}
