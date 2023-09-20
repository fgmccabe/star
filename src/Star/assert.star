star.assert{
  import star.
  public import star.location.

  public assrt:(boolean,string,string) => ().
  assrt(Tst,Msg,Lc) => valof{
    if Tst then{
      showMsg("assert #(Msg) at #(Lc)")
    }
    else{
      showMsg("failed assert #(Msg) at #(Lc)");
      _exit(1)
    };
    valis ()
  }

  public shwMsg:all t ~~ display[t] |: (t,string,string) => ().
  shwMsg(Vl,Msg,Lc) => valof{
    showMsg("#(Msg) = $(Vl) at #(Lc)");
    valis ()
  }
}
