test.cfunc{
  -- a hand expanded version of func.star

  import star.
  import test.fog.

  lMsg:all e ~~ (string)=>action[e,()].
  lMsg(Msg) =>
    case _logmsg(Msg) in
      {(X)=> _valis(())}.

  lMsg(Msg) =>
    _sequence(
      _valis(()),
      (_) => ((_)=>_valis(()))(_logmsg(Msg))).

  public main:()=>action[(),()].
  main() =>
    case fog(K(3),id)(4) in {(X)=>
	_sequence(lMsg("$(X)"),
	  (_) =>
	    _valis(()))}.
}
	   
