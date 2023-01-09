star.debug{
  import star.


  public contract all s ~~ debug[s] ::= {
    _newLoc:(option[locn],s) => ().
    _enterFun:(string,cons[option[string]],s) => ().
    _exitFun:(string,option[string],s) => ().
  }


  public implementation debug[()] => {
    _newLoc(?Lc,_) => _debugger_new_loc(Lc).
    _newLoc(.none,_) => ().

    _enterFun(Nm,Args,_) => _debugger_enter(Nm,Args).
    _exitFun(Nm,Reslt,_) => _debugger_return(Nm,Res).
  }
}
  
  
