test.disp{
  boolean ::= .true | .false.

  public ss ::= ss(string) | ssSeq(cons[ss]).

  public all t ~~ cons[t] ::= .nil | cons(t,cons[t]).

  -- Displayable contract
  public contract all t ~~ display[t] ::= {
    disp:(t)=>ss.
  }

  public implementation display[ss] => {
    disp(X) => X
  }
}

