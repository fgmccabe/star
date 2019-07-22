star.core{
  -- special core to use in emergency
  public boolean ::= true | false.

  public maybe[t] ::= impossible | just(t).
}
