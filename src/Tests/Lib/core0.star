star.core0{
  -- special core to use in emergency
  public boolean ::= .true | .false.

  public contract all c ~~ sizeable[c] ::= {
    size:(c) => integer.
    isEmpty:(c) => boolean.
  }

  
}
