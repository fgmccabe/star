test.bench.deltablue{
  import star.
  import star.assert.
  import test.lib.timer.

  direction ::= .forward | .backward.

  strength ::= .strongest
  | .required
  | .strong_preferred
  | .preferred
  | .strong_default
  | .defawlt
  | .weak_default
  | .weakest.

  strengthValue:(strength)=>integer.
  strengthValue(.strongest) => 0.
  strengthValue(.required) => 1.
  strengthValue(.strong_preferred) => 2.
  strengthValue(.preferred) => 3.
  strengthValue(.strong_default) => 4.
  strengthValue(.defawlt) => 5.
  strengthValue(.weak_default) => 6.
  strengthValue(.weakest) => 7.

  implementation comp[strength] => {
    X < Y => strengthValue(X) < strengthValue(Y).
    X >= Y => strengthValue(X) >= strengthValue(Y).
  }

  implementation equality[strength] => {
    X == Y => strengthValue(X)==strengthValue(Y)
  }

  contract all c ~~ hasConstraint[c] ::= {
    strength:(c) => strength.
    isInput:(c) => boolean.
    isSatisfied:(c)=>boolean.
  }

  constraint ::= .binary(strength,ref boolean,variable,variable,ref direction)
  | .unary(strength,ref boolean,variable).
    

  variable ::= var{
    value : ref integer.
    references : ref vect[constraint].
  }


}
