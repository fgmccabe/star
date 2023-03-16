star.compiler.re{
  import star.
  import star.compiler.token.
  import star.compiler.location.

  public regexp ::=
    .emptyRE(option[locn]) |
    .ordRE(char,option[locn]) |
    .orRE(regexp,regexp,option[locn]) |
    .catRE(regexp,regexp,option[locn]) |
    .starRE(regexp,option[locn]) |
    .plusRE(regexp,option[locn]) |
    .optRE(regexp,option[locn]) |
    .strRE(string,option[locn]) |
    .charsRE(cons[char],option[locn]) |
    .negCharsRE(cons[char],option[locn]) |
    .periodRE(option[locn]) |
    .startRE(option[locn]) |
    .endRE(option[locn]).

  
  

}
