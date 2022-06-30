test.opg{
  import star.
  import star.script.

  ast ::= _name(string) |
     _qnme(string) |    
    _tuple(string,cons[ast]) |
    _integer(integer) |
    _float(float) |
    _string(string) |
    _apply(ast,ast).

  implementation display[ast] => let{.
    dsp(_name(Nm)) => "%#(Nm)".
    dsp(_qnme(Nm)) => "'#(Nm)'".
    dsp(_integer(Ix)) => "$(Ix)".
    dsp(_float(Dx)) => "$(Dx)".
    dsp(_string(Cx)) => "$(Cx)".
    dsp(_tuple("()",Els)) => "(#(interleave(Els//dsp,",")*))".
    dsp(_tuple("[]",Els)) => "[#(interleave(Els//dsp,",")*)]".
    dsp(_tuple("{}",Els)) => "[#(interleave(Els//dsp,". ")*)]".
    dsp(_tuple(B,Els)) => "[#(B)#(interleave(Els//dsp,",")*)]".
    dsp(_apply(O,A)) => "#(dsp(O))#(dsp(A))".
  .} in {
    disp(A) => dsp(A)
  }

  main:()=>().
  main() => valof{
    show <|'+'(5,a)|>;
    valis ()
  }
}
 
