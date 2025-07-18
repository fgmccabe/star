test.opg{
  import star.
  import star.assert.
  import star.location.
  import star.quote.

  ast ::= .name_(string) |
    .qnme_(string) |    
    .tuple_(string,cons[ast]) |
    .integer_(integer) |
    .big_(bigint) |
    .float_(float) |
    .char_(char) |
    .string_(string) |
  .apply_(ast,ast).

  implementation quote[ast] => {
    _name(_,N) => .name_(N).
    _qnme(_,N) => .qnme_(N).
    _integer(_,Ix) => .integer_(Ix).
    _biginteger(_,Bx) => .big_(Bx).
    _float(_,Dx) => .float_(Dx).
    _char(_,Cx) => .char_(Cx).
    _string(_,Sx) => .string_(Sx).
    _tuple(_,Nm,As) => .tuple_(Nm,As).
    _apply(_,Nm,A) => .apply_(Nm,A).
  }

  implementation display[ast] => let{.
    dsp(.name_(Nm)) => "%#(Nm)".
    dsp(.qnme_(Nm)) => "'#(Nm)'".
    dsp(.integer_(Ix)) => "$(Ix)".
    dsp(.float_(Dx)) => "$(Dx)".
    dsp(.string_(Cx)) => "$(Cx)".
    dsp(.tuple_("()",Els)) => "(#(interleave(Els//dsp,",")*))".
    dsp(.tuple_("[]",Els)) => "[#(interleave(Els//dsp,",")*)]".
    dsp(.tuple_("{}",Els)) => "[#(interleave(Els//dsp,". ")*)]".
    dsp(.tuple_(B,Els)) => "[#(B)#(interleave(Els//dsp,",")*)]".
    dsp(.apply_(.name_("<||>"),A)) => "<|#(dsp(A))|>".
    dsp(.apply_(O,A)) => "#(dsp(O))#(dsp(A))".
  .} in {
    disp(A) => dsp(A)
  }

  main:()=>().
  main() => valof{
    show (<|'+'(5,a)|>:ast);
    valis ()
  }
}
 
