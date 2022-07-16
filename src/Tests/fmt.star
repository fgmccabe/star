test.fmt{
  import star.
  import star.script.

  TstStr = "theBeginningTheMiddleTheEnd".
  Middle = "theMiddle".

  main:()=>().
  main()=>valof{
    bar = "bar";
    show "Foo#(bar)";
    show "ten = $(10):9.9;";
    assert "ten = $(10):00;"=="ten = 10";
    assert "Foo#(bar)"=="Foobar";
    show "$(TstStr):C100*;";
    assert "$(Middle):C20*;" == "*****theMiddle******";
    assert "$(Middle):L20*;" == "theMiddle***********";
    assert "$(Middle):R20*;" == "***********theMiddle";
    assert "$(Middle):5C15;" == "     theMi     ";
    assert "$(Middle):5L15;" == "theMi          ";
    assert "$(Middle):5R15;" == "          theMi";
    assert "$(Middle):-5C15;" == "     iddle     ";
    assert "$(Middle):-5L15;" == "iddle          ";
    assert "$(Middle):-5R15;" == "          iddle";
    valis ()
  }
}
