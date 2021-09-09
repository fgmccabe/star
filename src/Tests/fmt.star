test.fmt{
  import star.
  import star.script.

  main:()=>action[(),()].
  main()=>action{
    bar .= "bar";
    show "Foo#(bar)";
    show "ten = $(10):9.9;";
    assert "ten = $(10):00;"=="ten = 10";
    assert "Foo#(bar)"=="Foobar";
  }
}
