test.fmt{
  import star.
  import star.script.

  main:()=>action[(),()].
  main()=>do{
    bar .= "bar";
    show "Foo#(bar)";
    assert "Foo#(bar)"=="Foobar"
  }
}
