test.chr{
  import star.
  import star.assert.

  C1 = `a`.
  C2 = `\n`.

  main:()=>().
  main()=>valof{
    show C1;
    assert C2==`\n`;
    for Ch in "てすと test" do{
      show Ch
    };
    showMsg("hello\nworld");
    showMsg(disp("hello\nworld"));
    assert disp("hello\nworld") == "\"hello\\nworld\"";
  }
}
