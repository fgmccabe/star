test.fm{
  import star.
  import star.assert.

  main:()=>().
  main()=>valof{
    show "--$(-15):-   0;--";

    assert "-- -115--" == "--$(-115):-   0;--"; -- 
  
    show "--$(5):00;--";

    assert "--$(5):00;--" == "--05--";

    show "--$(5): 0;--";
    assert "--$(25):  0;--" == "-- 25--";	-- 

    assert "--$(-16):-   0;--" == "--  -16--"; -- 
    show "--$(-16):-   0;--";

    assert "--$(-17):+   0;--" == "--  -17--";
    show "--$(-17):+   0;--";

    assert "--$(18):+   0;--" == "--  +18--";
    show "--$(18):+   0;--";

    assert "--$(120345567):999,999,999,999;--" == "--120,345,567--";
    show "--$(120345567):999,999,999,999;--";

    Amnt = -563;
    assert "Balance: $(Amnt):(999900.00); ---" == "Balance: (05.63) ---";
    show "Balance: $(Amnt):(999900.00); ---";

    assert "Balance: $(Amnt):(999900.00); ---" == "Balance: (05.63) ---";
    show "Balance: $(-Amnt):(999900.00); ---";

    C = `\u22a6;` ;

    show "Unicode: $(([C]:cons[char])::string)/$(_codePoint(C)):XXXXX;";

    bar = "bar";
    show "Foo#(bar)";
    valis ()
  }
}
