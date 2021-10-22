test.fm{
  import star.
  import star.script.

  main:()=>action[(),()].
  main()=>action{
    show "--$(-15):-   0;--";

    assert "--  -15--" == "--$(-15):-   0;--"; -- 
  
    show "--$(5):00;--";

    assert "--$(5):00;--" == "--05--";

    show "--$(5): 0;--";
    assert "--$(5): 0;--" == "-- 5--";	-- 

    assert "--$(-15):-   0;--" == "--  -15--"; -- 
    show "--$(-15):-   0;--";

    assert "--$(-15):+   0;--" == "--  -15--";
    show "--$(-15):+   0;--";

    assert "--$(15):+   0;--" == "--  +15--";
    show "--$(15):+   0;--";

    assert "--$(120345567):999,999,999,999;--" == "--120,345,567--";
    show "--$(120345567):999,999,999,999;--";

    Amnt .= -563;
    assert "Balance: $(Amnt):P999900.00P; ---" == "Balance: (05.63) ---";
    show "Balance: $(Amnt):P999900.00P; ---";

    assert "Balance: $(Amnt):P999900.00P; ---" == "Balance: (05.63) ---";
    show "Balance: $(-Amnt):P999900.00P; ---";

    C .= `\u22a6;` ;

    show "Unicode: $(([C]:cons[char])::string)/$(_codePoint(C)):XXXXX;";

    bar .= "bar";
    show "Foo#(bar)"
  }
}
