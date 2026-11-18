test.strop{
  import star.
  import star.assert.

  -- Exercise the string/char operations in star.strings

  main:(){}.
  main(){
    -- equality and comparison
    assert "abc"=="abc";
    assert ~("abc"=="abd");
    assert "abc"<"abd";
    assert "abd">="abc";
    assert "abc">="abc";

    assert `a`==`a`;
    assert ~(`a`==`b`);
    assert `a`<`b`;
    assert `b`>=`a`;

    assert "てすと"=="てすと";
    assert ~("てすと"=="とすて");
    assert "あ"<"い";
    assert "い">="あ";

    assert `あ`==`あ`;
    assert ~(`あ`==`い`);
    assert `あ`<`い`;
    assert `い`>=`あ`;

    -- size/isEmpty
    assert size("hello")==5;
    assert [|"hello"|]==5;
    assert isEmpty("");
    assert ~isEmpty("x");

    -- multi-byte text is measured in code points, not bytes
    assert size("てすと")==3;
    assert [|"てすと"|]==3;

    -- display
    assert disp("hello\nworld") == "\"hello\\nworld\"";
    assert disp(`a`) == "`a`";

    -- indexing (option-returning)
    HW = "helloworld";
    assert `h` ?= HW[0];
    assert `d` ?= HW[9];
    assert HW[-1]==.none;
    assert HW[10]==.none;

    -- indexing on multi-byte text uses code-point indices
    MB = "てすと";
    assert `て` ?= MB[0];
    assert `す` ?= MB[1];
    assert `と` ?= MB[2];
    assert MB[3]==.none;

    -- put/remove
    assert HW[0->`b`] == "belloworld";
    assert HW[9->`b`] == "helloworlb";
    assert HW[~0] == "elloworld";
    assert HW[~9] == "helloworl";

    -- put/remove on multi-byte text, at any code-point index
    assert MB[0->`ス`] == "スすと";
    assert MB[~0] == "すと";

    MB2 = "てすとだよ";
    assert MB2[1->`X`] == "てXとだよ";
    assert MB2[3->`X`] == "てすとXよ";
    assert MB2[~1] == "てとだよ";
    assert MB2[~3] == "てすとよ";

    -- concatenation
    assert "hello"++"world" == "helloworld";
    assert ["a","b","c"]* == "abc";
    assert (["x","y","z"]:cons[string])* == "xyz";

    assert "てす"++"と" == "てすと";
    assert ["て","す","と"]* == "てすと";

    -- glue
    assert prepend(`h`,"ello") == "hello";
    assert append("hell",`o`) == "hello";

    assert prepend(`て`,"すと") == "てすと";
    assert append("てす",`と`) == "てすと";

    -- slice
    assert HW[0:5] == "hello";
    assert HW[5:10] == "world";
    assert HW[0:0] == "";

    assert _sub_str("てすと",0,2) == "てす";

    -- splice: replace a slice of a string with new content
    SP = ref "helloworld";
    SP := _splice(SP!,0,5,"HELLO");
    assert SP! == "HELLOworld";
    SP := _splice(SP!,5,10,"WORLD");
    assert SP! == "HELLOWORLD";

    assert _splice("helloとだよ",5,6,"世界") == "hello世界だよ";

    assert reverse("hello") == "olleh";
    assert reverse("hello世界だよ") == "よだ界世olleh";

    -- subString/strFind/strPrefix
    assert subString("hello world",6,5) == "world";
    assert strFind("hello world","world",0) == .some(6);
    assert strFind("hello world","xyz",0) == .none;
    assert strPrefix("hello","hello world");
    assert ~strPrefix("world","hello world");
    assert strFind("star.topsort@Γ_0*defEntry","*",0) == .some(16);

    assert strFind("こんにちは世界","世界",0) == .some(5);
    assert strFind("こんにちは世界","さようなら",0) == .none;
    assert strPrefix("こんにちは","こんにちは世界");
    assert ~strPrefix("世界","こんにちは世界");

    -- explode/implode via coercion
    assert explode("abc") == [`a`,`b`,`c`];
    assert ("abc"::cons[char]) == [`a`,`b`,`c`];
    assert (([`a`,`b`,`c`]:cons[char])::string) == "abc";
    assert (`a`::string) == "a";
    assert (`a`::integer) == 97;
    assert (97::char) == `a`;

    assert explode("あいう") == [`あ`,`い`,`う`];
    assert ("あいう"::cons[char]) == [`あ`,`い`,`う`];
    assert (([`あ`,`い`,`う`]:cons[char])::string) == "あいう";
    assert (`あ`::string) == "あ";
    assert ((`あ`::integer)::char) == `あ`;

    -- char classification
    assert isDigit(`5`);
    assert ~isDigit(`a`);
    assert digitVal(`7`)==7;
    assert digitChar(7)==`7`;
    assert isHexDigit(`f`) == .some(15);
    assert isHexDigit(`F`) == .some(15);
    assert isHexDigit(`9`) == .some(9);
    assert isHexDigit(`g`) == .none;
    assert isSpace(` `);
    assert isSpace(`\n`);
    assert ~isSpace(`a`);
    assert isLetter(`a`);
    assert ~isLetter(`5`);
    assert isAlphaNum(`a`);
    assert isAlphaNum(`5`);
    assert ~isAlphaNum(` `);

    assert isLetter(`あ`);
    assert ~isDigit(`あ`);
    assert isHexDigit(`あ`) == .none;
    assert isAlphaNum(`あ`);
    assert ~isSpace(`あ`);

    -- genSym produces fresh, prefixed names
    G1 = genSym("sym");
    G2 = genSym("sym");
    assert strPrefix("sym",G1);
    assert strPrefix("sym",G2);
    assert ~(G1==G2);

    -- iteration (via the generate/iter protocol) visits each code point
    -- in order, not each byte
    Collected = ref ([]:cons[char]);
    for Ch in "てすと" do{
      Collected := [Ch,..Collected!]
    };
    assert reverse(Collected!) == [`て`,`す`,`と`];

    -- hashable: equal strings/chars hash equal, over ascii and multi-byte text
    assert hash("abc") == hash("abc");
    assert hash(`a`) == hash(`a`);
    assert hash("てすと") == hash("てすと");
    assert hash(`て`) == hash(`て`);

    -- stringQuote escapes control characters, without the surrounding
    -- quotes that disp adds
    assert stringQuote("hello\nworld") == "hello\\nworld";
    assert stringQuote("てすと") == "てすと";

    -- format[char]: q=quoted escape, c=raw char, x=hex code point, else decimal
    assert _format(`a`,"q") == "a";
    assert _format(`\n`,"q") == "\\n";
    assert _format(`a`,"c") == "a";
    assert _format(`a`,"x") == "61";
    assert _format(`a`,"d") == "97";

    -- format[string]: [srcWidth]align(l/r/c)width[pad]
    assert _format("hello","l10") == "hello     ";
    assert _format("hello","r10") == "     hello";
    assert _format("hello","c11") == "   hello   ";
    assert _format("hello","l10*") == "hello*****";
    assert _format("helloworld","3l5") == "hel  ";
    assert _format("helloworld","-3l5") == "rld  ";

    -- format[string] on multi-byte text, including the default (no
    -- explicit width) case and right-to-left source selection
    assert _format("てすとだよ","l3") == "てすと";
    assert _format("てすとだよ","l") == "てすとだよ";
    assert _format("てすとだよ","-3l5") == "とだよ  ";
    assert _format("てすとだよ","-2r6") == "    だよ";

    showMsg("all string tests passed")
  }
}
