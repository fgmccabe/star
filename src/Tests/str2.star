test.str2{
  import star.
  import star.assert.

  main:(){}.
  main(){
    HW = "helloworld";
    show HW[1];
    show HW[0];
    show HW[-1];
    show HW[9];
    show HW[10];
    show HW[11];

    show HW[0->`b`];

    assert HW[-1]==.none;
    assert `h` ?= HW[0];
    assert `d` ?= HW[9];
    assert HW[10]==.none;

    assert HW[0->`b`]=="belloworld";
    assert HW[5->`b`]=="helloborld";
    assert HW[9->`b`]=="helloworlb";
    assert HW[10->`b`]=="helloworldb";

    assert HW[~0] == "elloworld";
    assert HW[~9] == "helloworl";
    assert HW[~5] == "helloorld";
    assert HW[~10] == "helloworld";
  }
}
