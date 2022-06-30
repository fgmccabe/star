test.rl{
  import star.
  import star.skew.
  import star.script.

  T:sk[string].
  T = ["zero","one","two","three","four","five","six","seven","eight","nine","ten"].

  main:() => ().
  main() => valof{
    show T;

    show T[0];
    show T[1];
    show T[2];
    show T[3];
    show T[4];
    show T[5];
    show T[6];
    show T[7];
    show T[8];
    show T[9];
    show T[10];

    assert T[0]==some("zero");
    assert T[1]==some("one");
    assert T[2]==some("two");
    assert T[3]==some("three");
    assert T[4]==some("four");
    assert T[5]==some("five");
    assert T[6]==some("six");
    assert T[7]==some("seven");
    assert T[8]==some("eight");
    assert T[9]==some("nine");
    assert T[10]==some("ten");

    show rev(SS);

    show T++SS;

    show SS++T;

    show SS^/((Nm)=>size(Nm)<5);

    assert rev(rev(SS))==SS;

    XX .= ref SS;

    XX[5] := "alpha";
    show XX!;

    for El in (XX!::cons[string]) do{
      logMsg("El: $(El)")
    };
    valis ()
  }

  SS = ["zero",..T].

  rev:all e ~~ (sk[e])=>sk[e].
  rev(Tt) => foldLeft((E,So)=>[E,..So],[],Tt).
}
