test.rrb{
  import star.
  import star.skew.
  import star.script.

  T:sk[string].
  T = ["one","two","three","four","five","six","seven","eight","nine","ten"].

  main:() => action[(),()].
  main() => action{
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

    show rev(SS);

    show T++SS;

    show SS++T;

    show (SS^/((Nm)=>size(Nm)<5));

    assert reverse(reverse(SS))==SS;

    XX .= ref SS;

    XX[5] := "alpha";
    logMsg("XX=$(XX!)");
  }

  SS = ["zero",..T].

  rev:all e ~~ (sk[e])=>sk[e].
  rev(Tt) => foldLeft((E,So)=>[E,..So],[],Tt).
}
