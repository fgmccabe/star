test.ra{
  import star.
  import test.ralist.
  import star.script.

  T:ra[string].
  T = ["one","two","three","four","five","six","seven","eight","nine","ten"].

  main:() => ().
  main() => valof{
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

    show SS;
    show rev(SS);

    show T++SS;

    show SS++T;

    show SS^/((Nm)=>size(Nm)<5);

    valis ()
  }

  SS = ["zero",..T].

  rev:all e ~~ (ra[e])=>ra[e].
  rev(Tt) => foldLeft((E,So)=>[E,..So],[],Tt).
}
