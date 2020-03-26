test.ra{
  import star.
  import test.ralist.
  import star.script.

  T:ra[string].
  T = ["one","two","three","four","five","six","seven","eight","nine","ten"].

  main:() => action[(),()].
  main() => do{
    show disp(T[0]);
    show disp(T[1]);
    show disp(T[2]);
    show disp(T[3]);
    show disp(T[4]);
    show disp(T[5]);
    show disp(T[6]);
    show disp(T[7]);
    show disp(T[8]);
    show disp(T[9]);
    show disp(T[10]);

    show disp(SS);
    show disp(rev(SS));

    show disp(T++SS);

    show disp(SS++T);

    show disp(SS^/((Nm)=>size(Nm)<5));

    show disp(SS)
  }

  SS = ["zero",..T].

  rev:all e ~~ (ra[e])=>ra[e].
  rev(Tt) => foldLeft((So,E)=>[E,..So],[],Tt).
}
