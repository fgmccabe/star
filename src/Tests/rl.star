test.rl{
  import star.
  import star.skew.

  T:sk[string].
  T = ["one","two","three","four","five","six","seven","eight","nine","ten"].

  show dump(T).

  show disp(T[0]).
  show disp(T[1]).
  show disp(T[2]).
  show disp(T[3]).
  show disp(T[4]).
  show disp(T[5]).
  show disp(T[6]).
  show disp(T[7]).
  show disp(T[8]).
  show disp(T[9]).
  show disp(T[10]).

  SS = ["zero",..T].
  show dump(SS).

  show disp(SS).

  rev:all e ~~ (sk[e])=>sk[e].
  rev(Tt) => foldLeft((So,E)=>[E,..So],[],Tt).

  show dump(rev(SS)).

  show dump(T++SS).

  show dump(SS++T).

  show dump(SS^/((Nm)=>size(Nm)<5)).

  assert rev(rev(SS))==SS.

  _main:(list[string]) => ().
  _main([]) =>
    valof action {
      XX := SS;

      XX[5] := "alpha";
      logMsg("XX=$(XX!)");

      for El in (XX!) do{
	logMsg("El: $(El)")
      }
    }.
  
}
