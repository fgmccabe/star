test.fr1{
  import star.

  crExp::=.crExp | tpl(cons[crExp]).

  typeOf:(cons[crExp])=>integer.
  typeOf(L) => size(L).

  tplLbl:(integer)=>string.
  tplLbl(Ix) => "$(Ix)".
  
  crTpl:(cons[crExp]) => (string,cons[crExp],integer).
  crTpl(Args) => let{
    Tp = typeOf(Args).
    Ar = size(Args).
  } in (tplLbl(Ar),Args,Tp).
}
