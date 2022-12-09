star.meta{
  -- Implement a mirror interface
  import star.core.

  public tipe ::= .nomimal(string) |
    .tupleTp(cons[tipe]) |
    .funTp(cons[tipe],tipe) |
    .faceTp(cons[(string,tipe)]) |
    .sqTp(tipe,tipe) |
    .tpFun(string,integer) |
    .allTp(string,tipe) |
    .exTp(string,tipe) |
    .constrainedTp(constraint,tipe).

  public constraint ::= .conTract(string,cons[tipe],cons[tipe]) |
    .implicit(string,tipe) |
    .hasType(tipe,string,tipe).

  public implementation display[tipe] => {
    disp(T) => showType(T,.true,10000)
  }

  shTipe:(tipe,boolean,integer) => string.
  shTipe(Tp,Sh,Dp) => case Tp in {
    .nominal(Nm) => Nm.
    .tupleType(A) => "(#(showTypes(A,Sh,Dp)*))".
    .funTp(As,R) => "(#(showTypes(As,Sh,Dp)*))=>#(shTipe(R,Sh,Dp))".
    .faceTp(Els,Tps) => "{#(showTypeEls(Els,Tps,Sh,Dp))}".
    .sqTp(O,A) => showTpExp(O,[A],Sh,Dp).
    .tpFun(Nm,Ar) => "#(Nm)/$(Ar)".
    .allTp(A,T) =>
      "all #(showBound(A,Dp))#(showMoreQuantified(T,Sh,Dp))".
    .exTp(A,T) =>
      "exists #(showBound(A,Dp))#(showMoreQuantified(T,Sh,Dp))".
    .constrainedTp(C,T) =>
      "#(showConstraint(C,Dp))#(showConstrained(T,Dp))".
  }

  showTypes:(cons[tipe],boolean,integer) => cons[string].
  showTypes(_,_,0) => ["..."].
  showTypes(E,Sh,Dp) => showEls(E,Sh,Dp-1,"").

  showConstraint(.conTract(Nm,T,D),Dp) => shContract(Nm,T,D,.false,Dp).
  showConstraint(.hasType(Tp,Fld,Fc),Dp) =>
    "#(shTipe(Tp,.false,Dp)) <~ {#(Fld):#(shTipe(Fc,.false,Dp))}".
  showConstraint(.implicit(Fld,Tp),Dp) =>
    "#(Fld) |= #(shTipe(Tp,.false,Dp))".

  shContract(Nm,Tps,[],Sh,Dp) => "#(Nm)[#(showTypes(Tps,Sh,Dp)*)]".
  shContract(Nm,Tps,Dps,Sh,Dp) => "#(Nm)[#(showTypes(Tps,Sh,Dp)*)->>#(showTypes(Dps,Sh,Dp)*)]".
  
  showConstrained(.constrainedTp(C,T),Dp) => "#(showConstraint(C,Dp)),#(showConstrained(T,Dp))".
  showConstrained(T,Dp) => "|:#(shTipe(T,.false,Dp))".


  public contract all e ~~ mirror[e] ::= {
    hasType:(e) => tipe.
  }

}



  
