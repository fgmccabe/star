star.meta{
  import star.

  -- The type meta language

  public tipe ::= .voidTp
  | .nominal(string)
  | .tupleTp(cons[tipe])
  | .funTp(cons[tipe],tipe,option[tipe])
  | .prcTp(cons[tipe],option[tipe])
  | .faceTp(cons[(string,tipe)],cons[(string,tipeRule)])
  | .sqTp(tipe,tipe)
  | .tpFun(string,integer)
  | .allTp(tipe,tipe)
  | .exTp(tipe,tipe)
  | .constrainedTp(constraint,tipe).

  public tipeRule ::= .tipeExists(tipe,tipe)
  | .conExists(string,cons[tipe],cons[tipe],tipe)
  | .tipeLambda(tipe,tipe)
  | .allRle(tipe,tipeRule).

  public constraint ::= .conTract(string,cons[tipe],cons[tipe]) |
    .implicit(string,tipe) |
    .hasField(tipe,string,tipe).

  public implementation display[tipe] => {
    disp(T) => shTipe(T,10000)
  }

  shTipe:(tipe,integer) => string.
  shTipe(Tp,Dp) => case Tp in {
    | .voidTp => "void"
    | .nominal(Nm) => Nm
    | .tupleTp(A) => "(#(shTipes(A,Dp)*))"
    | .funTp(As,R,Th) => "(#(shTipes(As,Dp)*))=>#(shTipe(R,Dp))#(shThrowing(Th,Dp))"
    | .prcTp(As,Th) => "(#(shTipes(As,Dp)*)){}#(shThrowing(Th,Dp))"
    | .faceTp(Els,Tps) => "{#(showFields(Els,Tps,Dp))}"
    | .sqTp(O,A) => showTpExp(O,[A],Dp)
    | .tpFun(Nm,Ar) => "#(Nm)/$(Ar)"
    | .allTp(A,T) => "all #(showBound(A,Dp))#(showMoreQuantified(T,Dp))"
    | .exTp(A,T) => "exists #(showBound(A,Dp))#(showMoreQuantified(T,Dp))"
    | .constrainedTp(C,T) => "#(showConstraint(C,Dp))#(showConstrained(T,Dp))"
  }

  shThrowing(.none,Dp) => "".
  shThrowing(.some(Tp),Dp) => " throws #(shTipe(Tp,Dp))".

  shTipes:(cons[tipe],integer) => cons[string].
  shTipes(_,0) => ["..."].
  shTipes(E,Dp) => showEls(E,Dp-1,"").

  showEls:(cons[tipe],integer,string) => cons[string].
  showEls([],_,_) => [].
  showEls([T,..Tps],Dp,Sep) => [Sep,shTipe(T,Dp),..showEls(Tps,Dp,", ")].

  showFields:(cons[(string,tipe)],cons[(string,tipeRule)],integer) => string.
  showFields(Els,Tps,Dp) =>
    interleave({"#(Nm)\:#(shTipe(Tp,Dp))" | (Nm,Tp) in Els} ++
      { shTipeRule(Rl,Dp) | (Nm,Rl) in Tps},".\n")*.

  showTpExp:(tipe,cons[tipe],integer) => string.
  showTpExp(.tpFun("ref",1),[R],Dp) => "ref #(shTipe(R,Dp-1))".
  showTpExp(.tpFun(Nm,Ar),A,Dp) where size(A)==Ar => "#(Nm)[#(shTipes(A,Dp-1)*)]".    
  showTpExp(.sqTp(O,A),R,Dp) => showTpExp(O,[A,..R],Dp).
  showTpExp(Op,A,Dp) => "#(shTipe(Op,Dp-1))[#(shTipes(A,Dp-1)*)]".    

  shTpExp:(tipe,string,string,integer) => string.
  shTpExp(.sqTp(T,A),Sep,R,Dp) => shTpExp(T,",","#(shTipe(A,Dp))#(Sep)#(R)",Dp).
  shTpExp(.tpFun(Nm,_),Sep,R,Dp) => "#(Nm)[#(R)".
  shTpExp(T,Sep,R,Dp) => "#(shTipe(T,Dp))[#(R)".

  showAllConstraints([],Dp) => "".
  showAllConstraints([C,..Cs],Dp) => "#(showConstraint(C,Dp))#(showMoreConstraints(Cs,Dp))".

  showMoreConstraints:(cons[constraint],integer) => string.
  showMoreConstraints([],_) => "|=".
  showMoreConstraints([C,..Cs],Dp) => ", #(showConstraint(C,Dp))#(showMoreConstraints(Cs,Dp))".

  showMoreQuantified(.allTp(V,T),Dp) => ", #(showBound(V,Dp))#(showMoreQuantified(T,Dp))".
  showMoreQuantified(T,Dp) => " ~~ #(shTipe(T,Dp))".

  showMoreXQuantified(.exTp(V,T),Dp) => ", #(showBound(V,Dp))#(showMoreXQuantified(T,Dp))".
  showMoreXQuantified(T,Dp) => " ~~ #(shTipe(T,Dp))".

  showBound(V,Dp) => shTipe(V,Dp).

  showConstraint(.conTract(Nm,T,D),Dp) => shContract(Nm,T,D,Dp).
  showConstraint(.hasField(Tp,Fld,Fc),Dp) => "#(shTipe(Tp,Dp)) <~ {#(Fld):#(shTipe(Fc,Dp))}".
  showConstraint(.implicit(Fld,Tp),Dp) => "#(Fld) |: #(shTipe(Tp,Dp))".

  showConstraint(.conTract(Nm,T,D),Dp) => shContract(Nm,T,D,Dp).
  showConstraint(.hasField(Tp,Fld,Fc),Dp) => "#(shTipe(Tp,Dp)) <~ {#(Fld):#(shTipe(Fc,Dp))}".
  showConstraint(.implicit(Fld,Tp),Dp) => "#(Fld) : #(shTipe(Tp,Dp))".

  shContract(Nm,Tps,[],Dp) => "#(Nm)[#(shTipes(Tps,Dp)*)]".
  shContract(Nm,Tps,Dps,Dp) => "#(Nm)[#(shTipes(Tps,Dp)*)->>#(shTipes(Dps,Dp)*)]".
  
  showConstrained(.constrainedTp(C,T),Dp) => "#(showConstraint(C,Dp)),#(showConstrained(T,Dp))".
  showConstrained(T,Dp) => "|=#(shTipe(T,Dp))".

  shTipeRule(.tipeExists(A,T),Dp) =>
    "#(shTipe(A,Dp)) <~ #(shTipe(T,Dp))".
  shTipeRule(.conExists(N,A,D,T),Dp) => "#(shContract(N,A,D,Dp)) ::= #(shTipe(T,Dp))".
  shTipeRule(.allRle(Q,R),Dp) =>
    "all #(showBound(Q,Dp)) #(showMoreQRule(R,Dp))".
  shTipeRule(.tipeLambda(A,T),Dp) =>
    "#(shTipe(A,Dp)) ~> #(shTipe(T,Dp))".

  showMoreQRule(.allRle(Q,R),Dp) =>
    ", #(showBound(Q,Dp))#(showMoreQRule(R,Dp))".
  showMoreQRule(R,Dp) =>
    " ~~ #(shTipeRule(R,Dp))".

  public contract all e ~~ mirror[e] ::= {
    hasType:(e) => tipe.
  }
}
