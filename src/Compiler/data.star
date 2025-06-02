star.compiler.data{
  import star.

  import star.multi.
  import star.pkg.
  import star.sort.

  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.types.

  public termLbl ::= .tLbl(string,integer).

  public data ::= .intgr(integer)
  | .bigi(bigint)
  | .flot(float)
  | .chr(char)
  | .strg(string)
  | .term(string,cons[data])
  | .symb(termLbl)
  | .clos(termLbl,data,tipe).

  public implementation display[termLbl] => {
    disp(.tLbl(Nm,Ar)) => "#(Nm)/$(Ar)".
  }

  public implementation sizeable[termLbl] => {
    size(.tLbl(_,Ar))=>Ar.

    isEmpty(.tLbl(_,Ar))=>Ar==0.
  }

  public implementation display[data] => let{.
    dispT(D) => case D in {
      | .intgr(Ix) => "$(Ix)"
      | .bigi(Ix) => "$(Ix)"
      | .flot(Dx) => disp(Dx)
      | .chr(Cx) => disp(Cx)
      | .strg(Sx) => disp(Sx)
      | .term(T,Args) where isTupleLbl(T) => "(#(dispTs(Args)))"
      | .term(Op,[]) => ".#(Op)"
      | .term(Op,Args) => ".#(Op)(#(dispTs(Args)))"
      | .symb(Sx) => "'$(Sx)'"
      | .clos(Lb,Fr,_) => "<$(Lb)\:#(dispT(Fr))>"
    }

    dispTs(Els) => interleave(Els//dispT,",")*.

    isTupleLbl(T) =>_str_start("()",T).
  .} in {
    disp(T) => dispT(T)
  }

  public implementation hashable[termLbl] => {
    hash(.tLbl(Nm,Ar))=>hash(Nm)*37+Ar.
  }

  public implementation hashable[data] => let{.
    hsh(D) => case D in {
      | .intgr(X) => X
      | .bigi(X) => hash(X)
      | .flot(X) => hash(X)
      | .chr(C) => hash(C)
      | .strg(S) => hash(S)
      | .symb(S) => hash(S)
      | .clos(Lb,Fr,_) => hash(Lb)*37+hash(Fr)
      | .term(Op,Args) =>
	foldRight((T,H)=>H*37+hsh(T),hash(Op)*37,Args)
    }
  .} in {
    hash(T) => hsh(T)
  }

  public implementation equality[termLbl] => {
    .tLbl(N1,A1)==.tLbl(N2,A2) => N1==N2 && A1==A2.
  }

  public implementation comp[termLbl] => let{
    lt(.tLbl(S1,A1),.tLbl(S2,A2)) => (S1<S2 || (S1==S2 && A1<A2)).
    ge(.tLbl(S1,A1),.tLbl(S2,A2)) => (S1>S2 || (S1==S2 && A1>=A2)).
  } in {
    (<) = lt.
    (>=) = ge
  }
  
  public implementation equality[data] => let{.
    eq(D1,D2) => case D1 in {
      | .intgr(X) => .intgr(Y).=D2 && X==Y
      | .bigi(X) => .bigi(Y).=D2 && X==Y
      | .flot(X) => .flot(Y).=D2 && X==Y
      | .chr(X) => .chr(Y).=D2 && X==Y
      | .strg(X) => .strg(Y).=D2 && X==Y
      | .symb(X) => .symb(Y).=D2 && X==Y
      | .clos(L1,F1,_) => .clos(L2,F2,_).=D2 && L1==L2 && eq(F1,F2)
      | .term(O1,A1) => .term(O2,A2).=D2 && O1==O2 && eqList(A1,A2)
      | _ default => .false
    }

    eqList(.nil,.nil)=>.true.
    eqList(.cons(E1,L1),.cons(E2,L2)) => eq(E1,E2) && eqList(L1,L2).
    eqList(_,_) default => .false.
  .} in {
    X==Y => eq(X,Y).
  }

  public mkTpl:(cons[data]) => data.
  mkTpl(A) where L.=size(A) => .term(tplLbl(L),A).

  public mkLst:(cons[data]) => data.
  mkLst(Els) => .term("[]",Els).

  public mkCons:(string,cons[data])=>data.
  mkCons(Nm,Args) => .term(Nm,Args).

  public isScalar:(data)=>boolean.
  isScalar(D) => case D in {
    | .intgr(_) => .true
    | .bigi(_) => .true
    | .flot(_) => .true
    | .chr(_) => .true
    | .strg(_) => .true
    | .symb(_) => .true
    | _ default => .false
  }

  public trueEnum:data.
  trueEnum = .term("true",[]).

  public falseEnum:data.
  falseEnum = .term("false",[]).



  public implementation coercion[locn,data]=>{
    _coerce(.locn(Pkg,Line,Col,Off,Ln))=>.some(mkTpl([.strg(Pkg),.intgr(Line),.intgr(Col),.intgr(Off),.intgr(Ln)])).
  }

  public implementation coercion[data,locn]=>{
    _coerce(.term("()5",[.strg(P),.intgr(L),.intgr(C),.intgr(O),.intgr(N)])) =>
      .some(.locn(P,L,C,O,N)).
  }

  public implementation all e ~~ coercion[e,data] |: coercion[option[e],data] => {
    _coerce(.none) => .some(.symb(.tLbl("none",0))).
    _coerce(.some(E)) => .some(mkCons("some",[E::data]))
  }

  public implementation all e ~~ coercion[data,e] |: coercion[data,option[e]] => {
    _coerce(.symb(.tLbl("none",0))) => .some(.none).
    _coerce(.term("some",[T])) => .some(.some(T::e))
  }
}
