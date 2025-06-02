star.compiler.meta{
  import star.

  import star.pkg.

  import star.compiler.ast.
  import star.compiler.data.
  import star.compiler.location.
  import star.compiler.types.

  public decl ::= .implDec(option[locn],string,string,tipe) |
  .accDec(option[locn],tipe,string,string,tipe) |
  .updDec(option[locn],tipe,string,string,tipe) |
  .conDec(option[locn],string,string,typeRule) |
  .tpeDec(option[locn],string,tipe,typeRule,indexMap) |
  .varDec(option[locn],string,string,tipe) |
  .funDec(option[locn],string,string,tipe) |
  .cnsDec(option[locn],string,string,tipe).

  public importSpec ::= .pkgImp(option[locn],visibility,pkg).

  public visibility ::= .priVate | .deFault | .pUblic | .transItive.

  public implementation display[visibility] => {
    disp(.priVate) => "private".
    disp(.pUblic) => "public".
    disp(.deFault) => "default".
    disp(.transItive) => "transitive".
  }

  public implementation comp[visibility] => {
    .priVate < .pUblic => .true.
    .priVate < .transItive => .true.
    .priVate < .deFault => .true.
    .deFault < .pUblic => .true.
    .deFault < .transItive => .true.
    .pUblic < .transItive => .true.
    _ < _ default => .false.

    .priVate >= .priVate => .true.
    .deFault >= .priVate => .true.
    .deFault >= .deFault => .true.
    .pUblic >= .priVate => .true.
    .pUblic >= .deFault => .true.
    .pUblic >= .transItive => .true.
    .pUblic >= .pUblic => .true.
    _ >= _ default => .false.
  }

  public implementation equality[visibility] => {
    .priVate == .priVate => .true.
    .deFault == .deFault => .true.
    .pUblic == .pUblic => .true.
    .transItive == .transItive => .true.
    _ == _ default => .false.
  }

  public pkgSpec ::= pkgSpec{
    pkg : pkg.
    imports : cons[importSpec].
    exports: cons[decl]}.

  public indexMap ~> map[termLbl,integer].

  public implementation display[importSpec] => let{
    dispSpc(.pkgImp(Lc,Vi,Pk)) => "$(Vi) import $(Pk)"
  } in {
    disp(S) => dispSpc(S)
  }

  public implementation display[pkgSpec] => {
    disp(P) =>
      "Package: $(P.pkg)\n  imports=#(interleave(P.imports//disp,"\n")*)\n  exports=#(interleave(P.exports//disp,"\n")*)".
  }

  public implementation display[decl] => {
    disp(Dc) => case Dc in {
      | .conDec(_,Nm,_,RlTp) => "Ctrct #(Nm)\:$(RlTp)"
      | .implDec(_,Nm,ImplNm,ImplTp) => "Impl #(Nm)[#(ImplNm)]\:$(ImplTp)"
      | .accDec(_,Tp,Fld,Fun,FunTp) => "Acc $(Tp).#(Fld) using #(Fun)\:$(FunTp)"
      | .updDec(_,Tp,Fld,Fun,FunTp) => "Upd $(Tp).#(Fld) using #(Fun)\:$(FunTp)"
      | .tpeDec(_,Nm,_,TpRl,Map) => "Type #(Nm)\::$(TpRl) $(Map)"
      | .varDec(_,Nm,FullNm,Tp) => "Var #(Nm)[#(FullNm)]\:$(Tp)"
      | .funDec(_,Nm,FullNm,Tp) => "Fun #(Nm)[#(FullNm)]\:$(Tp)"
      | .cnsDec(_,Nm,FullNm,Tp) => "Con #(Nm)[#(FullNm)]\:$(Tp)"
    }
  }

  public contract all e ~~ hasName[e] ::= {
    lclName:(e) => option[string].
    fullName:(e) => option[string].
  }

  public implementation hasName[decl] => {
    lclName(D) => case D in {
      | .conDec(_,Nm,_,_) => .some(Nm)
      | .implDec(_,Nm,_,_) => .some(Nm)
      | .tpeDec(_,Nm,_,TpRl,_) => .some(Nm)
      | .varDec(_,Nm,FullNm,Tp) => .some(Nm)
      | .funDec(_,Nm,FullNm,Tp) => .some(Nm)
      | .cnsDec(_,Nm,FullNm,Tp) => .some(Nm)
      | _ default => .none
    }.
    fullName(D) => case D in {
      | .conDec(_,_,FullNm,_) => .some(FullNm)
      | .implDec(_,_,FullNm,_) => .some(FullNm)
      | .tpeDec(_,Nm,Tp,_,_) => .some(tpName(Tp))
      | .varDec(_,Nm,FullNm,Tp) => .some(FullNm)
      | .funDec(_,Nm,FullNm,Tp) => .some(FullNm)
      | .cnsDec(_,Nm,FullNm,Tp) => .some(FullNm)
      | _ default => .none
    }.
  }
  
  public mainDefined:(cons[decl])=>boolean.
  mainDefined(Decs) => {? .funDec(_,"_main",_,_) in Decs ?}.

  public codePolicy ::= .hardDefinition | .softDefinition.

  public implementation display[codePolicy] => {
    disp(.hardDefinition) => "hard".
    disp(.softDefinition) => "soft".
  }
}
