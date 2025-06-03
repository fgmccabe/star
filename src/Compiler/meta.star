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

  public stdTypes:cons[decl].
  stdTypes = [.tpeDec(.none,"integer",intType,.typeExists(intType,emptyFace),[]),
    .tpeDec(.none,"bigint",bigintType,.typeExists(bigintType,emptyFace),[]),
    .tpeDec(.none,"float",fltType,.typeExists(fltType,emptyFace),[]),
    .tpeDec(.none,"boolean",boolType,.typeExists(boolType,emptyFace),[
	.tLbl("false",0)->0, .tLbl("true",0)->1]),
    .cnsDec(.none,"true","true",enumType(boolType)),
    .cnsDec(.none,"false","false",enumType(boolType)),
    .tpeDec(.none,"char",chrType,.typeExists(chrType,emptyFace),[]),
    .tpeDec(.none,"string",strType,.typeExists(strType,emptyFace),[]),
    .tpeDec(.none,"cons",.tpFun("cons",1),
      .allRule(.kVar("e"),
	.typeExists(lstType(.kVar("e")),emptyFace)),[.tLbl("cons",2)->0,.tLbl("nil",0)->1]),
    .cnsDec(.none,"cons","cons",
      .allType(.kVar("e"),consType(.tupleType([.kVar("e"),lstType(.kVar("e"))]),
	  lstType(.kVar("e"))))),
    .cnsDec(.none,"nil","nil",
      .allType(.kVar("e"),consType(.tupleType([]),lstType(.kVar("e"))))),
    .tpeDec(.none,"option",.tpFun("option",1),
      .allRule(.kVar("e"),
	.typeExists(optType(.kVar("e")),emptyFace)),[.tLbl("none",0)->0,.tLbl("some",1)->1]),
    .cnsDec(.none,"some","some",
      .allType(.kVar("e"),consType(.tupleType([.kVar("e")]),
	  optType(.kVar("e"))))),
    .cnsDec(.none,"none","none",
      .allType(.kVar("e"),consType(.tupleType([]),optType(.kVar("e"))))),

    .tpeDec(.none,"thunk",.tpFun("thunk",1),
      .allRule(.kVar("e"),
	.typeExists(thunkType(.kVar("e")),emptyFace)),[]),
    .tpeDec(.none,"fiber",.tpFun("fiber",2),
      .allRule(.kVar("a"),
	.allRule(.kVar("e"),
	  .typeExists(makeTpExp("fiber",
	      [.kVar("a"),.kVar("e")]),emptyFace))),[]),
    .tpeDec(.none,"ioHandle",ioType,.typeExists(ioType,emptyFace),[]),
    .tpeDec(.none,"future",.tpFun("future",2),
      .allRule(.kVar("v"),
	.allRule(.kVar("e"),
	  .typeExists(futureType(.kVar("v"),.kVar("e")),
	    emptyFace))),[]),
    .tpeDec(.none,"errorCode",errorCodeType,.typeExists(errorCodeType,emptyFace),
      [.tLbl("divZero",0)->0,
	.tLbl("eCONNECT",0)->1,
	.tLbl("eDEAD",0)->2,
	.tLbl("eFAIL",0)->3,
	.tLbl("eINTRUPT",0)->4,
	.tLbl("eIOERROR",0)->5,
	.tLbl("eNOFILE",0)->6,
	.tLbl("eNOPERM",0)->7,
	.tLbl("eNOTDIR",0)->8,
	.tLbl("eNOTFND",0)->9,
	.tLbl("eINVAL",0)->10,
	.tLbl("eRANGE",0)->11,
	.tLbl("eEOF",0)->12,
	.tLbl("hasValue",0)->13,
	.tLbl("noValue",0)->14]),
    .cnsDec(.none,"eINTRUPT","eINTRUPT",enumType(errorCodeType)),
    .cnsDec(.none,"eNOTDIR","eNOTDIR",enumType(errorCodeType)),
    .cnsDec(.none,"eNOFILE","eNOFILE",enumType(errorCodeType)),
    .cnsDec(.none,"eNOTFND","eNOTFND",enumType(errorCodeType)),
    .cnsDec(.none,"eINVAL","eINVAL",enumType(errorCodeType)),
    .cnsDec(.none,"eRANGE","eRANGE",enumType(errorCodeType)),
    .cnsDec(.none,"eNOPERM","eNOPERM",enumType(errorCodeType)),
    .cnsDec(.none,"eFAIL","eFAIL",enumType(errorCodeType)),
    .cnsDec(.none,"eIOERROR","eIOERROR",enumType(errorCodeType)),
    .cnsDec(.none,"eCONNECT","eCONNECT",enumType(errorCodeType)),
    .cnsDec(.none,"eDEAD","eDEAD",enumType(errorCodeType)),
    .cnsDec(.none,"divZero","divZero",enumType(errorCodeType)),
    .cnsDec(.none,"noValue","noValue",enumType(errorCodeType)),
    .cnsDec(.none,"hasValue","hasValue",enumType(errorCodeType)),
    .cnsDec(.none,"eEOF","eEOF",enumType(errorCodeType))
  ]
}
