star.compiler.abstract{
  import star.
  import star.pkg.

  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.operators.

  public aType ::= .aNominal(string,cons[aType])
  | .aTypeVar(string)
  | .aTypeFun(string,integer)
  | .aTupleType(cons[aType])
  | .aRecordType(cons[string,aType],cons[aTypeRule])
  | .aReferenceType(aType)
  | .aFuctionType(cons[aType],aType)
  | .aThrowingType(cons[aType],aType,aType)
  | .aConstructorType(aType,aType)
  | .aConstrainedType(aConstraint,aType)
  | .aAllType(aType,aType)
  | .aExistType(aType,aType).
  -- .aEncapsulatedTyoe(aExp,string)

  public aConstraint ::= .aContract(string,cons[aType],cons[aType])
  | .aImplicit(string,aType)
  | .aField(string,aType).

  public aTypeRule ::= .aTypeAlias(aType,aType).
}
  

  


  
