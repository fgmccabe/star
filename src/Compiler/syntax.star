star.compiler.syntax{
  import star.
  import star.compiler.ast.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.token.
  import star.compiler.errors.

  type --> nominalType.
  type --> typeVariable.
  type --> tupleType.
  type --> recordType.
  type --> referenceType.
  type --> functionType.
  type --> constructorType.
  type --> constrainedType.
--  type --> encapsulatedType.

  nominalType --> identifier.
  nominalType --> identifier, lb, type * comma, rb.

  lp --> [.tok(_,.lftTok("()"))].
  rp --> [.tok(_,.rgtTok("()"))].
  lb --> [.tok(_,.lftTok("[]"))].
  rb --> [.tok(_,.rgtTok("[]"))].
  lbr --> [.tok(_,.lftTok("{}"))].
  rbr --> [.tok(_,.rgtTok("{}"))].

  comma --> [.tok(_,.idTok(","))].
  
  
}
