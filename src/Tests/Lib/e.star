test.e{
  equality@"defines functions associated with semantic equality".
  public contract all x ~~ equality[x] ::= {
    (==)@"semantic equality is defined explicitly".
  }

  (=!=)@"semantic inequality defined in terms of equality".
}
