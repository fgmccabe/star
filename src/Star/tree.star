star.tree{
  import star.

  public all e ~~ tree[e] ::= eTree | node(tree[e],e,tree[e]).
  
}
