book.example.map.city{
  import star.

  public city ::= city{
    name:string.
    county : string.
    pop : integer
  }

  public implementation display[city] => {
    disp(C) => "#(C.name) in #(C.county)".
  }

  public neighnor ::= .neighbor(string,string,integer).

  public implementation display[neighbor] => {
    disp(.neighbor(F,T,D)) => "#(F) ~ #(T) = $(D)".
  }

}
