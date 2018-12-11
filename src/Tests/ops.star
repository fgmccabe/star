test.ops{
  import star.
  import star.parse.

  string = prse{ [0c"]; T<-strchr* ; [0c"] ^^ T::string }.

  strchr = _item.
}
