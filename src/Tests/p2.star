test.p2{
  import star.
  import star.parse.

  pos:parser[integer,integer].
  pos --> [X where X>0] ^^ X.

  show disp(parse(pos,[1,2,3]))::string.
  show disp(parse(pos,[0,2,3]))::string.

  assert parse(pos,[0,2]) == [].
}
