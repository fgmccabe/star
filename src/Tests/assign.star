test.assign{
  import star.

  alpha := 23.

  assert alpha!==23.

  checkInc:()=>boolean.
  checkInc() where _ .= (alpha := alpha!+1) => alpha!==24.

  assert checkInc().

  show alpha!.

  rec : {name: ref string. age:integer}.
  rec = {
    name := "".
    age = alpha!.
  }.

  assert (rec.name:="fred")=="fred".
  show rec.name!.
}
