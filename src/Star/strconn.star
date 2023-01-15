star.structured.conn{
  import star.

  nursery:all e ~~ (cons[task[e]]) => e.
  nursery(Ts) => _spawn((This) => valof{
      Q := Ts::qc[task[e]];

      while .true do{
	if [T,..Rs] ?= Q! then{
	  Q := Rs;
	  
	
      }
    }
  ).
}
