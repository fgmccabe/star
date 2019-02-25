test.dl{
  import star.
  import test.fact.

  -- Test iteration constructs

  SS : (integer,list[integer]) => action[(),boolean].
  SS(Ix,Lx) => do {
    if Ix in Lx then
      return true
    else
      return false
  }

  ST : (integer,list[integer]) => action[(),boolean].
  ST(Ix,Lx) => do {
    return Ix in Lx
  }

  show "SS(1,[3,2,1])=\(valof SS(1,[3,2,1]))".
  show "SS(4,[3,2,1])=\(valof SS(4,[3,2,1]))".

  show "ST(1,[3,2,1])=\(valof ST(1,[3,2,1]))".
  show "ST(4,[3,2,1])=\(valof ST(4,[3,2,1]))".
}
