star.compiler.leb{
  import star.
  import star.multi.

  -- Encode and decode leb sequences

  public encodeLeb:(integer)=>cons[integer].
  encodeLeb(Ix) => valof{
    byte = Ix .&. 0x7f;
    val = Ix .>>>. 7;
    sign = byte .&. 0x40;

    if (val==0 && sign == 0) || (val==-1 && sign!=0) then
      valis [byte]
    else{
      valis [byte .|. 0x80,..encodeLeb(val)]
    }
  }

  public decodeLeb:(cons[integer])=>(integer,cons[integer]).
  decodeLeb(Bytes) => decode(Bytes,0,0).

  decode([B,..Bs],SoFar,Shift) => valof{
    SoF = SoFar .|. ((B.&.0x7f).<<.Shift);
    if B.&.0x80 != 0 then{
      valis decode(Bs,SoF,Shift+7)
    } else if B .&. 0x40 != 0 then
      valis (SoF .|. (.~.0 .<<.Shift),Bs)
    else
    valis (SoF,Bs)
  }
}
