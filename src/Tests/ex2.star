test.ex2{
  main:()=>().
  main()=>valof{
    try{
      _logmsg(_stringOf(sqrt(10.4),0));
      _logmsg(_stringOf(sqrt(-1.0),0));
    } catch {
      ErrCode => { _logmsg("out with a #(dspEr(ErrCode))"); valis () }
    };
  }

  dspEr(.eINTRUPT) => "eINTRUPT".
  dspEr(.eNOFILE) => "eNOFIL".
  dspEr(.eNOTDIR) => "eNOTDIR".
  dspEr(.eNOTFND) => "eNOTFND".
  dspEr(.eINVAL) => "eINVAL".
  dspEr(.eRANGE) => "eRANGE".
  dspEr(.eNOPERM) => "eNOPERM".
  dspEr(.eFAIL) => "eFAIL".
  dspEr(.eIOERROR) => "eIOERROR".
  dspEr(.eCONNECT) => "eCONNECT".
  dspEr(.eDEAD) => "eDEAD".
  dspEr(.divZero) => "divZero".
  dspEr(.noValue) => "noValue".
  dspEr(.hasValue) => "hasValue".
  dspEr(.eof) => "eof".
}
