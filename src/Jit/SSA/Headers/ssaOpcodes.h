//
// Created by Francis McCabe on 1/15/26.
//

#ifndef STAR_SSAOPCODES_H
#define STAR_SSAOPCODES_H

typedef enum {
  ssHalt,
  sAbort,

  sCall,
  sOCall,
  sEscape,
  sXCall,
  sXOCall,
  sXEscape,
  sTCall,
  sTOCall,
  sEntry,

  sRet,
  sXRet,

  sBlock,
  sValof,
  sBreak,
  sResult,
  sLoop,

  sFiber,
  sSuspend,
  sResume,
  sRetire,
  sUnderflow,

  sMvL,
  sMv,
  sSV,

  sLdG,
  sStG,

  sSav,
  sLdSav,
  sTstSav,
  sStSav,
  sTSav,

  sCell,
  sGet,
  sAssign,

  sCLbl,
  sCInt,
  sCChar,
  sCFlt,
  sCLit,
  sNth,
  sStNth,

  sIf,
  sIfNot,

  sICase,
  sCase,
  sIxCase,

  sIAdd,
  sISub,
  sIMul,
  sIDiv,
  sIMod,
  sIAbs,

  sIEq,
  sILt,
  sIGe,

  sCEq,
  sCLt,
  sCGe,

  sBAnd,
  sBOr,
  sBXor,
  sBLsl,
  sBLsr,
  sBAsr,
  sBNot,

  sFAdd,
  sFSub,
  sFMul,
  sFDiv,
  sFMod,
  sFAbs,

  sFEq,
  sFLt,
  sFGe,

  sAlloc,
  sClosure,

  sFrame,

  sLine,
  sBind,
  sdBug,
} ssaOpcode;

#endif //STAR_SSAOPCODES_H
