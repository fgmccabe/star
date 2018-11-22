//
// Created by Francis McCabe on 11/21/18.
//

#include <assert.h>
#include <globals.h>
#include "arith.h"
#include "turm.h"

// Implement a hash lookup of a tuple

ReturnStatus g__tpl_hash_get(processPo P, ptrPo tos) {
  normalPo tpl = C_TERM(tos[0]);
  integer tplLen = termArity(tpl);
  termPo key = tos[1];

  integer kx = termHash(key);

  for (integer ix = kx; ix < tplLen; ix++) {
    termPo bkt = nthArg(tpl, ix);

    if (bkt == voidEnum || !isNormalPo(bkt)) {
      ReturnStatus ret = {.ret=Ok, .result=voidEnum};
      return ret;
    } else {
      normalPo pair = C_TERM(bkt);
      if (compareTerm(nthArg(pair, 0), key) == same) {
        ReturnStatus ret = {.ret=Ok, .result=nthArg(pair, 1)};
        return ret;
      }
    }
  }
  for (integer ix = 0; ix < kx; ix++) {
    termPo bkt = nthArg(tpl, ix);

    if (bkt == voidEnum || !isNormalPo(bkt)) {
      ReturnStatus ret = {.ret=Ok, .result=voidEnum};
      return ret;
    } else {
      normalPo pair = C_TERM(bkt);
      if (compareTerm(nthArg(pair, 0), key) == same) {
        ReturnStatus ret = {.ret=Ok, .result=nthArg(pair, 1)};
        return ret;
      }
    }
  }

  ReturnStatus ret = {.ret=Ok, .result=voidEnum};
  return ret;

}
