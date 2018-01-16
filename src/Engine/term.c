//
// Created by Francis McCabe on 1/15/18.
//

#include "termP.h"
#include <assert.h>

labelPo C_LBL(termPo t){
  assert(hasClass(t,labelClass));

  return (labelPo)t;
}
