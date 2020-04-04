//
// Created by Francis McCabe on 4/1/20.
//

#include "jitP.h"

#include "pool.h"

static poolPo compilerPool;

void initJit(){
  compilerPool = newPool(sizeof(JitCompiler),8);
}
