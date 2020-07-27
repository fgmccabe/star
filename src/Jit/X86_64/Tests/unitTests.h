//
// Created by Francis McCabe on 7/17/20.
//

#ifndef STAR_UNITTESTS_H
#define STAR_UNITTESTS_H

#include "ooio.h"
#include "x86_64.h"

typedef retCode (*tester)();
retCode run_test(tester test);
retCode all_tests();
extern int tests_run;

x64CtxPo setupCtx(x64CtxPo ctx);

retCode cmpBytes(u8 *lft, u8 *rgt, integer count);
retCode checkResult(u8 *src, integer srcLen, x64CtxPo ctx);

#endif //STAR_UNITTESTS_H
