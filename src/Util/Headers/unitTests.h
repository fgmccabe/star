//
// Created by Francis McCabe on 7/17/20.
//

#ifndef STAR_UNITTESTS_H
#define STAR_UNITTESTS_H

#include "ooio.h"

typedef retCode (*tester)();
retCode run_test(tester test);
retCode all_tests();
extern int tests_run;

extern logical debugUnitTests;

retCode cmpBytes(byte *lft, byte *rgt, integer count);
retCode checkReslt(int64 test, int64 verify, char *msg);

#define negRet(Exp) STMT_WRAP({ retCode ret=(Exp); if(ret==Ok)return Fail; })

#endif //STAR_UNITTESTS_H
