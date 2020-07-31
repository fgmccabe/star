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

retCode cmpBytes(byte *lft, byte *rgt, integer count);

#endif //STAR_UNITTESTS_H
