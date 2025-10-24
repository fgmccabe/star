//
// Created by Francis McCabe on 10/24/25.
//

#ifndef STAR_SHUFFLETESTS_H
#define STAR_SHUFFLETESTS_H

#include "macros.h"
#include "sort.h"

void showGroups(ArgSpec defs[], int32 groups, int32 arity);
void showDefs(ArgSpec defs[], int32 arity);
void collectGroup(argSpecPo args, int32 arity, int32 groupNo, argSpecPo* group);

#endif //STAR_SHUFFLETESTS_H