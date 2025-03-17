//
// Created by Francis McCabe on 3/14/25.
//

#ifndef STAR_CODETREEP_H
#define STAR_CODETREEP_H

#include "codeTree.h"

typedef struct CodeNode {
  int32 start;
  int32 end;
  codeTreePo left;
  codeTreePo right;
} CodeNodeRecord;

#endif //STAR_CODETREEP_H
