//
// Created by Francis McCabe on 2/11/24.
//

#ifndef STAR_ESCAPE_H
#define STAR_ESCAPE_H

typedef enum {
  Normal,
  Abnormal
} escapeReturn;

typedef struct return_code_ {
  escapeReturn ret;
  termPo cont;
  termPo result;
} ReturnStatus;

#endif //STAR_ESCAPE_H
