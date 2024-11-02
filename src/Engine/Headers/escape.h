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

typedef struct escape_record_ *escapePo;

typedef ReturnStatus (*libFun)(heapPo h);

escapePo getEscape(uint32 escNo);
char *escapeName(escapePo esc);
int32 escapeArity(escapePo esc);
libFun escapeFun(escapePo esc);

#endif //STAR_ESCAPE_H
