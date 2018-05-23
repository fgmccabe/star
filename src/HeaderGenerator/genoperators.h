//
// Created by Francis McCabe on 5/22/18.
//

#ifndef CAFE_GENOPERATORS_H
#define CAFE_GENOPERATORS_H

typedef enum {
  tokenOnly,
  infixOp,
  prefixOp,
  postfixOp,
  unknownOperatorStyle
} OperatorStyle;

void genInfix(char *op, int left, int prior, int right, char *cmt);
void genPrefix(char *op, int prior, int right, char *cmt);
void genPostfix(char *op, int left, int prior, char *cmt);
void genToken(char *op, char *cmt);

#endif //CAFE_GENOPERATORS_H
