//
// Created by Francis McCabe on 5/22/18.
//

#ifndef STAR_GENOPERATORS_H
#define STAR_GENOPERATORS_H

typedef enum {
  tokenOnly,
  infixOp,
  prefixOp,
  postfixOp,
  brackets,
  unknownOperatorStyle
} OperatorStyle;

void genInfix(char *op, char *ast, int left, int prior, int right, logical isKeyword, char *cmt);
void genPrefix(char *op, char *ast, int prior, int right, logical isKeyword, char *cmt);
void genPostfix(char *op, char *ast, int left, int prior, logical isKeyword, char *cmt);
void genBracket(char *op, integer prior, char *left, char *right, char *cmt);
void genToken(char *op, char *cmt);

#endif //STAR_GENOPERATORS_H
