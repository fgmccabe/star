//
// Created by Francis McCabe on 1/8/20.
//

#include <regexpP.h>
#include "regexpP.h"
#include "pool.h"

static poolPo prpool = NULL;
static regexpPo emptyRegexp;

static regexpPo allocR(regexpOp op, codePoint chr, regexpPo left, regexpPo right);

static void initRegexp(void) {
  if (prpool == NULL) {
    prpool = newPool(sizeof(Regexp), 1024);
    emptyRegexp = allocR(empty, (codePoint) 0, Null, Null);
  }
}

regexpPo allocR(regexpOp op, codePoint chr, regexpPo left, regexpPo right) {
  regexpPo reg = allocPool(prpool);
  reg->op = op;
  switch (op) {
    case dot:
      break;
    case literal:
      reg->arg.chr = chr;
      break;
    case disj:
    case seq:
      reg->arg.choice.left = left;
      reg->arg.choice.right = right;
      break;
  }
  return reg;
}

regexpPo parseRegexp(char *ptn, integer ptnLength) {
  initRegexp();

}

static regexpPo pRegexp(char *ptn, integer *pos, integer length);

static regexpPo pRegexps(char *ptn, integer *pos, integer length) {
  regexpPo left = pRegexp(ptn, pos, length);
  while (*pos < length) {
    switch(ptn[*pos]){
      case '|':{
        regexpPo right = pRegexp(ptn, pos, length);
        left = allocR(disj, 0, left, right);
        continue;
      }
      case '*':{
        left = allocR(star,0,left,Null);
        continue;
      }
      case '+':{
        left = allocR(seq,0,left,allocR(star,0,left,Null));
        continue;
      }
      case ')':
        return left;
      default:{
        regexpPo right = pRegexp(ptn, pos, length);
        left = allocR(seq, 0, left, right);
        continue;
      }
    }
  }
}

regexpPo pRegexp(char *ptn, integer *pos, integer length) {
  char p = ptn[(*pos)++];
  switch (p) {
    case '.':
      return allocR(dot, (codePoint) 0, Null, Null);
    case '[': {
      char c = ptn[(*pos)++];
      regexpPo choice = emptyRegexp;
      while ((*pos)++ < length && c != ']') {
        regexpPo chr = allocR(literal, c, Null, Null);

        if (choice == emptyRegexp)
          choice = chr;
        else {
          choice = allocR(disj, (codePoint) 0, choice, chr);
        }
      }
      return choice;
    }
    case '(': {
      regexpPo inner = pRegexps(ptn, pos, length);
      if (ptn[++(*pos)] != ')')
        syserr("expecting ')");
      return inner;
    }
    case '\\':
      p = ptn[(*pos)++]; // fall through to literal case
    default:
      return allocR(literal, (codePoint) p, Null, Null);
  }
}

void closeRegexp(regexpPo reg) {
  switch (reg->op) {
    case empty:
      return;
    case dot:
    case literal:
      freePool(prpool, reg);
      return;
    case disj:
    case seq: {
      closeRegexp(reg->arg.choice.left);
      closeRegexp(reg->arg.choice.right);
      freePool(prpool, reg);
    }
    case star:{
      closeRegexp(reg->arg.choice.left);
      freePool(prpool, reg);
    }
  }
}

retCode matchRegexp(regexpPo r,ioPo in);

typedef struct {
  regexpPo state;
  integer inputCount;
} MatchState, *matchStatePo;

static retCode match(ioPo in,regexpPo r){
  bufferPo str = newStringBuffer();
  integer inputCount = 0;
  MatchState fringe[1024];
  integer fringeSize;

  fringe[0] = {r,0};
  fringeSize = 1;
}
