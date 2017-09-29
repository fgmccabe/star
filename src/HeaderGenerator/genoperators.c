#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <stringBuffer.h>
#include "trie.h"
#include "ooio.h"
#include "template.h"

/* Generate a Python module or Star package, that knows about the standard operators */

static void genInfix(hashPo operators, char *op, int left, int prior, int right, char *cmt);
static void genPrefix(hashPo operators, char *op, int prior, int right, char *cmt);
static void genPostfix(hashPo operators, char *op, int left, int prior, char *cmt);
static void genToken(char *op, char *cmt);
static retCode procEntries(void *n, void *r, void *c);

#undef lastOp
#define lastOp sep = "\t";

static char *pC(char *buff, long *ix, char c);

static char *pS(char *buff, char *s) {
  char *p = buff;
  long ix = 0;

  while (*s != '\0') {
    pC(buff, &ix, *s++);
  }
  buff[ix] = '\0';
  return buff;
}

static char *pC(char *buff, long *ix, char c) {
  switch (c) {
    case '\'':
    case '"':
    case '\\':
      buff[(*ix)++] = '\\';
    default:
      buff[(*ix)++] = c;
      buff[*ix] = '\0';
  }
  return buff;
}

enum {
  genPython, genProlog, genStar
} genMode = genProlog;

typedef struct {
  char *name;
  char *cmt;
} TokenRecord, *tokenPo;

char *prefix = NULL;
char *template = "starops.py.plate";
static triePo tokenTrie;
static poolPo opPool;

static void initTries() {
  tokenTrie = emptyTrie();

  opPool = newPool(sizeof(TokenRecord), 128);
}

int getOptions(int argc, char **argv) {
  int opt;
  extern char *optarg;
  extern int optind;

  while ((opt = getopt(argc, argv, "pc:t:y")) >= 0) {
    switch (opt) {
      case 'p':
        genMode = genProlog;
        break;
      case 'y':
        genMode = genPython;
        break;
      case 'c':
        genMode = genStar;
        prefix = optarg;
        break;
      case 't':
        template = optarg;
        break;
      default:;
    }
  }
  return optind;
}

typedef struct {
  ioPo out;
  char *sep;
  char last[MAXLINE];
} FollowCl;

static void dumpFollows(char *K, void *V, void *cl) {
  FollowCl *c = (FollowCl *) cl;

  char *prefix = (char *) calloc(sizeof(char), strlen(K));
  for (int ix = 0; ix < strlen(K) - 1; ix++)
    prefix[ix] = K[ix];
  char last = K[strlen(K) - 1];
  char b1[32], b2[32], b3[32];

  pS(&b1[0], prefix);
  long ix = 0;
  pC(b2, &ix, last);
  pS(b3, K);

  switch (genMode) {
    case genPython:
      if (uniCmp(prefix, c->last) != same) {
        uniCpy(c->last, NumberOf(c->last), prefix);
        outMsg(c->out, "%s    '%s': {'%s': '%s'", c->sep, b1, b2, b3);
        c->sep = "},\n";
      } else {
        outMsg(c->out, ", '%s': '%s'", b2, b3);
      }
      break;
    case genProlog:
      outMsg(c->out, "  follows('%s','%s','%s').\n", b1, b2, b3);
      break;
    case genStar:
      break;
  }

}

static void genFollows(ioPo out) {
  FollowCl cl = {out, "", "##"};
  processTrie(tokenTrie, dumpFollows, &cl, True);
  outMsg(out, "%s", cl.sep);
}

static void dumpFinal(char *K, void *V, void *cl) {
  tokenPo op = (tokenPo) V;
  ioPo out = (ioPo) cl;
  char b1[32], b2[32];

  if (op != NULL) {
    switch (genMode) {
      case genPython:
        outMsg(out, "'%#s', ", op->name);
        break;

      case genProlog:
        outMsg(out, "  final('%s',\"%s\").\t /* %s */\n", pS(b1, K), pS(b2, op->name), op->cmt);
        break;
      case genStar:
        break;
    }
  }
}

static void genFinal(ioPo out) {
  processTrie(tokenTrie, dumpFinal, out, False);
}

logical isAlphaNumeric(char *p) {
  if (*p != '\0' && isalpha(*p++)) {
    while (*p != '\0' && isalnum(*p++));
    return True;
  }
  return False;
}

int main(int argc, char **argv) {
  initTries();
  int narg = getOptions(argc, argv);

  if (narg < 0) {
    fprintf(stdout, "bad args");
    exit(1);
  } else {
    ioPo plate = openInFile(template, utf8Encoding);
    hashPo operators = NewHash(128, (hashFun) uniHash, (compFun) uniCmp, NULL);

    ioPo out;

    if (narg < argc)
      out = openOutFile(argv[narg], utf8Encoding);
    else
      out = OpenStdout();

#undef lastInfOp
#define lastInfOp
#undef lastPreOp
#define lastPreOp
#undef lastPostOp
#define lastPostOp

#undef infixOp
#define infixOp(op, left, prior, right, cmt) genInfix(operators,op,left,prior,right,cmt);

#undef prefixOp
#define prefixOp(op, prior, right, cmt) genPrefix(operators,op,prior,right,cmt);

#undef postfixOp
#define postfixOp(op, left, prior, cmt) genPostfix(operators,op,left,prior,cmt);

#undef token
#define token(op, cmt) genToken(op,cmt);

#include "operators.h"

    // Load up the variable table
    bufferPo operBuff = newStringBuffer();
    ProcessTable(procEntries, operators, operBuff);

    long len;
    char *allOps = getTextFromBuffer(&len, operBuff);

    hashPo vars = NewHash(8, (hashFun) uniHash, (compFun) uniCmp, NULL);
    hashPut(vars, "Operators", allOps);

    // dumpTrie(tokenTrie,OpenStdout());

    bufferPo followBuff = newStringBuffer();
    genFollows(O_IO(followBuff));
    hashPut(vars, "Follows", getTextFromBuffer(&len, followBuff));

    bufferPo finalBuff = newStringBuffer();
    genFinal(O_IO(finalBuff));
    hashPut(vars, "Final", getTextFromBuffer(&len, finalBuff));

    processTemplate(out, plate, vars, NULL, NULL);

    flushFile(out);
    closeFile(out);
    exit(0);
  }
}

typedef enum {
  prefixOp,
  infixOp,
  postfixOp
} operatorStyle;

typedef struct _operator_ {
  char name[MAXLINE];
  operatorStyle style;
  int left, prior, right;
} Operator, *opPo;

typedef struct _pair_ *pairPo;

typedef struct _pair_ {
  opPo op;
  pairPo next;
} Pair;

static void genToken(char *op, char *cmt) {
  tokenPo opRecord = (tokenPo) allocPool(opPool);
  opRecord->name = op;
  opRecord->cmt = cmt;
  if (!isAlphaNumeric(op))
    addToTrie(op, opRecord, tokenTrie);
}

static void genOper(hashPo operators, char *op, operatorStyle style, int left, int prior, int right) {
  opPo oper = (opPo) malloc(sizeof(Operator));
  strcpy(oper->name, op);
  oper->style = style;
  oper->left = left;
  oper->prior = prior;
  oper->right = right;

  pairPo p = (pairPo) malloc(sizeof(Pair));
  p->op = oper;
  p->next = (pairPo) hashGet(operators, oper->name);

  hashPut(operators, oper->name, p);
}

static void genInfix(hashPo operators, char *op, int left, int prior, int right, char *cmt) {
  genOper(operators, op, infixOp, left, prior, right);
  genToken(op, cmt);
}

static void genPrefix(hashPo operators, char *op, int prior, int right, char *cmt) {
  genOper(operators, op, prefixOp, 0, prior, right);
  genToken(op, cmt);
}

static void genPostfix(hashPo operators, char *op, int left, int prior, char *cmt) {
  genOper(operators, op, postfixOp, left, prior, 0);
  genToken(op, cmt);
}

static retCode procOper(ioPo out, char *sep, opPo op) {
  switch (genMode) {
    case genPython:
      switch (op->style) {
        case prefixOp:
          return outMsg(out, "%sPrefixOp(%d, %d)", sep, op->prior, op->right);
        case infixOp:
          return outMsg(out, "%sInfixOp(%d, %d, %d)", sep, op->left, op->prior, op->right);
        case postfixOp:
          return outMsg(out, "%sPostfixOp(%d, %d)", sep, op->left, op->prior);
      }
    case genProlog:
      switch (op->style) {
        case prefixOp:
          return outMsg(out, "%sprefixOp(%d, %d)", sep, op->prior, op->right);
        case infixOp:
          return outMsg(out, "%sinfixOp(%d, %d, %d)", sep, op->left, op->prior, op->right);
        case postfixOp:
          return outMsg(out, "%spostfixOp(%d, %d)", sep, op->left, op->prior);
      }
    case genStar:
      return Error;
  }
}

static retCode procEntries(void *n, void *r, void *c) {
  ioPo out = (ioPo) c;
  pairPo p = (pairPo) r;
  char *nm = (char *) n;

  retCode ret = Ok;
  char *sep = "";

  switch (genMode) {
    case genPython:
      ret = outMsg(out, "    '%#s': [", nm);
      break;
    case genProlog:
      ret = outMsg(out, "  operator(\"%#s\", [", nm);
      break;
    default:
      break;
  }

  while (ret == Ok && p != NULL) {
    ret = procOper(out, sep, p->op);
    p = p->next;
    sep = ", ";
  }

  if (ret == Ok)
    switch (genMode) {
      case genPython:
        ret = outStr(out, "],\n");
        break;
      case genProlog:
        ret = outStr(out, "]).\n");
        break;
      default:
        break;
    }
  return ret;
}
