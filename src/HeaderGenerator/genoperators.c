#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <stringBuffer.h>
#include "trie.h"
#include "ooio.h"
#include "formioP.h"
#include "template.h"
#include "formexts.h"

#include "genoperators.h"
#include "parseOperators.h"

static retCode procEntries(void *n, void *r, void *c);

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
  genProlog, genStar
} genMode = genProlog;

typedef struct {
  char name[1024];
  char cmt[1024];
} TokenRecord, *tokenPo;

char *prefix = NULL;
char *templateFn = "starops.py.plate";
char *opers = "operators.json";

static triePo tokenTrie;
static poolPo opPool;
static hashPo operators;

static void initTries() {
  tokenTrie = emptyTrie();

  opPool = newPool(sizeof(TokenRecord), 128);
  operators = NewHash(128, (hashFun) uniHash, (compFun) uniCmp, NULL);

}

int getOptions(int argc, char **argv) {
  int opt;

  while ((opt = getopt(argc, argv, "pc:t:o:")) >= 0) {
    switch (opt) {
      case 'p':
        genMode = genProlog;
        break;
      case 'c':
        genMode = genStar;
        prefix = optarg;
        break;
      case 'o':
        opers = optarg;
        break;
      case 't':
        templateFn = optarg;
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

static void dumpFollows(char *prefix, codePoint last, void *V, void *cl) {
  FollowCl *c = (FollowCl *) cl;

  switch (genMode) {
    case genProlog:
      outMsg(c->out, "  follows('%P','%#c','%P%#c').\n", prefix, last, prefix, last);
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

static void dumpFinal(char *prefix, codePoint last, void *V, void *cl) {
  tokenPo op = (tokenPo) V;
  ioPo out = (ioPo) cl;

  if (op != NULL) {
    switch (genMode) {
      case genProlog:
        outMsg(out, "  final('%P%#c',\"%P\").\t /* %s */\n", prefix, last, op->name, op->cmt);
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
  initLogfile("-");
  installMsgProc('P', genQuotedStr);
  int narg = getOptions(argc, argv);

  if (narg < 0) {
    fprintf(stdout, "bad args");
    exit(1);
  } else if (parseOperators(opers) == Ok) {
    ioPo plate = openInFile(templateFn, utf8Encoding);

    ioPo out;

    if (narg < argc)
      out = openOutFile(argv[narg], utf8Encoding);
    else
      out = Stdout();

    // Load up the variable table
    bufferPo operBuff = newStringBuffer();
    ProcessTable(procEntries, operators, operBuff);

    integer len;
    char *allOps = getTextFromBuffer(&len, operBuff);

    hashPo vars = NewHash(8, (hashFun) uniHash, (compFun) uniCmp, NULL);
    hashPut(vars, "Operators", allOps);

    // dumpTrie(tokenTrie,Stdout());

    bufferPo followBuff = newStringBuffer();
    genFollows(O_IO(followBuff));
    hashPut(vars, "Follows", getTextFromBuffer(&len, followBuff));

    bufferPo finalBuff = newStringBuffer();
    genFinal(O_IO(finalBuff));
    hashPut(vars, "Final", getTextFromBuffer(&len, finalBuff));

    retCode ret = processTemplate(out, plate, vars, NULL, NULL);

    closeFile(out);
    exit(0);
  }
}

typedef struct _operator_ {
  char name[MAXLINE];
  OperatorStyle style;
  int left, prior, right;
} Operator, *opPo;

typedef struct _pair_ *pairPo;

typedef struct _pair_ {
  opPo op;
  pairPo next;
} Pair;

void genToken(char *op, char *cmt) {
  tokenPo tk = (tokenPo) allocPool(opPool);
  uniCpy(tk->name,NumberOf(tk->name),op);
  uniCpy(tk->cmt,NumberOf(tk->cmt),cmt);

  if (!isAlphaNumeric(op))
    addToTrie(op, tk, tokenTrie);
}

static opPo genOper(char *op, OperatorStyle style, int left, int prior, int right) {
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
  return oper;
}

void genInfix(char *op, int left, int prior, int right, char *cmt) {
  opPo oper = genOper(op, infixOp, left, prior, right);
  genToken(oper->name, cmt);
}

void genPrefix(char *op, int prior, int right, char *cmt) {
  opPo oper = genOper(op, prefixOp, 0, prior, right);
  genToken(oper->name, cmt);
}

void genPostfix(char *op, int left, int prior, char *cmt) {
  opPo oper = genOper(op, postfixOp, left, prior, 0);
  genToken(oper->name, cmt);
}

static retCode procOper(ioPo out, char *sep, opPo op) {
  switch (genMode) {
    case genProlog:
      switch (op->style) {
        case prefixOp:
          return outMsg(out, "%sprefixOp(%d, %d)", sep, op->prior, op->right);
        case infixOp:
          return outMsg(out, "%sinfixOp(%d, %d, %d)", sep, op->left, op->prior, op->right);
        case postfixOp:
          return outMsg(out, "%spostfixOp(%d, %d)", sep, op->left, op->prior);
        default:
          return Error;
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
    case genProlog:
      ret = outMsg(out, "  operator(\"%P\", [", nm);
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
      case genProlog:
        ret = outStr(out, "]).\n");
        break;
      default:
        break;
    }
  return ret;
}

static inline byte hxDgit(integer h) {
  if (h < 10)
    return (byte) (h | '0');
  else
    return (byte) (h + 'a' - 10);
}
