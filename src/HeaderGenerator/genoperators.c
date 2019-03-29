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

static retCode procOperator(void *n, void *r, void *c);
static retCode procBrackets(void *n, void *r, void *c);
static retCode genTexiStr(ioPo f, void *data, long depth, long precision, logical alt);

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
  genProlog, genStar, genTexi
} genMode = genProlog;

typedef struct {
  char name[1024];
  char cmt[1024];
} TokenRecord, *tokenPo;

char *prefix = NULL;
char *templateFn = "starops.py.plate";
char *opers = "operators.json";
char date[MAXLINE] = "";

typedef struct {
  char name[16];
  char left[16];
  char right[16];
  integer priority;
} BrktRecord, *bracketPo;

static triePo tokenTrie;
static poolPo opPool;
static hashPo operators;
static hashPo bracketTbl;

static void initTries() {
  tokenTrie = emptyTrie();

  opPool = newPool(sizeof(TokenRecord), 128);
  operators = NewHash(128, (hashFun) uniHash, (compFun) uniCmp, NULL);
  bracketTbl = NewHash(128, (hashFun) uniHash, (compFun) uniCmp, NULL);
}

int getOptions(int argc, char **argv) {
  int opt;

  while ((opt = getopt(argc, argv, "psit:o:d:D")) >= 0) {
    switch (opt) {
      case 'p':
        genMode = genProlog;
        break;
      case 's':
        genMode = genStar;
        break;
      case 'i':
        genMode = genTexi;
        break;
      case 'o':
        opers = optarg;
        break;
      case 't':
        templateFn = optarg;
        break;
      case 'd':
        uniCpy(date, NumberOf(date), optarg);
        break;
      case 'D':
        traceParse = True;
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
      outMsg(c->out, "  follows(\"%P\",0c%#c) => some(\"%P%#c\").\n", prefix, last, prefix, last);
      break;
    case genTexi:
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
        outMsg(out, "  final(\"%P\") => true.  /* %s */\n", op->name, op->cmt);
        break;
      case genTexi:
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
  installMsgProc('I', genTexiStr);

  int narg = getOptions(argc, argv);

  if (narg < 0) {
    fprintf(stdout, "bad args");
    exit(1);
  } else if (parseOperators(opers) == Ok) {
    if (uniStrLen(date) == 0) {
      time_t rawtime;
      time(&rawtime);
      struct tm *timeinfo = localtime(&rawtime);

      strftime(date, NumberOf(date), "%c", timeinfo);
    }
    ioPo plate = openInFile(templateFn, utf8Encoding);

    ioPo out;

    if (narg < argc)
      out = openOutFile(argv[narg], utf8Encoding);
    else
      out = Stdout();

    // Load up the variable table
    bufferPo operBuff = newStringBuffer();
    ProcessTable(procOperator, operators, operBuff);

    integer len;
    char *allOps = getTextFromBuffer(operBuff, &len);

    hashPo vars = NewHash(8, (hashFun) uniHash, (compFun) uniCmp, NULL);
    hashPut(vars, "Operators", allOps);

    bufferPo bracketBuff = newStringBuffer();
    ProcessTable(procBrackets, bracketTbl, bracketBuff);

    char *allBkts = getTextFromBuffer(bracketBuff, &len);
    hashPut(vars, "Brackets", allBkts);

    hashPut(vars, "Date", date);

    // dumpTrie(tokenTrie,Stdout());

    bufferPo followBuff = newStringBuffer();
    genFollows(O_IO(followBuff));
    hashPut(vars, "Follows", getTextFromBuffer(followBuff, &len));

    bufferPo finalBuff = newStringBuffer();
    genFinal(O_IO(finalBuff));
    hashPut(vars, "Final", getTextFromBuffer(finalBuff, &len));

    retCode ret = processTemplate(out, plate, vars, NULL, NULL);

    flushOut();
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
  uniCpy(tk->name, NumberOf(tk->name), op);
  uniCpy(tk->cmt, NumberOf(tk->cmt), cmt);

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

void genBracket(char *op, integer prior, char *left, char *right, char *cmt) {
  bracketPo bkt = (bracketPo) malloc(sizeof(BrktRecord));

  strcpy(bkt->name, op);
  strcpy(bkt->left, left);
  strcpy(bkt->right, right);
  bkt->priority = prior;

  hashPut(bracketTbl, bkt->name, bkt);

  genToken(bkt->left, cmt);
  genToken(bkt->right, cmt);
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
      switch (op->style) {
        case prefixOp:
          return outMsg(out, "%sprefixOp(%d,%d)", sep, op->prior, op->right);
        case infixOp:
          return outMsg(out, "%sinfixOp(%d,%d,%d)", sep, op->left, op->prior, op->right);
        case postfixOp:
          return outMsg(out, "%spostfixOp(%d,%d)", sep, op->left, op->prior);
        default:
          return Error;
      }
    case genTexi:
      return Ok;
  }
}

static retCode procOperator(void *n, void *r, void *c) {
  ioPo out = (ioPo) c;
  pairPo p = (pairPo) r;
  char *nm = (char *) n;

  retCode ret = Ok;
  char *sep = "";

  switch (genMode) {
    case genProlog:
      ret = outMsg(out, "  operator(\"%P\", [", nm);
      break;
    case genStar:

      ret = outMsg(out, "  oper(\"%P\") => [", nm);
      break;
    case genTexi: {
      while (p != NULL && ret == Ok) {
        opPo op = p->op;
        switch (op->style) {
          case prefixOp: {
            char *type = (op->right == op->prior ? "associative" : "non-associative");
            ret = outMsg(out, "@item @code{%I}\n@tab %s prefix\n@tab %d\n", nm, type, op->prior);
            break;
          }
          case infixOp: {
            char *type = (op->right == op->prior ? "right associative" :
                          op->left == op->prior ? "left associative" : "non-associative");
            ret = outMsg(out, "@item @code{%I}\n@tab %s infix\n@tab %d\n", nm, type, op->prior);
            break;
          }
          case postfixOp: {
            char *type = (op->left == op->prior ? "associative" : "non-associative");
            ret = outMsg(out, "@item @code{%I}\n@tab %s postfix\n@tab %d\n", nm, type, op->prior);
            break;
          }
          default:
            return Error;
        }
        p = p->next;
      }
      return ret;
    }
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
      case genStar:
        ret = outStr(out, "].\n");
      default:
        break;
    }
  return ret;
}

retCode procBrackets(void *n, void *r, void *c) {
  ioPo out = (ioPo) c;
  bracketPo b = (bracketPo) r;
  char *nm = (char *) n;

  retCode ret = Ok;
  char *sep = "";

  switch (genMode) {
    case genProlog:
      ret = outMsg(out, "  bracket(\"%P\", \"%P\", \"%P\", %d).\n", nm, b->left, b->right, b->priority);
      break;
    case genStar:
      ret = outMsg(out, "  isBracket(\"%P\") => some(bkt(\"%P\",\"%P\",\"%P\",%d)).\n", b->left, b->left, b->name,
                   b->right, b->priority);
      if (ret == Ok)
        ret = outMsg(out, "  isBracket(\"%P\") => some(bkt(\"%P\",\"%P\",\"%P\",%d)).\n", b->right, b->left, b->name,
                     b->right, b->priority);
      if (ret == Ok)
        ret = outMsg(out, "  isBracket(\"%P\") => some(bkt(\"%P\",\"%P\",\"%P\",%d)).\n", b->name, b->left, b->name,
                     b->right, b->priority);
      break;
    default:
      break;
  }

  return ret;
}

static inline byte hxDgit(integer h) {
  if (h < 10)
    return (byte) (((byte) h) | (byte) '0');
  else
    return (byte) (h + 'a' - 10);
}

static retCode quoteChar(ioPo f, codePoint ch) {
  retCode ret;
  switch (ch) {
    case '\a':
      ret = outStr(f, "\\a");
      break;
    case '\b':
      ret = outStr(f, "\\b");
      break;
    case '\x7f':
      ret = outStr(f, "\\d");
      break;
    case '\x1b':
      ret = outStr(f, "\\e");
      break;
    case '\f':
      ret = outStr(f, "\\f");
      break;
    case '\n':
      ret = outStr(f, "\\n");
      break;
    case '\r':
      ret = outStr(f, "\\r");
      break;
    case '\t':
      ret = outStr(f, "\\t");
      break;
    case '\v':
      ret = outStr(f, "\\v");
      break;
    case '\\':
      ret = outStr(f, "\\\\");
      break;
    case '\"':
      ret = outStr(f, "\\\"");
      break;
    case '@':
      ret = outStr(f,"@@");
      break;
    default:
      if (ch < ' ') {
        ret = outChar(f, '\\');
        if (ret == Ok)
          ret = outChar(f, ((ch >> 6) & 3) | '0');
        if (ret == Ok)
          ret = outChar(f, ((ch >> 3) & 7) | '0');
        if (ret == Ok)
          ret = outChar(f, (ch & 7) | '0');
      } else
        ret = outChar(f, ch);
  }
  return ret;
}

retCode genTexiStr(ioPo f, void *data, long depth, long precision, logical alt) {
  char *txt = (char *) data;
  integer len = (integer) uniStrLen(txt);
  integer pos = 0;

  retCode ret = Ok;
  while (ret == Ok && pos < len) {
    codePoint cp = nextCodePoint(txt, &pos, len);
    ret = quoteChar(f, cp);
  }
  return ret;
}
