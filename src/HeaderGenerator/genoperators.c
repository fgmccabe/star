#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <stringBuffer.h>
#include "stringTrie.h"
#include "formioP.h"
#include "template.h"
#include "formexts.h"

#include "genoperators.h"

static retCode procOperator(void *n, void *r, void *c);
static retCode procToken(void *n, void *r, void *c);
static retCode procBrackets(void *n, void *r, void *c);
static retCode genTexiStr(ioPo f, void *data, long depth, long precision, logical alt);

static char *pC(char *buff, long *ix, char c);

static char *pS(char *buff, char *s) {
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
  genProlog, genStar, genTexi, genEmacs
} genMode = genProlog;

typedef struct {
  char name[1024];
  char cmt[1024];
} TokenRecord, *tokenPo;

char *templateFn = "starops.py.plate";
char date[MAXLINE] = "";

typedef struct {
  char name[16];
  char left[16];
  char right[16];
  integer priority;
  char sep[16];
} BrktRecord, *bracketPo;

static stringTriePo tokenTrie;
static poolPo opPool;
static hashPo operators;
static hashPo bracketTbl;
static hashPo keywords;

static void initTries() {
  tokenTrie = emptyStringTrie();

  opPool = newPool(sizeof(TokenRecord), 128);
  operators = newHash(128, (hashFun) uniHash, (compFun) uniCmp, NULL);
  bracketTbl = newHash(128, (hashFun) uniHash, (compFun) uniCmp, NULL);
  keywords = newHash(128, (hashFun) uniHash, (compFun) uniCmp, NULL);
}

int getOptions(int argc, char **argv) {
  int opt;

  while ((opt = getopt(argc, argv, "psiet:d:")) >= 0) {
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
      case 'e':
        genMode = genEmacs;
        break;
      case 't':
        templateFn = optarg;
        break;
      case 'd':
        uniCpy(date, NumberOf(date), optarg);
        break;
      default:;
    }
  }
  return optind;
}

typedef struct {
  ioPo first;
  ioPo follow;
  char *sep;
  char last[MAXLINE];
} FollowCl;

static void dumpFollows(char *prefix, codePoint last, void *V, void *cl) {
  FollowCl *c = (FollowCl *) cl;

  switch (genMode) {
    case genProlog:
      outMsg(c->follow, "  follows('%P','%#c','%P%#c').\n", prefix, last, prefix, last);
      break;
    case genStar:
      if (uniCmp(prefix, "") == same)
        outMsg(c->first, "    | `%#c` => .some(\"%P%#c\")\n", last, prefix, last);
      else
        outMsg(c->follow, "    | (\"%P\",`%#c`) => .some(\"%P%#c\")\n", prefix, last, prefix, last);
      break;
    case genTexi:
    default:
      break;
  }
}

static void genFollows(ioPo out) {
  FollowCl cl = {out, out, "", "##"};
  processStringTrie(tokenTrie, dumpFollows, &cl, True);
  outMsg(out, "%s", cl.sep);
}

static void dumpFinal(char *prefix, codePoint last, void *V, void *cl) {
  tokenPo op = (tokenPo) V;
  ioPo out = (ioPo) cl;

  if (op != NULL) {
    switch (genMode) {
      case genProlog:
        outMsg(out, "  final('%P',\"%P\").\t /* %s */\n", op->name, op->name, op->cmt);
        break;
      case genStar:
        outMsg(out, "    | \"%P\" => .true  /* %s */\n", op->name, op->cmt);
        break;
      case genTexi:
      default:
        break;
    }
  }
}

static void genFinal(ioPo out) {
  processStringTrie(tokenTrie, dumpFinal, out, False);
}

logical isAlphaNumeric(char *p) {
  if (*p != '\0' && isalpha(*p++)) {
    while (*p != '\0' && isalnum(*p++));
    return True;
  }
  return False;
}

static retCode genKeyword(void *n, void *r, void *c);

int main(int argc, char **argv) {
  initTries();
  initLogfile("-");
  installMsgProc('P', genQuotedStr);
  installMsgProc('I', genTexiStr);

  int narg = getOptions(argc, argv);

  if (narg < 0) {
    fprintf(stdout, "bad args");
    exit(1);
  } else {

#define infixOp(Op,LPr,Pr,RPr,Desc,IsKw)  genInfix(Op, LPr, Pr, RPr, IsKw,Desc);
#define prefixOp(Op,Pr,RPr,Desc,IsKw)  genPrefix(Op, Pr, RPr, IsKw,Desc);
#define postfixOp(Op,LPr,Pr,Desc,IsKw)  genPostfix(Op, LPr, Pr, IsKw,Desc);
#define token(Tk,Desc,IsKw)   genToken(Tk, Desc, IsKw);
#define bracket(Op,Lft,Rgt,Sep,Pr,Desc)  genBracket(Op,Pr, Lft, Rgt, Sep, Desc);

#include "operators.h"
#undef infixOp
#undef prefixOp
#undef postfixOp
#undef token
#undef bracket

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
    strBufferPo operBuff = newStringBuffer();
    processHashTable(procOperator, operators, operBuff);

    integer len;
    char *allOps = getTextFromBuffer(operBuff, &len);

    hashPo vars = newHash(8, (hashFun) uniHash, (compFun) uniCmp, NULL);
    hashPut(vars, "Operators", allOps);

    strBufferPo tokenBuff = newStringBuffer();
    processHashTable(procToken, operators, tokenBuff);
    char *allTokens = getTextFromBuffer(tokenBuff, &len);

    hashPut(vars, "Tokens", allTokens);

    strBufferPo bracketBuff = newStringBuffer();
    processHashTable(procBrackets, bracketTbl, bracketBuff);

    char *allBkts = getTextFromBuffer(bracketBuff, &len);
    hashPut(vars, "Brackets", allBkts);

    strBufferPo keywordsBuff = newStringBuffer();
    processHashTable(genKeyword, keywords, keywordsBuff);
    char *allKeywords = getTextFromBuffer(keywordsBuff, &len);
    hashPut(vars, "Keywords", allKeywords);

    hashPut(vars, "Date", date);

    // dumpTrie(tokenTrie,Stdout());

    strBufferPo followBuff = newStringBuffer();
    switch (genMode) {
      case genStar: {
        strBufferPo firstBuff = newStringBuffer();
        FollowCl cl = {O_IO(firstBuff), O_IO(followBuff), "", "##"};
        processStringTrie(tokenTrie, dumpFollows, &cl, True);
        outMsg(out, "%s", cl.sep);
        hashPut(vars, "Follows", getTextFromBuffer(followBuff, &len));
        hashPut(vars, "First", getTextFromBuffer(firstBuff, &len));
        break;
      }
      default:
        genFollows(O_IO(followBuff));
        hashPut(vars, "Follows", getTextFromBuffer(followBuff, &len));
    }

    strBufferPo finalBuff = newStringBuffer();
    genFinal(O_IO(finalBuff));
    hashPut(vars, "Final", getTextFromBuffer(finalBuff, &len));

    processTemplate(out, plate, vars, NULL, NULL);

    flushOut();
    closeIo(out);
    exit(0);
  }
}

typedef struct operator_ {
  char name[MAXLINE];
  char cmt[MAXLINE];
  OperatorStyle style;
  int left, prior, right;
} Operator, *opPo;

typedef struct pair_ *pairPo;

typedef struct pair_ {
  opPo op;
  pairPo next;
} Pair;

void genToken(char *op, char *cmt, logical isKeyword) {
  tokenPo tk = (tokenPo) allocPool(opPool);
  uniCpy(tk->name, NumberOf(tk->name), op);
  uniCpy(tk->cmt, NumberOf(tk->cmt), cmt);

  if (isKeyword)
    hashPut(keywords, tk->name, tk->name);

  if (!isAlphaNumeric(op))
    addToStringTrie(op, tk, tokenTrie);
}

static opPo genOper(char *op, char *cmt, OperatorStyle style, int left, int prior, int right, logical isKeyword) {
  opPo oper = (opPo) malloc(sizeof(Operator));
  strcpy(oper->name, op);
  strcpy(oper->cmt, cmt);
  oper->style = style;
  oper->left = left;
  oper->prior = prior;
  oper->right = right;

  pairPo p = (pairPo) malloc(sizeof(Pair));
  p->op = oper;
  p->next = (pairPo) hashGet(operators, oper->name);

  hashPut(operators, oper->name, p);

  if (isKeyword)
    hashPut(keywords, oper->name, oper->name);
  return oper;
}

void genInfix(char *op, int left, int prior, int right, logical isKeyword, char *cmt) {
  opPo oper = genOper(op, cmt, infixOp, left, prior, right, isKeyword);
  genToken(oper->name, cmt, isKeyword);
}

void genPrefix(char *op, int prior, int right, logical isKeyword, char *cmt) {
  opPo oper = genOper(op, cmt, prefixOp, 0, prior, right, isKeyword);
  genToken(oper->name, cmt, isKeyword);
}

void genPostfix(char *op, int left, int prior, logical isKeyword, char *cmt) {
  opPo oper = genOper(op, cmt, postfixOp, left, prior, 0, isKeyword);
  genToken(oper->name, cmt, isKeyword);
}

void genBracket(char *op, integer prior, char *left, char *right, char *sep, char *cmt) {
  bracketPo bkt = (bracketPo) malloc(sizeof(BrktRecord));

  strcpy(bkt->name, op);
  strcpy(bkt->left, left);
  strcpy(bkt->right, right);
  strcpy(bkt->sep, sep);
  bkt->priority = prior;

  hashPut(bracketTbl, bkt->name, bkt);

  genToken(bkt->left, cmt, True);
  genToken(bkt->right, cmt, True);
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
          return outMsg(out, "%s.prefixOp(%d,%d)", sep, op->prior, op->right);
        case infixOp:
          return outMsg(out, "%s.infixOp(%d,%d,%d)", sep, op->left, op->prior, op->right);
        case postfixOp:
          return outMsg(out, "%s.postfixOp(%d,%d)", sep, op->left, op->prior);
        default:
          return Error;
      }
    case genTexi:
      return Ok;
    case genEmacs:
      switch (op->style) {
        case prefixOp:
          return outMsg(out, "%s(prefix %d %d)", sep, op->prior, op->right);
        case infixOp:
          return outMsg(out, "%s(infix %d %d %d)", sep, op->left, op->prior, op->right);
        case postfixOp:
          return outMsg(out, "%s(postfix %d %d)", sep, op->left, op->prior);
        default:
          return Error;
      }
    default:
      return Error;
  }
}

static retCode procOperator(void *n, void *r, void *c) {
  ioPo out = (ioPo) c;
  pairPo p = (pairPo) r;
  char *nm = (char *) n;

  char *sep = "";

  switch (genMode) {
    case genProlog: {
      retCode ret = outMsg(out, "  operator(\"%P\", [", nm);
      while (ret == Ok && p != NULL) {
        ret = procOper(out, sep, p->op);
        p = p->next;
        sep = ", ";
      }
      if (ret == Ok)
        ret = outStr(out, "]).\n");
      return ret;
    }
    case genStar: {
      retCode ret = outMsg(out, "    | \"%P\" => [", nm);
      while (ret == Ok && p != NULL) {
        ret = procOper(out, sep, p->op);
        p = p->next;
        sep = ", ";
      }
      if (ret == Ok)
        ret = outStr(out, "]\n");
      return ret;
    }
    case genTexi: {
      retCode ret = Ok;
      while (p != NULL && ret == Ok) {
        opPo op = p->op;
        switch (op->style) {
          case prefixOp: {
            char *type = (op->right == op->prior ? "associative" : "non-associative");
            ret = outMsg(out, "@item @code{%I}\n@tab %s prefix\n@tab %d\n@tab %I\n", nm, type, op->prior, op->cmt);
            break;
          }
          case infixOp: {
            char *type = (op->right == op->prior ? "right associative" :
                          op->left == op->prior ? "left associative" : "non-associative");
            ret = outMsg(out, "@item @code{%I}\n@tab %s infix\n@tab %d\n@tab %I\n", nm, type, op->prior, op->cmt);
            break;
          }
          case postfixOp: {
            char *type = (op->left == op->prior ? "associative" : "non-associative");
            ret = outMsg(out, "@item @code{%I}\n@tab %s postfix\n@tab %d\n@tab %I\n", nm, type, op->prior, op->cmt);
            break;
          }
          default:
            return Error;
        }
        p = p->next;
      }
      return ret;
    }
    case genEmacs: {
      retCode ret = outMsg(out, "  (\"%P\" (", nm);

      while (ret == Ok && p != NULL) {
        ret = procOper(out, " ", p->op);
        p = p->next;
      }

      if (ret == Ok)
        ret = outStr(out, "))\n");
      return ret;
    }
    default:
      return Error;
  }
}

retCode procToken(void *n, void *r, void *c) {
  ioPo out = (ioPo) c;
  pairPo p = (pairPo) r;
  char *nm = (char *) n;

  static char *sep = "";
  static int tokenCount = 0;

  switch (genMode) {
    case genProlog: {
      return outMsg(out, "  token(\"%P\").\n", nm);
    }
    case genStar: {
      return outMsg(out, "  | token(\"%P\") => .true\n", sep, nm);
    }
    case genTexi: {
      if (!isAlphaNumeric(nm)) {
        if (tokenCount++ % 5 == 0) {
          sep = "  @item";
        } else {
          sep = "  @tab";
        }
        return outMsg(out, "%s @code{%I}\n", sep, nm);
      } else
        return Ok;
    }
    case genEmacs: {
      return outMsg(out, "  \"%P\" ", nm);
    }
    default:
      return Error;
  }
}

retCode genKeyword(void *n, void *r, void *c) {
  ioPo out = (ioPo) c;
  char *name = (char *) r;

  switch (genMode) {
    case genProlog:
      return outMsg(out, "  keyword(\"%P\").\n", name);
    case genStar:
      return outMsg(out, "    | \"%P\" => .true\n", name);
    case genTexi: {
      static int col = 0;
      char *tag = "tab";

      if (isUniIdentifier(name, uniStrLen(name))) {
        if (col % 3 == 0) {
          col = 0;
          tag = "item";
        }
        col++;
        return outMsg(out, "@%s @code{%I}\n", tag, name);
      } else
        return Ok;
    }
    case genEmacs: {
      if (isUniIdentifier(name, uniStrLen(name)))
        return outMsg(out, "\"%P\"\n", name);
      else
        return Ok;
    }
    default:
      return Error;
  }
}

retCode procBrackets(void *n, void *r, void *c) {
  ioPo out = (ioPo) c;
  bracketPo b = (bracketPo) r;
  char *nm = (char *) n;

  retCode ret = Ok;

  switch (genMode) {
    case genProlog:
      ret = outMsg(out, "  bracket(\"%P\", \"%P\", \"%P\", \"%P\", %d).\n", nm, b->left, b->right, b->sep, b->priority);
      break;
    case genStar:
      ret = outMsg(out, "    | \"%P\" => .some(.bkt(\"%P\",\"%P\",\"%P\",\"%P\",%d))\n", b->left, b->left,
                   b->name, b->right, b->sep, b->priority);
      if (ret == Ok)
        ret = outMsg(out, "    | \"%P\" => .some(.bkt(\"%P\",\"%P\",\"%P\",\"%P\",%d))\n", b->right, b->left,
                     b->name, b->right, b->sep, b->priority);
      if (ret == Ok)
        ret = outMsg(out, "    | \"%P\" => .some(.bkt(\"%P\",\"%P\",\"%P\",\"%P\",%d))\n",
                     b->name, b->left, b->name, b->right, b->sep, b->priority);
      break;
    case genEmacs:
      return outMsg(out, "  ( \"%P\" \"%P\" \"%P\" %d)\n", nm, b->left, b->right, b->priority);
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
      ret = outStr(f, "@@");
      break;
    case '{':
      ret = outStr(f, "@{");
      break;
    case '}':
      ret = outStr(f, "@}");
      break;
    default:
      if (ch < ' ') {
        ret = outChar(f, '\\');
        if (ret == Ok)
          ret = outChar(f, ((ch >> 6u) & 3u) | (unsigned) '0');
        if (ret == Ok)
          ret = outChar(f, ((ch >> 3u) & 7u) | (unsigned) '0');
        if (ret == Ok)
          ret = outChar(f, (ch & 7u) | (unsigned) '0');
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
