//
// Created by Francis McCabe on 1/13/18.
//
#include "config.h"
#include <ooio.h>
#include "signature.h"

static logical validSig(char *sig, integer *start, integer end);
static logical validConstraint(char *sig, integer *start, integer end);

static logical skipId(const char *sig, integer *start, integer end) {
  integer pos = *start;
  char quote = sig[pos++];

  while (pos < end && sig[pos] != quote) {
    if (sig[pos] == '\\') {
      pos++;
    }
    pos++;
  }

  *start = pos + 1;

  return (logical) (pos <= end);
}

static logical skipInt(const char *sig, integer *start, integer end) {
  while (*start < end) {
    codePoint cp = (codePoint) sig[*start];
    if (isNdChar(cp))
      (*start)++;
    else
      break;
  }
  return (logical) (*start <= end);
}

static integer decodeArity(const char *sig, integer *start, integer end) {
  integer arity = 0;

  while (*start < end) {
    codePoint cp = (codePoint) sig[*start];
    if (isNdChar(cp)) {
      arity = arity * 10 + digitValue(cp);
      (*start)++;
    } else
      break;
  }
  return arity;
}

logical validSig(char *sig, integer *start, integer end) {
  switch (sig[(*start)++]) {
    case anySig:
    case voidSig:
    case thisSig:
    case intSig:
    case bigSig:
    case fltSig:
    case logSig:
    case chrSig:
    case strSig:
      return True;
    case kvrSig:
    case tpeSig:
      return skipId(sig, start, end);
    case refSig:
      return validSig(sig, start, end);
    case tpfnSig:
    case kfnSig:
      return (logical) (skipInt(sig, start, end) && skipId(sig, start, end));
    case tplSig: {
      while (*start < end && sig[*start] != ')')
        if (!validSig(sig, start, end))
          return False;
      if (sig[*start] == ')') {
        (*start)++;
        return True;
      } else
        return False;
    }
    case tpeExpSig: {
      if (validSig(sig, start, end))
        return validSig(sig, start, end);
      else
        return False;
    }
    case lstSig:
    case vctSig:
      return validSig(sig, start, end);
    case faceSig: {
      if (sig[(*start)++] != '{')
        return False;
      while (*start < end && sig[*start] != '}')
        if (!skipId(sig, start, end) || !validSig(sig, start, end))
          return False;
      if (sig[*start] == '}') {
        (*start)++;
        return True;
      } else
        return False;
    }
    case funSig:        /* Function signature */
    case conSig:        /* Constructor function */
    case contSig:       // Continuation
    case xstSig:        /* Existential quantifier */
    case allSig:        /* Universal quantifier */
    case tpruleSig:
    case tplambdaSig:
      if (validSig(sig, start, end))
        return validSig(sig, start, end);
      else
        return False;
    case constrainedSig:
      return (logical) (validSig(sig, start, end) && validConstraint(sig, start, end));
    default:
      return False;      /* Not a valid signature */
  }
}

logical validConstraint(char *sig, integer *start, integer end) {
  switch (sig[(*start)++]) {
    case contractCon:
      return (logical) (skipId(sig, start, end) && validSig(sig, start, end) && validSig(sig, start, end));
    case hasFieldCon:
      return (logical) validSig(sig, start, end) && validSig(sig, start, end);
    case implicitCon:
      return (logical) (skipId(sig, start, end) && validSig(sig, start, end));
    case raisesCon:
    case throwsCon:
      return validSig(sig, start, end);
    default:
      return False;
  }
}

static retCode tplArity(const char *sig, int32 *arity, integer *start, integer end);
static retCode constraintArity(const char *sig, int32 *arity, integer *start, integer end);

static retCode funArity(const char *sig, int32 *arity, integer *start, integer end) {
  switch (sig[(*start)++]) {
    case tplSig: {
      *arity = 0;
      while (*start < end && sig[*start] != ')') {
        tryRet(skipSig(sig, start, end));
        (*arity)++;
      }

      if (sig[*start] == ')') {
        (*start)++;
        return Ok;
      } else
        return Error;
    }
    case funSig:        /* Function signature */
      return tplArity(sig, arity, start, end);
    case xstSig:        /* Existential quantifier */
    case allSig:        /* Universal quantifier */
      tryRet(skipSig(sig, start, end));
      return funArity(sig, arity, start, end);
    case constrainedSig: // Constrained signature, add 1 to arity
      tryRet(funArity(sig, arity, start, end));
      return constraintArity(sig, arity, start, end);
    default:
      return Error;      /* Not a valid signature */
  }
}

retCode funSigArity(const char *sig, integer length, int32 *arity) {
  integer pos = 0;
  return funArity(sig, arity, &pos, length);
}

retCode funSigReturns(const char *sig, integer length, int32 *count) {
  integer pos = 0;
  int32 ignore;
  tryRet(funArity(sig, &ignore, &pos, length));
  if (sig[pos] == tplSig)
    return tplArity(sig, count, &pos, length);
  else {
    *count = 1;
    return Ok;
  }
}

static retCode constraintArity(const char *sig, int32 *arity, integer *start, integer end) {
  switch (sig[(*start)++]) {
    case contractCon:
      (*arity)++;
      return skipSig(sig, start, end);
    case hasFieldCon:
      (*arity)++;
      return skipSig(sig, start, end);
    case implicitCon:
      (*arity)++;
      return skipSig(sig, start, end);
    case throwsCon:
      return skipSig(sig, start, end);
    default:
      return Error;
  }
}

static retCode tplArity(const char *sig, int32 *arity, integer *start, integer end) {
  switch (sig[(*start)++]) {
    case tplSig: {
      *arity = 0;
      while (*start < end && sig[*start] != ')') {
        tryRet(skipSig(sig, start, end));
        (*arity)++;
      }

      if (sig[*start] == ')') {
        (*start)++;
        return Ok;
      } else
        return Error;
    }
    case xstSig:        /* Existential quantifier */
    case allSig:        /* Universal quantifier */
      tryRet(skipSig(sig, start, end));
      return tplArity(sig, arity, start, end);
    default:
      return Error;      /* Not a valid signature */
  }
}

static retCode skipConstrnt(const char *sig, integer *start, integer end);

retCode skipSig(const char *sig, integer *start, integer end) {
  if ((*start) < end) {
    switch (sig[(*start)++]) {
      case anySig:
      case voidSig:
      case thisSig:
      case intSig:
      case bigSig:
      case fltSig:
      case logSig:
      case chrSig:
      case strSig:
        return Ok;
      case kvrSig:
      case tpeSig:
        if (skipId(sig, start, end))
          return Ok;
        else
          return Error;
      case refSig:
        return skipSig(sig, start, end);
      case kfnSig:
      case tpfnSig:
        if (skipInt(sig, start, end) && skipId(sig, start, end))
          return Ok;
        else
          return Error;
      case tpeExpSig:
        tryRet(skipSig(sig, start, end));
        return skipSig(sig, start, end);
      case lstSig:
      case vctSig:
        return skipSig(sig, start, end);
      case tplSig: {
        while (*start < end && sig[*start] != ')')
          tryRet(skipSig(sig, start, end));

        if (sig[*start] == ')') {
          (*start)++;
          return Ok;
        } else
          return Error;
      }
      case faceSig: {
        while (*start < end && sig[*start] != '}') {
          if (!skipId(sig, start, end))
            return Error;
          else
            tryRet(skipSig(sig, start, end));
        }

        if (sig[*start] == '}') {
          (*start)++;
          return Ok;
        } else
          return Error;
      }
      case funSig:        /* Function signature */
      case conSig:        /* Constructor function */
      case contSig:       // Continuation
      case xstSig:        /* Existential quantifier */
      case allSig:        /* Universal quantifier */
      case tpruleSig:
      case tplambdaSig:
        tryRet(skipSig(sig, start, end));
        return skipSig(sig, start, end);
      case constrainedSig:
        tryRet(skipSig(sig, start, end));

        return skipConstrnt(sig, start, end);
      default:
        return Error;      /* Not a valid signature */
    }
  } else
    return Error;
}

retCode skipConstrnt(const char *sig, integer *start, integer end) {
  if (*start < end) {
    switch (sig[(*start)++]) {
      case contractCon:
        if (skipId(sig, start, end)) {
          tryRet(skipSig(sig, start, end));
          return skipSig(sig, start, end);
        } else
          return Error;
      case hasFieldCon:
        tryRet(skipSig(sig, start, end));
        return skipSig(sig, start, end);
      case implicitCon:
        if (skipId(sig, start, end)) {
          return skipSig(sig, start, end);
        } else
          return Error;
      case raisesCon:
      case throwsCon:
        return skipSig(sig, start, end);
      default:
        return Error;
    }
  } else
    return Error;
}

retCode skipIdentifier(ioPo in) {
  codePoint quote;
  retCode ret = inChar(in, &quote);

  if (ret == Ok) {
    codePoint ch;
    ret = inChar(in, &ch);

    while (ret == Ok && ch != quote) {
      if (ch == '\\') {
        ret = inChar(in, &ch);
      }
      if (ret == Ok)
        ret = inChar(in, &ch);
    }
  }
  return ret;
}

static retCode skipInteger(ioPo in) {
  codePoint ch;
  retCode ret = inChar(in, &ch);

  while (ret == Ok && isNdChar(ch))
    ret = inChar(in, &ch);
  if (ret == Ok)
    unGetChar(in, ch);

  return ret;
}

static retCode skipFields(ioPo in) {
  retCode ret = isLookingAt(in, "{");
  while (ret == Ok && isLookingAt(in, "}") != Ok) {
    ret = skipIdentifier(in);
    if (ret == Ok)
      ret = skipSignature(in);
  }
  return ret;
}

static retCode skipConstraint(ioPo in) {
  codePoint ch;

  if (inChar(in, &ch) == Ok) {
    switch (ch) {
      case contractCon:
        tryRet(skipIdentifier(in));
        tryRet(skipSignature(in));
        return skipSignature(in);
      case hasFieldCon:
        tryRet(skipSignature(in));
        return skipSignature(in);
      case implicitCon:
        tryRet(skipIdentifier(in));
        return skipSignature(in);
      case raisesCon:
      case throwsCon:
        return skipSignature(in);
      default:
        return Error;
    }
  }
  return Error;
}

retCode skipSignature(ioPo in) {
  codePoint ch;
  retCode ret = inChar(in, &ch);

  if (ret == Ok) {
    switch (ch) {
      case anySig:
      case voidSig:
      case thisSig:
      case intSig:
      case bigSig:
      case fltSig:
      case logSig:
      case chrSig:
      case strSig:
        return Ok;
      case kvrSig:
      case tpeSig:
        return skipIdentifier(in);
      case refSig:
        return skipSignature(in);
      case tpfnSig:
      case kfnSig:
        ret = skipInteger(in);
        if (ret == Ok)
          ret = skipIdentifier(in);
        return ret;
      case tpeExpSig: {
        ret = skipSignature(in);
        if (ret == Ok)
          return skipSignature(in);
        else
          return ret;
      }

      case lstSig:
      case vctSig:
        return skipSignature(in);
      case tplSig: {
        while (ret == Ok && isLookingAt(in, ")") != Ok)
          ret = skipSignature(in);
        return ret;
      }
      case faceSig: {
        ret = skipFields(in);
        if (ret == Ok)
          ret = skipFields(in);
        return ret;
      }
      case funSig:        /* Function signature */
      case conSig:        /* Constructor function */
      case contSig:       // Continuation
      case xstSig:        /* Existential quantifier */
      case allSig:        /* Universal quantifier */
      case tpruleSig:
      case tplambdaSig:
      case funDep: {
        ret = skipSignature(in);
        if (ret == Ok)
          ret = skipSignature(in);
        return ret;
      }
      case constrainedSig:
        ret = skipSignature(in);
        if (ret == Ok)
          ret = skipConstraint(in);

        return ret;
      default:
        return Error;
    }
  }
  return ret;
}

static retCode showSigId(ioPo out, const char *sig, integer *start, integer end) {
  char qt = sig[(*start)++];
  while (*start < end) {
    char ch = sig[(*start)++];
    if (sig[ch] == qt)
      break;
    else
      tryRet(outChar(out, (codePoint) ch));
  }
  if (*start < end)
    return Ok;
  else
    return Error;
}

static retCode showSigTplEls(ioPo out, const char *sig, integer *start, integer end) {
  char *sep = "";
  while (*start < end && sig[*start] != ')') {
    tryRet(outStr(out, sep));
    sep = ", ";
    tryRet(showSignature(out, sig, start, end));
  }

  if (sig[*start] == ')') {
    (*start)++;
    return Ok;
  } else
    return Error;
}

retCode showSignature(ioPo out, const char *sig, integer *start, integer end) {
  switch (sig[(*start)++]) {
    case anySig:
      return outStr(out, "_");
    case voidSig:
      return outStr(out, "void");
    case thisSig:
      return outStr(out, "this");
    case intSig:
      return outStr(out, "integer");
    case bigSig:
      return outStr(out, "bigint");
    case fltSig:
      return outStr(out, "float");
    case logSig:
      return outStr(out, "logical");
    case chrSig:
      return outStr(out, "char");
    case strSig:
      return outStr(out, "string");
    case kvrSig:
      tryRet(showSigId(out, sig, start, end));
      return outStr(out, "%");
    case tpfnSig:
    case kfnSig: {
      integer arity = decodeArity(sig, start, end);
      tryRet(showSigId(out, sig, start, end));
      tryRet(outStr(out, "/"));
      return outInt(out, arity);
    }
    case tpeSig:
      return showSigId(out, sig, start, end);
    case refSig:
      tryRet(outStr(out, "ref("));
      tryRet(showSignature(out, sig, start, end));
      return outStr(out, ")");
    case tpeExpSig:
      tryRet(showSignature(out, sig, start, end));
      tryRet(outStr(out, "["));
      tryRet(showSignature(out, sig, start, end));
      return outStr(out, "]");
    case lstSig:
      tryRet(outStr(out, "cons["));
      tryRet(showSignature(out, sig, start, end));
      return outStr(out, "]");
    case vctSig:
      tryRet(outStr(out, "vect["));
      tryRet(showSignature(out, sig, start, end));
      return outStr(out, "]");
    case tplSig: {
      tryRet(outChar(out, '('));
      tryRet(showSigTplEls(out, sig, start, end));
      return outChar(out, ')');
    }
    case faceSig: {
      tryRet(outChar(out, '{'));
      char *sep = "";

      while (*start < end && sig[*start] != '}') {
        tryRet(outStr(out, sep));
        sep = ", ";
        tryRet(showSigId(out, sig, start, end));
        tryRet(outStr(out, ":"));
        tryRet(showSignature(out, sig, start, end));
      }

      if (sig[*start] == '}') {
        (*start)++;
        return outChar(out, '}');
      } else
        return Error;
    }
    case funSig:        /* Function signature */
      tryRet(showSignature(out, sig, start, end));
      tryRet(outStr(out, "=>"));
      return showSignature(out, sig, start, end);
    case conSig:        /* Constructor function */
      tryRet(showSignature(out, sig, start, end));
      tryRet(outStr(out, " <=> "));
      return showSignature(out, sig, start, end);
    case contSig:        /* Continuation signature */
      tryRet(showSignature(out, sig, start, end));
      tryRet(outStr(out, "=>>"));
      return showSignature(out, sig, start, end);
    case xstSig:        /* Existential quantifier */
      tryRet(outStr(out, "exists "));
      tryRet(showSignature(out, sig, start, end));
      tryRet(outStr(out, " ~~ "));
      return showSignature(out, sig, start, end);
    case allSig:        /* Universal quantifier */
      tryRet(outStr(out, "all "));
      tryRet(showSignature(out, sig, start, end));
      tryRet(outStr(out, " ~~ "));
      return showSignature(out, sig, start, end);
    case constrainedSig:
      tryRet(showConstraint(out, sig, start, end));
      tryRet(outStr(out, "|:"));
      return showSignature(out, sig, start, end);
    case tpruleSig:
      tryRet(showSignature(out, sig, start, end));
      tryRet(outStr(out, "<~"));
      return showSignature(out, sig, start, end);
    case tplambdaSig:
      tryRet(showSignature(out, sig, start, end));
      tryRet(outStr(out, "~>"));
      return showSignature(out, sig, start, end);
    default:
      return Error;      /* Not a valid signature */
  }
}

retCode showConstraint(ioPo out, const char *sig, integer *start, integer end) {
  if (*start < end) {
    switch (sig[(*start)++]) {

      case contractCon:
        tryRet(showSigId(out, sig, start, end));
        if (sig[(*start)++] != '(')
          return Error;
        else {
          tryRet(outStr(out, "["));
          tryRet(showSigTplEls(out, sig, start, end));
          if (sig[(*start)++] != '(')
            return Error;
          else {
            if (sig[*start] != ')') {
              tryRet(outStr(out, "->>"));
              tryRet(showSigTplEls(out, sig, start, end));
            } else
              (*start)++;
          }
          return outStr(out, "]");
        }
      case hasFieldCon:
        tryRet(showSignature(out, sig, start, end));
        tryRet(outStr(out, "<~"));
        return showSignature(out, sig, start, end);
      case implicitCon:
        tryRet(outStr(out, "implicit "));
        tryRet(showSigId(out, sig, start, end));
        tryRet(outStr(out, " : "));
        return showSignature(out, sig, start, end);
      case raisesCon:
        tryRet(outStr(out, "raises "));
        return showSignature(out, sig, start, end);
      case throwsCon:
        tryRet(outStr(out, "throws "));
        return showSignature(out, sig, start, end);
      default:
        return Error;
    }
  } else
    return Error;
}
