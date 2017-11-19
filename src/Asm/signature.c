
#include <ooio.h>
#include "signature.h"

static logical validSig(char *sig, integer *start, integer end);
static logical validConstraint(char *sig, integer *start, integer end);

logical validSignature(char *sig) {
  integer pos = 0;
  integer end = uniStrLen(sig);

  return (logical) (validSig(sig, &pos, end) && pos == end);
}

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
  }
  return (logical) (*start <= end);
}

logical validSig(char *sig, integer *start, integer end) {
  switch (sig[(*start)++]) {
    case voidSig:
    case thisSig:
    case intSig:
    case fltSig:
    case logSig:
    case strSig:
      return True;
    case kvrSig:
    case tpeSig:
      return skipId(sig, start, end);
    case refSig:
      return validSig(sig,start,env);
    case kfnSig:
      return (logical) (skipId(sig, start, end) && skipInt(sig, start, end));
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
    case tpeExpSig:
      return (logical) (skipId(sig, start, end) && validSig(sig, start, end));
    case lstSig:
      return validSig(sig, start, end);
    case faceSig: {
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
    case conSig:        /* Type constructor */
    case xstSig:       /* Existential quantifier */
    case allSig:        /* Universal quantifier */
    case tpruleSig:
    case tplambdaSig:
      if (validConstraint(sig, start, end))
        return validConstraint(sig, start, end);
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
    case univCon:
      return (logical) (validSig(sig, start, end) && validConstraint(sig, start, end));
    case contractCon:
      return (logical) (skipId(sig, start, end) && validSig(sig, start, end) && validSig(sig, start, end));
    case implementsCon:
      return (logical) (skipId(sig, start, end) && validSig(sig, start, end));
    case constrainedCon:
      if (validConstraint(sig, start, end))
        return validConstraint(sig, start, end);
      else
        return False;
    default:
      return False;
  }
}

static retCode tplArity(char *sig, integer *arity, integer *start, integer end);

static retCode funArity(char *sig, integer *arity, integer *start, integer end) {
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
    default:
      return Error;      /* Not a valid signature */
  }
  return Error;
}

retCode functionArity(char *sig, integer *arity) {
  integer pos = 0;
  integer end = uniStrLen(sig);

  return funArity(sig, arity, &pos, end);
}

static retCode tplArity(char *sig, integer *arity, integer *start, integer end) {
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

retCode tupleArity(char *sig, integer *arity) {
  integer pos = 0;
  integer end = uniStrLen(sig);

  return tplArity(sig, arity, &pos, end);
}

retCode skipSig(char *sig, integer *start, integer end) {
  if ((*start) < end) {
    switch (sig[(*start)++]) {
      case voidSig:
      case thisSig:
      case intSig:
      case fltSig:
      case logSig:
      case strSig:
        return Ok;
      case kvrSig:
      case tpeSig:
        if (skipId(sig, start, end))
          return Ok;
        else
          return Error;
      case refSig:
        return skipSig(sig,start,end);
      case kfnSig:
        if (skipId(sig, start, end) && skipInt(sig, start, end))
          return Ok;
        else
          return Error;
      case tpeExpSig:
        if (skipId(sig, start, end))
          return skipSig(sig, start, end);
        else
          return Error;
      case lstSig:
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
      case conSig:        /* Type constructor */
      case xstSig:        /* Existential quantifier */
      case allSig:        /* Universal quantifier */
      case tpruleSig:
      case tplambdaSig:
        tryRet(skipSig(sig, start, end));
        return skipSig(sig, start, end);
      case constrainedSig:
        tryRet(skipSig(sig, start, end));

        return skipConstraint(sig, start, end);
      default:
        return Error;      /* Not a valid signature */
    }
  } else
    return Error;
}

retCode skipConstraint(char *sig, integer *start, integer end) {
  if (*start < end) {
    switch (sig[(*start)++]) {
      case univCon:
        tryRet(skipSig(sig, start, end));
        return skipConstraint(sig, start, end);
      case contractCon:
        if (skipId(sig, start, end)) {
          tryRet(skipSig(sig, start, end));
          return skipSig(sig, start, end);
        } else
          return Error;
      case implementsCon:
        if (skipId(sig, start, end)) {
          return skipSig(sig, start, end);
        } else
          return Error;
      case constrainedCon:
        tryRet(skipConstraint(sig, start, end));
        return skipConstraint(sig, start, end);
      default:
        return Error;
    }
  } else
    return Error;
}

retCode showSig(ioPo out, char *sig) {
  integer pos = 0;
  integer end = uniStrLen(sig);

  return showSignature(out, sig, &pos, end);
}

static retCode showSigId(ioPo out, char *sig, integer *start, integer end) {
  char qt = sig[(*start)++];
  while (*start < end) {
    char ch = sig[(*start)]++;
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

static retCode showSigInt(ioPo out, char *sig, integer *start, integer end) {
  while (*start < end) {
    char ch = sig[(*start)]++;
    if (isNdChar((codePoint) ch))
      tryRet(outChar(out, (codePoint) ch));
  }
  if (*start < end)
    return Ok;
  else
    return Error;
}

static retCode showSigTplEls(ioPo out, char *sig, integer *start, integer end) {
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

retCode showSignature(ioPo out, char *sig, integer *start, integer end) {
  switch (sig[(*start)++]) {
    case voidSig:
      return outStr(out, "void");
    case thisSig:
      return outStr(out, "this");
    case intSig:
      return outStr(out, "integer");
    case fltSig:
      return outStr(out, "float");
    case logSig:
      return outStr(out, "logical");
    case strSig:
      return outStr(out, "string");
    case kvrSig:
      tryRet(showSigId(out, sig, start, end));
      return outStr(out, "%");
    case kfnSig:
      tryRet(showSigId(out, sig, start, end));
      tryRet(outStr(out, "/"));
      return showSigInt(out, sig, start, end);
    case tpeSig:
      return showSigId(out, sig, start, end);
    case refSig:
      tryRet(outStr(out,"ref("));
      tryRet(showSignature(out,sig,start,end));
      return outStr(out,")");
    case tpeExpSig:
      tryRet(showSigId(out, sig, start, end));
      tryRet(outStr(out, "["));
      if (sig[(*start)++] != '(')
        return Error;
      else {
        tryRet(showSigTplEls(out, sig, start, end));
        return outStr(out, "]");
      }
    case lstSig:
      tryRet(outStr(out, "list["));
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
    case conSig:        /* Type constructor */
      tryRet(showSignature(out, sig, start, end));
      tryRet(outStr(out, " <=> "));
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

retCode showConstraint(ioPo out, char *sig, integer *start, integer end) {
  if (*start < end) {
    switch (sig[(*start)++]) {
      case univCon:
        tryRet(outStr(out, "all "));
        tryRet(showSignature(out, sig, start, end));
        tryRet(outStr(out, " ~~ "));
        return showConstraint(out, sig, start, end);
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
      case implementsCon:
        tryRet(showSigId(out, sig, start, end));
        tryRet(outStr(out, "<~"));
        return showSignature(out, sig, start, end);
      case constrainedCon:
        tryRet(showConstraint(out, sig, start, end));
        tryRet(outStr(out, ", "));
        return showConstraint(out, sig, start, end);
      default:
        return Error;
    }
  } else
    return Error;
}
