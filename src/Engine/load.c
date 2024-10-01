/*
  Copyright (c) 2016, 2017, 2018. Francis G. McCabe
 */

#include <string.h>
#include "pkgP.h"

#include "engine.h"
#include <globals.h>
#include "signature.h"
#include "decodeP.h"             /* pick up the term encoding definitions */
#include "manifest.h"
#include "codeP.h"
#include "labelsP.h"
#include "verifyP.h"

tracingLevel tracePkg = noTracing;

static retCode decodePkgName(ioPo in, packagePo pkg, char *errorMsg, integer msgLen);

static retCode decodeImportsSig(strBufferPo sigBuffer, char *errorMsg, long msgLen, pickupPkg pickup, void *cl);

static retCode loadDefs(ioPo in, heapPo h, packagePo owner, char *errorMsg, long msgLen);

static char stringSig[] = {strTrm, 0};
static char *pkgSig = "n6o6\1()6\1";

static retCode ldPackage(packagePo pkg, char *errorMsg, long msgSize, pickupPkg pickup, void *cl) {
  char codeName[MAXFILELEN];
  retCode ret = manifestResource(pkg, "code", codeName, NumberOf(codeName));

  if (ret != Ok) {
    strMsg(errorMsg, msgSize, "cannot determine code for %P%_", pkg);
    return Error;
  } else {
    char codeFlNm[MAXFILELEN];
    strMsg(codeFlNm, NumberOf(codeFlNm), "%s/%s", repoDir, codeName);

    ioPo file = openInFile(codeFlNm, utf8Encoding);

#ifdef TRACEPKG
    if (tracePkg >= generalTracing)
      logMsg(logFile, "loading package %P from file %s", pkg, codeFlNm);
#endif

    if (file != NULL) {
      if (fileStatus(file) == Ok) {

        skipShellPreamble(O_FILE(file));

        PackageRec lddPkg;
        strBufferPo sigBuffer = newStringBuffer();

        if ((ret = isLookingAt(file, stringSig)) == Ok)
          ret = decodeText(file, sigBuffer);

        rewindStrBuffer(sigBuffer);
        ioPo pkgIn = O_IO(sigBuffer);

        if (ret == Ok && isLookingAt(pkgIn, pkgSig) == Ok)
          ret = decodePkgName(pkgIn, &lddPkg, errorMsg, msgSize);
        else
          ret = Error;

        if (ret == Ok && uniCmp(lddPkg.packageName, pkg->packageName) != same) {
          closeIo(O_IO(sigBuffer));
          strMsg(errorMsg, msgSize, "loaded package: %P not what was expected %P\n", &lddPkg, pkg);
          return Error;
        }

        if (ret == Ok) {
          integer opSig = -1;

          ret = decodeInteger(O_IO(sigBuffer), &opSig);

          if (ret != Ok || opSig != OPCODE_SIGNATURE) {
            closeIo(O_IO(sigBuffer));
            strMsg(errorMsg, msgSize, "package: %P has invalid instruction signature\n", pkg);
            return Error;
          }

          if (pickup != Null)
            ret = decodeImportsSig(sigBuffer, errorMsg, msgSize, pickup, cl);

          if (ret == Ok)
            ret = skipEncoded(O_IO(sigBuffer), errorMsg, msgSize); // Skip export declarations

          if (ret == Ok)
            ret = skipEncoded(O_IO(sigBuffer), errorMsg, msgSize); // Skip local declarations

          if (ret == Ok)
            ret = loadDefs(O_IO(sigBuffer), globalHeap, markLoaded(lddPkg.packageName, lddPkg.version), errorMsg,
                           msgSize);
        }
        closeIo(O_IO(sigBuffer));
      }

      closeIo(file);

      if (ret != Ok)
        logMsg(logFile, "problem in loading %P: %s", pkg, errorMsg);

      return ret;
    } else {
      strMsg(errorMsg, msgSize, "package %P not found", pkg);
      return Eof;
    }
  }
}

retCode loadPackage(packagePo p, char *errorMsg, long msgSize, void *cl) {
  char *version = loadedVersion(p->packageName);

  if (version != NULL) {
    if (!compatiblVersion(p->version, version)) {
      strMsg(errorMsg, msgSize,
             "invalid version of package already loaded: %P,"
             "version %s expected", p->version);
      return Error;
    } else
      return Ok; // already loaded correct version
  } else
    return ldPackage(p, errorMsg, msgSize, loadPackage, cl);
}

retCode installPackage(char *pkgText, long pkgTxtLen, char *errorMsg, long msgSize, pickupPkg pickup, void *cl) {
  strBufferPo inBuff = newReadStringBuffer(pkgText, pkgTxtLen);

  retCode ret;
  PackageRec lddPkg;
  strBufferPo sigBuffer = newStringBuffer();

  if ((ret = isLookingAt(O_IO(inBuff), "s")) == Ok)
    ret = decodeText(O_IO(inBuff), sigBuffer);

  if (ret == Ok) {
    rewindStrBuffer(sigBuffer);

    ioPo pkgIn = O_IO(sigBuffer);

    if (isLookingAt(pkgIn, pkgSig) == Ok)
      ret = decodePkgName(pkgIn, &lddPkg, errorMsg, msgSize);

    if (ret == Ok && !isLoadedPackage(&lddPkg)) {
      ret = decodeImportsSig(sigBuffer, errorMsg, msgSize, pickup, cl);

      if (ret == Ok)
        ret = skipEncoded(pkgIn, errorMsg, msgSize); // Skip export declarations

      if (ret == Ok)
        ret = skipEncoded(pkgIn, errorMsg, msgSize); // Skip local declarations

      if (ret == Ok)
        ret = loadDefs(pkgIn, globalHeap, markLoaded(lddPkg.packageName, lddPkg.version), errorMsg, msgSize);
    }
#ifdef TRACEPKG
    else if (tracePkg >= generalTracing)
      logMsg(logFile, "package %P already installed", &lddPkg);
#endif
  }

  closeIo(O_IO(inBuff));
  closeIo(O_IO(sigBuffer));

#ifdef TRACEPKG
  if (tracePkg >= generalTracing)
    logMsg(logFile, "package %P installed", &lddPkg);
#endif

  if (ret == Eof)
    return Ok;
  else
    return ret;
}

static retCode decodeImportsSig(strBufferPo sigBuffer, char *errorMsg, long msgLen, pickupPkg pickup, void *cl) {
  rewindStrBuffer(sigBuffer);
  ioPo in = O_IO(sigBuffer);

  if (isLookingAt(in, pkgSig) == Ok) {
    retCode ret = skipEncoded(in, errorMsg, msgLen); // The package name
    if (ret != Ok)
      return ret;
    ret = skipEncoded(in, errorMsg, msgLen); // The opcode signature
    if (ret != Ok)
      return ret;

    if (isLookingAt(in, "n") == Ok) {
      integer len;
      ret = decInt(in, &len);

      // The imports are next in the signature
      if (ret == Ok)
        ret = skipEncoded(in, errorMsg, msgLen); // Move over the tuple constructor
      integer ix = 0;
      while (ix++ < len) {
        PackageRec lddPkg;

        if (ret == Ok)
          ret = decodePkgName(in, &lddPkg, errorMsg, msgLen);

        if (ret == Ok)
          ret = pickup(&lddPkg, errorMsg, msgLen, cl);
        if (ret != Ok)
          return ret;
      }
      return ret;
    } else {
      strMsg(errorMsg, msgLen, "invalid package encoding");
      return Error;
    }
  } else {
    strMsg(errorMsg, msgLen, "invalid package signature encoding");
    return Error;
  }
}

retCode decodePkgName(ioPo in, packagePo pkg, char *errorMsg, integer msgLen) {
  if (isLookingAt(in, "n2o2\1pkg\1s") == Ok) {
    strBufferPo pkgB = fixedStringBuffer(pkg->packageName, NumberOf(pkg->packageName));
    strBufferPo vrB = fixedStringBuffer(pkg->version, NumberOf(pkg->version));

    retCode ret = decodeText(O_IO(in), pkgB);

    if (ret == Ok) {
      if (isLookingAt(in, "e\1*\1") == Ok || isLookingAt(in, "o0\1*\1") == Ok)
        outStr(O_IO(vrB), "*");
      else if (isLookingAt(in, "s") == Ok) {
        ret = decodeText(O_IO(in), vrB);
      } else {
        strMsg(errorMsg, msgLen, "expecting a package name");
        return Error;
      }
    }

    outByte(O_IO(pkgB), 0);
    outByte(O_IO(vrB), 0);

    closeIo(O_IO(pkgB));
    closeIo(O_IO(vrB));
    return ret;
  } else {
    strMsg(errorMsg, msgLen, "invalid package name encoding");
    return Error;
  }
}

retCode decodeTplCount(ioPo in, integer *count, char *errMsg, integer msgLen) {
  if (isLookingAt(in, "n") == Ok) {
    char nm[MAXLINE];
    integer ar;
    retCode ret = decInt(in, count);
    if (ret == Ok) {
      ret = decodeLbl(in, nm, NumberOf(nm), &ar, errMsg, msgLen);
      if (ret == Ok) {
        if (ar != *count) {
          strMsg(errMsg, msgLen, "invalid tuple arity encoding");
          return Error;
        }
      }
    }
    return ret;
  } else
    return Fail;
}

static char *consPreamble = "n3o3\1cons\1";
static char *structPreamble = "n3o3\1struct\1";
static char *typePreamble = "n3o3\1type\1";
static char *fieldPreamble = "n2o2\1()2\1";
static char *funcPreamble = "n8o8\1func\1";
static char *globalPreamble = "n7o7\1global\1";

typedef enum {
  hardDef,
  softDef
} DefinitionMode;

static retCode loadFunc(ioPo in, heapPo H, packagePo owner, char *errorMsg, long msgSize);
static retCode loadGlobal(ioPo in, heapPo H, packagePo owner, char *errorMsg, long msgSize);
static retCode loadType(ioPo in, heapPo h, packagePo owner, char *errorMsg, long msgSize);
static retCode loadCtor(ioPo in, heapPo h, packagePo owner, char *errorMsg, long msgSize);

retCode loadDefs(ioPo in, heapPo h, packagePo owner, char *errorMsg, long msgLen) {
  integer count;
  if (decodeTplCount(in, &count, errorMsg, msgLen) == Ok) {
    retCode ret = Ok;

    for (integer ix = 0; ret == Ok && ix < count; ix++) {
      if (isLookingAt(in, funcPreamble) == Ok)
        ret = loadFunc(in, h, owner, errorMsg, msgLen);
      else if (isLookingAt(in, globalPreamble) == Ok)
        ret = loadGlobal(in, h, owner, errorMsg, msgLen);
      else if (isLookingAt(in, typePreamble) == Ok)
        ret = loadType(in, h, owner, errorMsg, msgLen);
      else if (isLookingAt(in, consPreamble) == Ok)
        ret = loadCtor(in, h, owner, errorMsg, msgLen);
      else if (isLookingAt(in, structPreamble) == Ok)
        ret = loadCtor(in, h, owner, errorMsg, msgLen);
      else {
        strMsg(errorMsg, msgLen, "invalid code stream");
        return Error;
      }
    }
    return ret;
  } else
    return Error;
}



// Decode a code segment.
// Each segment consists of
// a. The program name/arity being defined
// b. An integer defining the constant id of the signature
// b. A tuple of the instructions
// c. A tuple of literals associated with the code segment
// d. A tuple of variable specifications
// All wrapped up as a tuple structure.


// Each variable specification consists of
// a. The pool constant for the variable name
// c. The variable number
// d. The block which defines its range


retCode decodePolicies(ioPo in, heapPo H, DefinitionMode *redefine, char *errorMsg, long msgSize) {
  integer policyCount;
  retCode ret = decodeTplCount(in, &policyCount, errorMsg, msgSize);
  for (integer ix = 0; ret == Ok && ix < policyCount; ix++) {
    char nameBuff[MAX_SYMB_LEN];
    if (decodeString(in, nameBuff, NumberOf(nameBuff)) == Ok) {
      if (uniIsLit(nameBuff, "soft")) {
        *redefine = softDef;
      } else; // ignore unknown policies
    } else {
      strMsg(errorMsg, msgSize, "problem in loading policy");
      return Error;
    }
  }
  return ret;
}

static integer maxDepth(blockPo block, normalPo constPool) {
  integer currDepth = 0;
  integer maxDepth = 0;

  insPo ins = block->ins;

  for (integer pc = 0; pc < block->insCount; pc++, ins++) {
    switch (ins->op) {

#define
#define instruction(Op, A1, A2, Dl, Tp, Cmt) \
      case Op:{                  \
        currDepth+=(Dl);        \
        if(currDepth>maxDepth)  \
          maxDepth = currDepth; \
        continue;                \
      }

#include "instructions.h"

#undef instruction

      default:;
    }
    if (ins->op == Unpack) {
      labelPo lbl = C_LBL(nthArg(constPool, ins->fst));
      currDepth += labelArity(lbl);
      if (currDepth > maxDepth)
        maxDepth = currDepth;
    }
  }
  return maxDepth;
}

retCode loadFunc(ioPo in, heapPo H, packagePo owner, char *errorMsg, long msgSize) {
  char prgName[MAX_SYMB_LEN];
  integer arity;
  integer lclCount = 0;
  DefinitionMode redefine = hardDef;

  retCode ret = decodeLbl(in, prgName, NumberOf(prgName), &arity, errorMsg, msgSize);

#ifdef TRACEPKG
  if (tracePkg >= detailedTracing)
    logMsg(logFile, "loading function %s/%d", &prgName, arity);
#endif

  if (ret == Ok)
    ret = decodePolicies(in, H, &redefine, errorMsg, msgSize);

  if (ret == Ok)
    ret = skipEncoded(in, errorMsg, msgSize); // Skip the code signature

  if (ret == Ok)
    ret = decodeInteger(in, &lclCount);

  if (ret == Ok) {
    blockPo block = Null;

    ret = decodeInstructionBlock(in, &block, errorMsg, msgSize);

    if (ret == Ok) {
      termPo pool = voidEnum;
      int root = gcAddRoot(H, &pool);
      EncodeSupport support = {errorMsg, msgSize, H};
      strBufferPo tmpBuffer = newStringBuffer();

      ret = decode(in, &support, H, &pool, tmpBuffer);

      if (ret == Ok) {
        termPo locals = voidEnum;
        gcAddRoot(H, &locals);
        ret = decode(in, &support, H, &locals, tmpBuffer);

        if (ret == Ok) {
          termPo lines = voidEnum;
          gcAddRoot(H, &lines);
          ret = decode(in, &support, H, &lines, tmpBuffer);

          if (ret == Ok) {
            labelPo lbl = declareLbl(prgName, arity, -1);

            if (labelCode(lbl) != Null) {
              if (redefine != softDef) {
                strMsg(errorMsg, msgSize, "attempt to redeclare method %A", lbl);
                ret = Error;
              } // Otherwise don't redefine
            } else {
              gcAddRoot(H, (ptrPo) &lbl);

              integer stackDelta = maxDepth(block, C_NORMAL(pool)) + lclCount;

              methodPo mtd = defineMtd(H, block, lclCount, stackDelta, lbl, C_NORMAL(pool),
                                       C_NORMAL(locals),
                                       C_NORMAL(lines));
              if (enableVerify)
                ret = verifyMethod(mtd, prgName, errorMsg, msgSize);

              if (ret == Ok && jitOnLoad)
                ret = jitMethod(mtd, errorMsg, msgSize);
            }
          }
        }
      }
      closeIo(O_IO(tmpBuffer));
      gcReleaseRoot(H, root);
    }
  }

  if (ret == Error)
    logMsg(logFile, "problem in loading %s/%d: %s", prgName, arity, errorMsg);

  return ret;
}

retCode loadGlobal(ioPo in, heapPo H, packagePo owner, char *errorMsg, long msgSize) {
  char prgName[MAX_SYMB_LEN];
  integer arity;
  integer lclCount = 0;

  retCode ret = decodeLbl(in, prgName, NumberOf(prgName), &arity, errorMsg, msgSize);

#ifdef TRACEPKG
  if (tracePkg >= detailedTracing)
    logMsg(logFile, "loading global %s/%d", &prgName, arity);
#endif

  if (ret == Ok)
    ret = skipEncoded(in, errorMsg, msgSize); // Skip the code signature

  if (ret == Ok)
    ret = decodeInteger(in, &lclCount);

  if (ret == Ok) {
    blockPo block = Null;

    ret = decodeInstructionBlock(in, &block, errorMsg, msgSize);

    if (ret == Ok) {
      termPo pool = voidEnum;
      int root = gcAddRoot(H, &pool);
      EncodeSupport support = {errorMsg, msgSize, H};
      strBufferPo tmpBuffer = newStringBuffer();

      ret = decode(in, &support, H, &pool, tmpBuffer);

      if (ret == Ok) {
        termPo locals = voidEnum;
        gcAddRoot(H, &locals);
        ret = decode(in, &support, H, &locals, tmpBuffer);

        if (ret == Ok) {
          termPo lines = voidEnum;
          gcAddRoot(H, &lines);
          ret = decode(in, &support, H, &lines, tmpBuffer);

          if (ret == Ok) {
            labelPo lbl = declareLbl(prgName, arity, -1);

            if (labelCode(lbl) != Null) {
              strMsg(errorMsg, msgSize, "attempt to redeclare global %A", lbl);
              ret = Error;
            } else {
              gcAddRoot(H, (ptrPo) &lbl);

              integer stackDelta = maxDepth(block, C_NORMAL(pool)) + lclCount;

              methodPo mtd = defineMtd(H, block, lclCount, stackDelta, lbl, C_NORMAL(pool),
                                       C_NORMAL(locals),
                                       C_NORMAL(lines));
              if (enableVerify)
                ret = verifyMethod(mtd, prgName, errorMsg, msgSize);
            }
          }
        }
      }
      closeIo(O_IO(tmpBuffer));
      gcReleaseRoot(H, root);
    }
  }

  if (ret == Error)
    logMsg(logFile, "problem in loading %s/%d: %s", prgName, arity, errorMsg);

  return ret;
}

retCode loadType(ioPo in, heapPo h, packagePo owner, char *errorMsg, long msgSize) {
  char typeName[MAX_SYMB_LEN];
  integer typeNameLen;

  retCode ret = decodeName(in, typeName, NumberOf(typeName), &typeNameLen);

#ifdef TRACEPKG
  if (tracePkg >= detailedTracing)
    logMsg(logFile, "loading type %S", &typeName, typeNameLen);
#endif

  if (ret == Ok)
    ret = skipEncoded(in, errorMsg, msgSize); // Type rule signature
  if (ret == Ok) {                             // Type index
    integer count;
    ret = decodeTplCount(in, &count, errorMsg, msgSize);

    if (ret == Ok) {

      for (integer ix = 0; ret == Ok && ix < count; ix++) {
        if (isLookingAt(in, fieldPreamble) == Ok) {
          char lblName[MAX_SYMB_LEN];
          integer arity;

          ret = decodeLbl(in, lblName, NumberOf(lblName), &arity, errorMsg, msgSize);
          if (ret == Ok) {
            integer index;
            ret = decodeInteger(in, &index);
            if (ret == Ok) {
#ifdef TRACEPKG
              if (tracePkg >= detailedTracing)
                logMsg(logFile, "defining label %s/%d @ %d", &lblName, arity, index);
#endif
              declareLbl(lblName, arity, index);
            }
          }
        }
      }

    }
  }
  return ret;
}

retCode loadCtor(ioPo in, heapPo h, packagePo owner, char *errorMsg, long msgSize) {
  char lblName[MAX_SYMB_LEN];
  integer arity;

  retCode ret = decodeLbl(in, lblName, NumberOf(lblName), &arity, errorMsg, msgSize);

#ifdef TRACEPKG
  if (tracePkg >= detailedTracing)
    logMsg(logFile, "loading struct %s/%d", &lblName, arity);
#endif

  if (ret == Ok) {
    ret = skipEncoded(in, errorMsg, msgSize);

    if (ret == Ok) {
      integer index;
      ret = decodeInteger(in, &index);
      if (ret == Ok) {
        declareLbl(lblName, arity, index);
      }
    }
  }

  return ret;
}
