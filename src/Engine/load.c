/*
  Copyright (c) 2016, 2017, 2018. Francis G. McCabe
 */

#include <stdlib.h>
#include "pkgP.h"

#include "engine.h"
#include <globals.h>
#include "signature.h"
#include "decodeP.h"             /* pick up the term encoding definitions */
#include "manifest.h"
#include "libEscapes.h"
#include "codeP.h"
#include "labelsP.h"
#include "verifyP.h"
#include "globalsP.h"

tracingLevel tracePkg = noTracing;

static retCode decodePkgName(ioPo in, packagePo pkg, char *errorMsg, integer msgLen);

static retCode decodeLoadedPkg(packagePo pkg, ioPo in, char *errorMsg, integer msgLen);

static retCode decodeImportsSig(bufferPo sigBuffer, char *errorMsg, long msgLen, pickupPkg pickup, void *cl);

static retCode decodeTplCount(ioPo in, integer *count, char *errMsg, integer msgLen);

static retCode loadDefs(ioPo in, heapPo h, packagePo owner, char *errorMsg, long msgLen);

static char stringSig[] = {strTrm, 0};
static char *pkgSig = "n4o4\1()4\1";

static retCode ldPackage(packagePo pkg, char *errorMsg, long msgSize, pickupPkg pickup, void *cl) {
  char codeName[MAXFILELEN];
  retCode ret = manifestResource(pkg, "code", codeName, NumberOf(codeName));

  if (ret !=Ok) {
    strMsg(errorMsg, msgSize, "cannot determine code for %P%_", pkg);
    return Error;
  } else {
    char codeFlNm[MAXFILELEN];
    strMsg(codeFlNm, NumberOf(codeFlNm), "%s/%s", repoDir, codeName);

    ioPo file = openInFile(codeFlNm, utf8Encoding);

#ifdef TRACEPKG
    if (tracePkg>=generalTracing)
      logMsg(logFile, "loading package %P from file %s", pkg, codeFlNm);
#endif

    if (file != NULL) {
      if (fileStatus(file) == Ok) {

        skipShellPreamble(O_FILE(file));

        PackageRec lddPkg;
        bufferPo sigBuffer = newStringBuffer();

        if ((ret = isLookingAt(file, stringSig)) == Ok)
          ret = decodeText(file, sigBuffer);

        if (ret == Ok) {
          rewindBuffer(sigBuffer);
          ret = decodeLoadedPkg(&lddPkg, O_IO(sigBuffer), errorMsg, msgSize);
        }

        if (ret == Ok && uniCmp(lddPkg.packageName, pkg->packageName) != same) {
          closeFile(O_IO(sigBuffer));
          strMsg(errorMsg, msgSize, "loaded package: %P not what was expected %P\n", &lddPkg, pkg);
          return Error;
        }

        if (ret == Ok) {
          if (pickup != Null)
            ret = decodeImportsSig(sigBuffer, errorMsg, msgSize, pickup, cl);

          if (ret == Ok)
            ret = loadDefs(O_IO(sigBuffer), currHeap, markLoaded(lddPkg.packageName, lddPkg.version), errorMsg,
                           msgSize);
        }
        closeFile(O_IO(sigBuffer));
      }

      closeFile(file);

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

retCode
installPackage(char *pkgText, long pkgTxtLen, heapPo H, char *errorMsg, long msgSize, pickupPkg pickup, void *cl) {
  bufferPo inBuff = newReadStringBuffer(pkgText, pkgTxtLen);

  retCode ret;
  PackageRec lddPkg;
  bufferPo sigBuffer = newStringBuffer();

  if ((ret = isLookingAt(O_IO(inBuff), "s")) == Ok)
    ret = decodeText(O_IO(inBuff), sigBuffer);

  if (ret == Ok) {
    rewindBuffer(sigBuffer);
    ret = decodeLoadedPkg(&lddPkg, O_IO(sigBuffer), errorMsg, msgSize);

    if (ret == Ok && !isLoadedPackage(&lddPkg)) {
      ret = decodeImportsSig(sigBuffer, errorMsg, msgSize, pickup, cl);

      if (ret == Ok)
        ret = loadDefs(O_IO(sigBuffer), H, markLoaded(lddPkg.packageName, lddPkg.version), errorMsg, msgSize);
    }
#ifdef TRACEPKG
    else if (tracePkg>=generalTracing)
      logMsg(logFile, "package %P already installed", &lddPkg);
#endif
  }

  closeFile(O_IO(inBuff));
  closeFile(O_IO(sigBuffer));

#ifdef TRACEPKG
  if (tracePkg>=generalTracing)
    logMsg(logFile, "package %P installed", &lddPkg);
#endif

  if (ret == Eof)
    return Ok;
  else
    return ret;
}

/*
 * A package signature consists of a tuple of 7 elements:
 * (pkg,imports,fields,types,contracts,implementations)
 *
 * We are only interested in the first two the pkg and the imports.
 */

retCode decodeLoadedPkg(packagePo pkg, ioPo in, char *errorMsg, integer msgLen) {
  if (isLookingAt(in, pkgSig) == Ok)
    return decodePkgName(in, pkg, errorMsg, msgLen);
  else
    return Error;
}

static retCode decodeImportsSig(bufferPo sigBuffer, char *errorMsg, long msgLen, pickupPkg pickup, void *cl) {
  rewindBuffer(sigBuffer);
  ioPo in = O_IO(sigBuffer);

  if (isLookingAt(in, pkgSig) == Ok) {
    retCode ret = skipEncoded(in, errorMsg, msgLen); // The package name
    if (ret != Ok)
      return ret;

    ret = skipEncoded(in, errorMsg, msgLen); // The package signature
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
    bufferPo pkgB = fixedStringBuffer(pkg->packageName, NumberOf(pkg->packageName));
    bufferPo vrB = fixedStringBuffer(pkg->version, NumberOf(pkg->version));

    retCode ret = decodeText(O_IO(in), pkgB);

    if (ret == Ok) {
      if (isLookingAt(in, "e\1*\1") == Ok)
        outStr(O_IO(vrB), "*");
      else if (isLookingAt(in, "s") == Ok) {
        ret = decodeText(O_IO(in), vrB);
      } else
        return Error;
    }

    outByte(O_IO(pkgB), 0);
    outByte(O_IO(vrB), 0);

    closeFile(O_IO(pkgB));
    closeFile(O_IO(vrB));
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

static char *funcPreamble = "n7o7\1()7\1";
static char *structPreamble = "n3o3\1()3\1";
static char *globalPreamble = "n7o7\1global\1";
static char *methodPreamble = "n7o7\1method\1";

static retCode loadFunc(ioPo in, heapPo H, packagePo owner, char *errorMsg, long msgSize);
static retCode loadStruct(ioPo in, heapPo h, packagePo owner, char *errorMsg, long msgSize);
static retCode loadGlobal(ioPo in, heapPo h, packagePo owner, char *errorMsg, long msgSize);

retCode loadDefs(ioPo in, heapPo h, packagePo owner, char *errorMsg, long msgLen) {
  integer count;
  if (decodeTplCount(in, &count, errorMsg, msgLen) == Ok) {
    retCode ret = Ok;

    for (integer ix = 0; ret == Ok && ix < count; ix++) {
      if (isLookingAt(in, funcPreamble) == Ok || isLookingAt(in, methodPreamble) == Ok)
        ret = loadFunc(in, h, owner, errorMsg, msgLen);
      else if (isLookingAt(in, structPreamble) == Ok)
        ret = loadStruct(in, h, owner, errorMsg, msgLen);
      else if (isLookingAt(in, globalPreamble) == Ok)
        ret = loadGlobal(in, h, owner, errorMsg, msgLen);
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
// d. The pc offset of the start of the validity range
// e. The pc offset of the end of the validity range

static void writeOperand(insPo *pc, uint32 val) {
  int32 upper = (val >> (unsigned) 16) & (unsigned) 0xffff;
  int32 lower = (val & (unsigned) 0xffff);

  *(*pc)++ = (uint16) upper;
  *(*pc)++ = (uint16) lower;
}

static retCode writeIntOperand(ioPo in, insPo *pc, integer *ix) {
  integer and;
  retCode ret = decodeInteger(in, &and);
  writeOperand(pc, (uint32) and);
  (*ix)++;
  return ret;
}

static retCode decodeIns(ioPo in, insPo *pc, integer *ix, integer *si, char *errorMsg, long msgSize) {
  integer op, and;
  char escNm[MAX_SYMB_LEN];
  retCode ret = decodeInteger(in, &op);

  if (ret == Ok) {
    *(*pc)++ = (insWord) op;
    (*ix)++;
    switch (op) {
#define sznOp
#define sztOs
#define szart return writeIntOperand(in,pc,ix);
#define szi32 if(ret==Ok){ret = writeIntOperand(in,pc,ix);}
#define szarg if(ret==Ok){ret = writeIntOperand(in,pc,ix); }
#define szlcl if(ret==Ok){ret = writeIntOperand(in,pc,ix); }
#define szlcs if(ret==Ok){ret = writeIntOperand(in,pc,ix);}
#define szoff if(ret==Ok){ret = writeIntOperand(in,pc,ix); }
#define szsym if(ret==Ok){ret = writeIntOperand(in,pc,ix); }
#define szEs if(ret==Ok){ret = decodeString(in,escNm,NumberOf(escNm)); writeOperand(pc,lookupEscape(escNm)); (*ix)++;}
#define szlit if(ret==Ok){ret = writeIntOperand(in,pc,ix); }
#define szlne if(ret==Ok){ret = writeIntOperand(in,pc,ix); }
#define szglb if(ret==Ok){ret = decodeString(in,escNm,NumberOf(escNm)); writeOperand(pc,globalVarNo(escNm)); (*ix)++;}

#define instruction(Op, A1, Dl, Cmt)    \
      case Op:          \
        (*si)+=Dl;      \
        sz##A1          \
        return ret;

#include "instructions.h"

#undef instruction
#undef szi32
#undef szart
#undef szarg
#undef szlcl
#undef szlcs
#undef szoff
#undef szsym
#undef szEs
#undef szlit
#undef szlne
#undef szglb
#undef sznOp
#undef sztOs
      default: {
        strMsg(errorMsg, msgSize, "invalid instruction encoding");
        return Error;
      }
    }
  }
  return ret;
}

static integer stackInc(insWord);

retCode loadFunc(ioPo in, heapPo H, packagePo owner, char *errorMsg, long msgSize) {
  char prgName[MAX_SYMB_LEN];
  integer arity;
  integer lclCount = 0;
  integer maxStack = 0;

  retCode ret = decodeLbl(in, prgName, NumberOf(prgName), &arity,
                          errorMsg, msgSize);

#ifdef TRACEPKG
  if (tracePkg>=detailedTracing)
    logMsg(logFile, "loading function %s/%d", &prgName, arity);
#endif

  if (ret == Ok)
    ret = skipEncoded(in, errorMsg, msgSize); // Skip the code signature

  if (ret == Ok)
    ret = decodeInteger(in, &lclCount);

  if (ret == Ok) {
    integer insCount;
    ret = decodeTplCount(in, &insCount, errorMsg, msgSize);

    if (ret == Ok) {
      insPo ins = (insPo) malloc(sizeof(insWord) * insCount * 2);
      insPo pc = ins;
      for (integer ix = 0; ret == Ok && ix < insCount;) {
        integer stackInc = 0;
        ret = decodeIns(in, &pc, &ix, &stackInc, errorMsg, msgSize);
        maxStack += stackInc;
      }

      if (ret == Ok) {
        termPo pool = voidEnum;
        int root = gcAddRoot(H, &pool);
        EncodeSupport support = {errorMsg, msgSize, H};
        bufferPo tmpBuffer = newStringBuffer();

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
              labelPo lbl = declareLbl(prgName, arity);
              gcAddRoot(H, (ptrPo) &lbl);

              methodPo mtd = defineMtd(H, ins, (integer) (pc - ins), lclCount, maxStack, lbl, C_TERM(pool),
                                       C_TERM(locals),
                                       C_TERM(lines));
              if (enableVerify)
                ret = verifyMethod(mtd, prgName, errorMsg, msgSize);
            }
          }
        }
        closeFile(O_IO(tmpBuffer));
        gcReleaseRoot(H, root);
      }
      free(ins);
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
  integer maxStack = 0;

  retCode ret = decodeLbl(in, prgName, NumberOf(prgName), &arity,
                          errorMsg, msgSize);

  if (arity != 0) {
    strMsg(errorMsg, msgSize, "global must have arity 0, not %d", arity);
    return Error;
  }

#ifdef TRACEPKG
  if (tracePkg>=detailedTracing)
    logMsg(logFile, "loading global %s", &prgName);
#endif

  if (ret == Ok)
    ret = skipEncoded(in, errorMsg, msgSize); // Skip the code signature

  if (ret == Ok)
    ret = decodeInteger(in, &lclCount);

  if (ret == Ok) {
    integer insCount;
    ret = decodeTplCount(in, &insCount, errorMsg, msgSize);

    if (ret == Ok) {
      insPo ins = (insPo) malloc(sizeof(insWord) * insCount * 2);
      insPo pc = ins;
      for (integer ix = 0; ret == Ok && ix < insCount;) {
        integer stackInc = 0;
        ret = decodeIns(in, &pc, &ix, &stackInc, errorMsg, msgSize);
        maxStack += stackInc;
      }

      if (ret == Ok) {
        termPo pool = voidEnum;
        int root = gcAddRoot(H, &pool);
        EncodeSupport support = {errorMsg, msgSize, H};
        bufferPo tmpBuffer = newStringBuffer();

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
              labelPo lbl = declareLbl(prgName, 0);
              gcAddRoot(H, (ptrPo) &lbl);

              methodPo mtd = defineMtd(H, ins, (integer) (pc - ins), lclCount, maxStack, lbl, C_TERM(pool),
                                       C_TERM(locals),
                                       C_TERM(lines));
              if (enableVerify)
                ret = verifyMethod(mtd, prgName, errorMsg, msgSize);

              if (ret == Ok) {
                normalPo glbSym = allocateStruct(H, lbl); /* allocate a closure on the heap */
                globalPo glb = globalVar(prgName, (termPo) glbSym);
                if (glb == Null)
                  ret = Fail;
              }
            }
          }
        }
        closeFile(O_IO(tmpBuffer));
        gcReleaseRoot(H, root);
      }
      free(ins);
    }
  }

  if (ret == Error)
    logMsg(logFile, "problem in loading %s/%d: %s", prgName, arity, errorMsg);

  return ret;
}

retCode loadStruct(ioPo in, heapPo h, packagePo owner, char *errorMsg, long msgSize) {
  char lblName[MAX_SYMB_LEN];
  integer arity;

  retCode ret = decodeLbl(in, lblName, NumberOf(lblName), &arity,
                          errorMsg, msgSize);

#ifdef TRACEPKG
  if (tracePkg>=detailedTracing)
    logMsg(logFile, "loading structure definition %s/%d", &lblName, arity);
#endif

  if (ret == Ok)
    ret = skipEncoded(in, errorMsg, msgSize); // Skip the structure signature

  if (ret == Ok) {
    labelPo lbl = declareLbl(lblName, arity);

    integer count;
    ret = decodeTplCount(in, &count, errorMsg, msgSize);

    if (ret == Ok) {
      fieldTblPo fieldTbl = newFieldTable(count);
      declareFields(lbl, fieldTbl);

      for (integer ix = 0; ret == Ok && ix < count; ix++) {
        if (isLookingAt(in, fieldPreamble) == Ok) {
          ret = decodeLbl(in, lblName, NumberOf(lblName), &arity,
                          errorMsg, msgSize);
          if (ret == Ok) {
            labelPo field = declareLbl(lblName, arity);
            ret = skipEncoded(in, errorMsg, msgSize); // Field signature
            if (ret == Ok) {
              integer offset, size;
              ret = decodeInteger(in, &offset);
              if (ret == Ok) {
                setFieldTblEntry(fieldTbl, field, offset);
                ret = decodeInteger(in, &size);
              }
            }
          }
        }
      }
      if (ret != Ok) {
        clearFieldTable(lbl);
      }
    }
  }
  return ret;
}
