/*
  Copyright (c) 2016, 2017, 2018. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */

#include <stdlib.h>
#include "base64.h"
#include "pkgP.h"

#include "engine.h"
#include <globals.h>
#include "signature.h"
#include "decodeP.h"             /* pick up the term encoding definitions */
#include "manifest.h"
#include "libEscapes.h"
#include "codeP.h"
#include "labels.h"

static retCode decodePkgName(ioPo in, packagePo pkg);
static retCode decodeLbl(ioPo in, char *nm, long nmLen, integer *arity);
static retCode decodeLoadedPkg(packagePo pkg, ioPo in);
static retCode decodeImportsSig(bufferPo sigBuffer, char *errorMsg, long msgLen, pickupPkg pickup, void *cl);
static retCode decodeTplCount(ioPo in, integer *count, char *errMsg, integer msgLen);

static retCode loadSegments(ioPo in, packagePo owner, char *errorMsg, long msgLen);

static char stringSig[] = {strTrm, 0};
static char *pkgSig = "n4o4'()4'";

static retCode ldPackage(packagePo pkg, char *errorMsg, long msgSize, pickupPkg pickup, void *cl) {
  char flNm[MAXFILELEN];
  char *fn = manifestRsrcFlNm(pkg, "code", flNm, NumberOf(flNm));
  retCode ret = Ok;

  if (fn == NULL) {
    logMsg(logFile, "cannot determine code for %P%_", pkg);
    return Error;
  }

  ioPo file = openInFile(fn, utf8Encoding);

#ifdef TRACEPKG
  if (tracePkg)
    logMsg(logFile, "loading package %P from file %s", pkg, fn);
#endif

  if (file != NULL) {
    if (fileStatus(file) == Ok) {
      skipShellPreamble(file);

      PackageRec lddPkg;
      bufferPo sigBuffer = newStringBuffer();

      if ((ret = isLookingAt(file, stringSig)) == Ok)
        ret = decodeText(file, sigBuffer);

      if (ret == Ok) {
        rewindBuffer(sigBuffer);
        ret = decodeLoadedPkg(&lddPkg, O_IO(sigBuffer));
      }

      if (ret == Ok && uniCmp(lddPkg.packageName, pkg->packageName) != same) {
        outMsg(logFile, "loaded package: %P not what was expected %P\n", &lddPkg, pkg);
        return Error;
      }

      if (ret == Ok) {
        if (pickup != Null)
          ret = decodeImportsSig(sigBuffer, errorMsg, msgSize, pickup, cl);

        if (ret == Ok)
          ret = loadSegments(O_IO(sigBuffer), markLoaded(lddPkg.packageName, lddPkg.version), errorMsg, msgSize);
      }
    }

    closeFile(file);

    if (ret == Error)
      logMsg(logFile, "problem in loading %P: %s", pkg, errorMsg);

    return ret;
  } else {
    strMsg(errorMsg, msgSize, "package %P not found", pkg);
    return Eof;
  }
}

retCode loadPackage(packagePo p, char *errorMsg, long msgSize, void *cl) {
  char *version = loadedVersion(p->packageName);

  if (version != NULL) {
    if (!compatiblVersion(p->version, version)) {
      logMsg(logFile, "invalid version of package already loaded: %P,"
                      "version %s expected", p->version);
      return Error;
    } else
      return Ok; // already loaded correct version
  } else
    return ldPackage(p, errorMsg, msgSize, loadPackage, cl);
}

retCode installPackage(char *pkgText, long pkgTxtLen, char *errorMsg, long msgSize, pickupPkg pickup, void *cl) {
  bufferPo inBuff = fixedStringBuffer(pkgText, pkgTxtLen);

  retCode ret;
  PackageRec lddPkg;
  bufferPo sigBuffer = newStringBuffer();

  if ((ret = isLookingAt(O_IO(inBuff), "s")) == Ok)
    ret = decodeText(O_IO(inBuff), sigBuffer);

  if (ret == Ok) {
    rewindBuffer(sigBuffer);
    ret = decodeLoadedPkg(&lddPkg, O_IO(sigBuffer));

    if (ret == Ok) {
      ret = decodeImportsSig(sigBuffer, errorMsg, msgSize, pickup, cl);

      if (ret == Ok)
        ret = loadSegments(O_IO(inBuff), markLoaded(lddPkg.packageName, lddPkg.version), errorMsg, msgSize);
    }
  }

  closeFile(O_IO(inBuff));

#ifdef TRACEPKG
  if (tracePkg)
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

retCode decodeLoadedPkg(packagePo pkg, ioPo in) {
  if (isLookingAt(in, pkgSig) == Ok)
    return decodePkgName(in, pkg);
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
          ret = decodePkgName(in, &lddPkg);

        if (ret == Ok)
          ret = pickup(&lddPkg, errorMsg, msgLen, cl);
        if (ret != Ok)
          return ret;
      }
      return ret;
    } else
      return Error;
  } else
    return Error;
}

retCode decodePkgName(ioPo in, packagePo pkg) {
  if (isLookingAt(in, "n2o2'pkg's") == Ok) {
    bufferPo pkgB = fixedStringBuffer(pkg->packageName, NumberOf(pkg->packageName));
    bufferPo vrB = fixedStringBuffer(pkg->version, NumberOf(pkg->version));

    retCode ret = decodeText(O_IO(in), pkgB);

    if (ret == Ok) {
      if (isLookingAt(in, "e'*'") == Ok)
        outStr(O_IO(vrB), "*");
      else if (isLookingAt(in, "s") == Ok) {
        ret = decodeText(O_IO(in), vrB);
      }
    }

    outByte(O_IO(pkgB), 0);
    outByte(O_IO(vrB), 0);

    closeFile(O_IO(pkgB));
    closeFile(O_IO(vrB));
    return ret;
  } else
    return Error;
}

retCode decodeLbl(ioPo in, char *nm, long nmLen, integer *arity) {
  if (isLookingAt(in, "o") == Ok) {
    retCode ret = decInt(O_IO(in), arity);

    if (ret != Ok)
      return ret;
    else {
      bufferPo pkgB = fixedStringBuffer(nm, nmLen);
      ret = decodeText(O_IO(in), pkgB);
      outByte(O_IO(pkgB), 0);
      closeFile(O_IO(pkgB));
      return ret;
    }
  } else
    return Error;
}

retCode decodeTplCount(ioPo in, integer *count, char *errMsg, integer msgLen) {
  if (isLookingAt(in, "n") == Ok) {
    char nm[MAXLINE];
    integer ar;
    retCode ret = decInt(in, count);
    if (ret == Ok) {
      ret = decodeLbl(in, nm, NumberOf(nm), &ar);
      if (ret == Ok) {
        if (ar != *count)
          return Error;
      }
    }
    return ret;
  } else
    return Fail;
}

static retCode loadCodeSegment(ioPo in, heapPo H, packagePo owner, char *errorMsg, long msgSize);

retCode loadSegments(ioPo in, packagePo owner, char *errorMsg, long msgLen) {
  integer count;
  if (decodeTplCount(in, &count, errorMsg, msgLen) == Ok) {
    retCode ret = Ok;

    for (integer ix = 0; ret == Ok && ix < count; ix++)
      ret = loadCodeSegment(in, currHeap, owner, errorMsg, msgLen);
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

static void writeOperand(insPo *pc, int32 val) {
  int32 upper = (val >> 16) & 0xffff;
  int32 lower = (val & 0xffff);

  *(*pc)++ = (uint16) upper;
  *(*pc)++ = (uint16) lower;
}

static retCode decodeIns(ioPo in, insPo *pc, integer *ix, char *errorMsg, long msgSize) {
  integer op, and;
  char escNm[MAX_SYMB_LEN];
  retCode ret = decodeInteger(in, &op);
  *(*pc)++ = (insWord) op;
  (*ix)++;
  switch (op) {
#define sznOp
#define sztOs
#define szi32 if(ret==Ok){ret = decodeInteger(in,&and); writeOperand(pc,(int32)and); (*ix)++;}
#define szarg if(ret==Ok){ret = decodeInteger(in,&and); writeOperand(pc,(int32)and); (*ix)++;}
#define szlcl if(ret==Ok){ret = decodeInteger(in,&and); writeOperand(pc,(int32)and); (*ix)++;}
#define szlcs if(ret==Ok){ret = decodeInteger(in,&and); writeOperand(pc,(int32)and); (*ix)++;}
#define szoff if(ret==Ok){ret = decodeInteger(in,&and); writeOperand(pc,(int32)and); (*ix)++;}
#define szEs if(ret==Ok){ret = decodeString(in,escNm,NumberOf(escNm)); writeOperand(pc,lookupEscape(escNm)); (*ix)++;}
#define szlit if(ret==Ok){ret = decodeInteger(in,&and);  writeOperand(pc,(int32)and); (*ix)++;}
#define szglb if(ret==Ok){ret = decodeString(in,escNm,NumberOf(escNm)); writeOperand(pc,globalVarNo(escNm)); (*ix)++;}

#define instruction(Op, A1, Cmt)    \
      case Op:          \
  sz##A1          \
    return ret;

#include "instructions.h"

#undef instruction
#undef szi32
#undef szarg
#undef szlcl
#undef szlcs
#undef szoff
#undef szEs
#undef szlit
#undef szglb
#undef sznOp
#undef sztOs
    default:
      return Error;
  }
}

retCode loadCodeSegment(ioPo in, heapPo H, packagePo owner, char *errorMsg, long msgSize) {
  retCode ret = isLookingAt(in, "n6o6'()6'");

  if (ret != Ok) {
    strMsg(errorMsg, msgSize, "invalid code stream");
    return Error;
  }
  char prgName[MAX_SYMB_LEN];
  integer arity;
  integer lclCount;

  ret = decodeLbl(in, prgName, NumberOf(prgName), &arity);

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
        ret = decodeIns(in, &pc, &ix, errorMsg, msgSize);
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
            labelPo lbl = declareLbl(prgName, arity);
            gcAddRoot(H, (ptrPo) &lbl);
            defineMtd(H, ins, (integer) (pc - ins), lclCount, lbl, C_TERM(pool), C_TERM(locals));
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


