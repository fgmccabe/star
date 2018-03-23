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

static retCode decodePkgName(ioPo in, char *nm, long nmLen, char *v, long vLen);
static retCode decodeLbl(ioPo in, char *nm, long nmLen, integer *arity);
static retCode decodeLoadedPkg(char *pkgNm, long nmLen, char *vrNm, long vrLen, ioPo in);
static retCode decodeImportsSig(bufferPo sigBuffer, char *errorMsg, long msgLen, pickupPkg pickup, void *cl);
static retCode decodeTplCount(ioPo in, integer *count, char *errMsg, integer msgLen);

static retCode loadSegments(ioPo in, pkgPo owner, char *errorMsg, long msgLen);

static char stringSig[] = {strTrm, 0};
static char *pkgSig = "n4o4'()4'";

static retCode ldPackage(char *pkgName, char *vers, char *errorMsg, long msgSize, pickupPkg pickup, void *cl) {
  char flNm[MAXFILELEN];
  char *fn = manifestRsrcFlNm(pkgName, vers, "code", flNm, NumberOf(flNm));
  retCode ret = Ok;

  if (fn == NULL) {
    outMsg(logFile, "cannot determine code for %s:%s%_", pkgName, vers);
    return Error;
  }

  ioPo file = openInFile(fn, utf8Encoding);

#ifdef TRACEPKG
  if (tracePkg)
    logMsg(logFile, "loading package %s:%s from file %s\n", pkgName, vers, fn);
#endif

  if (file != NULL) {
    if (fileStatus(file) == Ok) {
      skipShellPreamble(file);

      char pkgNm[MAX_SYMB_LEN];
      char vrNm[MAX_SYMB_LEN];
      bufferPo sigBuffer = newStringBuffer();

      if ((ret = isLookingAt(file, stringSig)) == Ok)
        ret = decodeText(file, sigBuffer);

      if (ret == Ok) {
        rewindBuffer(sigBuffer);
        ret = decodeLoadedPkg(pkgNm, NumberOf(pkgNm), vrNm, NumberOf(vrNm), O_IO(sigBuffer));
      }

      if (ret == Ok && uniCmp((char *) pkgNm, pkgName) != same) {
        outMsg(logFile, "loaded package: %s not what was expected %T\n", (char *) pkgNm, pkgName);
        return Error;
      }

      if (ret == Ok) {
        pkgPo pkg = markLoaded(pkgNm, vrNm);

        if (pickup != Null)
          ret = decodeImportsSig(sigBuffer, errorMsg, msgSize, pickup, cl);

        if (ret == Ok)
          ret = loadSegments(O_IO(sigBuffer), pkg, errorMsg, msgSize);
      }
    }

    closeFile(file);

#ifdef TRACEPKG
    if (tracePkg) {
      if (ret != Error)
        logMsg(logFile, "package %s loaded\n", pkgName);
      else
        logMsg(logFile, "problem in loading %s: %s", pkgName, errorMsg);
    }
#endif

    return ret;
  } else {
    strMsg(errorMsg, msgSize, "package %s not found", pkgName);
    return Eof;
  }
}

retCode loadPackage(char *pkg, char *vers, char *errorMsg, long msgSize, void *cl) {
  char *version = loadedVersion(pkg);

  if (version != NULL) {
    if (!compatiblVersion(vers, version)) {
      outMsg(logFile, "invalid version of package already loaded: %s:%s,"
        "version %s expected\n", pkg, version, vers);
      return Error;
    } else
      return Ok; // already loaded correct version
  } else
    return ldPackage(pkg, vers, errorMsg, msgSize, loadPackage, cl);
}

retCode installPackage(char *pkgText, long pkgTxtLen, char *errorMsg, long msgSize, pickupPkg pickup, void *cl) {
  bufferPo inBuff = fixedStringBuffer(pkgText, pkgTxtLen);

  retCode ret;
  char pkgNm[MAX_SYMB_LEN];
  char vrNm[MAX_SYMB_LEN];
  bufferPo sigBuffer = newStringBuffer();

  if ((ret = isLookingAt(O_IO(inBuff), "s")) == Ok)
    ret = decodeText(O_IO(inBuff), sigBuffer);

  if (ret == Ok) {
    rewindBuffer(sigBuffer);
    ret = decodeLoadedPkg(pkgNm, NumberOf(pkgNm), vrNm, NumberOf(vrNm), O_IO(sigBuffer));

    if (ret == Ok) {

      pkgPo pkg = markLoaded(pkgNm, vrNm);

      ret = decodeImportsSig(sigBuffer, errorMsg, msgSize, pickup, cl);

      if (ret == Ok)
        ret = loadSegments(O_IO(inBuff), pkg, errorMsg, msgSize);
    }
  }

  closeFile(O_IO(inBuff));

#ifdef TRACEPKG
  if (tracePkg)
    logMsg(logFile, "package %s installed\n", pkgNm);
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

retCode decodeLoadedPkg(char *pkgNm, long nmLen, char *vrNm, long vrLen, ioPo in) {
  if (isLookingAt(in, pkgSig) == Ok)
    return decodePkgName(in, pkgNm, nmLen, vrNm, vrLen);
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
        if (ret == Ok)
          ret = isLookingAt(in, "n2o2'import'");

        if (ret == Ok)
          ret = skipEncoded(in, errorMsg, msgLen);  // skip the private/public flag

        char pkgNm[MAX_SYMB_LEN];
        char vrNm[MAX_SYMB_LEN];

        if (ret == Ok)
          ret = decodePkgName(in, &pkgNm[0], NumberOf(pkgNm), &vrNm[0], NumberOf(vrNm));

        if (ret == Ok)
          ret = pickup(pkgNm, vrNm, errorMsg, msgLen, cl);
        if (ret != Ok)
          return ret;
      }
      return ret;
    } else
      return Error;
  } else
    return Error;
}

retCode decodePkgName(ioPo in, char *nm, long nmLen, char *v, long vLen) {
  if (isLookingAt(in, "n2o2'pkg's") == Ok) {
    bufferPo pkgB = fixedStringBuffer(nm, nmLen);
    bufferPo vrB = fixedStringBuffer(v, vLen);

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

static retCode loadCodeSegment(ioPo in, heapPo H, pkgPo owner, char *errorMsg, long msgSize);

retCode loadSegments(ioPo in, pkgPo owner, char *errorMsg, long msgLen) {
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
#define szi32 if(ret==Ok){ret = decodeInteger(in,&and); writeOperand(pc,(int32)and); (*ix)++;}
#define szarg if(ret==Ok){ret = decodeInteger(in,&and); writeOperand(pc,(int32)and); (*ix)++;}
#define szlcl if(ret==Ok){ret = decodeInteger(in,&and); writeOperand(pc,(int32)and); (*ix)++;}
#define szlcs if(ret==Ok){ret = decodeInteger(in,&and); writeOperand(pc,(int32)and); (*ix)++;}
#define szoff if(ret==Ok){ret = decodeInteger(in,&and); writeOperand(pc,(int32)and); (*ix)++;}
#define szEs if(ret==Ok){ret = decodeString(in,escNm,NumberOf(escNm)); writeOperand(pc,lookupEscape(escNm)); (*ix)++;}
#define szlit if(ret==Ok){ret = decodeInteger(in,&and);  writeOperand(pc,(int32)and); (*ix)++;}

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
#undef sznOp
    default:
      return Error;
  }
}

retCode loadCodeSegment(ioPo in, heapPo H, pkgPo owner, char *errorMsg, long msgSize) {
  retCode ret = isLookingAt(in, "n5o5'()5'");

  if (ret != Ok) {
    strMsg(errorMsg, msgSize, "invalid code stream");
    return Error;
  }
  char prgName[MAX_SYMB_LEN];
  integer arity;

  ret = decodeLbl(in, prgName, NumberOf(prgName), &arity);

  if (ret == Ok)
    ret = skipEncoded(in, errorMsg, msgSize); // Skip the code signature

  if (ret == Ok) {
    integer insCount;
    ret = decodeTplCount(in, &insCount, errorMsg, msgSize);

    if (ret == Ok) {
      insPo ins = (insPo) malloc(sizeof(insWord) * insCount * 2);
      insPo pc = ins;
      for (integer ix = 0; ret == Ok && ix < insCount;) {
        ret = decodeIns(in, &pc, &ix, errorMsg, msgSize);
      }

      integer lclCnt = 0;

      if (ins[0] == Enter) {
        lclCnt = (ins[1] << 16) | ins[2];
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
            defineMtd(H, ins, (integer) (pc - ins), lclCnt, lbl, C_TERM(pool), C_TERM(locals));
          }
        }
        closeFile(O_IO(tmpBuffer));
        gcReleaseRoot(H, root);
      }
      free(ins);
      return Ok;
    }
  }
  return ret;
}


