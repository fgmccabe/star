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
#include "signature.h"
#include "decodeP.h"             /* pick up the term encoding definitions */
#include "manifest.h"
#include "libEscapes.h"
#include "codeP.h"

typedef retCode (*pickupPkg)(char *pkgNm, char *vers, char *errorMsg, long msgLen, void *cl);

static retCode decodePkgName(ioPo in, char *nm, long nmLen, char *v, long vLen);
static retCode decodePrgName(ioPo in, char *nm, long nmLen, integer *arity);
static retCode decodeLoadedPkg(char *pkgNm, long nmLen, char *vrNm, long vrLen, ioPo in);
static retCode decodeImportsSig(bufferPo sigBuffer, char *errorMsg, long msgLen, pickupPkg pickup, void *cl);
static retCode decodeTplCount(ioPo in, integer *count);

static retCode loadSegments(ioPo file, pkgPo owner, char *errorMsg, long msgLen);

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

#ifdef RESOURCETRACE
  if (traceResource)
    outMsg(logFile, "loading package %s:%s from file %s\n", pkgName, vers, fn);
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
        outMsg(logFile, "loaded package: %s not what was expected %w\n", (char *) pkgNm, pkgName);
        return Error;
      }

      pkgPo pkg = markLoaded(pkgNm, vrNm);

      ret = decodeImportsSig(sigBuffer, errorMsg, msgSize, pickup, cl);

      if (ret == Ok)
        ret = loadSegments(file, pkg, errorMsg, msgSize);
    }

    closeFile(file);

#ifdef RESOURCETRACE
    if (traceResource) {
      if (ret != Error)
        logMsg(logFile, "package %s loaded\n", pkgName);
      else
        logMsg(logFile, "problem in loading %s: %s", pkgName, errorMsg);
    }
#endif

    if (ret == Eof)
      return Ok;
    else
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

static retCode
installPackage(char *pkgText, long pkgTxtLen, char *errorMsg, long msgSize, pickupPkg pickup, void *cl) {
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

#ifdef RESOURCETRACE
  if (traceResource)
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
    retCode ret = skipEncoded(in, errorMsg, msgLen);
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

    retCode ret = decodeName(O_IO(in), pkgB);

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

retCode decodePrgName(ioPo in, char *nm, long nmLen, integer *arity) {
  if (isLookingAt(in, "p") == Ok) {
    retCode ret = decInt(O_IO(in), arity);

    if (ret != Ok)
      return ret;
    else {
      bufferPo pkgB = fixedStringBuffer(nm, nmLen);
      ret = decodeName(O_IO(in), pkgB);
      outByte(O_IO(pkgB), 0);
      closeFile(O_IO(pkgB));
      return ret;
    }
  } else
    return Error;
}

retCode decodeTplCount(ioPo in, integer *cound) {
  if (isLookingAt(in, "()") == Ok) {
    return Error;
  } else
    return Fail;
}

static retCode loadCodeSegment(ioPo in, heapPo heap, pkgPo owner, char *errorMsg, long msgSize);

retCode loadSegments(ioPo file, pkgPo owner, char *errorMsg, long msgLen) {
  retCode ret = Ok;

  while (ret == Ok) {
    ret = loadCodeSegment(file, currHeap, owner, errorMsg, msgLen);
  }

  return ret;
}


// Decode a code segment.
// Each segment consists of
// a. The program name/arity being defined
// b. An integer defining the constant id of the signature
// b. A tuple of the instructions
// c. A tuple of literals associated with the code segment
// d. A tuple of variable specifications
// e. A tuple of frame entries
// All wrapped up as a #code structure.

// Each frame entry consists of
// a. The pool constant of the frame type signature
// b. The program counter the frame applies to

// Each variable specification consists of
// a. The pool constant for the variable name
// b. The pool constant for the variable type
// c. The variable number
// d. The pc offset of the start of the validity range
// e. The pc offset of the end of the validity range

static integer writeOperand(insPo ins, int32 val, integer ix) {
  int32 upper = (val >> 16) & 0xffff;
  int32 lower = (val & 0xffff);

  ins[ix++] = (uint16) upper;
  ins[ix++] = (uint16) lower;
  return ix;
}

retCode loadCodeSegment(ioPo in, heapPo heap, pkgPo owner, char *errorMsg, long msgSize) {
  EncodeSupport sp = {
    errorMsg,
    msgSize,
    heap,
  };

  if (isFileAtEof(in) == Eof)
    return Eof;
  else {
    retCode ret = isLookingAt(in, "n6o6'#code'");

    if (ret != Ok) {
      strMsg(errorMsg, msgSize, "invalid code stream");
      return Error;
    }
    char prgName[MAX_SYMB_LEN];
    integer arity;

    ret = decodePrgName(in, prgName, NumberOf(prgName), &arity);

#ifdef RESOURCETRACE
    if (traceResource)
      outMsg(logFile, "loading code %s/%d\n%_", prgName, arity);
#endif

    integer sigNum;
    if (ret == Ok) {
      ret = decInt(in, &sigNum);

      integer insCount;
      if (ret == Ok) {
        ret = decodeTplCount(in, &insCount);

        if (ret == Ok) {
          insPo ins = (insPo) malloc(sizeof(insWord) * insCount);
          for (integer ix = 0; ret == Ok && ix < insCount;) {
            integer op, and;
            char escNm[MAX_SYMB_LEN];
            ret = decInt(in, &op);
            ins[ix++] = (insWord) op;
            switch (op) {
#define sznOp
#define szi32 if(ret==Ok){ret = decInt(in,&and); ix=writeOperand(ins,(int32)and,ix);}
#define szarg if(ret==Ok){ret = decInt(in,&and); ix=writeOperand(ins,(int32)and,ix);}
#define szlcl if(ret==Ok){ret = decInt(in,&and); ix=writeOperand(ins,(int32)and,ix);}
#define szoff if(ret==Ok){ret = decInt(in,&and); ix=writeOperand(ins,(int32)and,ix);}
#define szEs if(ret==Ok){ret = decodeNm(in,escNm,NumberOf(escNm)); ix=writeOperand(ins,lookupEscape(escNm),ix);}
#define szlit if(ret==Ok){ret = decInt(in,&and);  ix=writeOperand(ins,and,ix);}

#define instruction(Op, A1, Cmt)    \
      case Op:          \
  sz##A1          \
    break;

#include "instructions.h"

#undef instruction
#undef szi32
#undef szarg
#undef szlcl
#undef szoff
#undef szEs
#undef szlit
#undef sznOp
              default:
                return Error;
            }
          }
          if (ret == Ok) {
            termPo pool;
            ret = decodeTerm(in, heap, &pool, errorMsg, msgSize);

            if (ret == Ok) {
              int root = gcAddRoot(&pool);

              termPo frames;
              ret = decodeTerm(in, heap, &frames, errorMsg, msgSize);

              if (ret == Ok) {
                gcAddRoot(&frames);

                termPo locals;
                ret = decodeTerm(in, heap, &locals, errorMsg, msgSize);

                if (ret == Ok) {
                  labelPo lbl = defineMtd(ins, insCount, prgName, arity, C_TERM(pool), C_TERM(locals));

                  gcReleaseRoot(root);
                  return installMethod(owner, lbl);
                }

              }
            }

          }
        }
      }
    }

    return ret;
  }
}
