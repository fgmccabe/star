/*
  Header file giving the interface for tuples
  Copyright (c) 2016, 2017. Francis G. McCabe
  
  Created by Francis McCabe on 2/3/17.

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */

#include "base64.h"
#include "pkgP.h"

#include "engine.h"
#include "signature.h"
#include "decodeP.h"             /* pick up the term encoding definitions */
#include "../Infra/Headers/manifestP.h"
#include "tpl.h"

static poolPo packagePool = NULL;
static hashPo loadedPackages = NULL;

void initPackages() {
  packagePool = newPool(sizeof(PackageRec), 128);
  loadedPackages = NewHash(128, (hashFun) uniHash, (compFun) uniCmp, NULL);
}

packagePo loadedPackage(char *package) {
  return (packagePo) hashGet(loadedPackages, package);
}

char *loadedVersion(char *package) {
  packagePo pkg = loadedPackage(package);

  if (pkg != NULL)
    return (char *) &pkg->version;

  return NULL;
}

char *pkgName(packagePo pkg) {
  return (char *) &pkg->packageName;
}

char *pkgVers(packagePo pkg) {
  return (char *) &pkg->version;
}

static logical compatiblVersion(char *rqVer, char *ver) {
  return (logical) (uniCmp(rqVer, "*") == same || uniCmp(rqVer, ver) == same);
}

static packagePo markLoaded(char *package, char *version) {
  packagePo pkg = loadedPackage(package);

  if (pkg != NULL) {
    if (!compatiblVersion((char *) &pkg->version, version))
      return Null;
    else
      return pkg;
  } else {
    pkg = (packagePo) allocPool(packagePool);
    uniCpy((char *) &pkg->packageName, NumberOf(pkg->packageName), package);
    uniCpy((char *) &pkg->version, NumberOf(pkg->version), version);
    hashPut(loadedPackages, &pkg->packageName, pkg);
    return pkg;
  }
}

typedef retCode (*pickupPkg)(char *pkgNm, char *vers, char *errorMsg, long msgLen, void *cl);

static retCode decodePkgName(ioPo in, char *nm, long nmLen, char *v, long vLen);
static retCode decodePrgName(ioPo in, char *nm, long nmLen, integer *arity);
static retCode decodeLoadedPkg(char *pkgNm, long nmLen, char *vrNm, long vrLen, bufferPo sigBuffer);
static retCode decodeImportsSig(bufferPo sigBuffer, char *errorMsg, long msgLen, pickupPkg pickup, void *cl);

static retCode loadSegments(ioPo file, packagePo owner, char *errorMsg, long msgLen);

static retCode ldPackage(char *pkgName, char *vers, char *errorMsg, long msgSize, pickupPkg pickup, void *cl) {
  char flNm[MAXFILELEN];
  char *fn = packageCodeFile(pkgName, vers, flNm, NumberOf(flNm));
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
    byte ch;

    if ((ch = inB(file)) == '#') { /* look for standard #!/.... header */
      if ((ch = inB(file)) == '!') {
        while (inByte(file, &ch) == Ok && ch != NEW_LINE);                      // consume the interpreter statement
      } else {
        putBackByte(file, ch);
        putBackByte(file, '#');
      }
    } else
      putBackByte(file, ch);

    if (fileStatus(file) == Ok) {
      char pkgNm[MAX_SYMB_LEN];
      char vrNm[MAX_SYMB_LEN];
      bufferPo sigBuffer = newStringBuffer();

      if ((ret = isLookingAt(file, "s")) == Ok)
        ret = decodeText(file, sigBuffer);

      if (ret == Ok)
        ret = decodeLoadedPkg(pkgNm, NumberOf(pkgNm), vrNm, NumberOf(vrNm), sigBuffer);

      if (ret == Ok && uniCmp((char *) pkgNm, pkgName) != same) {
        outMsg(logFile, "loaded package: %s not what was expected %w\n", (char *) pkgNm, pkgName);
        return Error;
      }

      packagePo pkg = markLoaded(pkgNm, vrNm);

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

  if (ret == Ok)
    ret = decodeLoadedPkg(pkgNm, NumberOf(pkgNm), vrNm, NumberOf(vrNm), sigBuffer);

  packagePo pkg = markLoaded(pkgNm, vrNm);

  ret = decodeImportsSig(sigBuffer, errorMsg, msgSize, pickup, cl);

  if (ret == Ok)
    ret = loadSegments(O_IO(inBuff), pkg, errorMsg, msgSize);

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

retCode decodeLoadedPkg(char *pkgNm, long nmLen, char *vrNm, long vrLen, bufferPo sigBuffer) {
  rewindBuffer(sigBuffer);

  if (isLookingAt(O_IO(sigBuffer), "n7o7'()7'") == Ok)
    return decodePkgName(O_IO(sigBuffer), pkgNm, nmLen, vrNm, vrLen);
  else
    return Error;
}

static retCode decodeImportsSig(bufferPo sigBuffer, char *errorMsg, long msgLen, pickupPkg pickup, void *cl) {
  rewindBuffer(sigBuffer);
  ioPo in = O_IO(sigBuffer);

  if (isLookingAt(in, "n7o7'()7'") == Ok) {
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

static retCode loadCodeSegment(ioPo in, heapPo heap, packagePo owner, char *errorMsg, long msgSize);

retCode loadSegments(ioPo file, packagePo owner, char *errorMsg, long msgLen) {
  retCode ret = Ok;

  while (ret == Ok) {
    ret = loadCodeSegment(file, NULL, owner, errorMsg, msgLen);
  }

  return ret;
}

/* swap bytes in the little endian game */
static inline void SwapBytes(unsigned long *x) {
  *x = ((*x & 0xff) << 8) | ((*x >> 8) & 0xff) | ((*x & 0x00ff0000L) << 8)
       | ((*x & 0xff000000L) >> 8);
}

static inline void SwapWords(unsigned long *x) {
  *x = (*x & 0x0000ffffL) << 16 | (*x & 0xffff0000L) >> 16;
}

static retCode in32(ioPo in, int32 *tgt) {
  byte b1 = 0, b2 = 0, b3 = 0, b4 = 0;

  retCode ret = inByte(in, &b1);

  if (ret == Ok)
    ret = inByte(in, &b2);

  if (ret == Ok)
    ret = inByte(in, &b3);

  if (ret == Ok)
    ret = inByte(in, &b4);

  if (ret == Ok)
    *tgt = b1 << 24 | b2 << 16 | b3 << 8 | b4;
  return ret;
}


static retCode in16(ioPo in, uint16 *tgt) {
  byte b1 = 0, b2 = 0;

  retCode ret = inByte(in, &b1);

  if (ret == Ok)
    ret = inByte(in, &b2);

  if (ret == Ok)
    *tgt = b1 << 8 | b2 ;
  return ret;
}

// Decode a code segment.
// Each segment consists of
// a. The program name/arity being defined
// b. A char * containing the code as a base64 encoded char *.
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

retCode loadCodeSegment(ioPo in, heapPo heap, packagePo owner, char *errorMsg, long msgSize) {
  EncodeSupport sp = {
    errorMsg,
    msgSize,
    heap,
  };

  if (isFileAtEof(in) == Eof)
    return Eof;
  else {
    retCode ret = isLookingAt(in, "n4o4'#code'");

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

    if (ret == Ok && isLookingAt(in, "s") == Ok) {
      bufferPo buff = newStringBuffer();

      ret = decodeText(in, buff); // Pick up the code - as base64 char *

      if (ret != Ok || isLookingAt(in, "n") != Ok) { // Look for the tuple of literals
        closeFile(O_IO(buff));
        return ret;
      }

      rewindBuffer(buff); // tmpBufer should contain base64 text

      integer litCount = 0;

      ret = decInt(in, &litCount);

      if (ret == Ok)
        ret = skipEncoded(in, errorMsg, msgSize);

      if (ret != Ok) {
        closeFile(O_IO(buff));
        return ret;
      } else {
        // Decode the base64 text
        bufferPo cdeBuffer = newStringBuffer();

        ret = decode64(O_IO(cdeBuffer), O_IO(buff));
        rewindBuffer(cdeBuffer);

        if (ret != Ok) {
          closeFile(O_IO(buff));
          closeFile((O_IO(cdeBuffer)));
          return ret;
        } else {
          integer codeCount = (bufferSize(cdeBuffer) / SIZEOF_INT) - 1;
          int32 signature;

          ret = in32(O_IO(cdeBuffer), &signature); // verify correct code signature

          ptrI pc = permCode((unsigned long) codeCount, (uinteger) litCount, owner);

          insPo cd = FirstInstruction(pc);
          ptrI el = kvoid;

          rootPo root = gcAddRoot(GH, &pc); /* in case of GC ... */
          gcAddRoot(GH, &el); /* we need a temporary pointer */

          ptrI prg = newProgLbl((char *) prgName, (short) arity);
          gcAddRoot(GH, &prg);

          /* get the instructions */
          for (long i = 0; ret == Ok && i < codeCount; i++)
            ret = in16(O_IO(cdeBuffer), &cd[i]);

          if (ret != Ok) {
            closeFile(O_IO(buff));
            closeFile((O_IO(cdeBuffer)));
            gcRemoveRoot(GH, root); /* clear the GC root */
            return ret;
          } else {
            /* Now convert the main code to handle little endians etc */
            if (signature == SIGNATURE) {
            } /* endian load same as endian save */
            else if (signature == SIGNBSWAP) { /* swap bytes keep words */
              unsigned long *xx = (unsigned long *) FirstInstruction(pc);
              long cnt = codeCount;
              for (; cnt--; xx++)
                SwapBytes(xx);
            } else if (signature == SIGNWSWAP) { /* swap words keep bytes */
              unsigned long *xx = (unsigned long *) FirstInstruction(pc);
              long cnt = codeCount;
              for (; cnt--; xx++)
                SwapWords(xx);
            } else if (signature == SIGNBWSWP) { /* swap words and bytes */
              unsigned long *xx = (unsigned long *) FirstInstruction(pc);
              long cnt = codeCount;
              for (; cnt--; xx++) {
                SwapWords(xx);
                SwapBytes(xx);
              }
            }

            codeV(pc)->arity = (uint16) arity; /* set the arity of the program */

            // Now we find the literals

            for (long i = 0; ret == Ok && i < litCount; i++) {
              if ((ret = decode(in, &sp, GH, &el, buff)) != Ok) /* read each element of term */
                break; /* we might need to skip out early */
              else {
                updateCodeLit(codeV(pc), i, el);
              }
            }

            if (ret == Ok) {
              ret = decode(in, &sp, GH, &el, buff);  // Decode the source map

              if (ret == Ok)
                codeV(pc)->srcMap = el;
            }

            closeFile(O_IO(buff));
            closeFile((O_IO(cdeBuffer)));

            gcRemoveRoot(GH, root); /* clear the GC root */

            if (ret == Ok && enableVerify)
              ret = verifyCode(pc, prgName, errorMsg, msgSize);

            if (ret == Ok)
              defineProg(prg, pc);

            return ret;
          }
        }
      }
    }
    return ret;
  }
}

typedef struct {
  heapPo H;
  ptrPo lst;
  ptrPo el;
  ptrPo pk;
  ptrPo vr;
} BuildSupportRec;

static retCode buildImport(char *pkg, char *ver, char *errorMsg, long msgLen, void *cl) {
  BuildSupportRec *x = (BuildSupportRec *) cl;

  *x->pk = allocateCString(x->H, (char *) pkg);
  *x->vr = allocateCString(x->H, (char *) ver);
  *x->el = tuplePair(x->H, *x->pk, *x->vr);
  *x->lst = consLsPair(x->H, *x->el, *x->lst);
  return Ok;
}

retCode g__install_pkg(processPo P, ptrPo a) {
  ptrI pkgText = deRefI(&a[1]);

  if (isvar(pkgText))
    return liberror(P, "_install_pkg", eINSUFARG);
  else if (!IsString(pkgText))
    return liberror(P, "_install_pkg", eINVAL);
  else {
    switchProcessState(P, wait_io); /* Potentially nec. to wait */

    heapPo H = &P->proc.heap;
    ptrI lst = emptyList;
    ptrI el = kvoid;
    ptrI ky = kvoid;
    ptrI vl = kvoid;
    rootPo root = gcAddRoot(H, &lst);

    gcAddRoot(H, &el);
    gcAddRoot(H, &ky);
    gcAddRoot(H, &vl);

    BuildSupportRec x = {
      H,
      &lst,
      &el,
      &ky,
      &vl
    };

    stringPo text = stringV(pkgText);

    retCode ret = installPackage(stringVal(text), stringLen(text), P->proc.errorMsg,
                                 NumberOf(P->proc.errorMsg), buildImport, &x);
    setProcessRunnable(P);

    gcRemoveRoot(H, root);

    switch (ret) {
      case Ok:
        return equal(P, &lst, &a[2]);
      case Error:
      case Eof:
        return liberror(P, "_install_pkg", eINVAL);
      case Fail:
        return Fail;
      case Space:
        outMsg(logFile, "Out of heap space, increase and try again\n%_");
        return liberror(P, "_install_pkg", eSPACE);
      default:
        return liberror(P, "_install_pkg", eINVAL);
    }
  }
}
