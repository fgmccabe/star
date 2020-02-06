//
// Created by Francis McCabe on 1/10/20.
//

#include "capabilityP.h"
#include <assert.h>
#include <string.h>

logical traceCapability = False;

static long capSize(specialClassPo cl, termPo o);
static termPo capCopy(specialClassPo cl, termPo dst, termPo src);
static termPo capScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical capCmp(specialClassPo cl, termPo o1, termPo o2);
static integer capHash(specialClassPo cl, termPo o);
static retCode capDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);

SpecialClass CapabilityClass = {
  .clss = Null,
  .sizeFun = capSize,
  .copyFun = capCopy,
  .scanFun = capScan,
  .compFun = capCmp,
  .hashFun = capHash,
  .dispFun = capDisp
};

void initCapability() {
  CapabilityClass.clss = specialClass;
}

clssPo capabilityClass = (clssPo) &CapabilityClass;

capabilityPo C_CAP(termPo t) {
  assert(hasClass(t, capabilityClass));
  return (capabilityPo) t;
}

const char *capabilityPath(termPo t, integer *size) {
  capabilityPo cap = C_CAP(t);
  *size = cap->length;
  return cap->path;
}

permission capabilityPerms(termPo t) {
  capabilityPo cap = C_CAP(t);
  return cap->perms;
}

capabilityPo allocateCapability(heapPo H, const char *path, integer pathLen, permission perms) {
  capabilityPo cap = (capabilityPo) allocateObject(H, capabilityClass, CapabilityCellCount(pathLen));

  cap->clss = capabilityClass;
  cap->perms = perms;
  cap->length = pathLen;

  memmove(cap->path, path, pathLen * sizeof(char));

#ifdef TRACECAPABILITY
  if(traceCapability)
    logMsg(logFile,"create capability: %S with permissions %x",path,pathLen,perms);
#endif

  return cap;
}

long capSize(specialClassPo cl, termPo o) {
  return CapabilityCellCount(C_CAP(o)->length);
}

termPo capCopy(specialClassPo cl, termPo dst, termPo src) {
  capabilityPo si = C_CAP(src);
  capabilityPo di = (capabilityPo) dst;
  *di = *si;

  memcpy(di->path, si->path, si->length);

  return ((termPo) di) + CapabilityCellCount(si->length);
}

termPo capScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  capabilityPo cap = C_CAP(o);

  return o + CapabilityCellCount(cap->length);
}

logical capCmp(specialClassPo cl, termPo o1, termPo o2) {
  integer l1, l2;
  const char *tx1 = capabilityPath(o1, &l1);
  const char *tx2 = capabilityPath(o2, &l2);

  return (logical) (uniNCmp(tx1, l1, tx2, l2) == same);
}

integer capHash(specialClassPo cl, termPo o) {
  capabilityPo cap = C_CAP(o);
  return uniNHash(cap->path, cap->length);
}

static retCode cpDisp(codePoint ch, integer ix, void *cl) {
  return outChar(O_IO(cl), ch);
}

static retCode outPerms(ioPo out, permission perms) {
  retCode ret = Ok;
  if ((perms & (unsigned)readPermission) == readPermission)
    ret = outChar(out, 'r');
  if (ret == Ok && (perms & (unsigned)writePermission) == writePermission)
    ret = outChar(out, 'w');
  if (ret == Ok && (perms & (unsigned)createPermission) == createPermission)
    ret = outChar(out, 'w');
  if (ret == Ok && (perms & (unsigned)deletePermission) == deletePermission)
    ret = outChar(out, 'w');
  return ret;
}

retCode capDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  capabilityPo cap = C_CAP(t);

  retCode ret = outStr(out, "[\"");

  if (ret == Ok)
    ret = processUnicodes(cap->path, cap->length, cpDisp, out);

  if (ret == Ok)
    ret = outStr(out, "\"](");

  if (ret == Ok)
    ret = outPerms(out, cap->perms);
  if (ret == Ok)
    ret = outStr(out, ")");
  return ret;
}

retCode
checkCapability(capabilityPo cap, char *subPath, integer pathLen, char *buffer, integer buffLen, permission perm) {
  if ((cap->perms & perm) != perm)
    return Fail;
  else {
    if (resolvePath(cap->path, cap->length, subPath, pathLen, buffer, buffLen) == Ok) {
      if (uniNCmp(buffer, cap->length, cap->path, cap->length) == same) {
        return Ok;
      }
    };
  }
  return Fail;
}
