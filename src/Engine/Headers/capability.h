//
// Created by Francis McCabe on 1/10/20.
//

#ifndef STAR_CAPABILITY_H
#define STAR_CAPABILITY_H

#include "term.h"
#include "heap.h"

// Capability structure
typedef struct capability_term *capabilityPo;

extern clssPo capabilityClass;

extern capabilityPo C_CAP(termPo t);

typedef enum {
  readPermission = 1u,
  writePermission = 2u,
  createPermission = 4u,
  deletePermission = 8u
} permission;

static inline logical isCapability(termPo p) {
  return hasClass(p, capabilityClass);
}

extern capabilityPo allocateCapability(heapPo H, const char *path, integer pathLen, permission perms);

#endif //STAR_CAPABILITY_H
