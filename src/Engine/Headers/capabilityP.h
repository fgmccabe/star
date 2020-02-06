//
// Created by Francis McCabe on 1/10/20.
//

#ifndef STAR_CAPABILITYP_H
#define STAR_CAPABILITYP_H

#include "capability.h"
#include "termP.h"

typedef struct capability_term {
  clssPo clss;                  // == capabilityClass
  integer length;
  permission perms;
  char path[ZEROARRAYSIZE];
} CapabilityRecord;

extern void initCapability();

#define CapabilityCellCount(len) CellCount(sizeof(CapabilityRecord)+(len)*sizeof(char))

extern logical traceCapability;

#endif //STAR_CAPABILITYP_H
