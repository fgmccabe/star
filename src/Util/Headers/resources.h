#ifndef _RESOURCES_H_
#define _RESOURCES_H_

#include <ooio.h>
#include <unicode.h>
#include <iostr.h>
#include "catalog.h"

typedef uniChar *(*Transducer)(uniChar *url);

extern uniChar *accessResource(uniChar *url);
extern void addTransducer(uniChar *scheme,Transducer trans);
extern void initTransducers();

extern uniChar *FILE, *HTTP;
#endif

