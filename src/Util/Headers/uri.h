#ifndef _IO_URI_H_
#define _IO_URI_H_

#include "unicode.h"
#include "io.h"

ioPo openURI(uniChar *uri,ioEncoding encoding);

uniChar *resolveURI(uniChar *base,uniChar *url,uniChar *buffer,long len);
uniChar *relativizeRI(uniChar *base,uniChar *url,uniChar *buffer,long len);
retCode checkRoot(uniChar *sys,uniChar *root,uniChar *user);
retCode parseURI(uniChar *uri, uniChar *scheme,long sLen,
		 uniChar *user,long uLen,uniChar *pass,long pLen,
		 uniChar *host,long hLen,long *port,
		 uniChar *path,long tLen,
		 uniChar *query,long qLen,uniChar *fragment,long fLen);
uniChar *defaultURI(uniChar *base);
uniChar *grabURI(uniChar *url);

typedef ioPo (*transducer)(uniChar *uri,ioEncoding encoding);

retCode registerTransducer(uniChar *scheme,transducer transducer);

#endif
