#ifndef _UTILS_H_
#define _UTILS_H_

#include <unicode.h>

extern uniChar *genSym(char *prefix);
extern uniChar *genUSym(uniChar *prefix);

extern uniChar *mkLabel(uniChar *prefix,int variant);
extern string mkIdent(char *prefix,int variant);

extern string mkInterned(char *symbol);

#define tryRet(Exp) do{ retCode ret=(Exp); if(ret!=Ok)return ret; }while(False)

#ifndef Null
#define Null ((void*)0)
#endif

#ifndef ALIGNED
#define ALIGN(ptr,size) (((((integer)ptr)+size-1)/size)*(size))
#define ALIGNED(ptr,size) (((((integer)ptr)+size-1)/size)*(size)==(integer)ptr)
#endif

#ifndef GROW
#define GROW(var,type) do{					\
    if(var##Pos>=var##Size){					\
      long nsize = (var##Size)*3/2;				\
      type *nbuffer = (type*)malloc(nsize*sizeof(type));	\
      for(long cx=0;cx<var##Pos;cx++)				\
	nbuffer[cx] = var[cx];					\
      free(var);						\
      var = nbuffer;						\
      var##Size = nsize;					\
    }								\
  } while(0)
#endif

#endif
