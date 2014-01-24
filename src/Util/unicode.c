/*
  Unicode encoding and decoding functions
  (c) 1994-2010 F.G. McCabe

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA  02111-1307, USA.

  Contact: fmccabe@gmail.com
*/

#include "config.h"		/* Invoke configuration header */
#include "ioP.h"
#include "hash.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

long utf8_uni(const unsigned char *str,long max,uniChar *buff,long len)
{
  long pos = 0;
  long i;

  for(i=0;i<max && pos<len;i++){
    int ch = *str++;

    if(ch<=0x7f)
      buff[pos++]=ch;
    else if(UC80(ch))
      buff[pos++]= UX80(ch)<<6|UXR(*str++);
    else if(UC800(ch)){
      uniChar code = UX800(ch)<<12;
      code |=UXR(*str++)<<6;
      code |=UXR(*str++);
      buff[pos++]=code;
    }
    else
      return -1;		/* invalid utf-8 character? */
  }
  if(pos<=len)
    return pos;
  else
    return -1;			/* we couldnt perform the mapping */
}

long uniCharUtf8Size(uniChar c)
{
  if(c<=0x7f)
    return 1;
   else if(0x80<=c && c<=0x7FF)
    return 2;
   else
    return 3;                   /* Needs to updated for 3.1 */
}

long uniStrLen(const uniChar *s)
{
  long len = 0;

  assert(s!=NULL);

  while(*s++!=0)
    len++;
  return len;
}

uniChar *uniCat(uniChar *dest,long len,const uniChar *src)
{
  int pos = 0;
  while(pos<len-1 && dest[pos]!=0)
    pos++;

  while(pos<len-1 && *src!=0)
    dest[pos++]=*src++;
  dest[pos]=0;
  return dest;
}

uniChar *uniTackOn(uniChar *dest,long len,uniChar ch)
{
  int pos = 0;
  while(pos<len-1 && dest[pos]!=0)
    pos++;
    
  if(pos<len-1){
    dest[pos++]=ch;
    dest[pos]='\0';
  }

  return dest;
}


uniChar *uniCpy(uniChar *dest,long len,const uniChar *src)
{
  int pos = 0;

  while(pos<len-1 && *src!=0)
    dest[pos++]=*src++;
  dest[pos]=0;
  return dest;
}

uniChar *uniNCpy(uniChar *dest,long len,const uniChar *src,long sLen)
{
  long pos = 0;
  long max = (sLen<len-1?sLen:len-1);

  while(pos<max && *src!=0)
    dest[pos++]=*src++;
  dest[pos]=0;
  return dest;
}

int uniCmp(uniChar *s1,uniChar *s2)
{
  long pos = 0;
  assert(s1!=NULL && s2!=NULL);

  while(s1[pos]==s2[pos]){
    if(s1[pos]==0)
      return 0;
    pos++;
  }

  if(s1[pos]<s2[pos] || s1[pos]==0)
    return -1;
  else
    return 1;
}

logical uniIsTail(uniChar *s1,uniChar *s2)
{
  long len = 0;
  uniChar *eS1 = uniEndStr(s1);
  
  while(*s2!=0){
    s2++; len++;
  }
  
  while(eS1>s1 && len-->0){
    if(*--eS1!=*--s2)
      return False;
  }
  return True;
}

uniChar *uniInsert(uniChar *dest,long len,const uniChar *src)
{
  long iLen = uniStrLen(src);
  long dLen = uniStrLen(dest)+1;
  
  assert(iLen+dLen<len);
  
  if(iLen+dLen<len){
    long end = dLen+iLen;
    long pos = dLen;
    while(--pos>0)              /* Shuffle up the old text */
      dest[--end]=dest[pos];
      
    for(pos=0;pos<iLen;pos++)
      dest[pos]=src[pos];
      
    return dest;
  }
  return NULL;                  /* Bomb out for now */
}

int uniNCmp(uniChar *s1,uniChar *s2,long l)
{
  long pos = 0;
  while(pos<l && s1[pos]==s2[pos]){
    if(s1[pos]==0)
      return 0;
    pos++;
  }
  if(pos<l)
    return s2[pos]-s1[pos];
  else
    return 0;
}

uniChar *uniDuplicate(uniChar *s)
{
  long len = uniStrLen(s);
  uniChar *copy = (uniChar*)malloc((len+1)*sizeof(uniChar));
  
  return uniCpy(copy,len+1,s);
}

uniChar *uniDup(uniChar *s,long len)
{
  uniChar *copy = (uniChar*)malloc((len+1)*sizeof(uniChar));
  
  return uniCpy(copy,len+1,s);
}

static hashPo interns;
static pthread_once_t internInit = PTHREAD_ONCE_INIT;

static void initInterns()
{
  interns = NewHash(127,(hashFun)uniHash,(compFun)uniCmp,NULL);
}

uniChar *uniIntern(uniChar *s)
{
  pthread_once(&internInit,initInterns);

  if(s==NULL)
    return NULL;
  else{
    uniChar *found = hashGet(interns,s);

    if(found==NULL){
      found = uniDuplicate(s);
      hashPut(interns,found,found);
    }

    return found;
  }
}

uniChar *uniNewStr(unsigned char *s)
{
  long len = strlen((char*)s);
  uniChar buff[len+1];
  
  _uni(s,buff,len+1);
  
  len = uniStrLen(buff);
  
  {
    uniChar *copy = (uniChar*)malloc((len+1)*sizeof(uniChar));
  
    return uniCpy(copy,len+1,buff);
  }
}

void uniFree(uniChar *s)
{
  free(s);
}

// Append a unicode string to the end of a unicode buffer
uniChar *uniAppend(uniChar *dest,long len,const uniChar *src)
{
  int pos = 0,i=0;
  while(pos<len-1 && dest[pos]!=0)
    pos++;

  for(i=0;i<uniStrLen(src)&&pos<len-1;i++,pos++)
    dest[pos]=src[i];

  dest[pos]='\0';

  return dest;
}

/* Tack on an ASCII string to the end of a unicode string */
/* This is only necessary 'cos C is not uniCode friendle */
uniChar *uniTack(uniChar *dest,long len,const char *src)
{
  int pos = 0;
  while(pos<len-1 && dest[pos]!=0)
    pos++;

  _uni((const unsigned char*)src,&dest[pos],len-pos);

  return dest;
}

/* Set an ASCII string into a unicode string */
/* This is only necessary 'cos C is not uniCode friendle */
uniChar *uniLit(uniChar *dest,long len,const char *src)
{
  _uni((const unsigned char *)src,dest,len);

  return dest;
}

/*
 * extract a substring from a unicode string and place in buffer
 */
uniChar *uniSplit(uniChar *s,long from,long to,uniChar *buffer,long len)
{
  int jx = 0;
  for(int ix=from;jx<len-1 && ix<to && s[ix]!='\0';ix++)
    buffer[jx++] = s[ix];
  buffer[jx] = '\0';
  return buffer;
}

uniChar *uniSearch(uniChar *s,long len,uniChar c)
{
  long pos = 0;
  while(pos<len && s[pos]!=0 && s[pos]!=c)
    pos++;

  if(pos<len && s[pos]==c)
    return &s[pos];
  else
    return NULL;
}

long uniIndexOf(uniChar *s,long len,uniChar c)
{
  for(long ix=0;ix<len && s[ix]!=0;ix++)
    if(s[ix]==c)
      return ix;
  return -1;
}

long uniLastIndexOf(uniChar *s,long len,uniChar c)
{
  long lx = -1;
  for(long ix=0;ix<len && s[ix]!=0;ix++)
    if(s[ix]==c)
      lx = ix;
  return lx;
}

uniChar *uniSubStr(uniChar *s,long len,int from,int cnt,uniChar *buff,int bLen)
{
  uniChar *src = &s[from];
  int ix;
  for(ix=0;ix<cnt && ix<bLen;ix++){
    buff[ix]=src[ix];
    if(src[ix]==0)
      break;
  }
  if(ix<bLen)
    buff[ix] = '\0';
  return buff;
}

uniChar *uniSearchAny(uniChar *s,long len,uniChar *term)
{
  long pos = 0;
  long termSize = uniStrLen(term);

  while(pos<len && s[pos]!=0 && uniSearch(term,termSize,s[pos])==NULL)
    pos++;

  if(pos<len && uniSearch(term,termSize,s[pos])!=NULL)
    return &s[pos];
  else
    return NULL;
}

uniChar *uniLast(uniChar *s,long l,uniChar c)
{
  long pos = 0;
  long last = -1;

  while(pos<l && s[pos]!=0){
    if(s[pos]==c)
      last=pos;
    pos++;
  }

  if(last>=0)
    return &s[last];
  else
    return NULL;
}

logical uniIsLit(uniChar *s1,char *s2)
{
  long pos = 0;
  while(s2[pos]!=0 && s1[pos]==s2[pos])
    pos++;

  return s2[pos]==0 && s1[pos]==0;
}

logical uniIsLitPrefix(uniChar *s1,char *s2)
{
  long pos = 0;
  while(s2[pos]!='\0' && s1[pos]==s2[pos])
    pos++;

  return s2[pos]==0;
}

uinteger uniHash(const uniChar *name)
{
  register integer hash = 0;

  if(name)
    while(*name){
      hash = hash*37+*name++;
      if(hash<0)
        hash=-hash;
    }

  return hash;
}

uniChar uniEmpty[] = {0};

uniChar *uniEndStr(uniChar *s)
{
  while(*s!=0)
    s++;
  return s;
}

uniChar *uniLower(uniChar *s,uniChar *d,long len)
{
  long max = uniStrLen(s);
  long i;
  
  if(max>len)
    max=len-1;

  for(i=0;i<max;i++)
    d[i]=lowerOf(s[i]);
  d[i]=0;
  return d;
}

long uni_utf8(const uniChar *s,long len,unsigned char *buff,long tlen)
{
  long pos = 0;
  long i=0;

  while(i<len && pos<tlen){
    uniChar c = s[i++];
    if(c<=0x7f)
      buff[pos++]=c;
    else if(0x80<=c && c<=0x7FF){
      if(pos>=tlen-1)
	return -1;
      buff[pos++]=U80|UX80(c>>6);
      buff[pos++]=UR|UXR(c);
    }
    else{
      if(pos>=tlen-2)
	return -1;
      buff[pos++]=U800|UX800(c>>12);
      buff[pos++]=U80|UX80(c>>6);
      buff[pos++]=UR|UXR(c);
    }
  }

  assert(pos<=tlen);
  return pos;
}

unsigned char *_utf(const uniChar *s,unsigned char *b,long len)
{
  long pos=uni_utf8(s,uniStrLen(s),b,len);

  if(pos>=0 && pos<len){
    b[pos]='\0';
    return b;
  }
  else
    return NULL;
}

uniChar *_uni(const unsigned char *s,uniChar *b,long len)
{
  long pos=utf8_uni(s,strlen((char*)s),b,len);

  if(pos>=0 && pos<len){
    b[pos]=0;
    return b;
  }
  else
    return NULL;
}

