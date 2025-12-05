#include "setP.h"
#include "pool.h"
#include "ooio.h"
#include "utils.h"
#include <stdlib.h>

static poolPo setPool = Null;

static void initSets(){
  if(setPool == Null){
    setPool = newPool(sizeof(SetRecord), 64);
  }
}

setPo createEmptySet(int32 min){
  initSets();

  setPo set = allocPool(setPool);
  set->count = 0;
  set->min = min;
  set->count = 0;
  set->data = Null;

  return set;
}

void deleteSet(setPo s){
  if(s->data!=Null)
    free(s->data);
  freePool(setPool, s);
}

retCode addToSet(setPo set, int32 k){
  int32 max = set->count*64+set->min;
  if(k>=set->min && k<max){
    int32 base = k-set->min;
    int32 el = base>>6;
    int32 mask = 1u<<(((unsigned)el)&63);
    set->data[el] |= mask;
    return Ok;
  } else{
    if(k<set->min){

    } else{
      
    }
  }
  return Ok;
}
retCode removeFromSet(setPo set, int32 k);

logical inSet(setPo set, int32 k){
  int32 max = set->count*64+set->min;
  if(k<set->min || k>=max || set->data==Null)
    return False;
  else{
    int32 base = k-set->min;
    int32 el = base>>6;
    int32 mask = 1u<<(((unsigned)el)&63);
    return (set->data[el]&mask)==mask;
  }
}

retCode processSet(setPo set, setElProc proc, void *cl){
  if(set->data==Null)
    return Ok;
  else{
    retCode ret = Ok;
    int32 max = set->count*64+set->min;
    for(int32 ix=set->min;ret==Ok && ix<max;ix++){
      int32 base = ix-set->min;
      int32 el = base>>6;
      int32 mask = 1u<<(((unsigned)el)&63);
      if((set->data[el]&mask)==mask)
        ret = proc(set,ix,cl);
    }
    return ret;
  }
}