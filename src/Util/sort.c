#include "sort.h"


static int64 partition(void *data,int64 from,int64 to,int64 pIx,
		       beforeFn before,swapFn swap,getFn get)
{
  int64 end = to-1;
  int64 stIx = from;

  swap(get(data,pIx),get(data,end));
  void *pivot = get(data,end);

  for(int64 ix=from;ix<end;ix++){
    void *lhs = get(data,ix);

    if(before(lhs,pivot)){
      void *tgt = get(data,stIx++);
      swap(tgt,lhs);
    }
  }
  swap(get(data,stIx),get(data,end));
  return stIx;
}

static void subSort(void *data,int64 from,int64 to,
		    beforeFn before,swapFn swap,getFn get)
{
  if(from<to){
    int64 pIx = partition(data,from,to,(from+to)/2,before,swap,get);
    subSort(data,from,pIx,before,swap,get);
    subSort(data,pIx+1,to,before,swap,get);
  }
}

void sort(void *data,int64 count,beforeFn before,swapFn swap,getFn get)
{
  subSort(data,0,count,before,swap,get);
}
