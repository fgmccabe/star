#include "config.h"
#include "ooio.h"

static long nfib(long N)
{
  if(N<2)
    return 1;
  else
    return nfib(N-1)+nfib(N-2)+1;
}

#include <sys/time.h>

static double getNanoTime()
{
  return ((double)clock())/CLOCKS_PER_SEC;
}

int main(int argc, char **args)
{
  {
    uniChar fn[]={'-',0};
    initLogfile(fn);
  }

  long arg = 42;
  double time = getNanoTime();
  long res = nfib(arg);
  time = getNanoTime()-time;

  double nanos = time*1.0e9/res;
	
  outMsg(logFile,"result is %ld in %f secs, %f nanos/call\n", 
	 res,time,nanos);
  flushOut();
}
