#include "config.h"
#include <ooio.h>
#include <iochnnl.h>

long tarai(long x, long y, long z)
{
    while (x > y) {
        long oldx = x, oldy = y;
        x = tarai(x - 1, y, z);
        y = tarai(y - 1, z, oldx);
        if (x <= y) break;
        z = tarai(z - 1, oldx, oldy);
    }
    return y;
}

long tar(long x,long y,long z)
{
  if(x<=y)
    return y;
  else
    return tar(tar(x-1,y,z),tar(y-1,z,x),tar(z-1,x,y));
}

#include <sys/time.h>

static double getNanoTime()
{
  return ((double)clock())/CLOCKS_PER_SEC;
}

int main(int argc, char **args)
{
  {
    char fn[]={'-',0};
    initLogfile(fn);
  }

  double time = getNanoTime();
  //long res = tarai(18,12,6);
  long res = tar(19,13,5);
  time = getNanoTime()-time;

  outMsg(logFile,"result is %ld in %f secs\n",res,time);
  flushOut();
}

