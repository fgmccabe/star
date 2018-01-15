/*
 * Some system functions
 */
#include <sys/time.h>
#include <time.h>
#include <errno.h>
#include <stdlib.h>

#include "signature.h"
#include "escape.h"

#include <ooio.h>

// Number of nano seconds
#define NANOS 1000000000


static integer nanos(ptrPo tos)
{
  return (integer)clock()*(NANOS/CLOCKS_PER_SEC);
}

static char nanoName[] = "nanos";
static char nanoSig[] = { funSig, '(', ')', intSig, 0 };
static EscapeRec escapeNano = {
  .name = nanoName,
  .sig = nanoSig,
  .arity = 0
};

void installSystem()
{
  installEscape("nanos", NULL, NULL);
}

void sleep(integer amnt)
{
  long nanos = amnt%NANOS;
  long secs = amnt/NANOS;
  struct timespec Amnt = { .tv_sec=secs, .tv_nsec=nanos };
  struct timespec SoFar;

  while(nanosleep(&Amnt,&SoFar)!=0){
    switch(errno){
    case ENOSYS:
      outMsg(logFile,"no nanosleep\n");
      exit(99);
    case EINTR:
      if(SoFar.tv_sec!=0 || SoFar.tv_nsec!=0)
	Amnt = SoFar;
      else
	return;
    default:
      outMsg(logFile,"problem in nanosleep");
      exit(99);
    }
  }
}
    

/* Fatal system error */
void syserr(const char * msg)
{
  outMsg(logFile,"Fatal error: %U\n", msg);
  exit(99);
}

void memerr()
{
  outMsg(logFile,"out of heap space\n");
  flushOut();
  exit(99);
}


