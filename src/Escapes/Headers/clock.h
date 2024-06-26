#ifndef _CLOCK_H_
#define _CLOCK_H_

#include <sys/time.h>

void initTime(void);

extern long timezone_offset;

void flushTimeQ();
void reset_timer(void);
struct timeval *nextTimeOut(void);
long taxiFlag(void);

double get_time(void);
double get_date(void);

#define SCHEDULETICKS 150	/* We re-schedule every 150 milliseconds */

#define SECSINDAY 86400		/* Number of seconds in a day */

#define NANO (1000000000)

#endif
