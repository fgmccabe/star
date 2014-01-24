/*
   Timer interface
   (c) 2006 F.G. McCabe

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
   
   Contact: Francis McCabe <frankmccabe@mac.com>
*/ 

#ifndef _OOIO_TIMER_H_
#define _OOIO_TIMER_H_

#include "config.h"

typedef void (*timeFun)(void *cl);

retCode setAlarm(number time,timeFun onWakeup,void *cl);
void cancelAlarm(void);

#ifdef _WIN32			/* Windows'95 specific stuff */
#include <winsock.h>
struct  itimerval {		/* duplicate Unix declaration */
  struct  timeval it_interval;	/* timer interval */
  struct  timeval it_value;	/* current value */
};
void gettimeofday(struct timeval *t, void *ignored);
#else				/* UNIX only */
#include <sys/time.h>
#endif

#endif

