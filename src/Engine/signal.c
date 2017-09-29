/*
  Unix Signal handling for the Star run-time engine
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.

*/
#include <signal.h>
#include "engine.h"			/* Main header file */

/* 
 * Signal handling functions
 */

static void sig_fatal(int sig)
{
  outMsg(logFile,"bus error or segmentation fault\n");
  lo_exit(EXIT_FAIL);
}

/* Handle the suspension of Go reasonably ... */
static void sig_suspend(int sig)
{
  if(!interactive){
    reset_stdin();		/* Reset the standard input channel */
    raise(SIGSTOP);             /* Actually suspend */
    setup_stdin();              /* Put it back */
  }
  else
    raise(SIGSTOP);             /* Actually suspend */
}

static void interruptMe(int ignored);

void setupSignals(void)
{
  signal(SIGBUS, sig_fatal);
  signal(SIGTSTP, sig_suspend);	/* When the user suspends an Go program */
  signal(SIGPIPE, SIG_IGN);	/* We not want to be interrupted by this */
  signal(SIGSEGV, sig_fatal);
  signal(SIGFPE, sig_fatal);
  signal(SIGQUIT,interruptMe);
  signal(SIGINT,interruptMe);
}

sigset_t stopInterrupts(void)	/* stop control-C from generating a signal */
{
  sigset_t mask;
  sigset_t current;

  sigemptyset(&mask);

  sigaddset(&mask,SIGQUIT);	/* The quit signal */
  sigaddset(&mask,SIGVTALRM);	/* The virtual timer signal */
  sigaddset(&mask,SIGALRM);	/* The real timer signal */

  sigprocmask(SIG_BLOCK,&mask,&current);
  return current;
}

void startInterrupts(sigset_t blocked)	/* enable interrupts again */
{
  sigprocmask(SIG_SETMASK,&blocked,NULL);
}

/* 
 * Interrupt handling -- on a control^C we send a message to the monitor process
 */

/* Warning --- important that SIGINT is blocked during this handler */

static void interruptMe(int sig) /* This one is invoked when user presses ^C */
{
  lo_exit(EXIT_FAIL);		/* We just abort everything */
}

void star_exit(int code)
{
  if(code!=0)
    outMsg(logFile,"Terminating with code %d\n",code);

#ifdef EXECTRACE
  if(traceCount)
    dumpInsCount();
#endif

  exit(code);
}


