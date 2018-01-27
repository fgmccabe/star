/*
  Signal management functions
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#ifndef _G_SIGNAL_H_
#define _G_SIGNAL_H_

#include <signal.h>

void setupSignals(void);
void startInterrupts(sigset_t blocked);	/* enable control-C interrupts */
sigset_t stopInterrupts(void);	/* stop control-C interruptes */
void star_exit(int);		/* When we want to stop */
void initSuspend(void);
#endif
