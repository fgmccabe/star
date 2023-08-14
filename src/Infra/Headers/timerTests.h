//
// Created by Francis McCabe on 4/16/23.
//

#ifndef STAR_TIMERTESTS_H
#define STAR_TIMERTESTS_H

typedef struct timer_record *timerPo;

extern logical enableTimers;

#define newTimer(Msg) (enableTimers ? startTimer_(Msg,False): Null)
#define startTimer(Msg) (enableTimers ? startTimer_(Msg,True): Null)
#define pauseTimer(t) STMT_WRAP(if(enableTimers) { pauseTimer_(t);})
#define resumeTimer(t) STMT_WRAP(if(enableTimers) { resumeTimer_(t);})

timerPo startTimer_(char *msg, logical running);
void pauseTimer_(timerPo timer);
void resumeTimer_(timerPo timer);
logical isTimerRunning(timerPo timer);

void initTimers();
void reportTimers();

#endif //STAR_TIMERTESTS_H
