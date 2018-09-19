//
// Created by Francis McCabe on 8/25/18.
// Support console history

#ifndef STAR_HISTORY_H
#define STAR_HISTORY_H

#include "file.h"
#include "strng.h"

void initHistory(char *historyFileName);
integer historyLength();    // How many line of history do we have?
strgPo fetchHistory(integer ix);
retCode appendHistoryLine(char *txt, integer len);


#endif //STAR_HISTORY_H
