// Statistics of one operation, after rpmop_s in RPM: it holds a stopwatch.
#ifndef OPERATION_H
#define OPERATION_H

#include "stopwatch.h"

struct operation {
  struct stopwatch begin;
  int count;
  stopwatch_time total;
};

#endif
