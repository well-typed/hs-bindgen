#ifndef STATIC_COUNTER_H
#define STATIC_COUNTER_H

static int counter = 0;

static int count(void) {
  return counter++;
}

static void reset(void) {
  counter = 0;
}

#endif
