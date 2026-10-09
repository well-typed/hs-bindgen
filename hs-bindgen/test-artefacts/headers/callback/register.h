// callback/register.h: a function that takes a callback over the struct of
// foo.h
#include "foo.h"

void register_cb(void (*cb)(struct foo *));
