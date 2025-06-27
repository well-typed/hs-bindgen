#include <stdio.h>

#include "cbits.h"

void show_rect(struct rect* r) {
    printf("{{%d, %d}, {%d, %d}}\n",
      r->topleft.x,
      r->topleft.y,
      r->bottomright.x,
      r->bottomright.y);
}