#include "macro.h"
#include "stdio.h"

YEAR getYear(date *d) { return d->year; }

int greeting_length(char *str) {
  int n = 0;
  while (str[n] != '\0') {
    n++;
  }
  return n;
}

void greet(char lang) {
  if (lang == CHINESE) {
    printf("你好\n");

  } else if (lang == JAPANESE) {
    printf("こんにちは\n");

  } else {
    printf("Warning: unknown language: %c\n", lang);
  }
  fflush(stdout);
}
