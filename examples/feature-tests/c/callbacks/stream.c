#include <stdlib.h>

#include "stream.h"

int stream_fib_array (int n, view_chunk view) {
  if (n <= 0) { return 0; };
  chunk curr;
  size_t chunk_size = sizeof(curr)/sizeof(curr[0]);
  curr[0] = 0;
  curr[1] = 1;
  for (int c = 0; c < n; c++) {
    for (int i = 2; i < chunk_size; i++) {
      curr[i] = curr[i-2] + curr[i-1];
    };
    int ret = view (c, &curr);
    if (ret < 0) { return ret; };
    curr[0] = curr[chunk_size-2] + curr[chunk_size-1];
    curr[1] = curr[chunk_size-1] + curr[0];
  };
  return 0;
};

