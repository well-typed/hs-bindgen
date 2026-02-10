#pragma once

typedef long long int chunk[4];
typedef int (*view_chunk)(int c, const chunk * xs);

extern int stream_fib_array (int n, view_chunk f);
