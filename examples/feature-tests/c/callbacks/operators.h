#pragma once

typedef int unary_op (int);

extern int square(int);

extern int apply1 (unary_op * f, int x);

typedef int bin_op(int, int);

extern int plus(int, int);

extern int apply2 (bin_op f, int x, int y);
