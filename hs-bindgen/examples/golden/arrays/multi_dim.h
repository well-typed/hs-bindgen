/* multi-dimensional arrays */

// array of known size of array of known size
int foo (int xss[3][4]);
int foo_const (const int xss[3][4]);

// array of unknown size of array of known size
int bar (int xss[][2]);
int bar_const (const int xss[][2]);

// typedef of array of known size of array of known size
typedef int matrix[4][3];
int baz (matrix xss);
int baz_const (const matrix xss);

// typedef of array of unknown size of array of known size
typedef int triplets[][3];
int quuz (triplets xss);
int quuz_const (const triplets xss);
