/* Arrays
 *
 * See https://en.cppreference.com/w/c/language/array.html
 */

/*
 * Storage class
 */

//! Global, complete, not initialised
int arr0[3];

//! Global, complete, initialised
int arr1[] = {1, 2, 3};

//! Global, extern, complete, not initialised
extern int arr2[3];

//! Global, extern, complete, initialised
extern int arr3[] = {2, 3, 4};

//! Global, static, complete, not initialised
static int arr4[3];

//! Global, static, complete, initialised
static int arr5[] = {3, 4, 5};

//! Global, incomplete
int arr6[];

//! Global, extern, incomplete
extern int arr7[];

//! Global, static, incomplete
static int arr8[3];

/*
 * Types
 */

typedef int triplet[3];

typedef int list[];

typedef int matrix[4][3];

typedef int tripletlist[][3];

struct Example {
    int triple[3];
    int sudoku[3][3];
};

/*
 * Globals
 */

//! Array of known size
extern int arr_1[3];

//! Array of known size, typedef
extern triplet arr_2;

//! Array of unknown size
extern int arr_3[];

//! Array of unknown size, typedef
extern list arr_4;

//! Multi-dimensional array of known size
extern int arr_5[4][3];

//! Multi-dimensional array of known size, typedef
extern matrix arr_6;

//! Multi-dimensional array of unknown size
extern int arr_7[][3];

//! Multi-dimensional array of unknown size, typedef
extern tripletlist arr_8;

/*
 * Function arguments
 */

//! Array of known size
int fun_1(int x, int xs[3]);

//! Array of known size, typedef
int fun_2(triplet xs);

//! Array of unknown size
int fun_3(int xs[]);

//! Array of unknown size, typedef
int fun_4(list xs);

//! Multi-dimensional array of known size
int fun_5(int xss[4][3]);

//! Multi-dimensional array of known size, typedef
int fun_6(matrix xss);

//! Multi-dimensional array of unknown size
int fun_7(int xss[][3]);

//! Multi-dimensional array of unknown size, typedef
int fun_8(tripletlist xss);

/*
 * Function results
 *
 * NOTE: an array can not be returned directly from a function, so we return a
 * pointer to the array instead. Note that it is atypical for C code to return
 * pointers to arrays from functions, but for completeness we include these
 * examples here anyway.
 */

//! Array of known size
int (*fun_9(void))[3];

//! Array of known size, typedef
triplet *fun_10(void);

//! Array of unknown size
int (*fun_11(void))[];

//! Array of unknown size, typedef
list *fun_12(void);

//! Multi-dimensional array of known size
int (*fun_13(void))[4][3];

//! Multi-dimensional array of known size, typedef
matrix *fun_14(void);

//! Multi-dimensional array of unknown size
int (*fun_15(void))[][3];

//! Multi-dimensional array of unknown size, typedef
tripletlist *fun_16(void);
