#pragma once

/**
 * Example without external includes
 */

/**
  * Simple struct with typedef of the same name
  *
  * Since the typedef has the same name as the struct tag, we don't
  * generate a separate newtype for this.
  */

typedef struct triple {
    int a;
    int b;
    int c;
} triple;

void mk_triple(int a, int b, int c, triple* triple);

/**
 * Simple enumeration example
 */

typedef enum index {
    A,
    B,
    C
} index;

int index_triple(triple* triple, index ix);

/**
 * Typedefs
 */

typedef int sum;
typedef double average;

sum sum_triple(triple* triple);
average average_triple(triple* triple);

/**
 * Some simple macros
 */

#define FIELD_OFFSET 4
#define EPSILON 0.1

#define PTR_TO_FIELD(ptr) ptr + 4

#define YEAR  int
#define MONTH int
#define DAY   int

typedef struct date {
    YEAR  year;
    MONTH month;
    DAY   day;
} date;

YEAR getYear(date* d);

/**
 * Unions
 */

typedef union occupation {
  struct student {
    char* university;
    int year;
  } student;

  struct employee {
    char* company;
    struct person* supervisor;
    int salary;
  } employee;
} occupation;

void print_occupation(int tag, occupation* o);

/**
 * Anonymous types
 */

struct rect {
  struct {
    int x;
    int y;
  } lower_left;

  struct {
    int x;
    int y;
  } upper_right;
};

typedef struct {
  int width;
  int height;
} *config;

/**
 * Awkward names
 */

typedef int adiós;
void 拜拜(void);
typedef int 数字;
void ϒ(void);
typedef int data;
void import(void);

/**
 * Flavours of enums
 */

enum signal {
  start = 1,
  pause,
  resume,
  stop
};

enum HTTP_status {
  ok           = 200,
  moved        = 301,
  bad_request  = 400,
  unauthorized = 401,
  not_found    = 404
};

enum descending {
  X       = 100,
  Y       = 99,
  Y_alias = 99,
  Z       = 98,
};

enum result {
  failed       = -1,
  success      = 0,
  postponed    = 1,
  already_done = 2
};

enum vote {
  infavour,
  against,
  abstain
} __attribute__((packed));

// Extract of `CXCursorKind` from `libclang`
// https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013
enum CXCursorKind {
  CXCursor_FirstExpr        = 100,
  CXCursor_UnexposedExpr    = 100,
  CXCursor_DeclRefExpr      = 101,
  CXCursor_MemberRefExpr    = 102,
  // .. many expressions omitted ..
  CXCursor_PackIndexingExpr = 156,
  CXCursor_LastExpr = CXCursor_PackIndexingExpr,

  CXCursor_FirstStmt              = 200,
  CXCursor_UnexposedStmt          = 200,
  CXCursor_LabelStmt              = 201,
  CXCursor_CompoundStmt           = 202,
  // .. many statements omitted ..
  CXCursor_OpenACCUpdateConstruct = 331,
  CXCursor_LastStmt = CXCursor_OpenACCUpdateConstruct,
};