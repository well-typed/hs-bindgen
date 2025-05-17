typedef int int_t;
typedef int_t int_t_t;

struct empty {
};

struct simple {
  int x_1_1;
  int x_1_2;
};

struct nested {
  struct inner1 {
    int x_2_1_1;
    int x_2_1_2;
  } x_2_1;
  struct {
    int x_2_2_1;
    int x_2_2_2;
  } x_2_2;
  int x_2_3;
};

typedef struct empty  empty_t;
typedef struct simple simple_t;
typedef struct nested nested_t;

typedef struct empty  *empty_p;
typedef struct simple *simple_p;
typedef struct nested *nested_p;

typedef empty_t unit;

typedef struct {
} anon_empty;

typedef struct {
  int x_3_1;
  int x_3_2;
} anon_simple;

typedef struct {
  struct inner2 {
    int x_4_1_1;
    int x_4_1_2;
  } x_4_1;
  struct {
    int x_4_2_1;
    int x_4_2_2;
  } x_4_2;
  int x_4_3;
} anon_nested;

typedef struct {
} *anon_empty_p;

typedef struct {
  int x_3_1;
  int x_3_2;
} *anon_simple_p;

typedef struct {
  struct inner3 {
    int x_4_1_1;
    int x_4_1_2;
  } x_4_1;
  struct {
    int x_4_2_1;
    int x_4_2_2;
  } x_4_2;
  int x_4_3;
} *anon_nested_p;

struct pointer_field {
  int x_5_1;
  struct {
    int x_5_2_1;
    int x_5_2_2;
  } *x_5_2;
};

/*
void previously_declared(struct simple *arg);

// gcc says
//
//   warning: ‘struct inline_named’ declared inside parameter list will not be
//   visible outside of this definition or declaration
//
// clang says
//
//   warning: declaration of 'struct inline_named' will not be visible outside
//   of this function [-Wvisibility]
//
// vscode (not sure what integration this uses says)
//
//   type definition is not allowed
void declared_inline_named(struct inline_named { int x_5; } *arg);

// gcc says
//
//   warning: anonymous struct declared inside parameter list will not be
//   visible outside of this definition or declaration
//
// vscode says
//
//   type definition is not allowed
//
// Interestingly, clang does not give _any_ warning here.
void declared_inline_anon(struct { int x_6; } *arg);
*/
