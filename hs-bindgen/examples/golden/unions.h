struct Dim2 {
    int x;
    int y;
};

struct Dim3 {
    int x;
    int y;
    int z;
};

union DimPayload {
    struct Dim2 dim2;
    struct Dim2 dim3;
};

struct Dim {
    int tag;
    union DimPayload payload;
};

// typedef name matches union tag
typedef union DimPayloadB {
    struct Dim2 dim2;
    struct Dim2 dim3;
} DimPayloadB;

struct DimB {
    int tag;
    DimPayloadB payload;
};

// union with anonymous struct fields
union AnonA {
    struct { double x; double y; } xy;
    struct { double r; double p; } polar;
};

/*
 * Nested anonymous declarations
 */

//! A union containing only an implicit, anonymous struct.
union u1 {
  struct {
    int x1;
    char x2;
  };
};

//! A union containing only an implicit, anonymous union.
union u2 {
  union {
    int x1;
    char x2;
  };
};

//! A union containing a regular field and an implicit, anonymous struct.
union u3 {
  int l1;
  struct {
    int x1;
    char x2;
  };
};

//! A union containing a regular field and an implicit, anonymous union.
union u4 {
  int l1;
  union {
    int x1;
    char x2;
  };
};

//! A union containing:
//! - a regular field
//! - an implicit, anonymous struct
//! - an implicit, anonymous union
union u5 {
  int uint32_t;
  struct {
    char x1;
    char x2;
    char x3;
    char x4;
  };
  union {
    char y1;
    char y2;
    char y3;
    char y4;
  };
};

