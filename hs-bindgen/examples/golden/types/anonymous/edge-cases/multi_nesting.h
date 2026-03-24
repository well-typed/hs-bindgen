#pragma once

/* -------------------------------------------------------------------------- */
/* >1 levels of nesting of anonymous objects */

// When detecting implicit fields for an object X, it is important to consider
// that any anonymous objects declared inside X can themselves can also have
// implicit fields. Examples of this multi-level nesting of anonymous objects
// are included in this header file.

// Naming: the types in this header are named after the order in which objects
// are nested. For example, USS means "union containing struct containing
// struct", or equivalently, "struct in struct in union".

struct SSS {
  struct {
    struct {
      int x;
    };
  };
};

union USS {
  struct {
    struct {
      int x;
    };
  };
};

struct SUS {
  union {
    struct {
      int x;
    };
  };
};

union UUS {
  union {
    struct {
      int x;
    };
  };
};

struct SSU {
  struct {
    union {
      int x;
    };
  };
};

union USU {
  struct {
    union {
      int x;
    };
  };
};

struct SUU {
  union {
    union {
      int x;
    };
  };
};

union UUU {
  union {
    union {
      int x;
    };
  };
};
