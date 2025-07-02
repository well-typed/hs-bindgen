/**
 * Test that we correctly distinguish between named and anonymous declarations
 *
 * For some reason clang gives us different results for short names (3
 * characters or less) and longer names (4 more), it's unclear why. We test both
 * in each category.
 *
 * NOTE: We can only successfully generate code for this with clang >= 19.1.0.
 */

#define STRUCT(name) name##_s
#define PACKED __attribute__((__packed__))

// simple name
struct a {};
struct struct1 {};

// name using macro
struct STRUCT(b) {};
struct STRUCT(struct2) {};

// simple name, simple attribute
struct __attribute__((__packed__)) c {};
struct __attribute__((__packed__)) struct3 {};

// simple name, attribute using macro
struct PACKED d {};
struct PACKED struct4 {};

// name using macro, attribute using macro
struct PACKED STRUCT(e) {};
struct PACKED STRUCT(struct5) {};

// anonymous
typedef struct {} f;
typedef struct {} typedef1;

// anonymous, simple attribute
typedef struct __attribute__((__packed__)) {} g;
typedef struct __attribute__((__packed__)) {} typedef2;

// anonymous, attribute using macro
typedef struct PACKED {} h;
typedef struct PACKED {} typedef3;
