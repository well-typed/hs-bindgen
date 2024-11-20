// struct without typedef
struct S1 {
    int a;
    char b;
};

// struct with typedef
typedef struct S2 {
    char a;
    int b;
    float c;
} S2_t;

// anonymous struct with typedef
typedef struct {
    char a;
} S3_t;

struct S4 {
    char b;
    int a;
    int *c;
};

// matching tags
typedef struct S5 {
    char a;
    int b;
} S5;

struct S6 { char a; int b; };
typedef struct S6 S6;
