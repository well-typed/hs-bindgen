struct MyStruct {
    int field1;
    char field2;
};

struct Struct2 {
    int x;
    char ys[5];
};

#define PLUS(x,y) x + y + 1L

struct Struct3 {
    int a;
    unsigned int b : 1;
    unsigned int c : 1;
    int d : 5;
};

static inline int my_fma(int x, int y, int z) {
    return x * y + z;
}
