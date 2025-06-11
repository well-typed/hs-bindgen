// Reparsing function declarations

// clang -Xclang -ast-dump file.c

#define I int
#define C char
#define F float
#define L long
#define S short

// very basic
char quux(F x, char y) { return 0; }
C *wam(float x, C *y) { return 0; }

// basic
char* foo1(float x, int (*g)(int)) { return 0; }
char* foo2(F x, int (*g)(int)) { return 0; }
C* foo3(float x, int (*g)(int)) { return 0; }

// return function pointer
int (*bar1(long x))(short) { return 0; }
int (*bar2(L x))(short) { return 0; }
int (*bar3(long x))(S) { return 0; }
I (*bar4(long x))(short) { return 0; }

// return array
int (*baz1(const int i))[2][3] {
    static int arr[2][3] = {
        {1, 2, 3},
        {11, 12, 13}
    };
    arr[1][1] = i;
    return &arr;
}
int (*baz2(const I i))[2][3] {
    static int arr[2][3] = {
        {1, 2, 3},
        {11, 12, 13}
    };
    arr[1][1] = i;
    return &arr;
}
I (*baz3(const int i))[2][3] {
    static int arr[2][3] = {
        {1, 2, 3},
        {11, 12, 13}
    };
    arr[1][1] = i;
    return &arr;
}

// neither arguments nor keyword void
I no_args_no_void();
