struct foo {
    int i;
    char c;
};

struct bar {
    struct foo foo1;
    struct foo foo2;
};

struct ex3 {
    struct {
        int ex3_a;
        char ex3_b;
    };
    float ex3_c;
};

// struct with inline struct which reference itself.
// linked list where odd values are ints, and even values are doubles
// by writing this way, we don't need forward declarations.
struct ex4_odd {
    int ex4_odd_value;
    struct ex4_even {
        double ex4_even_value;
        struct ex4_odd *next;
    } *next;
};
