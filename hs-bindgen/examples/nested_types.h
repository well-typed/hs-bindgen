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

struct ex4 {
    struct ex4_b {
        int ex3_a;
        char ex3_b;
        struct ex4_b *recur;
    } linkedlist;
    float ex3_c;
};
