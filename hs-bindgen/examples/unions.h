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
