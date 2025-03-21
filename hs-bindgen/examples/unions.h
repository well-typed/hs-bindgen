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
