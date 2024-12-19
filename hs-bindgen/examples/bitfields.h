struct flags {
    char fieldX;
    int flagA : 1;
    int flagB : 1;
    int flagC : 1;
    char fieldY;
    int bits  : 2;
};

// various bitfields which don't add to multiplies of 32
// having compiler sometimes insert padding between fields.
struct overflow32 {
    int x : 17;
    int y : 17;
    int z : 17;
};

struct overflow32b {
    long x : 17;
    long y : 17;
    long z : 17;
};

struct overflow32c {
    long x : 17;
    int  y : 17;
    long z : 17;
};

struct overflow64 {
    long x : 33;
    long y : 33;
};

// alignment
struct alignA {
	unsigned char x : 1;
	int y : 10;
};

struct alignB {
	unsigned char x : 7;
	int y : 31;
};
