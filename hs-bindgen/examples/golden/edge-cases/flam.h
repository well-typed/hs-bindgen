// flexible array members
struct pascal {
    int len;
    char data[];
};

// anonymous struct as flam
struct foo {
	int len;
	struct {
		int x;
		int y;
	} bar[];
};

// offset and sizeof not the same
struct diff {
	long first;
	char second;
	char flam[];
};

//! The flexible array member is a multi-dimensional array of unknown size. In
//! particular, it is a is an array of unknown size, where each element is of
//! type length-3-array-of-int.
struct triplets {
  int len;
  int flam[][3];
};