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
