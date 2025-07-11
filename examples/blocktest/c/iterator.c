#include "Block.h"

#include "iterator.h"

// Toggle

Toggle makeToggle(bool start) {
    __block int b = start;

    return Block_copy( ^(void) {
        bool ret = b;
        b = !b;
        return ret;
    });
}

bool toggleNext(Toggle block) {
    return block();
}

void releaseToggle(Toggle block) {
    Block_release(block);
}

// Counter

Counter makeCounter(int start, int increment) {
	__block int i = start; // __block storage makes it assignable

	return Block_copy( ^(void) {
		int ret = i;
		i += increment;
		return ret;
	});
}

int counterNext(Counter block) {
    return block();
}

void releaseCounter(Counter block) {
	Block_release(block);
}

// Variable increment

VarCounter makeVarCounter(int start) {
	__block int i = start; // __block storage makes it assignable

	return Block_copy( ^(int increment) {
		int ret = i;
		i += increment;
		return ret;
	});
}

int varCounterNext(VarCounter block, int increment) {
    return block(increment);
}

void releaseVarCounter(VarCounter block) {
    Block_release(block);
}

