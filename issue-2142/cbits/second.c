#include "second.h"

void initSecondHandle(FirstHandle firstHandle, SecondHandle *pHandle) {
    *pHandle = firstHandle + 1;
}
