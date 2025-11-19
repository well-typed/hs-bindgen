// Toggle between true and false

typedef bool (^Toggle)();
Toggle makeToggle(bool start);
bool toggleNext(Toggle block);
void releaseToggle(Toggle block);

// Simple counter, fixed increment

typedef int(^Counter)();
Counter makeCounter(int start, int increment);
int counterNext(Counter block);
void releaseCounter(Counter block);

// Counter with variable increment

typedef int(^VarCounter)(int increment);
VarCounter makeVarCounter(int start);
int varCounterNext(VarCounter block, int increment);
void releaseVarCounter(VarCounter block);
