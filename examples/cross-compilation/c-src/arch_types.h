/*
 * arch_types.h - A minimal header demonstrating architecture-dependent types
 *
 * This header is specifically designed to show the differences between
 * 32-bit and 64-bit platforms. The types used have different sizes
 * depending on the target architecture's data model.
 *
 * Data Models:
 *   - LP64  (Linux/macOS 64-bit): long and pointers are 64-bit
 *   - ILP32 (Linux/macOS 32-bit): int, long, and pointers are 32-bit
 *   - LLP64 (Windows 64-bit):     long is 32-bit, pointers are 64-bit
 *
 * This header intentionally avoids #include directives to make
 * cross-compilation simpler (no target sysroot needed).
 */

#ifndef ARCH_TYPES_H
#define ARCH_TYPES_H

/*
 * ArchInfo: A struct that demonstrates architecture-dependent sizing
 *
 * On 64-bit Linux/macOS (LP64):
 *   - sizeof(ArchInfo) = 24 bytes (8+8+4+1+3pad)
 *   - alignment = 8 bytes
 *
 * On 32-bit Linux (ILP32):
 *   - sizeof(ArchInfo) = 16 bytes (4+4+4+1+3pad)
 *   - alignment = 4 bytes
 */
struct ArchInfo {
    /* 'long' is 8 bytes on LP64, 4 bytes on ILP32 */
    long value;

    /* Pointers are 8 bytes on 64-bit, 4 bytes on 32-bit */
    void *data;

    /* 'int' is 4 bytes on common platforms */
    int count;

    /* 'char' is 1 byte */
    char flags;

    /* Padding will be added here to align the struct
     * On 64-bit: 3 bytes padding to align to 8-byte boundary
     * On 32-bit: 3 bytes padding to align to 4-byte boundary
     */
};

/*
 * PointerArray: Shows how pointer arrays differ in size
 *
 * An array of 4 pointers:
 *   - On 64-bit: 4 * 8 = 32 bytes
 *   - On 32-bit: 4 * 4 = 16 bytes
 */
struct PointerArray {
    void *items[4];
    int length;
};

/*
 * NestedStruct: A more complex example with nested pointers
 *
 * Shows how nested structures with pointers compound size differences.
 */
struct NestedStruct {
    struct ArchInfo *info;      /* pointer size varies */
    struct NestedStruct *next;  /* pointer size varies */
    long sequence_number;       /* long size varies */
};

/*
 * Some simple constants to verify macro handling works across platforms
 */
#define ARCH_TYPES_VERSION 1
#define MAX_ITEMS 100
#define MAGIC_NUMBER 0xDEADBEEF

/*
 * Function declarations (for FFI binding generation)
 *
 * These demonstrate how function signatures with architecture-dependent
 * types are handled in cross-compilation.
 */

/* Returns the size of ArchInfo at compile time */
int get_arch_info_size(void);

/* Initializes an ArchInfo structure */
void init_arch_info(struct ArchInfo *info, long value, int count);

/* Processes a pointer array */
int process_pointer_array(struct PointerArray *arr);

/* Creates a linked list node */
struct NestedStruct *create_node(long seq);

#endif
