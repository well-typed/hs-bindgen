/* This example header contains various definitions
 * to smoke test hs-bindgen code generation end-to-end.
 * See test-th and test-pp tests.
 */

/**
 * @file test_01.h
 *
 * @brief Example header containing various definitions to smoke test hs-bindgen code generation end-to-end.
 * @details This file demonstrates various Doxygen documentation features
 *          including commands, lists, code examples, and more.
 *
 * @note This is a test file for documentation generation
 * @warning Do not use in production without review
 */

#ifndef TEST_01_H
#define TEST_01_H

/* Standard headers. */
#include <stdlib.h>

/**
 * @struct StructBasic
 * @brief Basic structure with two fields.
 * @details This structure demonstrates simple field documentation
 *          with inline comments.
 *
 * Example usage:
 *
 * @code
 * struct StructBasic sb;
 * sb.field1 = 42;
 * sb.field2 = 'A';
 * @endcode
 *
 * @invariant field1 should be positive in normal usage
 * @since 1.0
 */
struct StructBasic {
    int field1;  /**< Integer field. @note Usually positive */
    char field2; /**< Character field. Valid range: @b A-Z */
};

/**
 * @struct StructFixedSizeArray
 * @brief Structure containing a fixed size array.
 *
 * @attention The array size is fixed at compile time
 *
 * Memory layout:
 * - 4 bytes for integer x
 * - 5 bytes for character array
 * - Padding may be added by compiler
 */
struct StructFixedSizeArray {
    int x;       /**< Integer value. @sa StructBasic::field1 */
    char ys[5];  /**< Fixed size character array of length 5
                      @warning Not null-terminated by default */
};

/**
 * @def PLUS(x,y)
 * @brief Macro that adds two values and adds 1L to the result.
 * @details This macro demonstrates parameter documentation and
 *          potential issues with macro expansion.
 *
 * @param[in] x First operand (should be parenthesized)
 * @param[in] y Second operand (should be parenthesized)
 * @return The result of x + y + 1L
 *
 * @warning This macro doesn't parenthesize arguments, so be careful with:
 * @code
 * PLUS(a & b, c | d)  // Probably not what you want!
 * @endcode
 *
 * @todo Add parentheses for safety
 */
#define PLUS(x,y) x + y + 1L

/**
 * @struct StructBitfield
 * @brief Structure with bitfield definitions.
 * @details Demonstrates various bitfield sizes and their documentation.
 *
 * Total size considerations:
 * -# Regular int field: 32 bits
 * -# Bitfields packed together: 7 bits total
 * -# Compiler may add padding
 *
 * @par Thread Safety:
 * Bitfield access is not atomic. Use appropriate synchronization.
 */
struct StructBitfield {
    int a;               /**< Integer field (full width) */
    unsigned int b : 1;  /**< 1-bit flag. @c 0 = false, @c 1 = true */
    unsigned int c : 1;  /**< 1-bit flag. Values: {0, 1} */
    int d : 5;          /**< 5-bit signed field. Range: [-16, 15] */
};

/**
 * @brief Multiply and add function (fused multiply-add).
 * @details Computes @f$ result = x \times y + z @f$
 *
 * This function demonstrates:
 * - Parameter direction documentation
 * - Mathematical formulas
 * - Example code
 *
 * @param[in] x First multiplier
 * @param[in] y Second multiplier
 * @param[in] z Addend
 * @return The result of (x * y) + z
 *
 * @par Example:
 * @code
 * int result = my_fma(3, 4, 5);  // Returns 17
 * @endcode
 *
 * @note No overflow checking is performed
 * @see https://en.wikipedia.org/wiki/Multiply%E2%80%93accumulate_operation
 */
static inline int my_fma(int x, int y, int z) {
    return x * y + z;
}

/**
 * @struct StructFLAM
 * @brief Structure with a flexible array member.
 * @details Contains a length field and a flexible array of long integers.
 *          This is a C99 feature.
 *
 * @par Memory Management:
 * Must be allocated dynamically with extra space for the array:
 * @code
 * size_t size = sizeof(struct StructFLAM) + n * sizeof(long);
 * struct StructFLAM *p = malloc(size);
 * @endcode
 *
 * @remark Flexible array members must be the last member
 * @since C99
 */
struct StructFLAM {
    int length;     /**< Number of elements in the flexible array
                         @invariant length >= 0 */
    long numbers[]; /**< Flexible array member of long integers
                         @warning Must not be accessed beyond length */
};

/**
 * @brief Allocate a StructFLAM with space for n elements.
 * @details Initializes the numbers array with values from 0 to n-1.
 *          This demonstrates proper allocation of flexible array members.
 *
 * Algorithm:
 * -# Allocate memory for structure plus array
 * -# Set length field
 * -# Initialize array elements sequentially
 *
 * @param[in] n Number of elements to allocate in the flexible array
 * @pre n >= 0
 * @return Pointer to the allocated StructFLAM, or NULL if allocation fails
 * @retval NULL if allocation fails or n < 0
 * @retval non-NULL pointer to initialized structure
 *
 * @par Error Handling:
 * Check return value for NULL before use.
 *
 * @b Example:
 * @code
 * struct StructFLAM *arr = flam_alloc(10);
 * if (arr) {
 *     // Use arr
 *     flam_free(arr);
 * }
 * @endcode
 *
 * @sa flam_free()
 */
static inline struct StructFLAM *flam_alloc(int n) {
    struct StructFLAM *ptr = malloc(sizeof(struct StructFLAM) + sizeof(long) * n);
    if (ptr) {
        ptr->length = n;
        for (int i = 0; i < n; ++i) {
            ptr->numbers[i] = i; /**< Initialize each element with its index */
        }
    }
    return ptr;
}

/**
 * @brief Free a previously allocated StructFLAM.
 *
 * @param[in,out] ptr Pointer to the StructFLAM to free
 * @pre ptr must be either NULL or a valid pointer returned by flam_alloc()
 * @post ptr is no longer valid after this call
 *
 * @note Safe to call with NULL pointer
 * @sa flam_alloc()
 */
static inline void flam_free(struct StructFLAM *ptr) {
    free(ptr);
}

/**
 * @brief Reverses the elements in the flexible array member in place.
 *
 * @param s Pointer to the structure containing the numbers to reverse.
 *          If @p s is NULL or @c s->length < 2, the function returns immediately.
 */
static inline void flam_reverse(struct StructFLAM *s) {
    if (!s || s->length < 2) return;

    for (int i = 0, j = s->length - 1; i < j; i++, j--) {
        long temp = s->numbers[i];
        s->numbers[i] = s->numbers[j];
        s->numbers[j] = temp;
    }
}

/**
 * @union longDouble
 * @brief Union representing a value as either a long long or a double.
 * @details This union demonstrates type punning capabilities.
 *
 * @warning Type punning through unions may have undefined behavior
 *          in some cases. Use with caution.
 *
 * Size: @c sizeof(double) or @c sizeof(long long), whichever is larger
 */
union longDouble {
    long long l; /**< Value interpreted as long long (typically 64 bits) */
    double d;    /**< Value interpreted as double (IEEE 754 double precision) */
};

/**
 * @defgroup enumerations Enumeration Types
 * @brief Various enumeration examples
 * @details These enumerations demonstrate different patterns and documentation styles.
 * @{
 */

/**
 * @enum EnumBasic
 * @brief Basic enumeration with sequential values starting at 0.
 *
 * This enumeration uses default values:
 * - First element starts at 0
 * - Subsequent elements increment by 1
 */
enum EnumBasic {
    ENUM_BASIC_A, /**< First value (implicitly 0) */
    ENUM_BASIC_B, /**< Second value (implicitly 1) */
    ENUM_BASIC_C  /**< Third value (implicitly 2) */
};

/**
 * @enum EnumNeg
 * @brief Enumeration starting with a negative value.
 *
 * Demonstrates that enums can have negative values.
 *
 */
enum EnumNeg {
    ENUM_NEG_A = -1, /**< Negative starting value */
    ENUM_NEG_B,      /**< Zero (incremented from -1) */
    ENUM_NEG_C       /**< Positive value */
};

/**
 * @enum EnumNonSeq
 * @brief Enumeration with non-sequential values.
 * @details Used for status codes or magic numbers.
 *
 * Common uses:
 * - HTTP status codes
 * - Error codes
 * - Protocol constants
 */
enum EnumNonSeq {
    ENUM_NON_SEQ_A = 200, /**< @brief OK status
                               @details Similar to HTTP 200 */
    ENUM_NON_SEQ_B = 301, /**< @brief Redirect status
                               @details Similar to HTTP 301 */
    ENUM_NON_SEQ_C = 404  /**< @brief Not found status
                               @details Similar to HTTP 404 */
};

/**
 * @enum EnumSame
 * @brief Enumeration with multiple constants sharing the same value.
 * @warning Be careful when switching on these values
 *
 * @deprecated Consider using unique values to avoid confusion
 */
enum EnumSame {
    ENUM_SAME_A = 0,   /**< Initial state */
    ENUM_SAME_B = 100, /**< State B */
    ENUM_SAME_C = 100, /**< State C @warning Same value as ENUM_SAME_B */
    ENUM_SAME_D = 200  /**< Final state */
};

/** @} */ // end of enumerations group

/**
 * @struct thing
 * @brief Simple structure with a single integer field.
 * @details This structure is used to demonstrate function parameters
 *          and return values with structures.
 *
 * @par Design Rationale:
 * Kept simple to test structure passing mechanisms.
 */
struct thing {
    int x; /**< Integer field. Can be negative. */
};

/**
 * @brief Returns the integer field of a thing.
 * @details Simple accessor function.
 *
 * @param[in] x A struct thing (passed by value)
 * @return The value of x.x
 *
 * @par Performance Note:
 * Structure is passed by value, which may be inefficient for larger structures.
 */
static inline int thing_fun_1(struct thing x) {
    return x.x;
}

/**
 * @brief Creates a struct thing initialized with the given integer.
 *
 * @param[in] x Integer to initialize the thing's field
 * @return A struct thing with field x set to the parameter
 *
 * @code
 * struct thing t = thing_fun_2(42);
 * assert(t.x == 42);
 * @endcode
 */
static inline struct thing thing_fun_2(int x) {
    struct thing res = { .x = x };
    return res;
}

/**
 * @brief Returns a struct thing with the field doubled.
 * @details Demonstrates structure transformation.
 *
 * @param[in] x A struct thing
 * @return A struct thing with field x equal to input x.x multiplied by 2
 *
 * @note May overflow if x.x > INT_MAX/2
 *
 * @par Example Chain:
 * @code
 * struct thing t1 = thing_fun_2(10);
 * struct thing t2 = thing_fun_3(t1);  // t2.x == 20
 * struct thing t3 = thing_fun_3(t2);  // t3.x == 40
 * @endcode
 */
static inline struct thing thing_fun_3(struct thing x) {
    struct thing res = { .x = x.x * 2 };
    return res;
}

/**
 * @brief Sums an integer and the elements of a fixed-size array of length 3.
 * @details The first element of the array is doubled before summing.
 *
 * @param[in] x Integer value
 * @param[in,out] xs Array of 3 integers (first element modified!)
 * @return Sum of x and the modified array elements
 *
 * @warning This function modifies the input array!
 *
 * @par Side Effects:
 * - @c xs[0] is multiplied by 2
 * - Other array elements unchanged
 *
 * @code
 * int arr[3] = {1, 2, 3};
 * int result = sum3(10, arr);
 * // result == 10 + 2 + 2 + 3 == 17
 * // arr is now {2, 2, 3}
 * @endcode
 */
static inline int sum3(int x, const int xs[3]) {
    const int x0 = xs[0] * 2;
    return x + x0 + xs[1] + xs[2];
}

/**
 * @typedef triple
 * @brief Type alias for an array of 3 integers.
 * @details Demonstrates typedef with arrays.
 *
 * @note Array parameters decay to pointers in C
 *
 * Usage patterns:
 * -# Stack allocation: @c triple t = {1, 2, 3};
 * -# Initialization: @c triple t = {0}; // All zeros
 * -# Parameter passing: Decays to @c int*
 */
typedef int triple[3];

/**
 * @brief Sums an integer and the elements of a triple array.
 * @details The first element of the array is tripled before summing.
 *
 * Comparison with sum3():
 * - sum3: multiplies first element by 2
 * - sum3b: multiplies first element by 3
 *
 * @param[in] x Integer value
 * @param[in,out] xs Triple array of integers
 * @return Sum of x and the modified array elements
 *
 * @pre xs must point to at least 3 integers
 * @post xs[0] = original xs[0] * 3
 *
 * @see sum3()
 */
static inline int sum3b(int x, const triple xs) {
    int x0 = xs[0] * 3;
    return x + x0 + xs[1] + xs[2];
}

#endif /* TEST_01_H */
