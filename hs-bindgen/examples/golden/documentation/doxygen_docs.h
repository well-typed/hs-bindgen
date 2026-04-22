/**
 * @file doxygen_docs.h
 * @brief Comprehensive test file for Doxygen documentation features
 * @author Test Author
 * @version 1.0
 * @date 2025-01-15
 * @copyright Copyright (c) 2025 Test Company
 * @mainpage Doxygen Test Documentation
 *
 * This file demonstrates all possible locations where Doxygen documentation can appear.
 *
 * @section intro_sec Introduction
 * This is a comprehensive test of Doxygen features.
 *
 * @section features_sec Features
 * - Complete C syntax coverage
 * - All Doxygen commands
 * - Documentation in every possible location
 */

#ifndef DOXYGEN_DOCS_H
#define DOXYGEN_DOCS_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/**
 * @defgroup core_types Core Data Types
 * @brief Fundamental data types and structures
 * @{
 */

/**
 * @def MAX_NAME_LENGTH
 * @brief Maximum length for name strings
 *
 */
#define MAX_NAME_LENGTH 64

/**
 * @def MIN(a, b)
 * @brief Returns the minimum of two values
 * @param a First value
 * @param b Second value
 * @return The smaller value
 */
#define MIN(a, b) ((a) < (b) ? (a) : (b))

/**
 * @typedef size_type
 * @brief Size type for this library
 *
 * This is the comment @b title
 */
typedef size_t size_type;

/**
 * @var extern int global_counter
 * @brief Global counter variable
 * @details This variable tracks the number of operations performed.
 */
extern int global_counter;

/**
 * @var extern const char* version_string
 * @brief Version string constant
 */
extern const char* version_string;

/**
 * @brief Forward declaration with documentation
 *
 * This is the comment @c title
 */
struct forward_declared_struct;

/**
 * @brief Forward declaration of union
 */
union forward_declared_union;

/**
 * @enum color_enum
 * @brief Color enumeration without typedef
 */
enum color_enum {
    COLOR_RED,   /**< Red color */
    COLOR_GREEN, /**< Green color */
    COLOR_BLUE   /**< Blue color */
};

/** @} */ /* end core_types */

/**
 * @defgroup functions Function Definitions
 * @brief All function definitions
 * @{
 */

/**
 * @brief Function with detailed parameter documentation
 *
 * This function shows different parameter directions and types.
 *
 * @param[in] input_data Input data buffer
 * @param[out] output_data Output data buffer
 * @param[in,out] size Size of data, updated on return
 * @return Status code (0 = success, -1 = error)
 */
int process_data(const uint8_t* input_data, uint8_t* output_data, size_t* size);

/**
 * @brief Function with inline commands and formatting
 *
 * This function uses @c inline @c code formatting and @b bold text.
 * It also demonstrates @e emphasized text.
 *
 * @param filename The @c char* filename to process
 * @return @c true if successful, @c false otherwise
 */
bool process_file(const char* filename);

/**
 * @brief Function with verbatim code blocks
 *
 * Example usage:
 * @code
 * int result = calculate_value(10, 20);
 * printf("Result: %d\n", result);
 * @endcode
 *
 * @param base Base value
 * @param multiplier Multiplier value
 * @return Calculated result
 */
int calculate_value(int base, int multiplier);

/**
 * @brief Function with HTML formatting
 *
 * This function demonstrates <b>HTML bold</b> and <i>italic</i> text.
 * It also shows <code>HTML code</code> formatting.
 *
 * <table>
 * <tr><th>Input</th><th>Output</th></tr>
 * <tr><td>0</td><td>false</td></tr>
 * <tr><td>1</td><td>true</td></tr>
 * </table>
 *
 * @param value Input value
 * @return Boolean result
 */
bool html_example(int value);

/**
 * @brief Function with lists and special formatting
 *
 * This function demonstrates:
 * - Bullet point lists
 *   * Nested list item 1
 *   * Nested list item 2
 * - Multiple items
 * - Nested formatting
 *
 * Numbered list:
 * 1. First @c item 1. item
 * 2. Second @b item
 * 3. Third item
 *
 * Other numbered list:
 * -# A
 * -# B
 * -# C
 *
 * @param items Array of items
 * @param count Number of items
 * @return Success status
 */
bool list_example(const char** items, size_t count);

/**
 * @brief Function with warnings and notes
 *
 * @warning This function may cause side effects
 * @note Use with caution in multithreaded environments
 * @see related_function() for similar functionality
 *
 * @param ptr Pointer to data
 * @return Modified pointer
 */
void* dangerous_function(void* ptr);

/**
 * @brief Function with return value details
 *
 * @param input Input string
 * @retval 0 Success
 * @retval -1 Invalid input
 * @retval -2 Memory allocation failed
 * @retval -3 Processing error
 */
int detailed_return_codes(const char* input);

/**
 * @brief Function with deprecated annotation
 *
 * @deprecated Use new_function() instead
 * @param old_param Legacy parameter
 * @return Legacy result
 */
int old_function(int old_param);

/**
 * @brief Function with version information
 *
 * @since 1.0
 * @version 1.2
 * @param data Input data
 * @return Processed data
 */
int versioned_function(int data);

/**
 * @brief Callback function type
 *
 * @param event_type Type of event
 * @param user_data User-provided data
 * @return Handling result
 */
typedef int (*event_callback_t)(int event_type, void* user_data);

/**
 * @brief Structure with documented fields
 *
 * This structure demonstrates field documentation.
 */
typedef struct {
    /** @brief Unique identifier */
    uint32_t id;

    /** @brief Human-readable name */
    char name[64];

    /** @brief Configuration flags */
    uint32_t flags;

    /** @brief Optional callback function
     *
     * See also: \ref event_callback_t
     *
     * */
    event_callback_t callback;

    /** @brief User data for callback */
    void* user_data;
} /** Configuration struct */ config_t;

/**
 * @brief Enumeration with documented values
 *
 * This enum shows different status codes.
 */
typedef enum {
    /** @brief Operation successful */
    STATUS_OK = 0,

    /** @brief Invalid parameter provided */
    STATUS_INVALID_PARAM = -1,

    /** @brief Memory allocation failed */
    STATUS_NO_MEMORY = -2,

    /** @brief Operation timed out */
    STATUS_TIMEOUT = -3,

    /** @brief Generic error */
    STATUS_ERROR = -99
} /** Status Code */ status_code_t;

/**
 * @union data_union_t
 * @brief Union with documented fields
 *
 * This union demonstrates different data representations.
 */
typedef union {
    int32_t as_int;    /**< @brief Integer representation */
    float as_float;    /**< @brief Float representation */
    uint8_t as_bytes[4]; /**< @brief Byte array representation */

    /**
     * @brief Structured representation
     * @details Allows access to high and low parts separately
     */
    struct {
        uint16_t low;  /**< @brief Low 16 bits */
        uint16_t high; /**< @brief High 16 bits */
    } /** As Parts Struct */ as_parts;
} /** Union Struct */ data_union_t;

/**
 * @struct bitfield_t
 * @brief Bit field structure
 *
 * Demonstrates bit field documentation.
 */
typedef struct {
    unsigned flag1 : 1;     /**< @brief First flag (1 bit) */
    unsigned flag2 : 1;     /**< @brief Second flag (1 bit) */
    unsigned counter : 6;   /**< @brief Counter value (6 bits) */
    unsigned reserved : 24; /**< @brief Reserved bits (24 bits) */
} bitfield_t;

/**
 * @typedef processor_fn_t
 * @brief Function pointer typedef
 *
 * @param input Input value
 * @param context Context pointer
 * @return Processed value
 */
typedef int (*processor_fn_t)(int input, void* context);

/**
 * @typedef filename_t
 * @brief Array typedef with size
 */
typedef char filename_t[256];

/**
 * @brief Static array parameter
 *
 * @param buffer Buffer with minimum size
 * @param size Actual buffer size
 * @return Number of bytes written
 */
int process_buffer(char buffer[static 64], size_t size);

/**
 * @brief Function with restrict pointers
 *
 * @param dest Destination buffer (restrict)
 * @param src Source buffer (restrict)
 * @param n Number of bytes
 * @return Destination pointer
 */
void* my_memcpy(void* restrict dest, const void* restrict src, size_t n);

/**
 * @brief Inline function
 *
 * @param x Input value
 * @return Doubled value
 */
static inline int double_value(int x) {
    return x * 2;
}

/**
 * @brief Structure with flexible array member
 *
 * Used for variable-length data buffers.
 */
struct flexible_array {
    size_t count;      /**< @brief Number of elements */
    int data[];        /**< @brief Flexible array member */
};

/**
 * @}
 */

/**
 * @defgroup advanced_features Advanced Features
 * @brief Advanced C and Doxygen features
 * @{
 */

/**
 * @brief Function with complex documentation
 *
 * This function demonstrates multiple documentation features:
 *
 * @par Description:
 * Performs complex data processing with multiple steps.
 *
 * @par Algorithm:
 * 10. Validate input parameters
 * 200. Allocate temporary buffers
 * 3000. Process data in chunks
 * 41235. Clean up resources
 *
 * @par Algorithm2:
 * @li Validate input parameters
 * @li Allocate temporary buffers
 * @li Process data in chunks
 * @li Clean up resources
 *
 * @par Example:
 * @code
 * config_t cfg = {
 *     .id = 1,
 *     .name = "test",
 *     .flags = 0,
 *     .callback = my_callback,
 *     .user_data = NULL
 * };
 *
 * status_code_t result = complex_function(&cfg, data, size);
 * if (result != STATUS_OK) {
 *     handle_error(result);
 * }
 * @endcode
 *
 * @param config Configuration structure (see \ref config_t)
 * @param data Input data buffer
 * @param size Size of input data
 * @return Status code indicating success or failure
 *
 * @pre config must not be NULL
 * @pre data must not be NULL if size > 0
 * @post Output data is written to config->user_data
 *
 * @warning May return NULL if memory allocation fails
 * @warning Sets errno to EINVAL if parameters are invalid
 */
status_code_t complex_function(config_t* config, const uint8_t* data, size_t size);

// Function attributes: const and pure

int hash (char * s) __attribute__ ((pure));

int square (int x) __attribute__ ((const));

/**
 * @}
 */

/**
 * @defgroup extra_coverage Extra Doxygen Coverage
 * @brief Additional Doxygen features for test coverage
 * @{
 */

/**
 * Auto-brief function without explicit @@brief tag.
 *
 * This tests that the first sentence is used as the brief description
 * when no explicit @@brief is present.
 *
 * @param x The input value
 * @return The negated value
 */
int auto_brief_func(int x);

/**
 * @brief Multi-paragraph details
 *
 * First paragraph of the detailed description. This explains the basic
 * purpose of the function.
 *
 * Second paragraph with more context. This includes information about
 * the algorithm and its complexity, which is O(n) in the input size.
 *
 * Third paragraph with usage notes. Callers should ensure that the
 * buffer is large enough before calling this function.
 *
 * @param buf Output buffer
 * @param len Buffer length
 */
void multi_paragraph_details(char* buf, size_t len);

/**
 * @brief Function with @@todo and @@remark
 *
 * @todo Optimize this function for large inputs
 * @todo Add support for negative values
 * @remark This function is thread-safe
 * @attention The caller must free the returned pointer
 *
 * @param n Input count
 * @return Allocated array
 */
int* todo_remark_attention(int n);

/**
 * @brief Struct with invariant
 *
 * @invariant capacity >= size at all times
 */
typedef struct {
    int* data;           /**< @brief Pointer to data */
    size_t size;         /**< @brief Current number of elements */
    size_t capacity;     /**< @brief Allocated capacity */
} dyn_array_t;

/**
 * @brief HTML entities: &amp; means AND, &lt;tag&gt; is a tag
 *
 * Handles values &lt; 0 and values &gt; 100 differently.
 * Copyright &copy; 2025.
 *
 * @param x Input (&gt; 0 required)
 * @return Result code
 */
int html_entities_func(int x);

/**
 * @brief Nested inline: @b bold, @c code, @b @c bold_code, @e @b emph_bold
 *
 * @param x Input value
 */
void nested_inline_format(int x);

/**
 * @brief Language-tagged code block
 *
 * Example usage:
 * @code{.c}
 * int result = tagged_code_example(42);
 * printf("Result: %d\n", result);
 * @endcode
 *
 * @param x Input value
 * @return Processed value
 */
int tagged_code_example(int x);

/**
 * \brief Function documented with backslash syntax
 *
 * \details This function uses backslash commands instead of @@ commands
 * to verify both syntaxes are handled equivalently.
 *
 * \param input The input string
 * \param output The output buffer
 * \return Number of bytes written
 *
 * \see auto_brief_func
 * \since 2.0
 */
int backslash_syntax(const char* input, char* output);

/**
 * @brief Struct with multiple anonymous inner structs
 *
 * Tests that doxygen comment enrichment correctly associates field comments
 * with anonymous inner structs at multiple nesting levels.
 */
typedef struct {
    /**
     * @brief Position in 2D space
     */
    struct {
        float x;  /**< @brief X coordinate */
        float y;  /**< @brief Y coordinate */
    } /** Position fields */ pos;

    /**
     * @brief Dimensions
     */
    struct {
        float w;  /**< @brief Width */
        float h;  /**< @brief Height */
    } /** Dimension fields */ dim;
} multi_anon_t;

/**
 * @brief Struct with a named inner struct
 *
 * Tests that doxygen comment enrichment uses the qualified name
 * "named_outer::named_inner" for lookups.
 */
struct named_outer {
    /** Named inner struct */
    struct named_inner {
        /** Inner field nx */
        int nx;
        /** Inner field ny */
        int ny;
    } inner_field;
    /** Outer field nz */
    int nz;
};

/**
 * @brief Deeply nested mix of named and anonymous structs
 *
 * Tests the full genealogy walk through mixed named/anonymous nesting.
 */
struct deep_outer {
    /** The named mid-level struct */
    struct deep_mid {
        /** Mid-level field */
        int m;
        /** Anonymous struct inside named mid */
        struct {
            /** Deep anonymous field */
            int deep_a;
        } anon_field;
    } mid_field;
    /** Outer-only field */
    int o;
};

/**
 * @brief Struct with unnamed anonymous field
 *
 * Tests the case where the anonymous inner struct has no field name.
 * The inner fields are flattened directly into the parent.
 */
typedef struct {
    /** Before field */
    int before;
    struct {
        /** Unnamed inner a */
        int ua;
        /** Unnamed inner b */
        int ub;
    };
    /** After field */
    int after;
} unnamed_field_t;

/** @} */ /* end extra_coverage */

/**
 * @brief API version number (not in any group).
 */
typedef int api_version_t;

#endif /* DOXYGEN_DOCS_H */
