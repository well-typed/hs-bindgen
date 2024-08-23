/**
 * A struct with a Doxygen comment
 */
struct S4 {
    /**
     * A field preceded by a Doxygen comment
     */
    char a;

    int b; /**< A field followed by a Doxygen comment */

    /**
     * A field that refers to another field
     *
     * See also @ref S4::a
     */
    float c;
};