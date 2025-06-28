#include <stdint.h>

/**
 * Global variables
 */

// Simple global variable
extern int simpleGlobal;

// Global variable of compound type
struct config {
  int x;
  int y;
};
extern struct config compoundGlobal1;

// Similar, but with inline declaration of struct
extern struct inline_struct { int x; int y; } compoundGlobal2;

/**
 * Non-extern non-static global variables
 *
 * These kinds of variables need to be treated with care, to avoid duplicate
 * symbols, but do exist in the wild. The `streamBinary` example is an extract
 * from a driver [1], which not only declares a non-extern non-static global,
 * but does not even use @pragma once@ or similar.
 *
 * We test with various kinds of initializers as we must explicitly ignore them
 * in our parser. The list here roughly follows the definition of `CXCursor`
 * [2], starting at `CXCursor_IntegerLiteral`; see also definition of 'varDecl'
 * in `HsBindgen.Frontend.Pass.Parse.Decl`.
 *
 * [1]: https://github.com/analogdevicesinc/no-OS/blob/main/drivers/rf-transceiver/talise/firmware/talise_stream_binary.h
 * [2]: https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013
 */

int     nesInteger     = 1;
float   nesFloating    = 1.2f;
// TODO: CXCursor_ImaginaryLiteral
char*   nesString1     = "hi";
char    nesString2[]   = "hi";
char    nesCharacter   = 'a';
int     nesParen       = (1);
int     nesUnary       = +5;
int     nesBinary      = 1 + 2;
int     nesConditional = 1 ? 2 : 3;
float   nesCast        = (float) 1;
int*    nesCompound    = (int []){2, 4, 6};
uint8_t nesInitList[]  = {0x00, 0x01, 0x02, 0x04};

// TODO: Enable this once we make it easier to change C standard in the tests
// bool    nesBool        = true; // since C23

/**
 * Constants
 *
 * TODO https://github.com/well-typed/hs-bindgen/issues/41
 */

// // Constant
// //
// // Although this is a constant, we don't expect an initializer (since it's
// // `extern`).
// extern const int globalConstant;

// // Constant, through typedef
// typedef const int ConstInt;
// extern ConstInt anotherGlobalConstant;

// // Constant, but local to the file
// //
// // Unlike with `extern`, in this we _do_ expect an initializer.
// static const int staticConst = 123;

// // No storage class specified
// //
// // Even though this is declared `const`, this is still an error (for the same
// // reason as `notActuallyGlobal`).
// const int classless;

/**
 * Error cases
 *
 * NOTE: We test the error for thread_local separately (as it requires C23).
 */

// Anonymous struct
//
// See 'UnexpectedAnonInExtern' for discussion.
extern struct { int x; int y; } unusableAnon;
