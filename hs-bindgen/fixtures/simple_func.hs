[
  DeclInlineCInclude
    "simple_func.h",
  DeclInlineC
    "double testmodule_erf (double arg1) { return erf(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "erf",
      foreignImportType = HsFun
        (HsPrimType HsPrimCDouble)
        (HsIO
          (HsPrimType HsPrimCDouble)),
      foreignImportOrigName =
      "testmodule_erf",
      foreignImportHeader =
      "simple_func.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimFloating PrimDouble)],
          functionRes = TypePrim
            (PrimFloating PrimDouble),
          functionHeader =
          "simple_func.h"}},
  DeclInlineCInclude
    "simple_func.h",
  DeclInlineC
    "double testmodule_bad_fma (double arg1, double arg2, double arg3) { return bad_fma(arg1, arg2, arg3); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "bad_fma",
      foreignImportType = HsFun
        (HsPrimType HsPrimCDouble)
        (HsFun
          (HsPrimType HsPrimCDouble)
          (HsFun
            (HsPrimType HsPrimCDouble)
            (HsIO
              (HsPrimType HsPrimCDouble)))),
      foreignImportOrigName =
      "testmodule_bad_fma",
      foreignImportHeader =
      "simple_func.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimFloating PrimDouble),
            TypePrim
              (PrimFloating PrimDouble),
            TypePrim
              (PrimFloating PrimDouble)],
          functionRes = TypePrim
            (PrimFloating PrimDouble),
          functionHeader =
          "simple_func.h"}},
  DeclInlineCInclude
    "simple_func.h",
  DeclInlineC
    "void testmodule_no_args (void) { no_args(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "no_args",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "testmodule_no_args",
      foreignImportHeader =
      "simple_func.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypeVoid,
          functionHeader =
          "simple_func.h"}},
  DeclInlineCInclude
    "simple_func.h",
  DeclInlineC
    "void testmodule_no_args_no_void (void) { no_args_no_void(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "no_args_no_void",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "testmodule_no_args_no_void",
      foreignImportHeader =
      "simple_func.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypeVoid,
          functionHeader =
          "simple_func.h"}},
  DeclInlineCInclude
    "simple_func.h",
  DeclInlineC
    "signed int testmodule_fun (char arg1, double arg2) { return fun(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun",
      foreignImportType = HsFun
        (HsPrimType HsPrimCChar)
        (HsFun
          (HsPrimType HsPrimCDouble)
          (HsIO (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "testmodule_fun",
      foreignImportHeader =
      "simple_func.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed))),
            TypePrim
              (PrimFloating PrimDouble)],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed),
          functionHeader =
          "simple_func.h"}}]
