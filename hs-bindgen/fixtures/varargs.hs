[
  DeclInlineCInclude "varargs.h",
  DeclInlineC
    "void testmodule_g (void) { g(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "g",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "testmodule_g",
      foreignImportHeader =
      "varargs.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypeVoid}}]
