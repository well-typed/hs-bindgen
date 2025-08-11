[
  DeclInlineCInclude "varargs.h",
  DeclInlineC
    "void test_internal_h (void) { h(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "h",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "test_internal_h",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Nothing}]
