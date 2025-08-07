[
  DeclInlineCInclude "varargs.h",
  DeclInlineC
    "void testmodule_h (void) { h(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "h",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "testmodule_h",
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
