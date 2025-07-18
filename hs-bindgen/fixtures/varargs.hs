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
