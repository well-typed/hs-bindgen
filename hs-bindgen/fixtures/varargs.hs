[
  DeclInlineCInclude "varargs.h",
  DeclInlineC
    "void hs_bindgen_test_varargs_0fd77c5efa209398 (void) { h(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "h",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_varargs_0fd77c5efa209398",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "h",
          commentLocation = Just
            "varargs.h:8:6",
          commentHeader = Just
            "varargs.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude "varargs.h",
  DeclInlineC
    "/* get_h_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_varargs_0a93e926c5626347 (void)) (void) { return &h; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_varargs_0a93e926c5626347",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_varargs_0a93e926c5626347",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "h",
          commentLocation = Just
            "varargs.h:8:6",
          commentHeader = Just
            "varargs.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
