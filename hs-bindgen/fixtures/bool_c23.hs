[
  DeclInlineCInclude "bool_c23.h",
  DeclInlineC
    "__attribute__ ((const)) _Bool *get_b_ptr (void) { return &b; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "b_ptr",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsPtr
          (HsPrimType HsPrimCBool)),
      foreignImportOrigName =
      "get_b_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim PrimBool),
      foreignImportComment = Nothing}]
