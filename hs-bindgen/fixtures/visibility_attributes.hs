[
  DeclInlineCInclude
    "visibility_attributes.h",
  DeclInlineC
    "void testmodule_f0 (void) { f0(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "f0",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "testmodule_f0",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid}},
  DeclInlineCInclude
    "visibility_attributes.h",
  DeclInlineC
    "void testmodule_f1 (void) { f1(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "f1",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "testmodule_f1",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid}},
  DeclInlineCInclude
    "visibility_attributes.h",
  DeclInlineC
    "__attribute__ ((const)) signed int *get_i0_ptr (void) { return &i0; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "i0",
      foreignImportType = HsPtr
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "get_i0_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed))},
  DeclInlineCInclude
    "visibility_attributes.h",
  DeclInlineC
    "__attribute__ ((const)) signed int *get_i1_ptr (void) { return &i1; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "i1",
      foreignImportType = HsPtr
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "get_i1_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed))}]
