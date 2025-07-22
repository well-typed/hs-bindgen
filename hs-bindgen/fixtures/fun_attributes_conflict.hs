[
  DeclInlineCInclude
    "fun_attributes_conflict.h",
  DeclInlineC
    "signed int testmodule_square_cp (signed int arg1) { return square_cp(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "square_cp",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "testmodule_square_cp",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionAttrs =
          FunctionAttributes
            HaskellPureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}},
  DeclInlineCInclude
    "fun_attributes_conflict.h",
  DeclInlineC
    "signed int testmodule_square_pc (signed int arg1) { return square_pc(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "square_pc",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "testmodule_square_pc",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionAttrs =
          FunctionAttributes
            HaskellPureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}},
  DeclInlineCInclude
    "fun_attributes_conflict.h",
  DeclInlineC
    "signed int testmodule_square_cc (signed int arg1) { return square_cc(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "square_cc",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "testmodule_square_cc",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionAttrs =
          FunctionAttributes
            HaskellPureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}},
  DeclInlineCInclude
    "fun_attributes_conflict.h",
  DeclInlineC
    "signed int testmodule_square_pp (signed int arg1) { return square_pp(arg1); }",
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "square_pp",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "testmodule_square_pp",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionAttrs =
          FunctionAttributes
            CPureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}}]
