[
  DeclInlineCInclude
    "fun_attributes_conflict.h",
  DeclInlineC
    "signed int test_internal_square_cp (signed int arg1) { return square_cp(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "square_cp",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "test_internal_square_cp",
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
            (PrimIntegral PrimInt Signed)},
      foreignImportComment =
      Just
        Comment {
          commentTitle =
          Just
            [
              TextContent
                "Conflicting attributes on functions for llvm/clang versions 18 and up"],
          commentOrigin = Just
            "square_cp(int)",
          commentChildren =
          [
            Paragraph
              [
                TextContent
                  "Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html"]]}},
  DeclInlineCInclude
    "fun_attributes_conflict.h",
  DeclInlineC
    "signed int test_internal_square_pc (signed int arg1) { return square_pc(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "square_pc",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "test_internal_square_pc",
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
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Nothing},
  DeclInlineCInclude
    "fun_attributes_conflict.h",
  DeclInlineC
    "signed int test_internal_square_cc (signed int arg1) { return square_cc(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "square_cc",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "test_internal_square_cc",
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
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Nothing},
  DeclInlineCInclude
    "fun_attributes_conflict.h",
  DeclInlineC
    "signed int test_internal_square_pp (signed int arg1) { return square_pp(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "square_pp",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "test_internal_square_pp",
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
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentChildren = [
            Paragraph
              [
                TextContent "Marked",
                Monospace
                  [
                    Bold
                      [
                        TextContent
                          "attribute((pure))"]]]]}}]
