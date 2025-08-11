[
  DeclInlineCInclude
    "fun_attributes_conflict.h",
  DeclInlineC
    "signed int hs_bindgen_test_fun_attributes_conflict_10e6b4d386eec8f7 (signed int arg1) { return square_cp(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "square_cp",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_conflict_10e6b4d386eec8f7",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
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
    "signed int hs_bindgen_test_fun_attributes_conflict_d8e5dd6836af0ac7 (signed int arg1) { return square_pc(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "square_pc",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_conflict_d8e5dd6836af0ac7",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            HaskellPureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Nothing},
  DeclInlineCInclude
    "fun_attributes_conflict.h",
  DeclInlineC
    "signed int hs_bindgen_test_fun_attributes_conflict_9a60da065e6486ac (signed int arg1) { return square_cc(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "square_cc",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_conflict_9a60da065e6486ac",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            HaskellPureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Nothing},
  DeclInlineCInclude
    "fun_attributes_conflict.h",
  DeclInlineC
    "signed int hs_bindgen_test_fun_attributes_conflict_1a2340fb8456aee3 (signed int arg1) { return square_pp(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "square_pp",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_fun_attributes_conflict_1a2340fb8456aee3",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
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
