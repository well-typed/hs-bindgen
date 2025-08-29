[
  DeclInlineCInclude "asm.h",
  DeclInlineC
    "/* get_asm_labeled_variable_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_asm_d2d42e5b0c00988a (void) { return &asm_labeled_variable; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_asm_d2d42e5b0c00988a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_asm_d2d42e5b0c00988a",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "asm.h",
  DeclInlineC
    "signed int hs_bindgen_test_asm_54c5278e738a284f (signed int arg1, signed int arg2) { return asm_labeled_function(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "asm_labeled_function",
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
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "y"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_asm_54c5278e738a284f",
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
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier "y"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "asm_labeled_function",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude "asm.h",
  DeclInlineC
    "/* get_asm_labeled_function_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_asm_6a616a08348f146e (void)) (signed int arg1, signed int arg2) { return &asm_labeled_function; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_asm_6a616a08348f146e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_asm_6a616a08348f146e",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
