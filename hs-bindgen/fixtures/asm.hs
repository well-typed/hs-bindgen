[
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
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_asm_labeled_variable_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_asm_d2d42e5b0c00988a (void) { return &asm_labeled_variable; } ",
          capiWrapperImport = "asm.h"},
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "asm_labeled_variable",
          commentLocation = Just
            "asm.h:2:12",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["asm.h"],
              headerInclude = "asm.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
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
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
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
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_asm_54c5278e738a284f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_asm_54c5278e738a284f (signed int arg1, signed int arg2) { return asm_labeled_function(arg1, arg2); }",
          capiWrapperImport = "asm.h"},
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
          commentLocation = Just
            "asm.h:4:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["asm.h"],
              headerInclude = "asm.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_asm_labeled_function_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_asm_6a616a08348f146e (void)) (signed int arg1, signed int arg2) { return &asm_labeled_function; } ",
          capiWrapperImport = "asm.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "asm_labeled_function",
          commentLocation = Just
            "asm.h:4:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["asm.h"],
              headerInclude = "asm.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
