[
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "list_example",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "items"),
          functionParameterType = HsPtr
            (HsPtr
              (HsPrimType HsPrimCChar)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "items",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "count"),
          functionParameterType =
          HsPrimType HsPrimCSize,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "count",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCBool)),
      foreignImportOrigName =
      "hs_bindgen_test_type_qualifiers_b42fb41209c21d6e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "_Bool hs_bindgen_test_type_qualifiers_b42fb41209c21d6e (char const **arg1, size_t arg2) { return list_example(arg1, arg2); }",
          capiWrapperImport =
          "type_qualifiers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "items",
                  nameHsIdent = Identifier
                    "items"})
              (TypePointer
                (TypePointer
                  (TypeConst
                    (TypePrim
                      (PrimChar
                        (PrimSignImplicit Nothing)))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "count",
                  nameHsIdent = Identifier
                    "count"})
              (TypePrim PrimSize)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            PrimBool},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "list_example",
          commentLocation = Just
            "type_qualifiers.h:14:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["type_qualifiers.h"],
              headerInclude =
              "type_qualifiers.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "list_example",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "items"),
          functionParameterType = HsPtr
            (HsPtr
              (HsPrimType HsPrimCChar)),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "items",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "count"),
          functionParameterType =
          HsPrimType HsPrimCSize,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "count",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCBool)),
      foreignImportOrigName =
      "hs_bindgen_test_type_qualifiers_41af05ef1797fa6d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "_Bool hs_bindgen_test_type_qualifiers_41af05ef1797fa6d (char const **arg1, size_t arg2) { return list_example(arg1, arg2); }",
          capiWrapperImport =
          "type_qualifiers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "items",
                  nameHsIdent = Identifier
                    "items"})
              (TypePointer
                (TypePointer
                  (TypeConst
                    (TypePrim
                      (PrimChar
                        (PrimSignImplicit Nothing)))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "count",
                  nameHsIdent = Identifier
                    "count"})
              (TypePrim PrimSize)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            PrimBool},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "list_example",
          commentLocation = Just
            "type_qualifiers.h:14:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["type_qualifiers.h"],
              headerInclude =
              "type_qualifiers.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_type_qualifiers_24b25f22222ce366",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPtr
                (HsPtr
                  (HsPrimType HsPrimCChar)))
              (HsFun
                (HsPrimType HsPrimCSize)
                (HsIO
                  (HsPrimType HsPrimCBool)))))),
      foreignImportOrigName =
      "hs_bindgen_test_type_qualifiers_24b25f22222ce366",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_list_example_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_type_qualifiers_24b25f22222ce366 (void)) (char const **arg1, size_t arg2) { return &list_example; } ",
          capiWrapperImport =
          "type_qualifiers.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePointer
              (TypePointer
                (TypeConst
                  (TypePrim
                    (PrimChar
                      (PrimSignImplicit Nothing))))),
            TypePrim PrimSize]
          (TypePrim PrimBool)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_type_qualifiers_3afcbd8536cf21bd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_type_qualifiers_3afcbd8536cf21bd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_a_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_type_qualifiers_3afcbd8536cf21bd (void) { return &a; } ",
          capiWrapperImport =
          "type_qualifiers.h"},
      foreignImportOrigin = Global
        (TypeConst
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_type_qualifiers_fcd0c984d664f6ee",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_type_qualifiers_fcd0c984d664f6ee",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_b_ptr */ __attribute__ ((const)) signed int const **hs_bindgen_test_type_qualifiers_fcd0c984d664f6ee (void) { return &b; } ",
          capiWrapperImport =
          "type_qualifiers.h"},
      foreignImportOrigin = Global
        (TypePointer
          (TypeConst
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_type_qualifiers_d61ea07e27589aef",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_type_qualifiers_d61ea07e27589aef",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_c_ptr */ __attribute__ ((const)) signed int *const *hs_bindgen_test_type_qualifiers_d61ea07e27589aef (void) { return &c; } ",
          capiWrapperImport =
          "type_qualifiers.h"},
      foreignImportOrigin = Global
        (TypeConst
          (TypePointer
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_type_qualifiers_d1d6489b06a70107",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_type_qualifiers_d1d6489b06a70107",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_d_ptr */ __attribute__ ((const)) signed int const *const *hs_bindgen_test_type_qualifiers_d1d6489b06a70107 (void) { return &d; } ",
          capiWrapperImport =
          "type_qualifiers.h"},
      foreignImportOrigin = Global
        (TypeConst
          (TypePointer
            (TypeConst
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple]
