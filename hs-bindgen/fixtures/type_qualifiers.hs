[
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_type_qualifiers_be05c6870fad4f33",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_type_qualifiers_be05c6870fad4f33",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_a_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_type_qualifiers_be05c6870fad4f33 (void) { return &a; } ",
          capiWrapperImport =
          "type_qualifiers.h"},
      foreignImportOrigin = Global
        (TypeConst
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "a",
          commentLocation = Just
            "type_qualifiers.h:5:18",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["type_qualifiers.h"],
              headerInclude =
              "type_qualifiers.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_type_qualifiers_401ecb7e80957164",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_type_qualifiers_401ecb7e80957164",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_b_ptr */ __attribute__ ((const)) signed int const **hs_bindgen_test_type_qualifiers_401ecb7e80957164 (void) { return &b; } ",
          capiWrapperImport =
          "type_qualifiers.h"},
      foreignImportOrigin = Global
        (TypePointer
          (TypeConst
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "b",
          commentLocation = Just
            "type_qualifiers.h:7:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["type_qualifiers.h"],
              headerInclude =
              "type_qualifiers.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_type_qualifiers_0b370289c6c19db4",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_type_qualifiers_0b370289c6c19db4",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_c_ptr */ __attribute__ ((const)) signed int *const *hs_bindgen_test_type_qualifiers_0b370289c6c19db4 (void) { return &c; } ",
          capiWrapperImport =
          "type_qualifiers.h"},
      foreignImportOrigin = Global
        (TypeConst
          (TypePointer
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "c",
          commentLocation = Just
            "type_qualifiers.h:9:19",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["type_qualifiers.h"],
              headerInclude =
              "type_qualifiers.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_type_qualifiers_cc41ed0d2b848565",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_type_qualifiers_cc41ed0d2b848565",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_d_ptr */ __attribute__ ((const)) signed int const *const *hs_bindgen_test_type_qualifiers_cc41ed0d2b848565 (void) { return &d; } ",
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
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "d",
          commentLocation = Just
            "type_qualifiers.h:11:25",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["type_qualifiers.h"],
              headerInclude =
              "type_qualifiers.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "list_example",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "items"),
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
            (HsName "@NsVar" "count"),
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
      "hs_bindgen_test_type_qualifiers_9d6d039971edcd60",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "_Bool hs_bindgen_test_type_qualifiers_9d6d039971edcd60 (char const **arg1, size_t arg2) { return list_example(arg1, arg2); }",
          capiWrapperImport =
          "type_qualifiers.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "items",
                  nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
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
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_type_qualifiers_c40a51053a97fb29",
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
      "hs_bindgen_test_type_qualifiers_c40a51053a97fb29",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_list_example_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_type_qualifiers_c40a51053a97fb29 (void)) (char const **arg1, size_t arg2) { return &list_example; } ",
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
  DeclSimple,
  DeclSimple]
