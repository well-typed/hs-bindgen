[
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_bool_c23_fcd0c984d664f6ee",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCBool))),
      foreignImportOrigName =
      "hs_bindgen_test_bool_c23_fcd0c984d664f6ee",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_b_ptr */ __attribute__ ((const)) _Bool *hs_bindgen_test_bool_c23_fcd0c984d664f6ee (void) { return &b; } ",
          capiWrapperImport =
          "bool_c23.h"},
      foreignImportOrigin = Global
        (TypePrim PrimBool),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "b",
          commentLocation = Just
            "bool_c23.h:3:13",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["bool_c23.h"],
              headerInclude = "bool_c23.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
