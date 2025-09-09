[
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_bool_c23_401ecb7e80957164",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCBool))),
      foreignImportOrigName =
      "hs_bindgen_test_bool_c23_401ecb7e80957164",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_b_ptr */ __attribute__ ((const)) _Bool *hs_bindgen_test_bool_c23_401ecb7e80957164 (void) { return &b; } ",
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
          commentHeader = Just
            "bool_c23.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
