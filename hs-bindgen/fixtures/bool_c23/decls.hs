[
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
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
          capiWrapperDefinition = concat
            [
              "/* get_b_ptr */\n",
              "__attribute__ ((const))\n",
              "_Bool *hs_bindgen_test_bool_c23_fcd0c984d664f6ee (void)\n",
              "{\n",
              "  return &b;\n",
              "}"],
          capiWrapperImport =
          "bool_c23.h"},
      foreignImportOrigin = Global
        (TypePrim PrimBool),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
