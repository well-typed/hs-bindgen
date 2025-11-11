[
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "h",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_varargs_a17c4f0272bbe42a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_varargs_a17c4f0272bbe42a (void)\n",
              "{\n",
              "  h();\n",
              "}"],
          capiWrapperImport =
          "varargs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "h",
          commentLocation = Just
            "varargs.h:8:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["varargs.h"],
              headerInclude = "varargs.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "h",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_varargs_ec9e8ecfd27bead3",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_varargs_ec9e8ecfd27bead3 (void)\n",
              "{\n",
              "  h();\n",
              "}"],
          capiWrapperImport =
          "varargs.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "h",
          commentLocation = Just
            "varargs.h:8:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["varargs.h"],
              headerInclude = "varargs.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_varargs_6344539fe0b25338",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_varargs_6344539fe0b25338",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_h_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_varargs_6344539fe0b25338 (void)) (void)\n",
              "{\n",
              "  return &h;\n",
              "}"],
          capiWrapperImport =
          "varargs.h"},
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
