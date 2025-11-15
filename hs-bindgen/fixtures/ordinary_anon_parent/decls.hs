[
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "_acos",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPrimType HsPrimCDouble)),
      foreignImportOrigName =
      "hs_bindgen_test_ordinary_anon_parent_49d2dc050c5f231a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "double hs_bindgen_test_ordinary_anon_parent_49d2dc050c5f231a (\n",
              "  double arg1\n",
              ")\n",
              "{\n",
              "  return _acos(arg1);\n",
              "}"],
          capiWrapperImport =
          "ordinary_anon_parent.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimFloating PrimDouble)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "_acos",
          commentLocation = Just
            "ordinary_anon_child.h:4:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["ordinary_anon_parent.h"],
              headerInclude =
              "ordinary_anon_child.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "_acos",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPrimType HsPrimCDouble)),
      foreignImportOrigName =
      "hs_bindgen_test_ordinary_anon_parent_faad0266dab4d429",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "double hs_bindgen_test_ordinary_anon_parent_faad0266dab4d429 (\n",
              "  double arg1\n",
              ")\n",
              "{\n",
              "  return _acos(arg1);\n",
              "}"],
          capiWrapperImport =
          "ordinary_anon_parent.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimFloating PrimDouble)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "_acos",
          commentLocation = Just
            "ordinary_anon_child.h:4:1",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["ordinary_anon_parent.h"],
              headerInclude =
              "ordinary_anon_child.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_ordinary_anon_parent_c6a8256d628dc56b",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCDouble)
              (HsIO
                (HsPrimType HsPrimCDouble))))),
      foreignImportOrigName =
      "hs_bindgen_test_ordinary_anon_parent_c6a8256d628dc56b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get__acos_ptr */\n",
              "__attribute__ ((const))\n",
              "double (*hs_bindgen_test_ordinary_anon_parent_c6a8256d628dc56b (void)) (\n",
              "  double arg1\n",
              ")\n",
              "{\n",
              "  return &_acos;\n",
              "}"],
          capiWrapperImport =
          "ordinary_anon_parent.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimFloating PrimDouble)]
          (TypePrim
            (PrimFloating PrimDouble))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
