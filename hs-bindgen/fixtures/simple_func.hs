[
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "erf",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "arg"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "arg",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPrimType HsPrimCDouble)),
      foreignImportOrigName =
      "hs_bindgen_test_simple_func_3919a2f9a4498aaa",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "double hs_bindgen_test_simple_func_3919a2f9a4498aaa (double arg1) { return erf(arg1); }",
          capiWrapperImport =
          "simple_func.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "arg",
                  nameHsIdent = HsIdentifier
                    "arg"})
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
          commentOrigin = Just "erf",
          commentLocation = Just
            "simple_func.h:1:8",
          commentHeader = Just
            "simple_func.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_simple_func_e3d5d1926d499ff8",
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
      "hs_bindgen_test_simple_func_e3d5d1926d499ff8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_erf_ptr */ __attribute__ ((const)) double (*hs_bindgen_test_simple_func_e3d5d1926d499ff8 (void)) (double arg1) { return &erf; } ",
          capiWrapperImport =
          "simple_func.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimFloating PrimDouble)]
          (TypePrim
            (PrimFloating PrimDouble))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "erf",
          commentLocation = Just
            "simple_func.h:1:8",
          commentHeader = Just
            "simple_func.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "bad_fma",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "y"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "z"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "z",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPrimType HsPrimCDouble)),
      foreignImportOrigName =
      "hs_bindgen_test_simple_func_6be780963284c499",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "double hs_bindgen_test_simple_func_6be780963284c499 (double arg1, double arg2, double arg3) { return bad_fma(arg1, arg2, arg3); }",
          capiWrapperImport =
          "simple_func.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimFloating PrimDouble)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier "y"})
              (TypePrim
                (PrimFloating PrimDouble)),
            _×_
              (Just
                NamePair {
                  nameC = Name "z",
                  nameHsIdent = HsIdentifier "z"})
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
          commentOrigin = Just "bad_fma",
          commentLocation = Just
            "simple_func.h:3:22",
          commentHeader = Just
            "simple_func.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_simple_func_6e78b576543cf13e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCDouble)
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsFun
                  (HsPrimType HsPrimCDouble)
                  (HsIO
                    (HsPrimType
                      HsPrimCDouble))))))),
      foreignImportOrigName =
      "hs_bindgen_test_simple_func_6e78b576543cf13e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_bad_fma_ptr */ __attribute__ ((const)) double (*hs_bindgen_test_simple_func_6e78b576543cf13e (void)) (double arg1, double arg2, double arg3) { return &bad_fma; } ",
          capiWrapperImport =
          "simple_func.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimFloating PrimDouble),
            TypePrim
              (PrimFloating PrimDouble),
            TypePrim
              (PrimFloating PrimDouble)]
          (TypePrim
            (PrimFloating PrimDouble))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "bad_fma",
          commentLocation = Just
            "simple_func.h:3:22",
          commentHeader = Just
            "simple_func.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "no_args",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_simple_func_63e35f316cc0a04e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_simple_func_63e35f316cc0a04e (void) { no_args(); }",
          capiWrapperImport =
          "simple_func.h"},
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
          commentOrigin = Just "no_args",
          commentLocation = Just
            "simple_func.h:7:6",
          commentHeader = Just
            "simple_func.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_simple_func_a41af67e28348e9e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_simple_func_a41af67e28348e9e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_no_args_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_simple_func_a41af67e28348e9e (void)) (void) { return &no_args; } ",
          capiWrapperImport =
          "simple_func.h"},
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "no_args",
          commentLocation = Just
            "simple_func.h:7:6",
          commentHeader = Just
            "simple_func.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "no_args_no_void",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_simple_func_9d7e58d4e189732b",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_simple_func_9d7e58d4e189732b (void) { no_args_no_void(); }",
          capiWrapperImport =
          "simple_func.h"},
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
          commentOrigin = Just
            "no_args_no_void",
          commentLocation = Just
            "simple_func.h:9:6",
          commentHeader = Just
            "simple_func.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_simple_func_1f43e6c47e963043",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsIO
              (HsPrimType HsPrimUnit)))),
      foreignImportOrigName =
      "hs_bindgen_test_simple_func_1f43e6c47e963043",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_no_args_no_void_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_simple_func_1f43e6c47e963043 (void)) (void) { return &no_args_no_void; } ",
          capiWrapperImport =
          "simple_func.h"},
      foreignImportOrigin = Global
        (TypeFun [] TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "no_args_no_void",
          commentLocation = Just
            "simple_func.h:9:6",
          commentHeader = Just
            "simple_func.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCChar,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "y"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_simple_func_a2c97786cd1ecc82",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_simple_func_a2c97786cd1ecc82 (char arg1, double arg2) { return fun(arg1, arg2); }",
          capiWrapperImport =
          "simple_func.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed)))),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier "y"})
              (TypePrim
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fun",
          commentLocation = Just
            "simple_func.h:11:5",
          commentHeader = Just
            "simple_func.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_simple_func_1400673a07a5e708",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCChar)
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_simple_func_1400673a07a5e708",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_fun_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_simple_func_1400673a07a5e708 (void)) (char arg1, double arg2) { return &fun; } ",
          capiWrapperImport =
          "simple_func.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed))),
            TypePrim
              (PrimFloating PrimDouble)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fun",
          commentLocation = Just
            "simple_func.h:11:5",
          commentHeader = Just
            "simple_func.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
