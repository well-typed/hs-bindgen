[
  DeclInlineCInclude
    "simple_func.h",
  DeclInlineC
    "double hs_bindgen_test_simple_func_3919a2f9a4498aaa (double arg1) { return erf(arg1); }",
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
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPrimType HsPrimCDouble)),
      foreignImportOrigName =
      "hs_bindgen_test_simple_func_3919a2f9a4498aaa",
      foreignImportCallConv =
      CallConvUserlandCAPI,
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
          commentChildren = []}},
  DeclInlineCInclude
    "simple_func.h",
  DeclInlineC
    "double hs_bindgen_test_simple_func_6be780963284c499 (double arg1, double arg2, double arg3) { return bad_fma(arg1, arg2, arg3); }",
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
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPrimType HsPrimCDouble)),
      foreignImportOrigName =
      "hs_bindgen_test_simple_func_6be780963284c499",
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
          commentChildren = []}},
  DeclInlineCInclude
    "simple_func.h",
  DeclInlineC
    "void hs_bindgen_test_simple_func_63e35f316cc0a04e (void) { no_args(); }",
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
      CallConvUserlandCAPI,
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
          commentChildren = []}},
  DeclInlineCInclude
    "simple_func.h",
  DeclInlineC
    "void hs_bindgen_test_simple_func_9d7e58d4e189732b (void) { no_args_no_void(); }",
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
      CallConvUserlandCAPI,
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
          commentChildren = []}},
  DeclInlineCInclude
    "simple_func.h",
  DeclInlineC
    "signed int hs_bindgen_test_simple_func_a2c97786cd1ecc82 (char arg1, double arg2) { return fun(arg1, arg2); }",
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
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_simple_func_a2c97786cd1ecc82",
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
          commentChildren = []}}]
