[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Toggle",
      newtypeConstr = HsName
        "@NsConstr"
        "Toggle",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Toggle",
        fieldType = HsBlock
          (HsIO (HsPrimType HsPrimCBool)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "iterator.h:3:16",
          declId = NamePair {
            nameC = Name "Toggle",
            nameHsIdent = HsIdentifier
              "Toggle"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "iterator.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Toggle",
              newtypeField = HsName
                "@NsVar"
                "un_Toggle"},
            typedefType = TypeBlock
              (TypeFun
                []
                (TypePrim PrimBool))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "Toggle",
          commentLocation = Just
            "iterator.h:3:16",
          commentHeader = Just
            "iterator.h",
          commentChildren = []}},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "Toggle hs_bindgen_test_iterator_4f34fce61cc68c9f (_Bool arg1) { return makeToggle(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "makeToggle",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "start"),
          functionParameterType =
          HsPrimType HsPrimCBool,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "start",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Toggle"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_4f34fce61cc68c9f",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "start",
                  nameHsIdent = HsIdentifier
                    "start"})
              (TypePrim PrimBool)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "Toggle",
                nameHsIdent = HsIdentifier
                  "Toggle"})},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "makeToggle",
          commentLocation = Just
            "iterator.h:4:8",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "/* get_makeToggle_ptr */ __attribute__ ((const)) Toggle (*hs_bindgen_test_iterator_03950e0c09bdb508 (void)) (_Bool arg1) { return &makeToggle; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_03950e0c09bdb508",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCBool)
              (HsIO
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "Toggle")))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_03950e0c09bdb508",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeFun
          [TypePrim PrimBool]
          (TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "Toggle",
                nameHsIdent = HsIdentifier
                  "Toggle"}))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "makeToggle",
          commentLocation = Just
            "iterator.h:4:8",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "_Bool hs_bindgen_test_iterator_bfb4e32e3a824c7e (Toggle arg1) { return toggleNext(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "toggleNext",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "block"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Toggle"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "block",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCBool)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_bfb4e32e3a824c7e",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = HsIdentifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "Toggle",
                    nameHsIdent = HsIdentifier
                      "Toggle"}))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            PrimBool},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "toggleNext",
          commentLocation = Just
            "iterator.h:5:6",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "/* get_toggleNext_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_iterator_9c2755ef750f5d45 (void)) (Toggle arg1) { return &toggleNext; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_9c2755ef750f5d45",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Toggle"))
              (HsIO
                (HsPrimType HsPrimCBool))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_9c2755ef750f5d45",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "Toggle",
                  nameHsIdent = HsIdentifier
                    "Toggle"})]
          (TypePrim PrimBool)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "toggleNext",
          commentLocation = Just
            "iterator.h:5:6",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "void hs_bindgen_test_iterator_8d23fba933ba9584 (Toggle arg1) { releaseToggle(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "releaseToggle",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "block"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Toggle"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "block",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_8d23fba933ba9584",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = HsIdentifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "Toggle",
                    nameHsIdent = HsIdentifier
                      "Toggle"}))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "releaseToggle",
          commentLocation = Just
            "iterator.h:6:6",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "/* get_releaseToggle_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_iterator_2f7023ef559c7cdc (void)) (Toggle arg1) { return &releaseToggle; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_2f7023ef559c7cdc",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Toggle"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_2f7023ef559c7cdc",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "Toggle",
                  nameHsIdent = HsIdentifier
                    "Toggle"})]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "releaseToggle",
          commentLocation = Just
            "iterator.h:6:6",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Counter",
      newtypeConstr = HsName
        "@NsConstr"
        "Counter",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Counter",
        fieldType = HsBlock
          (HsIO (HsPrimType HsPrimCInt)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "iterator.h:10:14",
          declId = NamePair {
            nameC = Name "Counter",
            nameHsIdent = HsIdentifier
              "Counter"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "iterator.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Counter",
              newtypeField = HsName
                "@NsVar"
                "un_Counter"},
            typedefType = TypeBlock
              (TypeFun
                []
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "Counter",
          commentLocation = Just
            "iterator.h:10:14",
          commentHeader = Just
            "iterator.h",
          commentChildren = []}},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "Counter hs_bindgen_test_iterator_5b455070cb6127b9 (signed int arg1, signed int arg2) { return makeCounter(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "makeCounter",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "start"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "start",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "increment"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "increment",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Counter"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_5b455070cb6127b9",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "start",
                  nameHsIdent = HsIdentifier
                    "start"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "increment",
                  nameHsIdent = HsIdentifier
                    "increment"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "Counter",
                nameHsIdent = HsIdentifier
                  "Counter"})},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "makeCounter",
          commentLocation = Just
            "iterator.h:11:9",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "/* get_makeCounter_ptr */ __attribute__ ((const)) Counter (*hs_bindgen_test_iterator_216174b924f641ef (void)) (signed int arg1, signed int arg2) { return &makeCounter; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_216174b924f641ef",
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
                  (HsTypRef
                    (HsName
                      "@NsTypeConstr"
                      "Counter"))))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_216174b924f641ef",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "Counter",
                nameHsIdent = HsIdentifier
                  "Counter"}))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "makeCounter",
          commentLocation = Just
            "iterator.h:11:9",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "signed int hs_bindgen_test_iterator_1eb9473844c466c6 (Counter arg1) { return counterNext(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "counterNext",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "block"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Counter"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "block",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_1eb9473844c466c6",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = HsIdentifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "Counter",
                    nameHsIdent = HsIdentifier
                      "Counter"}))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "counterNext",
          commentLocation = Just
            "iterator.h:12:5",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "/* get_counterNext_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_iterator_9d967a23215cebaa (void)) (Counter arg1) { return &counterNext; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_9d967a23215cebaa",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Counter"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_9d967a23215cebaa",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "Counter",
                  nameHsIdent = HsIdentifier
                    "Counter"})]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "counterNext",
          commentLocation = Just
            "iterator.h:12:5",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "void hs_bindgen_test_iterator_4bd3562b992f2f1c (Counter arg1) { releaseCounter(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "releaseCounter",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "block"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Counter"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "block",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_4bd3562b992f2f1c",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = HsIdentifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "Counter",
                    nameHsIdent = HsIdentifier
                      "Counter"}))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "releaseCounter",
          commentLocation = Just
            "iterator.h:13:6",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "/* get_releaseCounter_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_iterator_315c7ff0ed90e2c8 (void)) (Counter arg1) { return &releaseCounter; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_315c7ff0ed90e2c8",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Counter"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_315c7ff0ed90e2c8",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "Counter",
                  nameHsIdent = HsIdentifier
                    "Counter"})]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "releaseCounter",
          commentLocation = Just
            "iterator.h:13:6",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "VarCounter",
      newtypeConstr = HsName
        "@NsConstr"
        "VarCounter",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_VarCounter",
        fieldType = HsBlock
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsIO (HsPrimType HsPrimCInt))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "iterator.h:17:14",
          declId = NamePair {
            nameC = Name "VarCounter",
            nameHsIdent = HsIdentifier
              "VarCounter"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "iterator.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "VarCounter",
              newtypeField = HsName
                "@NsVar"
                "un_VarCounter"},
            typedefType = TypeBlock
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "VarCounter",
          commentLocation = Just
            "iterator.h:17:14",
          commentHeader = Just
            "iterator.h",
          commentChildren = []}},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "VarCounter hs_bindgen_test_iterator_0fc005ef62990438 (signed int arg1) { return makeVarCounter(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "makeVarCounter",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "start"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "start",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "VarCounter"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_0fc005ef62990438",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "start",
                  nameHsIdent = HsIdentifier
                    "start"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "VarCounter",
                nameHsIdent = HsIdentifier
                  "VarCounter"})},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "makeVarCounter",
          commentLocation = Just
            "iterator.h:18:12",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "/* get_makeVarCounter_ptr */ __attribute__ ((const)) VarCounter (*hs_bindgen_test_iterator_a29c0a830311b22a (void)) (signed int arg1) { return &makeVarCounter; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_a29c0a830311b22a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "VarCounter")))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_a29c0a830311b22a",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "VarCounter",
                nameHsIdent = HsIdentifier
                  "VarCounter"}))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "makeVarCounter",
          commentLocation = Just
            "iterator.h:18:12",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "signed int hs_bindgen_test_iterator_a88cd5c9559b5d52 (VarCounter arg1, signed int arg2) { return varCounterNext(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "varCounterNext",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "block"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "VarCounter"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "block",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "increment"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just
                "increment",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_a88cd5c9559b5d52",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = HsIdentifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "VarCounter",
                    nameHsIdent = HsIdentifier
                      "VarCounter"})),
            _×_
              (Just
                NamePair {
                  nameC = Name "increment",
                  nameHsIdent = HsIdentifier
                    "increment"})
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
            "varCounterNext",
          commentLocation = Just
            "iterator.h:19:5",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "/* get_varCounterNext_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_iterator_cd9433fb0fa76d19 (void)) (VarCounter arg1, signed int arg2) { return &varCounterNext; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_cd9433fb0fa76d19",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "VarCounter"))
              (HsFun
                (HsPrimType HsPrimCInt)
                (HsIO
                  (HsPrimType HsPrimCInt)))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_cd9433fb0fa76d19",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "VarCounter",
                  nameHsIdent = HsIdentifier
                    "VarCounter"}),
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "varCounterNext",
          commentLocation = Just
            "iterator.h:19:5",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "void hs_bindgen_test_iterator_2d2d26e60eea04a8 (VarCounter arg1) { releaseVarCounter(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "releaseVarCounter",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "block"),
          functionParameterType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "VarCounter"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "block",
              commentLocation = Nothing,
              commentHeader = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_2d2d26e60eea04a8",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = HsIdentifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "VarCounter",
                    nameHsIdent = HsIdentifier
                      "VarCounter"}))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "releaseVarCounter",
          commentLocation = Just
            "iterator.h:20:6",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "/* get_releaseVarCounter_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_iterator_bad5305a8cb077b0 (void)) (VarCounter arg1) { return &releaseVarCounter; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_bad5305a8cb077b0",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "VarCounter"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_bad5305a8cb077b0",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "VarCounter",
                  nameHsIdent = HsIdentifier
                    "VarCounter"})]
          TypeVoid),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "releaseVarCounter",
          commentLocation = Just
            "iterator.h:20:6",
          commentHeader = Just
            "iterator.h",
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
