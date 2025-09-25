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
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []}},
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
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []}},
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
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []}},
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Toggle"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_900530c0457bf5ee",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "Toggle hs_bindgen_test_iterator_900530c0457bf5ee (_Bool arg1) { return makeToggle(arg1); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCBool)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_0584fdb88b39872d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "_Bool hs_bindgen_test_iterator_0584fdb88b39872d (Toggle arg1) { return toggleNext(arg1); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_6d013ab8b38fc1d9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_iterator_6d013ab8b38fc1d9 (Toggle arg1) { releaseToggle(arg1); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
              commentHeaderInfo = Nothing,
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Counter"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_10d749b6b17037ba",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "Counter hs_bindgen_test_iterator_10d749b6b17037ba (signed int arg1, signed int arg2) { return makeCounter(arg1, arg2); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_9695aacc59a66573",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_iterator_9695aacc59a66573 (Counter arg1) { return counterNext(arg1); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_49a436aa15c7ea70",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_iterator_49a436aa15c7ea70 (Counter arg1) { releaseCounter(arg1); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "VarCounter"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_f12a50c4468834b5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "VarCounter hs_bindgen_test_iterator_f12a50c4468834b5 (signed int arg1) { return makeVarCounter(arg1); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
              commentHeaderInfo = Nothing,
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_2c8a2667de9b1ffb",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_iterator_2c8a2667de9b1ffb (VarCounter arg1, signed int arg2) { return varCounterNext(arg1, arg2); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_023ae1f9abf46556",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_iterator_023ae1f9abf46556 (VarCounter arg1) { releaseVarCounter(arg1); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Toggle"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_bd3974ddabfde9b8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "Toggle hs_bindgen_test_iterator_bd3974ddabfde9b8 (_Bool arg1) { return makeToggle(arg1); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCBool)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_055df53eaf199b0e",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "_Bool hs_bindgen_test_iterator_055df53eaf199b0e (Toggle arg1) { return toggleNext(arg1); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_07fb95f9614be94f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_iterator_07fb95f9614be94f (Toggle arg1) { releaseToggle(arg1); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
              commentHeaderInfo = Nothing,
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Counter"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_af5aabad780cc152",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "Counter hs_bindgen_test_iterator_af5aabad780cc152 (signed int arg1, signed int arg2) { return makeCounter(arg1, arg2); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_009eccead86c2acf",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_iterator_009eccead86c2acf (Counter arg1) { return counterNext(arg1); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_74f266597157c353",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_iterator_74f266597157c353 (Counter arg1) { releaseCounter(arg1); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "VarCounter"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_61a09dd9011981f5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "VarCounter hs_bindgen_test_iterator_61a09dd9011981f5 (signed int arg1) { return makeVarCounter(arg1); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
              commentHeaderInfo = Nothing,
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_4d944ebb7dc53d23",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_iterator_4d944ebb7dc53d23 (VarCounter arg1, signed int arg2) { return varCounterNext(arg1, arg2); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_502ca8978fd91294",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_iterator_502ca8978fd91294 (VarCounter arg1) { releaseVarCounter(arg1); }",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_504a6a44ef649697",
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
      "hs_bindgen_test_iterator_504a6a44ef649697",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_makeToggle_ptr */ __attribute__ ((const)) Toggle (*hs_bindgen_test_iterator_504a6a44ef649697 (void)) (_Bool arg1) { return &makeToggle; } ",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_ee784d0363e34151",
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
      "hs_bindgen_test_iterator_ee784d0363e34151",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_toggleNext_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_iterator_ee784d0363e34151 (void)) (Toggle arg1) { return &toggleNext; } ",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_864850832eaf96b9",
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
      "hs_bindgen_test_iterator_864850832eaf96b9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_releaseToggle_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_iterator_864850832eaf96b9 (void)) (Toggle arg1) { return &releaseToggle; } ",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_48b98d306e2a8d53",
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
      "hs_bindgen_test_iterator_48b98d306e2a8d53",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_makeCounter_ptr */ __attribute__ ((const)) Counter (*hs_bindgen_test_iterator_48b98d306e2a8d53 (void)) (signed int arg1, signed int arg2) { return &makeCounter; } ",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_aeb21db5034e4d66",
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
      "hs_bindgen_test_iterator_aeb21db5034e4d66",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_counterNext_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_iterator_aeb21db5034e4d66 (void)) (Counter arg1) { return &counterNext; } ",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_8e1661e238f6f451",
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
      "hs_bindgen_test_iterator_8e1661e238f6f451",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_releaseCounter_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_iterator_8e1661e238f6f451 (void)) (Counter arg1) { return &releaseCounter; } ",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_b14e88e9cf7a56b8",
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
      "hs_bindgen_test_iterator_b14e88e9cf7a56b8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_makeVarCounter_ptr */ __attribute__ ((const)) VarCounter (*hs_bindgen_test_iterator_b14e88e9cf7a56b8 (void)) (signed int arg1) { return &makeVarCounter; } ",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_4d10204c4166188d",
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
      "hs_bindgen_test_iterator_4d10204c4166188d",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_varCounterNext_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_iterator_4d10204c4166188d (void)) (VarCounter arg1, signed int arg2) { return &varCounterNext; } ",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_iterator_bde04ef01335be42",
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
      "hs_bindgen_test_iterator_bde04ef01335be42",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_releaseVarCounter_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_iterator_bde04ef01335be42 (void)) (VarCounter arg1) { return &releaseVarCounter; } ",
          capiWrapperImport =
          "iterator.h"},
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
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["iterator.h"],
              headerInclude = "iterator.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
