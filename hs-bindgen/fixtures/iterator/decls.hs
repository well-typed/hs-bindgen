[
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Toggle",
      newtypeConstr = Name
        "@NsConstr"
        "Toggle",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
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
              newtypeConstr = Name
                "@NsConstr"
                "Toggle",
              newtypeField = Name
                "@NsVar"
                "un_Toggle"},
            typedefType = TypeBlock
              (TypeFun
                []
                (TypePrim PrimBool))},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
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
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Toggle"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_Toggle",
          hasFieldInstanceFieldType =
          HsBlock
            (HsIO (HsPrimType HsPrimCBool)),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Toggle"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_Toggle",
          hasCFieldInstanceCFieldType =
          HsBlock
            (HsIO (HsPrimType HsPrimCBool)),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "Counter",
      newtypeConstr = Name
        "@NsConstr"
        "Counter",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
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
              newtypeConstr = Name
                "@NsConstr"
                "Counter",
              newtypeField = Name
                "@NsVar"
                "un_Counter"},
            typedefType = TypeBlock
              (TypeFun
                []
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))},
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
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
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Counter"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_Counter",
          hasFieldInstanceFieldType =
          HsBlock
            (HsIO (HsPrimType HsPrimCInt)),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "Counter"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_Counter",
          hasCFieldInstanceCFieldType =
          HsBlock
            (HsIO (HsPrimType HsPrimCInt)),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = Name
        "@NsTypeConstr"
        "VarCounter",
      newtypeConstr = Name
        "@NsConstr"
        "VarCounter",
      newtypeField = Field {
        fieldName = Name
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
            nameHsIdent = Identifier
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
              newtypeConstr = Name
                "@NsConstr"
                "VarCounter",
              newtypeField = Name
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
        declSpec = DeclSpec {
          declSpecC = Nothing,
          declSpecHs = Nothing}},
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
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "VarCounter"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "un_VarCounter",
          hasFieldInstanceFieldType =
          HsBlock
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name
              "@NsTypeConstr"
              "VarCounter"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "un_VarCounter",
          hasCFieldInstanceCFieldType =
          HsBlock
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))),
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "makeToggle",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "start"),
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
            (Name
              "@NsTypeConstr"
              "Toggle"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_900530c0457bf5ee",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "Toggle hs_bindgen_test_iterator_900530c0457bf5ee (\n",
              "  _Bool arg1\n",
              ")\n",
              "{\n",
              "  return makeToggle(arg1);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "start",
                  nameHsIdent = Identifier
                    "start"})
              (TypePrim PrimBool)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "Toggle",
                nameHsIdent = Identifier
                  "Toggle"}
              (TypeBlock
                (TypeFun
                  []
                  (TypePrim PrimBool))))},
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
      foreignImportName = Name
        "@NsVar"
        "toggleNext",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "block"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Toggle"),
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
          capiWrapperDefinition = concat
            [
              "_Bool hs_bindgen_test_iterator_0584fdb88b39872d (\n",
              "  Toggle arg1\n",
              ")\n",
              "{\n",
              "  return toggleNext(arg1);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = Identifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "Toggle",
                    nameHsIdent = Identifier
                      "Toggle"}
                  (TypeBlock
                    (TypeFun
                      []
                      (TypePrim PrimBool)))))],
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
      foreignImportName = Name
        "@NsVar"
        "releaseToggle",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "block"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Toggle"),
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
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_iterator_6d013ab8b38fc1d9 (\n",
              "  Toggle arg1\n",
              ")\n",
              "{\n",
              "  releaseToggle(arg1);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = Identifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "Toggle",
                    nameHsIdent = Identifier
                      "Toggle"}
                  (TypeBlock
                    (TypeFun
                      []
                      (TypePrim PrimBool)))))],
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
      foreignImportName = Name
        "@NsVar"
        "makeCounter",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "start"),
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
            (Name "@NsVar" "increment"),
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
            (Name
              "@NsTypeConstr"
              "Counter"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_10d749b6b17037ba",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "Counter hs_bindgen_test_iterator_10d749b6b17037ba (\n",
              "  signed int arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return makeCounter(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "start",
                  nameHsIdent = Identifier
                    "start"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "increment",
                  nameHsIdent = Identifier
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
                nameHsIdent = Identifier
                  "Counter"}
              (TypeBlock
                (TypeFun
                  []
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))))},
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
      foreignImportName = Name
        "@NsVar"
        "counterNext",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "block"),
          functionParameterType = HsTypRef
            (Name
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
          capiWrapperDefinition = concat
            [
              "signed int hs_bindgen_test_iterator_9695aacc59a66573 (\n",
              "  Counter arg1\n",
              ")\n",
              "{\n",
              "  return counterNext(arg1);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = Identifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "Counter",
                    nameHsIdent = Identifier
                      "Counter"}
                  (TypeBlock
                    (TypeFun
                      []
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
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
      foreignImportName = Name
        "@NsVar"
        "releaseCounter",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "block"),
          functionParameterType = HsTypRef
            (Name
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
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_iterator_49a436aa15c7ea70 (\n",
              "  Counter arg1\n",
              ")\n",
              "{\n",
              "  releaseCounter(arg1);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = Identifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "Counter",
                    nameHsIdent = Identifier
                      "Counter"}
                  (TypeBlock
                    (TypeFun
                      []
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
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
      foreignImportName = Name
        "@NsVar"
        "makeVarCounter",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "start"),
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
            (Name
              "@NsTypeConstr"
              "VarCounter"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_f12a50c4468834b5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "VarCounter hs_bindgen_test_iterator_f12a50c4468834b5 (\n",
              "  signed int arg1\n",
              ")\n",
              "{\n",
              "  return makeVarCounter(arg1);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "start",
                  nameHsIdent = Identifier
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
                nameHsIdent = Identifier
                  "VarCounter"}
              (TypeBlock
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))))},
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
      foreignImportName = Name
        "@NsVar"
        "varCounterNext",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "block"),
          functionParameterType = HsTypRef
            (Name
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
            (Name "@NsVar" "increment"),
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
          capiWrapperDefinition = concat
            [
              "signed int hs_bindgen_test_iterator_2c8a2667de9b1ffb (\n",
              "  VarCounter arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return varCounterNext(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = Identifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "VarCounter",
                    nameHsIdent = Identifier
                      "VarCounter"}
                  (TypeBlock
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "increment",
                  nameHsIdent = Identifier
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
      foreignImportName = Name
        "@NsVar"
        "releaseVarCounter",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "block"),
          functionParameterType = HsTypRef
            (Name
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
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_iterator_023ae1f9abf46556 (\n",
              "  VarCounter arg1\n",
              ")\n",
              "{\n",
              "  releaseVarCounter(arg1);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = Identifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "VarCounter",
                    nameHsIdent = Identifier
                      "VarCounter"}
                  (TypeBlock
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
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
      foreignImportName = Name
        "@NsVar"
        "makeToggle",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "start"),
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
            (Name
              "@NsTypeConstr"
              "Toggle"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_bd3974ddabfde9b8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "Toggle hs_bindgen_test_iterator_bd3974ddabfde9b8 (\n",
              "  _Bool arg1\n",
              ")\n",
              "{\n",
              "  return makeToggle(arg1);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "start",
                  nameHsIdent = Identifier
                    "start"})
              (TypePrim PrimBool)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "Toggle",
                nameHsIdent = Identifier
                  "Toggle"}
              (TypeBlock
                (TypeFun
                  []
                  (TypePrim PrimBool))))},
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
      foreignImportName = Name
        "@NsVar"
        "toggleNext",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "block"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Toggle"),
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
          capiWrapperDefinition = concat
            [
              "_Bool hs_bindgen_test_iterator_055df53eaf199b0e (\n",
              "  Toggle arg1\n",
              ")\n",
              "{\n",
              "  return toggleNext(arg1);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = Identifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "Toggle",
                    nameHsIdent = Identifier
                      "Toggle"}
                  (TypeBlock
                    (TypeFun
                      []
                      (TypePrim PrimBool)))))],
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
      foreignImportName = Name
        "@NsVar"
        "releaseToggle",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "block"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Toggle"),
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
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_iterator_07fb95f9614be94f (\n",
              "  Toggle arg1\n",
              ")\n",
              "{\n",
              "  releaseToggle(arg1);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = Identifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "Toggle",
                    nameHsIdent = Identifier
                      "Toggle"}
                  (TypeBlock
                    (TypeFun
                      []
                      (TypePrim PrimBool)))))],
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
      foreignImportName = Name
        "@NsVar"
        "makeCounter",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "start"),
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
            (Name "@NsVar" "increment"),
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
            (Name
              "@NsTypeConstr"
              "Counter"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_af5aabad780cc152",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "Counter hs_bindgen_test_iterator_af5aabad780cc152 (\n",
              "  signed int arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return makeCounter(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "start",
                  nameHsIdent = Identifier
                    "start"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "increment",
                  nameHsIdent = Identifier
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
                nameHsIdent = Identifier
                  "Counter"}
              (TypeBlock
                (TypeFun
                  []
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))))},
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
      foreignImportName = Name
        "@NsVar"
        "counterNext",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "block"),
          functionParameterType = HsTypRef
            (Name
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
          capiWrapperDefinition = concat
            [
              "signed int hs_bindgen_test_iterator_009eccead86c2acf (\n",
              "  Counter arg1\n",
              ")\n",
              "{\n",
              "  return counterNext(arg1);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = Identifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "Counter",
                    nameHsIdent = Identifier
                      "Counter"}
                  (TypeBlock
                    (TypeFun
                      []
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
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
      foreignImportName = Name
        "@NsVar"
        "releaseCounter",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "block"),
          functionParameterType = HsTypRef
            (Name
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
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_iterator_74f266597157c353 (\n",
              "  Counter arg1\n",
              ")\n",
              "{\n",
              "  releaseCounter(arg1);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = Identifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "Counter",
                    nameHsIdent = Identifier
                      "Counter"}
                  (TypeBlock
                    (TypeFun
                      []
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
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
      foreignImportName = Name
        "@NsVar"
        "makeVarCounter",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "start"),
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
            (Name
              "@NsTypeConstr"
              "VarCounter"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_61a09dd9011981f5",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "VarCounter hs_bindgen_test_iterator_61a09dd9011981f5 (\n",
              "  signed int arg1\n",
              ")\n",
              "{\n",
              "  return makeVarCounter(arg1);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "start",
                  nameHsIdent = Identifier
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
                nameHsIdent = Identifier
                  "VarCounter"}
              (TypeBlock
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed)))))},
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
      foreignImportName = Name
        "@NsVar"
        "varCounterNext",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "block"),
          functionParameterType = HsTypRef
            (Name
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
            (Name "@NsVar" "increment"),
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
          capiWrapperDefinition = concat
            [
              "signed int hs_bindgen_test_iterator_4d944ebb7dc53d23 (\n",
              "  VarCounter arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return varCounterNext(arg1, arg2);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = Identifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "VarCounter",
                    nameHsIdent = Identifier
                      "VarCounter"}
                  (TypeBlock
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed)))))),
            _×_
              (Just
                NamePair {
                  nameC = Name "increment",
                  nameHsIdent = Identifier
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
      foreignImportName = Name
        "@NsVar"
        "releaseVarCounter",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "block"),
          functionParameterType = HsTypRef
            (Name
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
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_iterator_502ca8978fd91294 (\n",
              "  VarCounter arg1\n",
              ")\n",
              "{\n",
              "  releaseVarCounter(arg1);\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "block",
                  nameHsIdent = Identifier
                    "block"})
              (TypeTypedef
                (TypedefRegular
                  NamePair {
                    nameC = Name "VarCounter",
                    nameHsIdent = Identifier
                      "VarCounter"}
                  (TypeBlock
                    (TypeFun
                      [
                        TypePrim
                          (PrimIntegral PrimInt Signed)]
                      (TypePrim
                        (PrimIntegral
                          PrimInt
                          Signed))))))],
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
      foreignImportName = Name
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
                  (Name
                    "@NsTypeConstr"
                    "Toggle")))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_504a6a44ef649697",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_makeToggle_ptr */\n",
              "__attribute__ ((const))\n",
              "Toggle (*hs_bindgen_test_iterator_504a6a44ef649697 (void)) (\n",
              "  _Bool arg1\n",
              ")\n",
              "{\n",
              "  return &makeToggle;\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Global
        (TypeFun
          [TypePrim PrimBool]
          (TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "Toggle",
                nameHsIdent = Identifier
                  "Toggle"}
              (TypeBlock
                (TypeFun
                  []
                  (TypePrim PrimBool)))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_iterator_ee784d0363e34151",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Toggle"))
              (HsIO
                (HsPrimType HsPrimCBool))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_ee784d0363e34151",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_toggleNext_ptr */\n",
              "__attribute__ ((const))\n",
              "_Bool (*hs_bindgen_test_iterator_ee784d0363e34151 (void)) (\n",
              "  Toggle arg1\n",
              ")\n",
              "{\n",
              "  return &toggleNext;\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "Toggle",
                  nameHsIdent = Identifier
                    "Toggle"}
                (TypeBlock
                  (TypeFun
                    []
                    (TypePrim PrimBool))))]
          (TypePrim PrimBool)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_iterator_864850832eaf96b9",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Toggle"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_864850832eaf96b9",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_releaseToggle_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_iterator_864850832eaf96b9 (void)) (\n",
              "  Toggle arg1\n",
              ")\n",
              "{\n",
              "  return &releaseToggle;\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "Toggle",
                  nameHsIdent = Identifier
                    "Toggle"}
                (TypeBlock
                  (TypeFun
                    []
                    (TypePrim PrimBool))))]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
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
                    (Name
                      "@NsTypeConstr"
                      "Counter"))))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_48b98d306e2a8d53",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_makeCounter_ptr */\n",
              "__attribute__ ((const))\n",
              "Counter (*hs_bindgen_test_iterator_48b98d306e2a8d53 (void)) (\n",
              "  signed int arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return &makeCounter;\n",
              "}"],
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
                nameHsIdent = Identifier
                  "Counter"}
              (TypeBlock
                (TypeFun
                  []
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_iterator_aeb21db5034e4d66",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
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
          concat
            [
              "/* get_counterNext_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_iterator_aeb21db5034e4d66 (void)) (\n",
              "  Counter arg1\n",
              ")\n",
              "{\n",
              "  return &counterNext;\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "Counter",
                  nameHsIdent = Identifier
                    "Counter"}
                (TypeBlock
                  (TypeFun
                    []
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_iterator_8e1661e238f6f451",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "Counter"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_8e1661e238f6f451",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_releaseCounter_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_iterator_8e1661e238f6f451 (void)) (\n",
              "  Counter arg1\n",
              ")\n",
              "{\n",
              "  return &releaseCounter;\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "Counter",
                  nameHsIdent = Identifier
                    "Counter"}
                (TypeBlock
                  (TypeFun
                    []
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
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
                  (Name
                    "@NsTypeConstr"
                    "VarCounter")))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_b14e88e9cf7a56b8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_makeVarCounter_ptr */\n",
              "__attribute__ ((const))\n",
              "VarCounter (*hs_bindgen_test_iterator_b14e88e9cf7a56b8 (void)) (\n",
              "  signed int arg1\n",
              ")\n",
              "{\n",
              "  return &makeVarCounter;\n",
              "}"],
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
                nameHsIdent = Identifier
                  "VarCounter"}
              (TypeBlock
                (TypeFun
                  [
                    TypePrim
                      (PrimIntegral PrimInt Signed)]
                  (TypePrim
                    (PrimIntegral
                      PrimInt
                      Signed))))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_iterator_4d10204c4166188d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
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
          concat
            [
              "/* get_varCounterNext_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_iterator_4d10204c4166188d (void)) (\n",
              "  VarCounter arg1,\n",
              "  signed int arg2\n",
              ")\n",
              "{\n",
              "  return &varCounterNext;\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "VarCounter",
                  nameHsIdent = Identifier
                    "VarCounter"}
                (TypeBlock
                  (TypeFun
                    [
                      TypePrim
                        (PrimIntegral PrimInt Signed)]
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed))))),
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_iterator_bde04ef01335be42",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name
                  "@NsTypeConstr"
                  "VarCounter"))
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_bde04ef01335be42",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_releaseVarCounter_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_iterator_bde04ef01335be42 (void)) (\n",
              "  VarCounter arg1\n",
              ")\n",
              "{\n",
              "  return &releaseVarCounter;\n",
              "}"],
          capiWrapperImport =
          "iterator.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "VarCounter",
                  nameHsIdent = Identifier
                    "VarCounter"}
                (TypeBlock
                  (TypeFun
                    [
                      TypePrim
                        (PrimIntegral PrimInt Signed)]
                    (TypePrim
                      (PrimIntegral
                        PrimInt
                        Signed)))))]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
