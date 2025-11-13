[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Thing",
      structConstr = Name
        "@NsConstr"
        "Thing",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "thing_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "struct_arg.h:3:9",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "thing_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Just
                "struct_arg.h:3:9",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["struct_arg.h"],
                  headerInclude = "struct_arg.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "struct_arg.h:2:8",
            declId = NamePair {
              nameC = Name "thing",
              nameHsIdent = Identifier
                "Thing"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["struct_arg.h"],
                headerInclude = "struct_arg.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Thing"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "struct_arg.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "thing_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            CTypeSpec {
              cTypeSpecIdentifier = Nothing,
              cTypeSpecInstances =
              Map.fromList []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "thing",
          commentLocation = Just
            "struct_arg.h:2:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["struct_arg.h"],
              headerInclude = "struct_arg.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Thing",
          structConstr = Name
            "@NsConstr"
            "Thing",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "thing_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "struct_arg.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "thing_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "x",
                  commentLocation = Just
                    "struct_arg.h:3:9",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["struct_arg.h"],
                      headerInclude = "struct_arg.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "struct_arg.h:2:8",
                declId = NamePair {
                  nameC = Name "thing",
                  nameHsIdent = Identifier
                    "Thing"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["struct_arg.h"],
                    headerInclude = "struct_arg.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Thing"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "struct_arg.h:3:9",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "thing_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                CTypeSpec {
                  cTypeSpecIdentifier = Nothing,
                  cTypeSpecInstances =
                  Map.fromList []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "thing",
              commentLocation = Just
                "struct_arg.h:2:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["struct_arg.h"],
                  headerInclude = "struct_arg.h"},
              commentChildren = []}}
        StorableInstance {
          storableSizeOf = 4,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Thing",
                  structConstr = Name
                    "@NsConstr"
                    "Thing",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "thing_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "struct_arg.h:3:9",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "thing_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "struct_arg.h:3:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["struct_arg.h"],
                              headerInclude = "struct_arg.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "struct_arg.h:2:8",
                        declId = NamePair {
                          nameC = Name "thing",
                          nameHsIdent = Identifier
                            "Thing"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["struct_arg.h"],
                            headerInclude = "struct_arg.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Thing"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "struct_arg.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "thing_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "thing",
                      commentLocation = Just
                        "struct_arg.h:2:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["struct_arg.h"],
                          headerInclude = "struct_arg.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "thing_x")
                  (Idx 0)]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = Name
                    "@NsTypeConstr"
                    "Thing",
                  structConstr = Name
                    "@NsConstr"
                    "Thing",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "thing_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "struct_arg.h:3:9",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "thing_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "x",
                          commentLocation = Just
                            "struct_arg.h:3:9",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["struct_arg.h"],
                              headerInclude = "struct_arg.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "struct_arg.h:2:8",
                        declId = NamePair {
                          nameC = Name "thing",
                          nameHsIdent = Identifier
                            "Thing"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["struct_arg.h"],
                            headerInclude = "struct_arg.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Thing"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "struct_arg.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "thing_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        CTypeSpec {
                          cTypeSpecIdentifier = Nothing,
                          cTypeSpecInstances =
                          Map.fromList []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "thing",
                      commentLocation = Just
                        "struct_arg.h:2:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["struct_arg.h"],
                          headerInclude = "struct_arg.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "thing_x")
                      (Idx 2)
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Thing",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Thing",
      deriveInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasCField
        HasCFieldInstance {
          hasCFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Thing"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "thing_x",
          hasCFieldInstanceCFieldType =
          HsPrimType HsPrimCInt,
          hasCFieldInstanceFieldOffset =
          0},
      defineInstanceComment =
      Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceHasField
        HasFieldInstance {
          hasFieldInstanceParentType =
          HsTypRef
            (Name "@NsTypeConstr" "Thing"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "thing_x",
          hasFieldInstanceFieldType =
          HsPrimType HsPrimCInt,
          hasFieldInstanceVia =
          ViaHasCField},
      defineInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "thing_fun_1_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "Thing")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_61dfa2c4506feb8f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "signed int hs_bindgen_test_struct_arg_61dfa2c4506feb8f (\n",
              "  struct thing *arg1\n",
              ")\n",
              "{\n",
              "  return thing_fun_1(*arg1);\n",
              "}"],
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = Identifier
                    "Thing"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Pointer-based API for",
              Identifier "thing_fun_1"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "thing_fun_1",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Thing"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (EApp (EApp (EGlobal CAPI_with) (EBound 0)) (ELam "y" (EApp (EFree "thing_fun_1_wrapper") (EBound 0))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = Identifier
                    "Thing"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_1",
          commentLocation = Just
            "struct_arg.h:6:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["struct_arg.h"],
              headerInclude = "struct_arg.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "thing_fun_2_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "Thing"))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_022cc8107f565c95",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_struct_arg_022cc8107f565c95 (\n",
              "  signed int arg1,\n",
              "  struct thing *arg2\n",
              ")\n",
              "{\n",
              "  *arg2 = thing_fun_2(arg1);\n",
              "}"],
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = Identifier
                "Thing"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Pointer-based API for",
              Identifier "thing_fun_2"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "thing_fun_2",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsTypRef
          (Name "@NsTypeConstr" "Thing")),
      functionDeclBody =
      `ELam "x" (EApp (EGlobal CAPI_allocaAndPeek) (ELam "z" (EApp (EApp (EFree "thing_fun_2_wrapper") (EBound 1)) (EBound 0))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = Identifier
                "Thing"}
            NameOriginInSource},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_2",
          commentLocation = Just
            "struct_arg.h:7:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["struct_arg.h"],
              headerInclude = "struct_arg.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "thing_fun_3a_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "Thing")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "z"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "Thing"))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_4d9304280cca3098",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_struct_arg_4d9304280cca3098 (\n",
              "  signed int arg1,\n",
              "  struct thing *arg2,\n",
              "  double arg3,\n",
              "  struct thing *arg4\n",
              ")\n",
              "{\n",
              "  *arg4 = thing_fun_3a(arg1, *arg2, arg3);\n",
              "}"],
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = Identifier
                    "Thing"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "z",
                  nameHsIdent = Identifier "z"})
              (TypePrim
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = Identifier
                "Thing"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Pointer-based API for",
              Identifier "thing_fun_3a"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "thing_fun_3a",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "y"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Thing"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "z"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "z",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsTypRef
          (Name "@NsTypeConstr" "Thing")),
      functionDeclBody =
      `ELam "x" (ELam "x" (ELam "x" (EApp (EApp (EGlobal CAPI_with) (EBound 1)) (ELam "y" (EApp (EGlobal CAPI_allocaAndPeek) (ELam "z" (EApp (EApp (EApp (EApp (EFree "thing_fun_3a_wrapper") (EBound 4)) (EBound 1)) (EBound 2)) (EBound 0))))))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = Identifier
                    "Thing"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "z",
                  nameHsIdent = Identifier "z"})
              (TypePrim
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = Identifier
                "Thing"}
            NameOriginInSource},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_3a",
          commentLocation = Just
            "struct_arg.h:9:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["struct_arg.h"],
              headerInclude = "struct_arg.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "thing_fun_3b_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "Thing")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "z"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCChar)),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_f39687b254852452",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "char hs_bindgen_test_struct_arg_f39687b254852452 (\n",
              "  signed int arg1,\n",
              "  struct thing *arg2,\n",
              "  double arg3\n",
              ")\n",
              "{\n",
              "  return thing_fun_3b(arg1, *arg2, arg3);\n",
              "}"],
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = Identifier
                    "Thing"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "z",
                  nameHsIdent = Identifier "z"})
              (TypePrim
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit
                (Just Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Pointer-based API for",
              Identifier "thing_fun_3b"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "thing_fun_3b",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "y"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Thing"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "z"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "z",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCChar),
      functionDeclBody =
      `ELam "x" (ELam "x" (ELam "x" (EApp (EApp (EGlobal CAPI_with) (EBound 1)) (ELam "y" (EApp (EApp (EApp (EFree "thing_fun_3b_wrapper") (EBound 3)) (EBound 0)) (EBound 1))))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = Identifier
                    "Thing"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "z",
                  nameHsIdent = Identifier "z"})
              (TypePrim
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit
                (Just Signed)))},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_3b",
          commentLocation = Just
            "struct_arg.h:10:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["struct_arg.h"],
              headerInclude = "struct_arg.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "thing_fun_1_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "Thing")),
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_409d8c948bf989f6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "signed int hs_bindgen_test_struct_arg_409d8c948bf989f6 (\n",
              "  struct thing *arg1\n",
              ")\n",
              "{\n",
              "  return thing_fun_1(*arg1);\n",
              "}"],
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = Identifier
                    "Thing"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Pointer-based API for",
              Identifier "thing_fun_1"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "thing_fun_1",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Thing"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCInt),
      functionDeclBody =
      `ELam "x" (EApp (EApp (EGlobal CAPI_with) (EBound 0)) (ELam "y" (EApp (EFree "thing_fun_1_wrapper") (EBound 0))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = Identifier
                    "Thing"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_1",
          commentLocation = Just
            "struct_arg.h:6:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["struct_arg.h"],
              headerInclude = "struct_arg.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "thing_fun_2_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "Thing"))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_2d20059791239ef2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_struct_arg_2d20059791239ef2 (\n",
              "  signed int arg1,\n",
              "  struct thing *arg2\n",
              ")\n",
              "{\n",
              "  *arg2 = thing_fun_2(arg1);\n",
              "}"],
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = Identifier
                "Thing"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Pointer-based API for",
              Identifier "thing_fun_2"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "thing_fun_2",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsTypRef
          (Name "@NsTypeConstr" "Thing")),
      functionDeclBody =
      `ELam "x" (EApp (EGlobal CAPI_allocaAndPeek) (ELam "z" (EApp (EApp (EFree "thing_fun_2_wrapper") (EBound 1)) (EBound 0))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = Identifier
                "Thing"}
            NameOriginInSource},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_2",
          commentLocation = Just
            "struct_arg.h:7:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["struct_arg.h"],
              headerInclude = "struct_arg.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "thing_fun_3a_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "Thing")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "z"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsTypRef
            (Name
              "@NsTypeConstr"
              "Thing"))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_ce442967da2c37cd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_struct_arg_ce442967da2c37cd (\n",
              "  signed int arg1,\n",
              "  struct thing *arg2,\n",
              "  double arg3,\n",
              "  struct thing *arg4\n",
              ")\n",
              "{\n",
              "  *arg4 = thing_fun_3a(arg1, *arg2, arg3);\n",
              "}"],
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = Identifier
                    "Thing"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "z",
                  nameHsIdent = Identifier "z"})
              (TypePrim
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = Identifier
                "Thing"}
            NameOriginInSource},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Pointer-based API for",
              Identifier "thing_fun_3a"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "thing_fun_3a",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "y"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Thing"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "z"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "z",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsTypRef
          (Name "@NsTypeConstr" "Thing")),
      functionDeclBody =
      `ELam "x" (ELam "x" (ELam "x" (EApp (EApp (EGlobal CAPI_with) (EBound 1)) (ELam "y" (EApp (EGlobal CAPI_allocaAndPeek) (ELam "z" (EApp (EApp (EApp (EApp (EFree "thing_fun_3a_wrapper") (EBound 4)) (EBound 1)) (EBound 2)) (EBound 0))))))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = Identifier
                    "Thing"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "z",
                  nameHsIdent = Identifier "z"})
              (TypePrim
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = Identifier
                "Thing"}
            NameOriginInSource},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_3a",
          commentLocation = Just
            "struct_arg.h:9:14",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["struct_arg.h"],
              headerInclude = "struct_arg.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "thing_fun_3b_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (Name "@NsTypeConstr" "Thing")),
          functionParameterComment =
          Nothing},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "z"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCChar)),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_e8bc8fce45854092",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "char hs_bindgen_test_struct_arg_e8bc8fce45854092 (\n",
              "  signed int arg1,\n",
              "  struct thing *arg2,\n",
              "  double arg3\n",
              ")\n",
              "{\n",
              "  return thing_fun_3b(arg1, *arg2, arg3);\n",
              "}"],
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = Identifier
                    "Thing"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "z",
                  nameHsIdent = Identifier "z"})
              (TypePrim
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit
                (Just Signed)))},
      foreignImportComment = Just
        Comment {
          commentTitle = Just
            [
              TextContent
                "Pointer-based API for",
              Identifier "thing_fun_3b"],
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclFunction
    FunctionDecl {
      functionDeclName = Name
        "@NsVar"
        "thing_fun_3b",
      functionDeclParameters = [
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "y"),
          functionParameterType = HsTypRef
            (Name "@NsTypeConstr" "Thing"),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (Name "@NsVar" "z"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "z",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      functionDeclResultType = HsIO
        (HsPrimType HsPrimCChar),
      functionDeclBody =
      `ELam "x" (ELam "x" (ELam "x" (EApp (EApp (EGlobal CAPI_with) (EBound 1)) (ELam "y" (EApp (EApp (EApp (EFree "thing_fun_3b_wrapper") (EBound 3)) (EBound 0)) (EBound 1))))))`,
      functionDeclOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = Identifier "y"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = Identifier
                    "Thing"}
                NameOriginInSource),
            _×_
              (Just
                NamePair {
                  nameC = Name "z",
                  nameHsIdent = Identifier "z"})
              (TypePrim
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit
                (Just Signed)))},
      functionDeclComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "thing_fun_3b",
          commentLocation = Just
            "struct_arg.h:10:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["struct_arg.h"],
              headerInclude = "struct_arg.h"},
          commentChildren = []}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_struct_arg_c5f0c295b311010a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (Name "@NsTypeConstr" "Thing"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_c5f0c295b311010a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_thing_fun_1_ptr */\n",
              "__attribute__ ((const))\n",
              "signed int (*hs_bindgen_test_struct_arg_c5f0c295b311010a (void)) (\n",
              "  struct thing arg1\n",
              ")\n",
              "{\n",
              "  return &thing_fun_1;\n",
              "}"],
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeStruct
              NamePair {
                nameC = Name "thing",
                nameHsIdent = Identifier
                  "Thing"}
              NameOriginInSource]
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
        "hs_bindgen_test_struct_arg_24edf6600396b62a",
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
                    "Thing")))))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_24edf6600396b62a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_thing_fun_2_ptr */\n",
              "__attribute__ ((const))\n",
              "struct thing (*hs_bindgen_test_struct_arg_24edf6600396b62a (void)) (\n",
              "  signed int arg1\n",
              ")\n",
              "{\n",
              "  return &thing_fun_2;\n",
              "}"],
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
          (TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = Identifier
                "Thing"}
            NameOriginInSource)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_struct_arg_29a42b48992cd0bf",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "Thing"))
                (HsFun
                  (HsPrimType HsPrimCDouble)
                  (HsIO
                    (HsTypRef
                      (Name
                        "@NsTypeConstr"
                        "Thing")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_29a42b48992cd0bf",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_thing_fun_3a_ptr */\n",
              "__attribute__ ((const))\n",
              "struct thing (*hs_bindgen_test_struct_arg_29a42b48992cd0bf (void)) (\n",
              "  signed int arg1,\n",
              "  struct thing arg2,\n",
              "  double arg3\n",
              ")\n",
              "{\n",
              "  return &thing_fun_3a;\n",
              "}"],
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypeStruct
              NamePair {
                nameC = Name "thing",
                nameHsIdent = Identifier
                  "Thing"}
              NameOriginInSource,
            TypePrim
              (PrimFloating PrimDouble)]
          (TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = Identifier
                "Thing"}
            NameOriginInSource)),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_struct_arg_0d6597dfc03e312f",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsFun
                (HsTypRef
                  (Name "@NsTypeConstr" "Thing"))
                (HsFun
                  (HsPrimType HsPrimCDouble)
                  (HsIO
                    (HsPrimType HsPrimCChar))))))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_0d6597dfc03e312f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "/* get_thing_fun_3b_ptr */\n",
              "__attribute__ ((const))\n",
              "char (*hs_bindgen_test_struct_arg_0d6597dfc03e312f (void)) (\n",
              "  signed int arg1,\n",
              "  struct thing arg2,\n",
              "  double arg3\n",
              ")\n",
              "{\n",
              "  return &thing_fun_3b;\n",
              "}"],
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypeStruct
              NamePair {
                nameC = Name "thing",
                nameHsIdent = Identifier
                  "Thing"}
              NameOriginInSource,
            TypePrim
              (PrimFloating PrimDouble)]
          (TypePrim
            (PrimChar
              (PrimSignImplicit
                (Just Signed))))),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
