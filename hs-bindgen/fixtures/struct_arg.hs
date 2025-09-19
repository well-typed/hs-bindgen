[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Thing",
      structConstr = HsName
        "@NsConstr"
        "Thing",
      structFields = [
        Field {
          fieldName = HsName
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
                  nameHsIdent = HsIdentifier
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
              nameHsIdent = HsIdentifier
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
                (HsName "@NsConstr" "Thing"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "struct_arg.h:3:9",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "thing_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
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
          structName = HsName
            "@NsTypeConstr"
            "Thing",
          structConstr = HsName
            "@NsConstr"
            "Thing",
          structFields = [
            Field {
              fieldName = HsName
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
                      nameHsIdent = HsIdentifier
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
                  nameHsIdent = HsIdentifier
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
                    (HsName "@NsConstr" "Thing"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "struct_arg.h:3:9",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "thing_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
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
                  structName = HsName
                    "@NsTypeConstr"
                    "Thing",
                  structConstr = HsName
                    "@NsConstr"
                    "Thing",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName "@NsConstr" "Thing"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "struct_arg.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "thing_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
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
              [PeekByteOff (Idx 0) 0]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Thing",
                  structConstr = HsName
                    "@NsConstr"
                    "Thing",
                  structFields = [
                    Field {
                      fieldName = HsName
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
                              nameHsIdent = HsIdentifier
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
                          nameHsIdent = HsIdentifier
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
                            (HsName "@NsConstr" "Thing"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "struct_arg.h:3:9",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "thing_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
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
                    PokeByteOff
                      (Idx 2)
                      0
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Thing",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Thing",
      deriveInstanceComment =
      Nothing},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_1_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Thing")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_61dfa2c4506feb8f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_struct_arg_61dfa2c4506feb8f (struct thing *arg1) { return thing_fun_1(*arg1); }",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = HsIdentifier
                    "Thing"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
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
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_2_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Thing"))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_022cc8107f565c95",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_struct_arg_022cc8107f565c95 (signed int arg1, struct thing *arg2) { *arg2 = thing_fun_2(arg1); }",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = HsIdentifier
                "Thing"}
            NameOriginInSource},
      foreignImportComment = Just
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
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_3a_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
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
            (HsName "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Thing")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Thing"))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_4d9304280cca3098",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_struct_arg_4d9304280cca3098 (signed int arg1, struct thing *arg2, double arg3, struct thing *arg4) { *arg4 = thing_fun_3a(arg1, *arg2, arg3); }",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier "y"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = HsIdentifier
                    "Thing"}
                NameOriginInSource),
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
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = HsIdentifier
                "Thing"}
            NameOriginInSource},
      foreignImportComment = Just
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
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_3b_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
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
            (HsName "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Thing")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCChar)),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_f39687b254852452",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "char hs_bindgen_test_struct_arg_f39687b254852452 (signed int arg1, struct thing *arg2, double arg3) { return thing_fun_3b(arg1, *arg2, arg3); }",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier "y"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = HsIdentifier
                    "Thing"}
                NameOriginInSource),
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
            (PrimChar
              (PrimSignImplicit
                (Just Signed)))},
      foreignImportComment = Just
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
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_1_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Thing")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_409d8c948bf989f6",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_struct_arg_409d8c948bf989f6 (struct thing *arg1) { return thing_fun_1(*arg1); }",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = HsIdentifier
                    "Thing"}
                NameOriginInSource)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Just
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
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_2_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Thing"))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_2d20059791239ef2",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_struct_arg_2d20059791239ef2 (signed int arg1, struct thing *arg2) { *arg2 = thing_fun_2(arg1); }",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = HsIdentifier
                "Thing"}
            NameOriginInSource},
      foreignImportComment = Just
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
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_3a_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
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
            (HsName "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Thing")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      HeapResultType
        (HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Thing"))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_ce442967da2c37cd",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "void hs_bindgen_test_struct_arg_ce442967da2c37cd (signed int arg1, struct thing *arg2, double arg3, struct thing *arg4) { *arg4 = thing_fun_3a(arg1, *arg2, arg3); }",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier "y"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = HsIdentifier
                    "Thing"}
                NameOriginInSource),
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
          functionRes = TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = HsIdentifier
                "Thing"}
            NameOriginInSource},
      foreignImportComment = Just
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
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "thing_fun_3b_wrapper",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
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
            (HsName "@NsVar" "y"),
          functionParameterType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Thing")),
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentLocation = Nothing,
              commentHeaderInfo = Nothing,
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCChar)),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_e8bc8fce45854092",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "char hs_bindgen_test_struct_arg_e8bc8fce45854092 (signed int arg1, struct thing *arg2, double arg3) { return thing_fun_3b(arg1, *arg2, arg3); }",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
              (TypePrim
                (PrimIntegral PrimInt Signed)),
            _×_
              (Just
                NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier "y"})
              (TypeStruct
                NamePair {
                  nameC = Name "thing",
                  nameHsIdent = HsIdentifier
                    "Thing"}
                NameOriginInSource),
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
            (PrimChar
              (PrimSignImplicit
                (Just Signed)))},
      foreignImportComment = Just
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
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_struct_arg_c5f0c295b311010a",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Thing"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_c5f0c295b311010a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_thing_fun_1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_struct_arg_c5f0c295b311010a (void)) (struct thing arg1) { return &thing_fun_1; } ",
          capiWrapperImport =
          "struct_arg.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypeStruct
              NamePair {
                nameC = Name "thing",
                nameHsIdent = HsIdentifier
                  "Thing"}
              NameOriginInSource]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
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
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
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
                  (HsName
                    "@NsTypeConstr"
                    "Thing")))))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_24edf6600396b62a",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_thing_fun_2_ptr */ __attribute__ ((const)) struct thing (*hs_bindgen_test_struct_arg_24edf6600396b62a (void)) (signed int arg1) { return &thing_fun_2; } ",
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
              nameHsIdent = HsIdentifier
                "Thing"}
            NameOriginInSource)),
      foreignImportComment = Just
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
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
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
                  (HsName
                    "@NsTypeConstr"
                    "Thing"))
                (HsFun
                  (HsPrimType HsPrimCDouble)
                  (HsIO
                    (HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Thing")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_29a42b48992cd0bf",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_thing_fun_3a_ptr */ __attribute__ ((const)) struct thing (*hs_bindgen_test_struct_arg_29a42b48992cd0bf (void)) (signed int arg1, struct thing arg2, double arg3) { return &thing_fun_3a; } ",
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
                nameHsIdent = HsIdentifier
                  "Thing"}
              NameOriginInSource,
            TypePrim
              (PrimFloating PrimDouble)]
          (TypeStruct
            NamePair {
              nameC = Name "thing",
              nameHsIdent = HsIdentifier
                "Thing"}
            NameOriginInSource)),
      foreignImportComment = Just
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
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
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
                  (HsName
                    "@NsTypeConstr"
                    "Thing"))
                (HsFun
                  (HsPrimType HsPrimCDouble)
                  (HsIO
                    (HsPrimType HsPrimCChar))))))),
      foreignImportOrigName =
      "hs_bindgen_test_struct_arg_0d6597dfc03e312f",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_thing_fun_3b_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_struct_arg_0d6597dfc03e312f (void)) (signed int arg1, struct thing arg2, double arg3) { return &thing_fun_3b; } ",
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
                nameHsIdent = HsIdentifier
                  "Thing"}
              NameOriginInSource,
            TypePrim
              (PrimFloating PrimDouble)]
          (TypePrim
            (PrimChar
              (PrimSignImplicit
                (Just Signed))))),
      foreignImportComment = Just
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
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
