[
  DeclData
    Struct {
      structName = Name
        "@NsTypeConstr"
        "Struct2",
      structConstr = Name
        "@NsConstr"
        "Struct2",
      structFields = [
        Field {
          fieldName = Name
            "@NsVar"
            "struct2_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "skip_over_long_double.h:14:7",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = Identifier
                    "struct2_x"},
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
                "skip_over_long_double.h:14:7",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["skip_over_long_double.h"],
                  headerInclude =
                  "skip_over_long_double.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "skip_over_long_double.h:13:8",
            declId = NamePair {
              nameC = Name "struct2",
              nameHsIdent = Identifier
                "Struct2"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["skip_over_long_double.h"],
                headerInclude =
                "skip_over_long_double.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (Name "@NsConstr" "Struct2"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "skip_over_long_double.h:14:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "struct2_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec {
            declSpecC = Nothing,
            declSpecHs = Nothing}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "struct2",
          commentLocation = Just
            "skip_over_long_double.h:13:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["skip_over_long_double.h"],
              headerInclude =
              "skip_over_long_double.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = Name
            "@NsTypeConstr"
            "Struct2",
          structConstr = Name
            "@NsConstr"
            "Struct2",
          structFields = [
            Field {
              fieldName = Name
                "@NsVar"
                "struct2_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "skip_over_long_double.h:14:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = Identifier
                        "struct2_x"},
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
                    "skip_over_long_double.h:14:7",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["skip_over_long_double.h"],
                      headerInclude =
                      "skip_over_long_double.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc =
                "skip_over_long_double.h:13:8",
                declId = NamePair {
                  nameC = Name "struct2",
                  nameHsIdent = Identifier
                    "Struct2"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["skip_over_long_double.h"],
                    headerInclude =
                    "skip_over_long_double.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (Name "@NsConstr" "Struct2"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "skip_over_long_double.h:14:7",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = Identifier
                            "struct2_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec {
                declSpecC = Nothing,
                declSpecHs = Nothing}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "struct2",
              commentLocation = Just
                "skip_over_long_double.h:13:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["skip_over_long_double.h"],
                  headerInclude =
                  "skip_over_long_double.h"},
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
                    "Struct2",
                  structConstr = Name
                    "@NsConstr"
                    "Struct2",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct2_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "skip_over_long_double.h:14:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "struct2_x"},
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
                            "skip_over_long_double.h:14:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["skip_over_long_double.h"],
                              headerInclude =
                              "skip_over_long_double.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "skip_over_long_double.h:13:8",
                        declId = NamePair {
                          nameC = Name "struct2",
                          nameHsIdent = Identifier
                            "Struct2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["skip_over_long_double.h"],
                            headerInclude =
                            "skip_over_long_double.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Struct2"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "skip_over_long_double.h:14:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "struct2_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "struct2",
                      commentLocation = Just
                        "skip_over_long_double.h:13:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["skip_over_long_double.h"],
                          headerInclude =
                          "skip_over_long_double.h"},
                      commentChildren = []}})
              [
                PeekCField
                  (HsStrLit "struct2_x")
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
                    "Struct2",
                  structConstr = Name
                    "@NsConstr"
                    "Struct2",
                  structFields = [
                    Field {
                      fieldName = Name
                        "@NsVar"
                        "struct2_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "skip_over_long_double.h:14:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = Identifier
                                "struct2_x"},
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
                            "skip_over_long_double.h:14:7",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["skip_over_long_double.h"],
                              headerInclude =
                              "skip_over_long_double.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc =
                        "skip_over_long_double.h:13:8",
                        declId = NamePair {
                          nameC = Name "struct2",
                          nameHsIdent = Identifier
                            "Struct2"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["skip_over_long_double.h"],
                            headerInclude =
                            "skip_over_long_double.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (Name "@NsConstr" "Struct2"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "skip_over_long_double.h:14:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = Identifier
                                    "struct2_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec {
                        declSpecC = Nothing,
                        declSpecHs = Nothing}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Just
                    Comment {
                      commentTitle = Nothing,
                      commentOrigin = Just "struct2",
                      commentLocation = Just
                        "skip_over_long_double.h:13:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["skip_over_long_double.h"],
                          headerInclude =
                          "skip_over_long_double.h"},
                      commentChildren = []}}
                (Add 1)
                (Seq
                  [
                    PokeCField
                      (HsStrLit "struct2_x")
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
        "Struct2",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = Name
        "@NsTypeConstr"
        "Struct2",
      deriveInstanceComment =
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
              "Struct2"),
          hasCFieldInstanceFieldName =
          Name "@NsVar" "struct2_x",
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
            (Name
              "@NsTypeConstr"
              "Struct2"),
          hasFieldInstanceFieldName = Name
            "@NsVar"
            "struct2_x",
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
        "fun2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_skip_over_long_double_fb32cb593bc1f7b8",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_skip_over_long_double_fb32cb593bc1f7b8 (\n",
              "  signed int arg1\n",
              ")\n",
              "{\n",
              "  fun2(arg1);\n",
              "}"],
          capiWrapperImport =
          "skip_over_long_double.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fun2",
          commentLocation = Just
            "skip_over_long_double.h:7:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["skip_over_long_double.h"],
              headerInclude =
              "skip_over_long_double.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "fun2",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Nothing,
          functionParameterType =
          HsPrimType HsPrimCInt,
          functionParameterComment =
          Nothing}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_skip_over_long_double_5ebf8088e71802cc",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition = concat
            [
              "void hs_bindgen_test_skip_over_long_double_5ebf8088e71802cc (\n",
              "  signed int arg1\n",
              ")\n",
              "{\n",
              "  fun2(arg1);\n",
              "}"],
          capiWrapperImport =
          "skip_over_long_double.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              Nothing
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "fun2",
          commentLocation = Just
            "skip_over_long_double.h:7:6",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["skip_over_long_double.h"],
              headerInclude =
              "skip_over_long_double.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = Name
        "@NsVar"
        "hs_bindgen_test_skip_over_long_double_d1bf59c1516f6bfa",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO
                (HsPrimType HsPrimUnit))))),
      foreignImportOrigName =
      "hs_bindgen_test_skip_over_long_double_d1bf59c1516f6bfa",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          concat
            [
              "/* get_fun2_ptr */\n",
              "__attribute__ ((const))\n",
              "void (*hs_bindgen_test_skip_over_long_double_d1bf59c1516f6bfa (void)) (\n",
              "  signed int arg1\n",
              ")\n",
              "{\n",
              "  return &fun2;\n",
              "}"],
          capiWrapperImport =
          "skip_over_long_double.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimInt Signed)]
          TypeVoid),
      foreignImportComment = Nothing,
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
