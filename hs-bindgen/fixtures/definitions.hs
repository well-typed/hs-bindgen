[
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "foo",
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
              commentHeaderInfo = Nothing,
              commentChildren = []}}],
      foreignImportResultType =
      NormalResultType
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_definitions_a7d624773bb0585c",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "signed int hs_bindgen_test_definitions_a7d624773bb0585c (double arg1) { return foo(arg1); }",
          capiWrapperImport =
          "definitions.h"},
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            _×_
              (Just
                NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier "x"})
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
          commentOrigin = Just "foo",
          commentLocation = Just
            "definitions.h:13:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["definitions.h"],
              headerInclude =
              "definitions.h"},
          commentChildren = []},
      foreignImportSafety = Safe},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_definitions_fb3e409881d8c524",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCDouble)
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportOrigName =
      "hs_bindgen_test_definitions_fb3e409881d8c524",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_foo_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_definitions_fb3e409881d8c524 (void)) (double arg1) { return &foo; } ",
          capiWrapperImport =
          "definitions.h"},
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimFloating PrimDouble)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "foo",
          commentLocation = Just
            "definitions.h:13:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["definitions.h"],
              headerInclude =
              "definitions.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_definitions_fc2aad2af9befead",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_definitions_fc2aad2af9befead",
      foreignImportCallConv =
      CallConvUserlandCAPI
        UserlandCapiWrapper {
          capiWrapperDefinition =
          "/* get_n_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_definitions_fc2aad2af9befead (void) { return &n; } ",
          capiWrapperImport =
          "definitions.h"},
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "n",
          commentLocation = Just
            "definitions.h:18:5",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["definitions.h"],
              headerInclude =
              "definitions.h"},
          commentChildren = []},
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "X",
      structConstr = HsName
        "@NsConstr"
        "X",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "x_n",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc =
                "definitions.h:23:16",
                fieldName = NamePair {
                  nameC = Name "n",
                  nameHsIdent = HsIdentifier
                    "x_n"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "n",
              commentLocation = Just
                "definitions.h:23:16",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["definitions.h"],
                  headerInclude =
                  "definitions.h"},
              commentChildren = []}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "definitions.h:23:8",
            declId = NamePair {
              nameC = Name "X",
              nameHsIdent = HsIdentifier "X"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeaderInfo = Just
              HeaderInfo {
                headerMainHeaders = NE.fromList
                  ["definitions.h"],
                headerInclude =
                "definitions.h"},
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "X"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "definitions.h:23:16",
                    fieldName = NamePair {
                      nameC = Name "n",
                      nameHsIdent = HsIdentifier
                        "x_n"},
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
          commentOrigin = Just "X",
          commentLocation = Just
            "definitions.h:23:8",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["definitions.h"],
              headerInclude =
              "definitions.h"},
          commentChildren = []}},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "X",
          structConstr = HsName
            "@NsConstr"
            "X",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "x_n",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc =
                    "definitions.h:23:16",
                    fieldName = NamePair {
                      nameC = Name "n",
                      nameHsIdent = HsIdentifier
                        "x_n"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                Comment {
                  commentTitle = Nothing,
                  commentOrigin = Just "n",
                  commentLocation = Just
                    "definitions.h:23:16",
                  commentHeaderInfo = Just
                    HeaderInfo {
                      headerMainHeaders = NE.fromList
                        ["definitions.h"],
                      headerInclude =
                      "definitions.h"},
                  commentChildren = []}}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "definitions.h:23:8",
                declId = NamePair {
                  nameC = Name "X",
                  nameHsIdent = HsIdentifier "X"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeaderInfo = Just
                  HeaderInfo {
                    headerMainHeaders = NE.fromList
                      ["definitions.h"],
                    headerInclude =
                    "definitions.h"},
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "X"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc =
                        "definitions.h:23:16",
                        fieldName = NamePair {
                          nameC = Name "n",
                          nameHsIdent = HsIdentifier
                            "x_n"},
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
              commentOrigin = Just "X",
              commentLocation = Just
                "definitions.h:23:8",
              commentHeaderInfo = Just
                HeaderInfo {
                  headerMainHeaders = NE.fromList
                    ["definitions.h"],
                  headerInclude =
                  "definitions.h"},
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
                    "X",
                  structConstr = HsName
                    "@NsConstr"
                    "X",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "x_n",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "definitions.h:23:16",
                            fieldName = NamePair {
                              nameC = Name "n",
                              nameHsIdent = HsIdentifier
                                "x_n"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "n",
                          commentLocation = Just
                            "definitions.h:23:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["definitions.h"],
                              headerInclude =
                              "definitions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "definitions.h:23:8",
                        declId = NamePair {
                          nameC = Name "X",
                          nameHsIdent = HsIdentifier "X"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["definitions.h"],
                            headerInclude =
                            "definitions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "X"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "definitions.h:23:16",
                                fieldName = NamePair {
                                  nameC = Name "n",
                                  nameHsIdent = HsIdentifier
                                    "x_n"},
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
                      commentOrigin = Just "X",
                      commentLocation = Just
                        "definitions.h:23:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["definitions.h"],
                          headerInclude =
                          "definitions.h"},
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
                    "X",
                  structConstr = HsName
                    "@NsConstr"
                    "X",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "x_n",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc =
                            "definitions.h:23:16",
                            fieldName = NamePair {
                              nameC = Name "n",
                              nameHsIdent = HsIdentifier
                                "x_n"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        Comment {
                          commentTitle = Nothing,
                          commentOrigin = Just "n",
                          commentLocation = Just
                            "definitions.h:23:16",
                          commentHeaderInfo = Just
                            HeaderInfo {
                              headerMainHeaders = NE.fromList
                                ["definitions.h"],
                              headerInclude =
                              "definitions.h"},
                          commentChildren = []}}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "definitions.h:23:8",
                        declId = NamePair {
                          nameC = Name "X",
                          nameHsIdent = HsIdentifier "X"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeaderInfo = Just
                          HeaderInfo {
                            headerMainHeaders = NE.fromList
                              ["definitions.h"],
                            headerInclude =
                            "definitions.h"},
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "X"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc =
                                "definitions.h:23:16",
                                fieldName = NamePair {
                                  nameC = Name "n",
                                  nameHsIdent = HsIdentifier
                                    "x_n"},
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
                      commentOrigin = Just "X",
                      commentLocation = Just
                        "definitions.h:23:8",
                      commentHeaderInfo = Just
                        HeaderInfo {
                          headerMainHeaders = NE.fromList
                            ["definitions.h"],
                          headerInclude =
                          "definitions.h"},
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
        "X",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "X",
      deriveInstanceComment =
      Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Y",
      newtypeConstr = HsName
        "@NsConstr"
        "Y",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Y",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "definitions.h:26:7",
          declId = NamePair {
            nameC = Name "Y",
            nameHsIdent = HsIdentifier "Y"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["definitions.h"],
              headerInclude =
              "definitions.h"},
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Y",
              newtypeField = HsName
                "@NsVar"
                "un_Y"},
            unionSizeof = 4,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "definitions.h:26:15",
                  fieldName = NamePair {
                    nameC = Name "m",
                    nameHsIdent = HsIdentifier
                      "y_m"},
                  fieldComment = Nothing},
                unionFieldType = TypePrim
                  (PrimIntegral PrimInt Signed)},
              UnionField {
                unionFieldInfo = FieldInfo {
                  fieldLoc =
                  "definitions.h:26:22",
                  fieldName = NamePair {
                    nameC = Name "o",
                    nameHsIdent = HsIdentifier
                      "y_o"},
                  fieldComment = Nothing},
                unionFieldType = TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "Y",
          commentLocation = Just
            "definitions.h:26:7",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["definitions.h"],
              headerInclude =
              "definitions.h"},
          commentChildren = []}},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveVia
        (HsSizedByteArray 4 4),
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Y",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_y_m",
      unionGetterType = HsPrimType
        HsPrimCInt,
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "Y",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "m",
          commentLocation = Just
            "definitions.h:26:15",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["definitions.h"],
              headerInclude =
              "definitions.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier "set_y_m"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_y_m",
      unionSetterType = HsPrimType
        HsPrimCInt,
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "Y",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier "get_y_m"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_y_o",
      unionGetterType = HsPrimType
        HsPrimCInt,
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "Y",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just "o",
          commentLocation = Just
            "definitions.h:26:22",
          commentHeaderInfo = Just
            HeaderInfo {
              headerMainHeaders = NE.fromList
                ["definitions.h"],
              headerInclude =
              "definitions.h"},
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier "set_y_o"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_y_o",
      unionSetterType = HsPrimType
        HsPrimCInt,
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "Y",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentLocation = Nothing,
          commentHeaderInfo = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier "get_y_o"]]}}]
