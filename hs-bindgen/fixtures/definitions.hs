[
  DeclInlineCInclude
    "definitions.h",
  DeclInlineC
    "signed int hs_bindgen_test_definitions_a7d624773bb0585c (double arg1) { return foo(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "foo",
      foreignImportType = HsFun
        (HsPrimType HsPrimCDouble)
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_definitions_a7d624773bb0585c",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePrim
              (PrimFloating PrimDouble)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Nothing},
  DeclInlineCInclude
    "definitions.h",
  DeclInlineC
    "__attribute__ ((const)) signed int *get_n_ptr (void) { return &n; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "n_ptr",
      foreignImportType = HsPtr
        (HsPrimType HsPrimCInt),
      foreignImportOrigName =
      "get_n_ptr",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Nothing},
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
              structFieldLoc =
              "definitions.h:23:16",
              structFieldName = NamePair {
                nameC = Name "n",
                nameHsIdent = HsIdentifier
                  "x_n"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "definitions.h:23:8",
            declId = NamePair {
              nameC = Name "X",
              nameHsIdent = HsIdentifier "X"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "definitions.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "X"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "definitions.h:23:16",
                  structFieldName = NamePair {
                    nameC = Name "n",
                    nameHsIdent = HsIdentifier
                      "x_n"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing}],
              structFlam = Nothing},
          declSpec = DeclSpec
            TypeSpec {
              typeSpecModule = Nothing,
              typeSpecIdentifier = Nothing,
              typeSpecInstances = Map.fromList
                []}},
      structInstances = Set.fromList
        [Eq, Show, Storable],
      structComment = Nothing},
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
                  structFieldLoc =
                  "definitions.h:23:16",
                  structFieldName = NamePair {
                    nameC = Name "n",
                    nameHsIdent = HsIdentifier
                      "x_n"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "definitions.h:23:8",
                declId = NamePair {
                  nameC = Name "X",
                  nameHsIdent = HsIdentifier "X"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "definitions.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "X"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc =
                      "definitions.h:23:16",
                      structFieldName = NamePair {
                        nameC = Name "n",
                        nameHsIdent = HsIdentifier
                          "x_n"},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing}],
                  structFlam = Nothing},
              declSpec = DeclSpec
                TypeSpec {
                  typeSpecModule = Nothing,
                  typeSpecIdentifier = Nothing,
                  typeSpecInstances = Map.fromList
                    []}},
          structInstances = Set.fromList
            [Eq, Show, Storable],
          structComment = Nothing}
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
                          structFieldLoc =
                          "definitions.h:23:16",
                          structFieldName = NamePair {
                            nameC = Name "n",
                            nameHsIdent = HsIdentifier
                              "x_n"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "definitions.h:23:8",
                        declId = NamePair {
                          nameC = Name "X",
                          nameHsIdent = HsIdentifier "X"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "definitions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "X"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "definitions.h:23:16",
                              structFieldName = NamePair {
                                nameC = Name "n",
                                nameHsIdent = HsIdentifier
                                  "x_n"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Nothing})
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
                          structFieldLoc =
                          "definitions.h:23:16",
                          structFieldName = NamePair {
                            nameC = Name "n",
                            nameHsIdent = HsIdentifier
                              "x_n"},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "definitions.h:23:8",
                        declId = NamePair {
                          nameC = Name "X",
                          nameHsIdent = HsIdentifier "X"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "definitions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "X"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "definitions.h:23:16",
                              structFieldName = NamePair {
                                nameC = Name "n",
                                nameHsIdent = HsIdentifier
                                  "x_n"},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing}],
                          structFlam = Nothing},
                      declSpec = DeclSpec
                        TypeSpec {
                          typeSpecModule = Nothing,
                          typeSpecIdentifier = Nothing,
                          typeSpecInstances = Map.fromList
                            []}},
                  structInstances = Set.fromList
                    [Eq, Show, Storable],
                  structComment = Nothing}
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
          declHeader = "definitions.h",
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
                unionFieldLoc =
                "definitions.h:26:15",
                unionFieldName = NamePair {
                  nameC = Name "m",
                  nameHsIdent = HsIdentifier
                    "y_m"},
                unionFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                unionFieldComment = Nothing},
              UnionField {
                unionFieldLoc =
                "definitions.h:26:22",
                unionFieldName = NamePair {
                  nameC = Name "o",
                  nameHsIdent = HsIdentifier
                    "y_o"},
                unionFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                unionFieldComment = Nothing}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Storable],
      newtypeComment = Nothing},
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
          commentOrigin = Nothing,
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
          commentOrigin = Nothing,
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
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier "get_y_o"]]}}]
