[
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_simpleGlobal_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_9e13cdab849fd6a3 (void) { return &simpleGlobal; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_9e13cdab849fd6a3",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_9e13cdab849fd6a3",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Just
        (Comment
          (Just
            [
              TextContent "Global variables"])
          (Just "globals.h:9:12")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Config",
      structConstr = HsName
        "@NsConstr"
        "Config",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "config_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:13:7",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "config_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "globals.h:13:7")
              (Just "globals.h")
              [])},
        Field {
          fieldName = HsName
            "@NsVar"
            "config_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:14:7",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "config_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "globals.h:14:7")
              (Just "globals.h")
              [])}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "globals.h:12:8",
            declId = NamePair {
              nameC = Name "config",
              nameHsIdent = HsIdentifier
                "Config"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "globals.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Config"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:13:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "config_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:14:7",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "config_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
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
        (Comment
          Nothing
          (Just "globals.h:12:8")
          (Just "globals.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Config",
          structConstr = HsName
            "@NsConstr"
            "Config",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "config_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:13:7",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "config_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "globals.h:13:7")
                  (Just "globals.h")
                  [])},
            Field {
              fieldName = HsName
                "@NsVar"
                "config_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:14:7",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "config_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "globals.h:14:7")
                  (Just "globals.h")
                  [])}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "globals.h:12:8",
                declId = NamePair {
                  nameC = Name "config",
                  nameHsIdent = HsIdentifier
                    "Config"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "globals.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Config"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:13:7",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "config_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:14:7",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "config_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
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
            (Comment
              Nothing
              (Just "globals.h:12:8")
              (Just "globals.h")
              [])}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Config",
                  structConstr = HsName
                    "@NsConstr"
                    "Config",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "config_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:13:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "config_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:13:7")
                          (Just "globals.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "config_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:14:7",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "config_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:14:7")
                          (Just "globals.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:12:8",
                        declId = NamePair {
                          nameC = Name "config",
                          nameHsIdent = HsIdentifier
                            "Config"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "globals.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Config"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:13:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "config_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:14:7",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "config_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                    (Comment
                      Nothing
                      (Just "globals.h:12:8")
                      (Just "globals.h")
                      [])})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Config",
                  structConstr = HsName
                    "@NsConstr"
                    "Config",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "config_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:13:7",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "config_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:13:7")
                          (Just "globals.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "config_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:14:7",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "config_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:14:7")
                          (Just "globals.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:12:8",
                        declId = NamePair {
                          nameC = Name "config",
                          nameHsIdent = HsIdentifier
                            "Config"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "globals.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Config"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:13:7",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "config_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:14:7",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "config_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                    (Comment
                      Nothing
                      (Just "globals.h:12:8")
                      (Just "globals.h")
                      [])}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      4
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Config",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Config",
      deriveInstanceComment =
      Nothing},
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_compoundGlobal1_ptr */ __attribute__ ((const)) struct config *hs_bindgen_test_globals_9093ee3b5b63dbb9 (void) { return &compoundGlobal1; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_9093ee3b5b63dbb9",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Config")))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_9093ee3b5b63dbb9",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeStruct
          NamePair {
            nameC = Name "config",
            nameHsIdent = HsIdentifier
              "Config"}
          NameOriginInSource),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:16:22")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Inline_struct",
      structConstr = HsName
        "@NsConstr"
        "Inline_struct",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "inline_struct_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:19:35",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "inline_struct_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "globals.h:19:35")
              (Just "globals.h")
              [])},
        Field {
          fieldName = HsName
            "@NsVar"
            "inline_struct_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:19:42",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "inline_struct_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "globals.h:19:42")
              (Just "globals.h")
              [])}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "globals.h:19:15",
            declId = NamePair {
              nameC = Name "inline_struct",
              nameHsIdent = HsIdentifier
                "Inline_struct"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "globals.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Inline_struct"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:19:35",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "inline_struct_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:19:42",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "inline_struct_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
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
        (Comment
          Nothing
          (Just "globals.h:19:15")
          (Just "globals.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Inline_struct",
          structConstr = HsName
            "@NsConstr"
            "Inline_struct",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "inline_struct_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:19:35",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "inline_struct_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "globals.h:19:35")
                  (Just "globals.h")
                  [])},
            Field {
              fieldName = HsName
                "@NsVar"
                "inline_struct_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:19:42",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "inline_struct_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "globals.h:19:42")
                  (Just "globals.h")
                  [])}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "globals.h:19:15",
                declId = NamePair {
                  nameC = Name "inline_struct",
                  nameHsIdent = HsIdentifier
                    "Inline_struct"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "globals.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "Inline_struct"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:19:35",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "inline_struct_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:19:42",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "inline_struct_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 32,
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
            (Comment
              Nothing
              (Just "globals.h:19:15")
              (Just "globals.h")
              [])}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Inline_struct",
                  structConstr = HsName
                    "@NsConstr"
                    "Inline_struct",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "inline_struct_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:19:35",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "inline_struct_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:19:35")
                          (Just "globals.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "inline_struct_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:19:42",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "inline_struct_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:19:42")
                          (Just "globals.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:19:15",
                        declId = NamePair {
                          nameC = Name "inline_struct",
                          nameHsIdent = HsIdentifier
                            "Inline_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "globals.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Inline_struct"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:19:35",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "inline_struct_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:19:42",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "inline_struct_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                    (Comment
                      Nothing
                      (Just "globals.h:19:15")
                      (Just "globals.h")
                      [])})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Inline_struct",
                  structConstr = HsName
                    "@NsConstr"
                    "Inline_struct",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "inline_struct_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:19:35",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "inline_struct_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:19:35")
                          (Just "globals.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "inline_struct_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:19:42",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "inline_struct_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:19:42")
                          (Just "globals.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:19:15",
                        declId = NamePair {
                          nameC = Name "inline_struct",
                          nameHsIdent = HsIdentifier
                            "Inline_struct"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "globals.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Inline_struct"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:19:35",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "inline_struct_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:19:42",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "inline_struct_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 32,
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
                    (Comment
                      Nothing
                      (Just "globals.h:19:15")
                      (Just "globals.h")
                      [])}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      4
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Inline_struct",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Inline_struct",
      deriveInstanceComment =
      Nothing},
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_compoundGlobal2_ptr */ __attribute__ ((const)) struct inline_struct *hs_bindgen_test_globals_35cfb530c6e3b540 (void) { return &compoundGlobal2; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_35cfb530c6e3b540",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Inline_struct")))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_35cfb530c6e3b540",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeStruct
          NamePair {
            nameC = Name "inline_struct",
            nameHsIdent = HsIdentifier
              "Inline_struct"}
          NameOriginInSource),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:19:47")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_nesInteger_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_d0e555bab6218b45 (void) { return &nesInteger; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_d0e555bab6218b45",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_d0e555bab6218b45",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment =
      Just
        (Comment
          (Just
            [
              TextContent
                "Non-extern non-static global variables"])
          (Just "globals.h:35:9")
          (Just "globals.h")
          [
            Paragraph
              [
                TextContent
                  "These kinds of variables need to be treated with care, to avoid duplicate",
                TextContent
                  "symbols, but do exist in the wild."],
            Paragraph
              [
                TextContent
                  "We test with various kinds of initializers as we must explicitly ignore them",
                TextContent
                  "in our parser. The list here roughly follows the definition of `CXCursor`",
                TextContent
                  "[1], starting at `CXCursor_IntegerLiteral`; see also definition of 'varDecl'",
                TextContent
                  "in `HsBindgen.Frontend.Pass.Parse.Decl`."],
            Paragraph
              [
                TextContent
                  "[1]: https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013"]]),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_nesFloating_ptr */ __attribute__ ((const)) float *hs_bindgen_test_globals_620d3eeb41be6814 (void) { return &nesFloating; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_620d3eeb41be6814",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCFloat))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_620d3eeb41be6814",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim
          (PrimFloating PrimFloat)),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:36:9")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_nesString1_ptr */ __attribute__ ((const)) char **hs_bindgen_test_globals_58609a874bbd4939 (void) { return &nesString1; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_58609a874bbd4939",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCChar)))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_58609a874bbd4939",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePointer
          (TypePrim
            (PrimChar
              (PrimSignImplicit
                (Just Signed))))),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:38:9")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_nesString2_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_globals_d24d15726a247083 (void))[3] { return &nesString2; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_d24d15726a247083",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              3
              (HsPrimType HsPrimCChar)))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_d24d15726a247083",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConstArray
          3
          (TypePrim
            (PrimChar
              (PrimSignImplicit
                (Just Signed))))),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:39:9")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_nesCharacter_ptr */ __attribute__ ((const)) char *hs_bindgen_test_globals_472e8cff06767166 (void) { return &nesCharacter; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_472e8cff06767166",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCChar))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_472e8cff06767166",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim
          (PrimChar
            (PrimSignImplicit
              (Just Signed)))),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:40:9")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_nesParen_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_3535fbeb41ad5a41 (void) { return &nesParen; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_3535fbeb41ad5a41",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_3535fbeb41ad5a41",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:41:9")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_nesUnary_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_c2e1dc65064ad658 (void) { return &nesUnary; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_c2e1dc65064ad658",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_c2e1dc65064ad658",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:42:9")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_nesBinary_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_3d0448526008a072 (void) { return &nesBinary; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_3d0448526008a072",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_3d0448526008a072",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:43:9")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_nesConditional_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_6733c2e7c59bc620 (void) { return &nesConditional; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_6733c2e7c59bc620",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_6733c2e7c59bc620",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim
          (PrimIntegral PrimInt Signed)),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:44:9")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_nesCast_ptr */ __attribute__ ((const)) float *hs_bindgen_test_globals_d6e6e72f287d9b41 (void) { return &nesCast; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_d6e6e72f287d9b41",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCFloat))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_d6e6e72f287d9b41",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim
          (PrimFloating PrimFloat)),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:45:9")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_nesCompound_ptr */ __attribute__ ((const)) signed int **hs_bindgen_test_globals_032905c6b7a5e39f (void) { return &nesCompound; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_032905c6b7a5e39f",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_032905c6b7a5e39f",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePointer
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:46:9")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_nesInitList_ptr */ __attribute__ ((const)) uint8_t (*hs_bindgen_test_globals_4012de1fec3423a7 (void))[4] { return &nesInitList; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_4012de1fec3423a7",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              4
              (HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "Word8"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "Word8"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]})))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_4012de1fec3423a7",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConstArray
          4
          (TypeExtBinding
            ResolvedExtBinding {
              extCName = QualName {
                qualNameName = Name "uint8_t",
                qualNameKind =
                NameKindOrdinary},
              extHsRef = ExtHsRef {
                extHsRefModule = HsModuleName
                  "HsBindgen.Runtime.Prelude",
                extHsRefIdentifier =
                HsIdentifier "Word8"},
              extHsSpec = TypeSpec {
                typeSpecModule = Just
                  (HsModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (HsIdentifier "Word8"),
                typeSpecInstances = Map.fromList
                  [
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}})),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:47:9")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_nesBool_ptr */ __attribute__ ((const)) _Bool *hs_bindgen_test_globals_f9fb23513d064767 (void) { return &nesBool; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_f9fb23513d064767",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCBool))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_f9fb23513d064767",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePrim PrimBool),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:48:9")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_streamBinary_ptr */ __attribute__ ((const)) uint8_t (*hs_bindgen_test_globals_92e68af3ae2ed3fb (void))[4096] { return &streamBinary; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_92e68af3ae2ed3fb",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              4096
              (HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "Word8"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "Word8"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]})))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_92e68af3ae2ed3fb",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConstArray
          4096
          (TypeExtBinding
            ResolvedExtBinding {
              extCName = QualName {
                qualNameName = Name "uint8_t",
                qualNameKind =
                NameKindOrdinary},
              extHsRef = ExtHsRef {
                extHsRefModule = HsModuleName
                  "HsBindgen.Runtime.Prelude",
                extHsRefIdentifier =
                HsIdentifier "Word8"},
              extHsSpec = TypeSpec {
                typeSpecModule = Just
                  (HsModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (HsIdentifier "Word8"),
                typeSpecInstances = Map.fromList
                  [
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}})),
      foreignImportComment =
      Just
        (Comment
          (Just
            [
              TextContent
                "Additional examples of global variables, abstracted from real examples"])
          (Just "globals.h:60:9")
          (Just "globals.h")
          [
            Paragraph
              [
                TextContent
                  "The `streamBinary`/`streamBinary_len` example comes from [1], and is an",
                TextContent
                  "example of a non-extern non-static global (indeed, the header does not even",
                TextContent "use",
                TextContent "",
                TextContent "once",
                TextContent "@",
                TextContent "or similar)."],
            Paragraph
              [
                TextContent
                  "[1]: https://github.com/analogdevicesinc/no-OS/blob/855c4b3c34f2297865e448661ba4fcc0931bf430/drivers/rf-transceiver/talise/firmware/talise_stream_binary.h#L322-L325"]]),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_streamBinary_len_ptr */ __attribute__ ((const)) uint32_t *hs_bindgen_test_globals_8d6f9f3043208163 (void) { return &streamBinary_len; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_8d6f9f3043208163",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsExtBinding
              ExtHsRef {
                extHsRefModule = HsModuleName
                  "HsBindgen.Runtime.Prelude",
                extHsRefIdentifier =
                HsIdentifier "Word32"}
              TypeSpec {
                typeSpecModule = Just
                  (HsModuleName
                    "HsBindgen.Runtime.Prelude"),
                typeSpecIdentifier = Just
                  (HsIdentifier "Word32"),
                typeSpecInstances = Map.fromList
                  [
                    _×_
                      Eq
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ord
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Enum
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Ix
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bounded
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Read
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Show
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Bits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      FiniteBits
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Integral
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Num
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Real
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      StaticSize
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      ReadRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      WriteRaw
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = []}),
                    _×_
                      Storable
                      (Require
                        InstanceSpec {
                          instanceSpecStrategy = Nothing,
                          instanceSpecConstraints = [
                            ]})]}))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_8d6f9f3043208163",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeExtBinding
          ResolvedExtBinding {
            extCName = QualName {
              qualNameName = Name "uint32_t",
              qualNameKind =
              NameKindOrdinary},
            extHsRef = ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "Word32"},
            extHsSpec = TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "Word32"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]}}),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:404:10")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Version_t",
      structConstr = HsName
        "@NsConstr"
        "Version_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "version_t_major",
          fieldType = HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "Word8"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "Word8"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:408:12",
                fieldName = NamePair {
                  nameC = Name "major",
                  nameHsIdent = HsIdentifier
                    "version_t_major"},
                fieldComment = Nothing},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint8_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "Word8"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "Word8"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}},
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "globals.h:408:12")
              (Just "globals.h")
              [])},
        Field {
          fieldName = HsName
            "@NsVar"
            "version_t_minor",
          fieldType = HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "Word16"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "Word16"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:409:12",
                fieldName = NamePair {
                  nameC = Name "minor",
                  nameHsIdent = HsIdentifier
                    "version_t_minor"},
                fieldComment = Nothing},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint16_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "Word16"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "Word16"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}},
              structFieldOffset = 16,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "globals.h:409:12")
              (Just "globals.h")
              [])},
        Field {
          fieldName = HsName
            "@NsVar"
            "version_t_patch",
          fieldType = HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "Word8"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "Word8"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:410:12",
                fieldName = NamePair {
                  nameC = Name "patch",
                  nameHsIdent = HsIdentifier
                    "version_t_patch"},
                fieldComment = Nothing},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint8_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "Word8"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "Word8"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}},
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "globals.h:410:12")
              (Just "globals.h")
              [])}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "globals.h:406:9",
            declId = NamePair {
              nameC = Name "version_t",
              nameHsIdent = HsIdentifier
                "Version_t"},
            declOrigin = NameOriginGenerated
              (AnonId "globals.h:406:9"),
            declAliases = [
              Name "version_t"],
            declHeader = "globals.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Version_t"),
              structSizeof = 6,
              structAlignment = 2,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:408:12",
                    fieldName = NamePair {
                      nameC = Name "major",
                      nameHsIdent = HsIdentifier
                        "version_t_major"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint8_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word8"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word8"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:409:12",
                    fieldName = NamePair {
                      nameC = Name "minor",
                      nameHsIdent = HsIdentifier
                        "version_t_minor"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint16_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word16"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word16"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:410:12",
                    fieldName = NamePair {
                      nameC = Name "patch",
                      nameHsIdent = HsIdentifier
                        "version_t_patch"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint8_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word8"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word8"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 32,
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
        (Comment
          Nothing
          (Just "globals.h:406:9")
          (Just "globals.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Version_t",
          structConstr = HsName
            "@NsConstr"
            "Version_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "version_t_major",
              fieldType = HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "Word8"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "Word8"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]},
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:408:12",
                    fieldName = NamePair {
                      nameC = Name "major",
                      nameHsIdent = HsIdentifier
                        "version_t_major"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint8_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word8"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word8"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "globals.h:408:12")
                  (Just "globals.h")
                  [])},
            Field {
              fieldName = HsName
                "@NsVar"
                "version_t_minor",
              fieldType = HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "Word16"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "Word16"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]},
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:409:12",
                    fieldName = NamePair {
                      nameC = Name "minor",
                      nameHsIdent = HsIdentifier
                        "version_t_minor"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint16_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word16"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word16"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "globals.h:409:12")
                  (Just "globals.h")
                  [])},
            Field {
              fieldName = HsName
                "@NsVar"
                "version_t_patch",
              fieldType = HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "Word8"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "Word8"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]},
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:410:12",
                    fieldName = NamePair {
                      nameC = Name "patch",
                      nameHsIdent = HsIdentifier
                        "version_t_patch"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint8_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word8"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word8"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "globals.h:410:12")
                  (Just "globals.h")
                  [])}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "globals.h:406:9",
                declId = NamePair {
                  nameC = Name "version_t",
                  nameHsIdent = HsIdentifier
                    "Version_t"},
                declOrigin = NameOriginGenerated
                  (AnonId "globals.h:406:9"),
                declAliases = [
                  Name "version_t"],
                declHeader = "globals.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "Version_t"),
                  structSizeof = 6,
                  structAlignment = 2,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:408:12",
                        fieldName = NamePair {
                          nameC = Name "major",
                          nameHsIdent = HsIdentifier
                            "version_t_major"},
                        fieldComment = Nothing},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint8_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtHsRef {
                            extHsRefModule = HsModuleName
                              "HsBindgen.Runtime.Prelude",
                            extHsRefIdentifier =
                            HsIdentifier "Word8"},
                          extHsSpec = TypeSpec {
                            typeSpecModule = Just
                              (HsModuleName
                                "HsBindgen.Runtime.Prelude"),
                            typeSpecIdentifier = Just
                              (HsIdentifier "Word8"),
                            typeSpecInstances = Map.fromList
                              [
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}},
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:409:12",
                        fieldName = NamePair {
                          nameC = Name "minor",
                          nameHsIdent = HsIdentifier
                            "version_t_minor"},
                        fieldComment = Nothing},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint16_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtHsRef {
                            extHsRefModule = HsModuleName
                              "HsBindgen.Runtime.Prelude",
                            extHsRefIdentifier =
                            HsIdentifier "Word16"},
                          extHsSpec = TypeSpec {
                            typeSpecModule = Just
                              (HsModuleName
                                "HsBindgen.Runtime.Prelude"),
                            typeSpecIdentifier = Just
                              (HsIdentifier "Word16"),
                            typeSpecInstances = Map.fromList
                              [
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}},
                      structFieldOffset = 16,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:410:12",
                        fieldName = NamePair {
                          nameC = Name "patch",
                          nameHsIdent = HsIdentifier
                            "version_t_patch"},
                        fieldComment = Nothing},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint8_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtHsRef {
                            extHsRefModule = HsModuleName
                              "HsBindgen.Runtime.Prelude",
                            extHsRefIdentifier =
                            HsIdentifier "Word8"},
                          extHsSpec = TypeSpec {
                            typeSpecModule = Just
                              (HsModuleName
                                "HsBindgen.Runtime.Prelude"),
                            typeSpecIdentifier = Just
                              (HsIdentifier "Word8"),
                            typeSpecInstances = Map.fromList
                              [
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}},
                      structFieldOffset = 32,
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
            (Comment
              Nothing
              (Just "globals.h:406:9")
              (Just "globals.h")
              [])}
        StorableInstance {
          storableSizeOf = 6,
          storableAlignment = 2,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Version_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Version_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "version_t_major",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word8"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word8"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:408:12",
                            fieldName = NamePair {
                              nameC = Name "major",
                              nameHsIdent = HsIdentifier
                                "version_t_major"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint8_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word8"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word8"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:408:12")
                          (Just "globals.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "version_t_minor",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word16"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word16"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:409:12",
                            fieldName = NamePair {
                              nameC = Name "minor",
                              nameHsIdent = HsIdentifier
                                "version_t_minor"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint16_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word16"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word16"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:409:12")
                          (Just "globals.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "version_t_patch",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word8"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word8"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:410:12",
                            fieldName = NamePair {
                              nameC = Name "patch",
                              nameHsIdent = HsIdentifier
                                "version_t_patch"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint8_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word8"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word8"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:410:12")
                          (Just "globals.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:406:9",
                        declId = NamePair {
                          nameC = Name "version_t",
                          nameHsIdent = HsIdentifier
                            "Version_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "globals.h:406:9"),
                        declAliases = [
                          Name "version_t"],
                        declHeader = "globals.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Version_t"),
                          structSizeof = 6,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:408:12",
                                fieldName = NamePair {
                                  nameC = Name "major",
                                  nameHsIdent = HsIdentifier
                                    "version_t_major"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint8_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word8"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word8"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:409:12",
                                fieldName = NamePair {
                                  nameC = Name "minor",
                                  nameHsIdent = HsIdentifier
                                    "version_t_minor"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint16_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word16"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word16"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 16,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:410:12",
                                fieldName = NamePair {
                                  nameC = Name "patch",
                                  nameHsIdent = HsIdentifier
                                    "version_t_patch"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint8_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word8"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word8"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 32,
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
                    (Comment
                      Nothing
                      (Just "globals.h:406:9")
                      (Just "globals.h")
                      [])})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 2,
                PeekByteOff (Idx 0) 4]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Version_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Version_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "version_t_major",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word8"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word8"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:408:12",
                            fieldName = NamePair {
                              nameC = Name "major",
                              nameHsIdent = HsIdentifier
                                "version_t_major"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint8_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word8"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word8"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:408:12")
                          (Just "globals.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "version_t_minor",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word16"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word16"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:409:12",
                            fieldName = NamePair {
                              nameC = Name "minor",
                              nameHsIdent = HsIdentifier
                                "version_t_minor"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint16_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word16"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word16"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:409:12")
                          (Just "globals.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "version_t_patch",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word8"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word8"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:410:12",
                            fieldName = NamePair {
                              nameC = Name "patch",
                              nameHsIdent = HsIdentifier
                                "version_t_patch"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint8_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word8"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word8"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:410:12")
                          (Just "globals.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:406:9",
                        declId = NamePair {
                          nameC = Name "version_t",
                          nameHsIdent = HsIdentifier
                            "Version_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "globals.h:406:9"),
                        declAliases = [
                          Name "version_t"],
                        declHeader = "globals.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Version_t"),
                          structSizeof = 6,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:408:12",
                                fieldName = NamePair {
                                  nameC = Name "major",
                                  nameHsIdent = HsIdentifier
                                    "version_t_major"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint8_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word8"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word8"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:409:12",
                                fieldName = NamePair {
                                  nameC = Name "minor",
                                  nameHsIdent = HsIdentifier
                                    "version_t_minor"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint16_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word16"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word16"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 16,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:410:12",
                                fieldName = NamePair {
                                  nameC = Name "patch",
                                  nameHsIdent = HsIdentifier
                                    "version_t_patch"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint8_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word8"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word8"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 32,
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
                    (Comment
                      Nothing
                      (Just "globals.h:406:9")
                      (Just "globals.h")
                      [])}
                (Add 3)
                (Seq
                  [
                    PokeByteOff (Idx 4) 0 (Idx 0),
                    PokeByteOff (Idx 4) 2 (Idx 1),
                    PokeByteOff
                      (Idx 4)
                      4
                      (Idx 2)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Version_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Version_t",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct1_t",
      structConstr = HsName
        "@NsConstr"
        "Struct1_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "struct1_t_x",
          fieldType = HsExtBinding
            ExtHsRef {
              extHsRefModule = HsModuleName
                "HsBindgen.Runtime.Prelude",
              extHsRefIdentifier =
              HsIdentifier "Word16"}
            TypeSpec {
              typeSpecModule = Just
                (HsModuleName
                  "HsBindgen.Runtime.Prelude"),
              typeSpecIdentifier = Just
                (HsIdentifier "Word16"),
              typeSpecInstances = Map.fromList
                [
                  _×_
                    Eq
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ord
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Enum
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Ix
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bounded
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Read
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Show
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Bits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    FiniteBits
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Integral
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Num
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Real
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    StaticSize
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    ReadRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    WriteRaw
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = []}),
                  _×_
                    Storable
                    (Require
                      InstanceSpec {
                        instanceSpecStrategy = Nothing,
                        instanceSpecConstraints = [
                          ]})]},
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:415:13",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "struct1_t_x"},
                fieldComment = Nothing},
              structFieldType = TypeExtBinding
                ResolvedExtBinding {
                  extCName = QualName {
                    qualNameName = Name "uint16_t",
                    qualNameKind =
                    NameKindOrdinary},
                  extHsRef = ExtHsRef {
                    extHsRefModule = HsModuleName
                      "HsBindgen.Runtime.Prelude",
                    extHsRefIdentifier =
                    HsIdentifier "Word16"},
                  extHsSpec = TypeSpec {
                    typeSpecModule = Just
                      (HsModuleName
                        "HsBindgen.Runtime.Prelude"),
                    typeSpecIdentifier = Just
                      (HsIdentifier "Word16"),
                    typeSpecInstances = Map.fromList
                      [
                        _×_
                          Eq
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ord
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Enum
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Ix
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bounded
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Read
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Show
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Bits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          FiniteBits
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Integral
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Num
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Real
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          StaticSize
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          ReadRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          WriteRaw
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = []}),
                        _×_
                          Storable
                          (Require
                            InstanceSpec {
                              instanceSpecStrategy = Nothing,
                              instanceSpecConstraints = [
                                ]})]}},
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "globals.h:415:13")
              (Just "globals.h")
              [])},
        Field {
          fieldName = HsName
            "@NsVar"
            "struct1_t_y",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:416:13",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "struct1_t_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                PrimBool,
              structFieldOffset = 16,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "globals.h:416:13")
              (Just "globals.h")
              [])},
        Field {
          fieldName = HsName
            "@NsVar"
            "struct1_t_version",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Version_t"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:417:13",
                fieldName = NamePair {
                  nameC = Name "version",
                  nameHsIdent = HsIdentifier
                    "struct1_t_version"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name "version_t")
                  (TypeStruct
                    NamePair {
                      nameC = Name "version_t",
                      nameHsIdent = HsIdentifier
                        "Version_t"}
                    (NameOriginGenerated
                      (AnonId "globals.h:406:9")))),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "globals.h:417:13")
              (Just "globals.h")
              [])}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "globals.h:413:9",
            declId = NamePair {
              nameC = Name "struct1_t",
              nameHsIdent = HsIdentifier
                "Struct1_t"},
            declOrigin = NameOriginGenerated
              (AnonId "globals.h:413:9"),
            declAliases = [
              Name "struct1_t"],
            declHeader = "globals.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Struct1_t"),
              structSizeof = 10,
              structAlignment = 2,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:415:13",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "struct1_t_x"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint16_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word16"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word16"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:416:13",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "struct1_t_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:417:13",
                    fieldName = NamePair {
                      nameC = Name "version",
                      nameHsIdent = HsIdentifier
                        "struct1_t_version"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "version_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "version_t",
                          nameHsIdent = HsIdentifier
                            "Version_t"}
                        (NameOriginGenerated
                          (AnonId "globals.h:406:9")))),
                  structFieldOffset = 32,
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
        (Comment
          Nothing
          (Just "globals.h:413:9")
          (Just "globals.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Struct1_t",
          structConstr = HsName
            "@NsConstr"
            "Struct1_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "struct1_t_x",
              fieldType = HsExtBinding
                ExtHsRef {
                  extHsRefModule = HsModuleName
                    "HsBindgen.Runtime.Prelude",
                  extHsRefIdentifier =
                  HsIdentifier "Word16"}
                TypeSpec {
                  typeSpecModule = Just
                    (HsModuleName
                      "HsBindgen.Runtime.Prelude"),
                  typeSpecIdentifier = Just
                    (HsIdentifier "Word16"),
                  typeSpecInstances = Map.fromList
                    [
                      _×_
                        Eq
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ord
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Enum
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Ix
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bounded
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Read
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Show
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Bits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        FiniteBits
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Integral
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Num
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Real
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        StaticSize
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        ReadRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        WriteRaw
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = []}),
                      _×_
                        Storable
                        (Require
                          InstanceSpec {
                            instanceSpecStrategy = Nothing,
                            instanceSpecConstraints = [
                              ]})]},
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:415:13",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "struct1_t_x"},
                    fieldComment = Nothing},
                  structFieldType = TypeExtBinding
                    ResolvedExtBinding {
                      extCName = QualName {
                        qualNameName = Name "uint16_t",
                        qualNameKind =
                        NameKindOrdinary},
                      extHsRef = ExtHsRef {
                        extHsRefModule = HsModuleName
                          "HsBindgen.Runtime.Prelude",
                        extHsRefIdentifier =
                        HsIdentifier "Word16"},
                      extHsSpec = TypeSpec {
                        typeSpecModule = Just
                          (HsModuleName
                            "HsBindgen.Runtime.Prelude"),
                        typeSpecIdentifier = Just
                          (HsIdentifier "Word16"),
                        typeSpecInstances = Map.fromList
                          [
                            _×_
                              Eq
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ord
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Enum
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Ix
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bounded
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Read
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Show
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Bits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              FiniteBits
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Integral
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Num
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Real
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              StaticSize
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              ReadRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              WriteRaw
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = []}),
                            _×_
                              Storable
                              (Require
                                InstanceSpec {
                                  instanceSpecStrategy = Nothing,
                                  instanceSpecConstraints = [
                                    ]})]}},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "globals.h:415:13")
                  (Just "globals.h")
                  [])},
            Field {
              fieldName = HsName
                "@NsVar"
                "struct1_t_y",
              fieldType = HsPrimType
                HsPrimCBool,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:416:13",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "struct1_t_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    PrimBool,
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "globals.h:416:13")
                  (Just "globals.h")
                  [])},
            Field {
              fieldName = HsName
                "@NsVar"
                "struct1_t_version",
              fieldType = HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Version_t"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:417:13",
                    fieldName = NamePair {
                      nameC = Name "version",
                      nameHsIdent = HsIdentifier
                        "struct1_t_version"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "version_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "version_t",
                          nameHsIdent = HsIdentifier
                            "Version_t"}
                        (NameOriginGenerated
                          (AnonId "globals.h:406:9")))),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "globals.h:417:13")
                  (Just "globals.h")
                  [])}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "globals.h:413:9",
                declId = NamePair {
                  nameC = Name "struct1_t",
                  nameHsIdent = HsIdentifier
                    "Struct1_t"},
                declOrigin = NameOriginGenerated
                  (AnonId "globals.h:413:9"),
                declAliases = [
                  Name "struct1_t"],
                declHeader = "globals.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "Struct1_t"),
                  structSizeof = 10,
                  structAlignment = 2,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:415:13",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "struct1_t_x"},
                        fieldComment = Nothing},
                      structFieldType = TypeExtBinding
                        ResolvedExtBinding {
                          extCName = QualName {
                            qualNameName = Name "uint16_t",
                            qualNameKind =
                            NameKindOrdinary},
                          extHsRef = ExtHsRef {
                            extHsRefModule = HsModuleName
                              "HsBindgen.Runtime.Prelude",
                            extHsRefIdentifier =
                            HsIdentifier "Word16"},
                          extHsSpec = TypeSpec {
                            typeSpecModule = Just
                              (HsModuleName
                                "HsBindgen.Runtime.Prelude"),
                            typeSpecIdentifier = Just
                              (HsIdentifier "Word16"),
                            typeSpecInstances = Map.fromList
                              [
                                _×_
                                  Eq
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ord
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Enum
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Ix
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bounded
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Read
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Show
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Bits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  FiniteBits
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Integral
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Num
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Real
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  StaticSize
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  ReadRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  WriteRaw
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = []}),
                                _×_
                                  Storable
                                  (Require
                                    InstanceSpec {
                                      instanceSpecStrategy = Nothing,
                                      instanceSpecConstraints = [
                                        ]})]}},
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:416:13",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "struct1_t_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        PrimBool,
                      structFieldOffset = 16,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:417:13",
                        fieldName = NamePair {
                          nameC = Name "version",
                          nameHsIdent = HsIdentifier
                            "struct1_t_version"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefSquashed
                          (Name "version_t")
                          (TypeStruct
                            NamePair {
                              nameC = Name "version_t",
                              nameHsIdent = HsIdentifier
                                "Version_t"}
                            (NameOriginGenerated
                              (AnonId "globals.h:406:9")))),
                      structFieldOffset = 32,
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
            (Comment
              Nothing
              (Just "globals.h:413:9")
              (Just "globals.h")
              [])}
        StorableInstance {
          storableSizeOf = 10,
          storableAlignment = 2,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Struct1_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Struct1_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "struct1_t_x",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word16"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word16"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:415:13",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "struct1_t_x"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint16_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word16"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word16"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:415:13")
                          (Just "globals.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "struct1_t_y",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:416:13",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "struct1_t_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:416:13")
                          (Just "globals.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "struct1_t_version",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Version_t"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:417:13",
                            fieldName = NamePair {
                              nameC = Name "version",
                              nameHsIdent = HsIdentifier
                                "struct1_t_version"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name "version_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name "version_t",
                                  nameHsIdent = HsIdentifier
                                    "Version_t"}
                                (NameOriginGenerated
                                  (AnonId "globals.h:406:9")))),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:417:13")
                          (Just "globals.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:413:9",
                        declId = NamePair {
                          nameC = Name "struct1_t",
                          nameHsIdent = HsIdentifier
                            "Struct1_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "globals.h:413:9"),
                        declAliases = [
                          Name "struct1_t"],
                        declHeader = "globals.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Struct1_t"),
                          structSizeof = 10,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:415:13",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "struct1_t_x"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint16_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word16"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word16"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:416:13",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "struct1_t_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 16,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:417:13",
                                fieldName = NamePair {
                                  nameC = Name "version",
                                  nameHsIdent = HsIdentifier
                                    "struct1_t_version"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name "version_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "version_t",
                                      nameHsIdent = HsIdentifier
                                        "Version_t"}
                                    (NameOriginGenerated
                                      (AnonId "globals.h:406:9")))),
                              structFieldOffset = 32,
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
                    (Comment
                      Nothing
                      (Just "globals.h:413:9")
                      (Just "globals.h")
                      [])})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 2,
                PeekByteOff (Idx 0) 4]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Struct1_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Struct1_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "struct1_t_x",
                      fieldType = HsExtBinding
                        ExtHsRef {
                          extHsRefModule = HsModuleName
                            "HsBindgen.Runtime.Prelude",
                          extHsRefIdentifier =
                          HsIdentifier "Word16"}
                        TypeSpec {
                          typeSpecModule = Just
                            (HsModuleName
                              "HsBindgen.Runtime.Prelude"),
                          typeSpecIdentifier = Just
                            (HsIdentifier "Word16"),
                          typeSpecInstances = Map.fromList
                            [
                              _×_
                                Eq
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ord
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Enum
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Ix
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bounded
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Read
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Show
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Bits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                FiniteBits
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Integral
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Num
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Real
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                StaticSize
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                ReadRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                WriteRaw
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = []}),
                              _×_
                                Storable
                                (Require
                                  InstanceSpec {
                                    instanceSpecStrategy = Nothing,
                                    instanceSpecConstraints = [
                                      ]})]},
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:415:13",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "struct1_t_x"},
                            fieldComment = Nothing},
                          structFieldType = TypeExtBinding
                            ResolvedExtBinding {
                              extCName = QualName {
                                qualNameName = Name "uint16_t",
                                qualNameKind =
                                NameKindOrdinary},
                              extHsRef = ExtHsRef {
                                extHsRefModule = HsModuleName
                                  "HsBindgen.Runtime.Prelude",
                                extHsRefIdentifier =
                                HsIdentifier "Word16"},
                              extHsSpec = TypeSpec {
                                typeSpecModule = Just
                                  (HsModuleName
                                    "HsBindgen.Runtime.Prelude"),
                                typeSpecIdentifier = Just
                                  (HsIdentifier "Word16"),
                                typeSpecInstances = Map.fromList
                                  [
                                    _×_
                                      Eq
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ord
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Enum
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Ix
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bounded
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Read
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Show
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Bits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      FiniteBits
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Integral
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Num
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Real
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      StaticSize
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      ReadRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      WriteRaw
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = []}),
                                    _×_
                                      Storable
                                      (Require
                                        InstanceSpec {
                                          instanceSpecStrategy = Nothing,
                                          instanceSpecConstraints = [
                                            ]})]}},
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:415:13")
                          (Just "globals.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "struct1_t_y",
                      fieldType = HsPrimType
                        HsPrimCBool,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:416:13",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "struct1_t_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            PrimBool,
                          structFieldOffset = 16,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:416:13")
                          (Just "globals.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "struct1_t_version",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Version_t"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:417:13",
                            fieldName = NamePair {
                              nameC = Name "version",
                              nameHsIdent = HsIdentifier
                                "struct1_t_version"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name "version_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name "version_t",
                                  nameHsIdent = HsIdentifier
                                    "Version_t"}
                                (NameOriginGenerated
                                  (AnonId "globals.h:406:9")))),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:417:13")
                          (Just "globals.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:413:9",
                        declId = NamePair {
                          nameC = Name "struct1_t",
                          nameHsIdent = HsIdentifier
                            "Struct1_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "globals.h:413:9"),
                        declAliases = [
                          Name "struct1_t"],
                        declHeader = "globals.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Struct1_t"),
                          structSizeof = 10,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:415:13",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "struct1_t_x"},
                                fieldComment = Nothing},
                              structFieldType = TypeExtBinding
                                ResolvedExtBinding {
                                  extCName = QualName {
                                    qualNameName = Name "uint16_t",
                                    qualNameKind =
                                    NameKindOrdinary},
                                  extHsRef = ExtHsRef {
                                    extHsRefModule = HsModuleName
                                      "HsBindgen.Runtime.Prelude",
                                    extHsRefIdentifier =
                                    HsIdentifier "Word16"},
                                  extHsSpec = TypeSpec {
                                    typeSpecModule = Just
                                      (HsModuleName
                                        "HsBindgen.Runtime.Prelude"),
                                    typeSpecIdentifier = Just
                                      (HsIdentifier "Word16"),
                                    typeSpecInstances = Map.fromList
                                      [
                                        _×_
                                          Eq
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ord
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Enum
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Ix
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bounded
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Read
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Show
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Bits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          FiniteBits
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Integral
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Num
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Real
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          StaticSize
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          ReadRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          WriteRaw
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = []}),
                                        _×_
                                          Storable
                                          (Require
                                            InstanceSpec {
                                              instanceSpecStrategy = Nothing,
                                              instanceSpecConstraints = [
                                                ]})]}},
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:416:13",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "struct1_t_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                PrimBool,
                              structFieldOffset = 16,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:417:13",
                                fieldName = NamePair {
                                  nameC = Name "version",
                                  nameHsIdent = HsIdentifier
                                    "struct1_t_version"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name "version_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "version_t",
                                      nameHsIdent = HsIdentifier
                                        "Version_t"}
                                    (NameOriginGenerated
                                      (AnonId "globals.h:406:9")))),
                              structFieldOffset = 32,
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
                    (Comment
                      Nothing
                      (Just "globals.h:413:9")
                      (Just "globals.h")
                      [])}
                (Add 3)
                (Seq
                  [
                    PokeByteOff (Idx 4) 0 (Idx 0),
                    PokeByteOff (Idx 4) 2 (Idx 1),
                    PokeByteOff
                      (Idx 4)
                      4
                      (Idx 2)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct1_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct1_t",
      deriveInstanceComment =
      Nothing},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Struct2_t",
      structConstr = HsName
        "@NsConstr"
        "Struct2_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "struct2_t_field1",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Struct1_t"),
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:422:13",
                fieldName = NamePair {
                  nameC = Name "field1",
                  nameHsIdent = HsIdentifier
                    "struct2_t_field1"},
                fieldComment = Nothing},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name "struct1_t")
                  (TypeStruct
                    NamePair {
                      nameC = Name "struct1_t",
                      nameHsIdent = HsIdentifier
                        "Struct1_t"}
                    (NameOriginGenerated
                      (AnonId "globals.h:413:9")))),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "globals.h:422:13")
              (Just "globals.h")
              [])}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "globals.h:420:9",
            declId = NamePair {
              nameC = Name "struct2_t",
              nameHsIdent = HsIdentifier
                "Struct2_t"},
            declOrigin = NameOriginGenerated
              (AnonId "globals.h:420:9"),
            declAliases = [
              Name "struct2_t"],
            declHeader = "globals.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Struct2_t"),
              structSizeof = 10,
              structAlignment = 2,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:422:13",
                    fieldName = NamePair {
                      nameC = Name "field1",
                      nameHsIdent = HsIdentifier
                        "struct2_t_field1"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "struct1_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "struct1_t",
                          nameHsIdent = HsIdentifier
                            "Struct1_t"}
                        (NameOriginGenerated
                          (AnonId "globals.h:413:9")))),
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
        (Comment
          Nothing
          (Just "globals.h:420:9")
          (Just "globals.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Struct2_t",
          structConstr = HsName
            "@NsConstr"
            "Struct2_t",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "struct2_t_field1",
              fieldType = HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Struct1_t"),
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:422:13",
                    fieldName = NamePair {
                      nameC = Name "field1",
                      nameHsIdent = HsIdentifier
                        "struct2_t_field1"},
                    fieldComment = Nothing},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "struct1_t")
                      (TypeStruct
                        NamePair {
                          nameC = Name "struct1_t",
                          nameHsIdent = HsIdentifier
                            "Struct1_t"}
                        (NameOriginGenerated
                          (AnonId "globals.h:413:9")))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "globals.h:422:13")
                  (Just "globals.h")
                  [])}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "globals.h:420:9",
                declId = NamePair {
                  nameC = Name "struct2_t",
                  nameHsIdent = HsIdentifier
                    "Struct2_t"},
                declOrigin = NameOriginGenerated
                  (AnonId "globals.h:420:9"),
                declAliases = [
                  Name "struct2_t"],
                declHeader = "globals.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName
                      "@NsConstr"
                      "Struct2_t"),
                  structSizeof = 10,
                  structAlignment = 2,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:422:13",
                        fieldName = NamePair {
                          nameC = Name "field1",
                          nameHsIdent = HsIdentifier
                            "struct2_t_field1"},
                        fieldComment = Nothing},
                      structFieldType = TypeTypedef
                        (TypedefSquashed
                          (Name "struct1_t")
                          (TypeStruct
                            NamePair {
                              nameC = Name "struct1_t",
                              nameHsIdent = HsIdentifier
                                "Struct1_t"}
                            (NameOriginGenerated
                              (AnonId "globals.h:413:9")))),
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
            (Comment
              Nothing
              (Just "globals.h:420:9")
              (Just "globals.h")
              [])}
        StorableInstance {
          storableSizeOf = 10,
          storableAlignment = 2,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Struct2_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Struct2_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "struct2_t_field1",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Struct1_t"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:422:13",
                            fieldName = NamePair {
                              nameC = Name "field1",
                              nameHsIdent = HsIdentifier
                                "struct2_t_field1"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name "struct1_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name "struct1_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct1_t"}
                                (NameOriginGenerated
                                  (AnonId "globals.h:413:9")))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:422:13")
                          (Just "globals.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:420:9",
                        declId = NamePair {
                          nameC = Name "struct2_t",
                          nameHsIdent = HsIdentifier
                            "Struct2_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "globals.h:420:9"),
                        declAliases = [
                          Name "struct2_t"],
                        declHeader = "globals.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Struct2_t"),
                          structSizeof = 10,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:422:13",
                                fieldName = NamePair {
                                  nameC = Name "field1",
                                  nameHsIdent = HsIdentifier
                                    "struct2_t_field1"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name "struct1_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "struct1_t",
                                      nameHsIdent = HsIdentifier
                                        "Struct1_t"}
                                    (NameOriginGenerated
                                      (AnonId "globals.h:413:9")))),
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
                    (Comment
                      Nothing
                      (Just "globals.h:420:9")
                      (Just "globals.h")
                      [])})
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
                    "Struct2_t",
                  structConstr = HsName
                    "@NsConstr"
                    "Struct2_t",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "struct2_t_field1",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Struct1_t"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:422:13",
                            fieldName = NamePair {
                              nameC = Name "field1",
                              nameHsIdent = HsIdentifier
                                "struct2_t_field1"},
                            fieldComment = Nothing},
                          structFieldType = TypeTypedef
                            (TypedefSquashed
                              (Name "struct1_t")
                              (TypeStruct
                                NamePair {
                                  nameC = Name "struct1_t",
                                  nameHsIdent = HsIdentifier
                                    "Struct1_t"}
                                (NameOriginGenerated
                                  (AnonId "globals.h:413:9")))),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:422:13")
                          (Just "globals.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:420:9",
                        declId = NamePair {
                          nameC = Name "struct2_t",
                          nameHsIdent = HsIdentifier
                            "Struct2_t"},
                        declOrigin = NameOriginGenerated
                          (AnonId "globals.h:420:9"),
                        declAliases = [
                          Name "struct2_t"],
                        declHeader = "globals.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName
                              "@NsConstr"
                              "Struct2_t"),
                          structSizeof = 10,
                          structAlignment = 2,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:422:13",
                                fieldName = NamePair {
                                  nameC = Name "field1",
                                  nameHsIdent = HsIdentifier
                                    "struct2_t_field1"},
                                fieldComment = Nothing},
                              structFieldType = TypeTypedef
                                (TypedefSquashed
                                  (Name "struct1_t")
                                  (TypeStruct
                                    NamePair {
                                      nameC = Name "struct1_t",
                                      nameHsIdent = HsIdentifier
                                        "Struct1_t"}
                                    (NameOriginGenerated
                                      (AnonId "globals.h:413:9")))),
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
                    (Comment
                      Nothing
                      (Just "globals.h:420:9")
                      (Just "globals.h")
                      [])}
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
        "Struct2_t",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Struct2_t",
      deriveInstanceComment =
      Nothing},
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_some_global_struct_ptr */ __attribute__ ((const)) struct2_t *hs_bindgen_test_globals_88ad1f87a451c285 (void) { return &some_global_struct; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_88ad1f87a451c285",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Struct2_t")))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_88ad1f87a451c285",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefSquashed
            (Name "struct2_t")
            (TypeStruct
              NamePair {
                nameC = Name "struct2_t",
                nameHsIdent = HsIdentifier
                  "Struct2_t"}
              (NameOriginGenerated
                (AnonId "globals.h:420:9"))))),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:425:11")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_globalConstant_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_globals_2875ba0f7feba4fd (void) { return &globalConstant; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_2875ba0f7feba4fd",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_2875ba0f7feba4fd",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConst
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment =
      Just
        (Comment
          (Just [TextContent "Constant"])
          (Just "globals.h:445:18")
          (Just "globals.h")
          [
            Paragraph
              [
                TextContent
                  "Although this is a constant, we don't expect an initializer (since it's",
                TextContent "`extern`)."]]),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "ConstInt",
      newtypeConstr = HsName
        "@NsConstr"
        "ConstInt",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_ConstInt",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "globals.h:448:19",
          declId = NamePair {
            nameC = Name "ConstInt",
            nameHsIdent = HsIdentifier
              "ConstInt"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "globals.h",
          declComment = Just
            [
              Paragraph
                [
                  TextContent
                    "Constant, through typedef"]]},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "ConstInt",
              newtypeField = HsName
                "@NsVar"
                "un_ConstInt"},
            typedefType = TypeConst
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Enum,
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable],
      newtypeComment = Just
        (Comment
          (Just
            [
              TextContent
                "Constant, through typedef"])
          (Just "globals.h:448:19")
          (Just "globals.h")
          [])},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Storable,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Ord,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Read,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Enum,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Ix,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bounded,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Bits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass =
      FiniteBits,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Integral,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Num,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveNewtype,
      deriveInstanceClass = Real,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ConstInt",
      deriveInstanceComment =
      Nothing},
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_anotherGlobalConstant_ptr */ __attribute__ ((const)) ConstInt *hs_bindgen_test_globals_6ebecf881bce1334 (void) { return &anotherGlobalConstant; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_6ebecf881bce1334",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "ConstInt")))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_6ebecf881bce1334",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefRegular
            NamePair {
              nameC = Name "ConstInt",
              nameHsIdent = HsIdentifier
                "ConstInt"})),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:449:17")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_staticConst_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_globals_2eea936ed4beec74 (void) { return &staticConst; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_2eea936ed4beec74",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_2eea936ed4beec74",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConst
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment =
      Just
        (Comment
          (Just
            [
              TextContent
                "Constant, but local to the file"])
          (Just "globals.h:454:18")
          (Just "globals.h")
          [
            Paragraph
              [
                TextContent
                  "Unlike with `extern`, in this we _do_ expect an initializer."]]),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_classless_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_globals_5d631acbb16c0e7e (void) { return &classless; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_5d631acbb16c0e7e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_5d631acbb16c0e7e",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConst
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportComment = Just
        (Comment
          (Just
            [
              TextContent
                "No storage class specified"])
          (Just "globals.h:457:11")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_constArray1_ptr */ __attribute__ ((const)) signed int const (*hs_bindgen_test_globals_0d7a9340f4ef8b2e (void))[4] { return &constArray1; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_0d7a9340f4ef8b2e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsConstArray
              4
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_0d7a9340f4ef8b2e",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConstArray
          4
          (TypeConst
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Just
        (Comment
          (Just
            [
              TextContent
                "A an array of size 4 containing constant integers"])
          (Just "globals.h:460:18")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "ConstIntArray",
      newtypeConstr = HsName
        "@NsConstr"
        "ConstIntArray",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_ConstIntArray",
        fieldType = HsIncompleteArray
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "globals.h:463:19",
          declId = NamePair {
            nameC = Name "ConstIntArray",
            nameHsIdent = HsIdentifier
              "ConstIntArray"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "globals.h",
          declComment = Just
            [
              Paragraph
                [
                  TextContent
                    "An array of uknown size containing constant integers"]]},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "ConstIntArray",
              newtypeField = HsName
                "@NsVar"
                "un_ConstIntArray"},
            typedefType =
            TypeIncompleteArray
              (TypeConst
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
        [Eq, Show],
      newtypeComment = Just
        (Comment
          (Just
            [
              TextContent
                "An array of uknown size containing constant integers"])
          (Just "globals.h:463:19")
          (Just "globals.h")
          [])},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ConstIntArray",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "ConstIntArray",
      deriveInstanceComment =
      Nothing},
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_constArray2_ptr */ __attribute__ ((const)) ConstIntArray *hs_bindgen_test_globals_7e09340985caec8d (void) { return &constArray2; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_7e09340985caec8d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "ConstIntArray")))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_7e09340985caec8d",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeTypedef
          (TypedefRegular
            NamePair {
              nameC = Name "ConstIntArray",
              nameHsIdent = HsIdentifier
                "ConstIntArray"})),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "globals.h:464:22")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Tuple",
      structConstr = HsName
        "@NsConstr"
        "Tuple",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "tuple_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:466:20",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "tuple_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "globals.h:466:20")
              (Just "globals.h")
              [])},
        Field {
          fieldName = HsName
            "@NsVar"
            "tuple_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "globals.h:466:33",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "tuple_y"},
                fieldComment = Nothing},
              structFieldType = TypeConst
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              structFieldOffset = 32,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "globals.h:466:33")
              (Just "globals.h")
              [])}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "globals.h:466:8",
            declId = NamePair {
              nameC = Name "tuple",
              nameHsIdent = HsIdentifier
                "Tuple"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "globals.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Tuple"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:466:20",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "tuple_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:466:33",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "tuple_y"},
                    fieldComment = Nothing},
                  structFieldType = TypeConst
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 32,
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
        (Comment
          Nothing
          (Just "globals.h:466:8")
          (Just "globals.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Tuple",
          structConstr = HsName
            "@NsConstr"
            "Tuple",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "tuple_x",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:466:20",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "tuple_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "globals.h:466:20")
                  (Just "globals.h")
                  [])},
            Field {
              fieldName = HsName
                "@NsVar"
                "tuple_y",
              fieldType = HsPrimType
                HsPrimCInt,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "globals.h:466:33",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "tuple_y"},
                    fieldComment = Nothing},
                  structFieldType = TypeConst
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "globals.h:466:33")
                  (Just "globals.h")
                  [])}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "globals.h:466:8",
                declId = NamePair {
                  nameC = Name "tuple",
                  nameHsIdent = HsIdentifier
                    "Tuple"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "globals.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Tuple"),
                  structSizeof = 8,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:466:20",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "tuple_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimIntegral PrimInt Signed),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "globals.h:466:33",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "tuple_y"},
                        fieldComment = Nothing},
                      structFieldType = TypeConst
                        (TypePrim
                          (PrimIntegral PrimInt Signed)),
                      structFieldOffset = 32,
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
            (Comment
              Nothing
              (Just "globals.h:466:8")
              (Just "globals.h")
              [])}
        StorableInstance {
          storableSizeOf = 8,
          storableAlignment = 4,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Tuple",
                  structConstr = HsName
                    "@NsConstr"
                    "Tuple",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "tuple_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:466:20",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "tuple_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:466:20")
                          (Just "globals.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "tuple_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:466:33",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "tuple_y"},
                            fieldComment = Nothing},
                          structFieldType = TypeConst
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:466:33")
                          (Just "globals.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:466:8",
                        declId = NamePair {
                          nameC = Name "tuple",
                          nameHsIdent = HsIdentifier
                            "Tuple"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "globals.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Tuple"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:466:20",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "tuple_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:466:33",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "tuple_y"},
                                fieldComment = Nothing},
                              structFieldType = TypeConst
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 32,
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
                    (Comment
                      Nothing
                      (Just "globals.h:466:8")
                      (Just "globals.h")
                      [])})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 4]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Tuple",
                  structConstr = HsName
                    "@NsConstr"
                    "Tuple",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "tuple_x",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:466:20",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "tuple_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:466:20")
                          (Just "globals.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "tuple_y",
                      fieldType = HsPrimType
                        HsPrimCInt,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "globals.h:466:33",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "tuple_y"},
                            fieldComment = Nothing},
                          structFieldType = TypeConst
                            (TypePrim
                              (PrimIntegral PrimInt Signed)),
                          structFieldOffset = 32,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "globals.h:466:33")
                          (Just "globals.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "globals.h:466:8",
                        declId = NamePair {
                          nameC = Name "tuple",
                          nameHsIdent = HsIdentifier
                            "Tuple"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "globals.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Tuple"),
                          structSizeof = 8,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:466:20",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "tuple_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimIntegral PrimInt Signed),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "globals.h:466:33",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "tuple_y"},
                                fieldComment = Nothing},
                              structFieldType = TypeConst
                                (TypePrim
                                  (PrimIntegral PrimInt Signed)),
                              structFieldOffset = 32,
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
                    (Comment
                      Nothing
                      (Just "globals.h:466:8")
                      (Just "globals.h")
                      [])}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      4
                      (Idx 1)])))},
      defineInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Show,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Tuple",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Tuple",
      deriveInstanceComment =
      Nothing},
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_constTuple_ptr */ __attribute__ ((const)) struct tuple const *hs_bindgen_test_globals_6f2e1968e15f0b9b (void) { return &constTuple; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_6f2e1968e15f0b9b",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Tuple")))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_6f2e1968e15f0b9b",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConst
          (TypeStruct
            NamePair {
              nameC = Name "tuple",
              nameHsIdent = HsIdentifier
                "Tuple"}
            NameOriginInSource)),
      foreignImportComment = Just
        (Comment
          (Just
            [
              TextContent "A constant tuple"])
          (Just "globals.h:468:27")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_nonConstTuple_ptr */ __attribute__ ((const)) struct tuple *hs_bindgen_test_globals_e8e62512a4e5d162 (void) { return &nonConstTuple; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_e8e62512a4e5d162",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Tuple")))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_e8e62512a4e5d162",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeStruct
          NamePair {
            nameC = Name "tuple",
            nameHsIdent = HsIdentifier
              "Tuple"}
          NameOriginInSource),
      foreignImportComment = Just
        (Comment
          (Just
            [
              TextContent
                "A non-constant tuple with a constant member"])
          (Just "globals.h:470:21")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_ptrToConstInt_ptr */ __attribute__ ((const)) signed int const **hs_bindgen_test_globals_e41146f6df20fe0d (void) { return &ptrToConstInt; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_e41146f6df20fe0d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_e41146f6df20fe0d",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypePointer
          (TypeConst
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Just
        (Comment
          (Just
            [
              TextContent
                "A pointer to const int"])
          (Just "globals.h:473:20")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_constPtrToInt_ptr */ __attribute__ ((const)) signed int *const *hs_bindgen_test_globals_83b0d0f5488b6a03 (void) { return &constPtrToInt; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_83b0d0f5488b6a03",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_83b0d0f5488b6a03",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConst
          (TypePointer
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportComment = Just
        (Comment
          (Just
            [
              TextContent
                "A const pointer to int"])
          (Just "globals.h:475:20")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclInlineCInclude "globals.h",
  DeclInlineC
    "/* get_constPtrToConstInt_ptr */ __attribute__ ((const)) signed int const *const *hs_bindgen_test_globals_247c0c91ce28a18d (void) { return &constPtrToConstInt; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_globals_247c0c91ce28a18d",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsPtr
              (HsPrimType HsPrimCInt)))),
      foreignImportOrigName =
      "hs_bindgen_test_globals_247c0c91ce28a18d",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeConst
          (TypePointer
            (TypeConst
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed))))),
      foreignImportComment = Just
        (Comment
          (Just
            [
              TextContent
                "A const pointer to const int"])
          (Just "globals.h:477:26")
          (Just "globals.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple,
  DeclSimple,
  DeclSimple]
