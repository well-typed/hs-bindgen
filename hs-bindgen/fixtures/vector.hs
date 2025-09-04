[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Vector",
      structConstr = HsName
        "@NsConstr"
        "Vector",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "vector_x",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "vector.h:2:12",
                fieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "vector_x"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 0,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "vector.h:2:12")
              (Just "vector.h")
              [])},
        Field {
          fieldName = HsName
            "@NsVar"
            "vector_y",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldInfo = FieldInfo {
                fieldLoc = "vector.h:3:12",
                fieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "vector_y"},
                fieldComment = Nothing},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 64,
              structFieldWidth = Nothing},
          fieldComment = Just
            (Comment
              Nothing
              (Just "vector.h:3:12")
              (Just "vector.h")
              [])}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "vector.h:1:9",
            declId = NamePair {
              nameC = Name "vector",
              nameHsIdent = HsIdentifier
                "Vector"},
            declOrigin = NameOriginGenerated
              (AnonId "vector.h:1:9"),
            declAliases = [Name "vector"],
            declHeader = "vector.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Vector"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "vector.h:2:12",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "vector_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "vector.h:3:12",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "vector_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 64,
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
          (Just "vector.h:1:9")
          (Just "vector.h")
          [])},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "Vector",
          structConstr = HsName
            "@NsConstr"
            "Vector",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "vector_x",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "vector.h:2:12",
                    fieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "vector_x"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "vector.h:2:12")
                  (Just "vector.h")
                  [])},
            Field {
              fieldName = HsName
                "@NsVar"
                "vector_y",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldInfo = FieldInfo {
                    fieldLoc = "vector.h:3:12",
                    fieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "vector_y"},
                    fieldComment = Nothing},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing},
              fieldComment = Just
                (Comment
                  Nothing
                  (Just "vector.h:3:12")
                  (Just "vector.h")
                  [])}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "vector.h:1:9",
                declId = NamePair {
                  nameC = Name "vector",
                  nameHsIdent = HsIdentifier
                    "Vector"},
                declOrigin = NameOriginGenerated
                  (AnonId "vector.h:1:9"),
                declAliases = [Name "vector"],
                declHeader = "vector.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "Vector"),
                  structSizeof = 16,
                  structAlignment = 8,
                  structFields = [
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "vector.h:2:12",
                        fieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "vector_x"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing},
                    StructField {
                      structFieldInfo = FieldInfo {
                        fieldLoc = "vector.h:3:12",
                        fieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "vector_y"},
                        fieldComment = Nothing},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
                      structFieldOffset = 64,
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
              (Just "vector.h:1:9")
              (Just "vector.h")
              [])}
        StorableInstance {
          storableSizeOf = 16,
          storableAlignment = 8,
          storablePeek = Lambda
            (NameHint "ptr")
            (Ap
              (StructCon
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Vector",
                  structConstr = HsName
                    "@NsConstr"
                    "Vector",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "vector_x",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "vector.h:2:12",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "vector_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "vector.h:2:12")
                          (Just "vector.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "vector_y",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "vector.h:3:12",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "vector_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "vector.h:3:12")
                          (Just "vector.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "vector.h:1:9",
                        declId = NamePair {
                          nameC = Name "vector",
                          nameHsIdent = HsIdentifier
                            "Vector"},
                        declOrigin = NameOriginGenerated
                          (AnonId "vector.h:1:9"),
                        declAliases = [Name "vector"],
                        declHeader = "vector.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Vector"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "vector.h:2:12",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "vector_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "vector.h:3:12",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "vector_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 64,
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
                      (Just "vector.h:1:9")
                      (Just "vector.h")
                      [])})
              [
                PeekByteOff (Idx 0) 0,
                PeekByteOff (Idx 0) 8]),
          storablePoke = Lambda
            (NameHint "ptr")
            (Lambda
              (NameHint "s")
              (ElimStruct
                (Idx 0)
                Struct {
                  structName = HsName
                    "@NsTypeConstr"
                    "Vector",
                  structConstr = HsName
                    "@NsConstr"
                    "Vector",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "vector_x",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "vector.h:2:12",
                            fieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "vector_x"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "vector.h:2:12")
                          (Just "vector.h")
                          [])},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "vector_y",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldInfo = FieldInfo {
                            fieldLoc = "vector.h:3:12",
                            fieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "vector_y"},
                            fieldComment = Nothing},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing},
                      fieldComment = Just
                        (Comment
                          Nothing
                          (Just "vector.h:3:12")
                          (Just "vector.h")
                          [])}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "vector.h:1:9",
                        declId = NamePair {
                          nameC = Name "vector",
                          nameHsIdent = HsIdentifier
                            "Vector"},
                        declOrigin = NameOriginGenerated
                          (AnonId "vector.h:1:9"),
                        declAliases = [Name "vector"],
                        declHeader = "vector.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "Vector"),
                          structSizeof = 16,
                          structAlignment = 8,
                          structFields = [
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "vector.h:2:12",
                                fieldName = NamePair {
                                  nameC = Name "x",
                                  nameHsIdent = HsIdentifier
                                    "vector_x"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing},
                            StructField {
                              structFieldInfo = FieldInfo {
                                fieldLoc = "vector.h:3:12",
                                fieldName = NamePair {
                                  nameC = Name "y",
                                  nameHsIdent = HsIdentifier
                                    "vector_y"},
                                fieldComment = Nothing},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 64,
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
                      (Just "vector.h:1:9")
                      (Just "vector.h")
                      [])}
                (Add 2)
                (Seq
                  [
                    PokeByteOff (Idx 3) 0 (Idx 0),
                    PokeByteOff
                      (Idx 3)
                      8
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
        "Vector",
      deriveInstanceComment =
      Nothing},
  DeclNewtypeInstance
    DeriveInstance {
      deriveInstanceStrategy =
      DeriveStock,
      deriveInstanceClass = Eq,
      deriveInstanceName = HsName
        "@NsTypeConstr"
        "Vector",
      deriveInstanceComment =
      Nothing},
  DeclInlineCInclude "vector.h",
  DeclInlineC
    "vector *hs_bindgen_test_vector_72a6c90b1b14a9b0 (double arg1, double arg2) { return new_vector(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "new_vector",
      foreignImportParameters = [
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "x"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            (Comment
              Nothing
              Nothing
              Nothing
              [])},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "y"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            (Comment
              Nothing
              Nothing
              Nothing
              [])}],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Vector")))),
      foreignImportOrigName =
      "hs_bindgen_test_vector_72a6c90b1b14a9b0",
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
                (PrimFloating PrimDouble))],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePointer
            (TypeTypedef
              (TypedefSquashed
                (Name "vector")
                (TypeStruct
                  NamePair {
                    nameC = Name "vector",
                    nameHsIdent = HsIdentifier
                      "Vector"}
                  (NameOriginGenerated
                    (AnonId "vector.h:1:9")))))},
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "vector.h:6:9")
          (Just "vector.h")
          []),
      foreignImportSafety = Safe},
  DeclInlineCInclude "vector.h",
  DeclInlineC
    "/* get_new_vector_ptr */ __attribute__ ((const)) vector *(*hs_bindgen_test_vector_94a1e2e4670c0a3e (void)) (double arg1, double arg2) { return &new_vector; } ",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "hs_bindgen_test_vector_94a1e2e4670c0a3e",
      foreignImportParameters = [],
      foreignImportResultType =
      NormalResultType
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCDouble)
              (HsFun
                (HsPrimType HsPrimCDouble)
                (HsIO
                  (HsPtr
                    (HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Vector")))))))),
      foreignImportOrigName =
      "hs_bindgen_test_vector_94a1e2e4670c0a3e",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Global
        (TypeFun
          [
            TypePrim
              (PrimFloating PrimDouble),
            TypePrim
              (PrimFloating PrimDouble)]
          (TypePointer
            (TypeTypedef
              (TypedefSquashed
                (Name "vector")
                (TypeStruct
                  NamePair {
                    nameC = Name "vector",
                    nameHsIdent = HsIdentifier
                      "Vector"}
                  (NameOriginGenerated
                    (AnonId "vector.h:1:9"))))))),
      foreignImportComment = Just
        (Comment
          Nothing
          (Just "vector.h:6:9")
          (Just "vector.h")
          []),
      foreignImportSafety = Unsafe},
  DeclSimple,
  DeclSimple]
