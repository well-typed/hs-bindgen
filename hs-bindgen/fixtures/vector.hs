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
              structFieldLoc =
              "vector.h:2:12",
              structFieldName = NamePair {
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "vector_x"},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing},
        Field {
          fieldName = HsName
            "@NsVar"
            "vector_y",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "vector.h:3:12",
              structFieldName = NamePair {
                nameC = Name "y",
                nameHsIdent = HsIdentifier
                  "vector_y"},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 64,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
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
                  structFieldLoc =
                  "vector.h:2:12",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "vector_x"},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
                StructField {
                  structFieldLoc =
                  "vector.h:3:12",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "vector_y"},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 64,
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
                  structFieldLoc =
                  "vector.h:2:12",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "vector_x"},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing},
            Field {
              fieldName = HsName
                "@NsVar"
                "vector_y",
              fieldType = HsPrimType
                HsPrimCDouble,
              fieldOrigin = StructField
                StructField {
                  structFieldLoc =
                  "vector.h:3:12",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "vector_y"},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 64,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
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
                      structFieldLoc =
                      "vector.h:2:12",
                      structFieldName = NamePair {
                        nameC = Name "x",
                        nameHsIdent = HsIdentifier
                          "vector_x"},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
                      structFieldOffset = 0,
                      structFieldWidth = Nothing,
                      structFieldComment = Nothing},
                    StructField {
                      structFieldLoc =
                      "vector.h:3:12",
                      structFieldName = NamePair {
                        nameC = Name "y",
                        nameHsIdent = HsIdentifier
                          "vector_y"},
                      structFieldType = TypePrim
                        (PrimFloating PrimDouble),
                      structFieldOffset = 64,
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
                          structFieldLoc =
                          "vector.h:2:12",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "vector_x"},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "vector_y",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "vector.h:3:12",
                          structFieldName = NamePair {
                            nameC = Name "y",
                            nameHsIdent = HsIdentifier
                              "vector_y"},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
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
                              structFieldLoc =
                              "vector.h:2:12",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "vector_x"},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc =
                              "vector.h:3:12",
                              structFieldName = NamePair {
                                nameC = Name "y",
                                nameHsIdent = HsIdentifier
                                  "vector_y"},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 64,
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
                          structFieldLoc =
                          "vector.h:2:12",
                          structFieldName = NamePair {
                            nameC = Name "x",
                            nameHsIdent = HsIdentifier
                              "vector_x"},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing},
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "vector_y",
                      fieldType = HsPrimType
                        HsPrimCDouble,
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "vector.h:3:12",
                          structFieldName = NamePair {
                            nameC = Name "y",
                            nameHsIdent = HsIdentifier
                              "vector_y"},
                          structFieldType = TypePrim
                            (PrimFloating PrimDouble),
                          structFieldOffset = 64,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
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
                              structFieldLoc =
                              "vector.h:2:12",
                              structFieldName = NamePair {
                                nameC = Name "x",
                                nameHsIdent = HsIdentifier
                                  "vector_x"},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 0,
                              structFieldWidth = Nothing,
                              structFieldComment = Nothing},
                            StructField {
                              structFieldLoc =
                              "vector.h:3:12",
                              structFieldName = NamePair {
                                nameC = Name "y",
                                nameHsIdent = HsIdentifier
                                  "vector_y"},
                              structFieldType = TypePrim
                                (PrimFloating PrimDouble),
                              structFieldOffset = 64,
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
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "x",
              commentChildren = []}},
        FunctionParameter {
          functionParameterName = Just
            (HsName "@NsVar" "y"),
          functionParameterType =
          HsPrimType HsPrimCDouble,
          functionParameterComment = Just
            Comment {
              commentTitle = Nothing,
              commentOrigin = Just "y",
              commentChildren = []}}],
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
        Comment {
          commentTitle = Nothing,
          commentOrigin = Just
            "new_vector",
          commentChildren = []}}]
