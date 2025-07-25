[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "UnionA",
      newtypeConstr = HsName
        "@NsConstr"
        "UnionA",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_UnionA",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "nested_unions.h:2:15",
          declId = NamePair {
            nameC = Name "unionA",
            nameHsIdent = HsIdentifier
              "UnionA"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "nested_unions.h",
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "UnionA",
              newtypeField = HsName
                "@NsVar"
                "un_UnionA"},
            unionSizeof = 4,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldLoc =
                "nested_unions.h:3:21",
                unionFieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = HsIdentifier
                    "unionA_a"},
                unionFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                unionFieldComment = Nothing},
              UnionField {
                unionFieldLoc =
                "nested_unions.h:4:22",
                unionFieldName = NamePair {
                  nameC = Name "b",
                  nameHsIdent = HsIdentifier
                    "unionA_b"},
                unionFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
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
        "UnionA",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_unionA_a",
      unionGetterType = HsPrimType
        HsPrimCInt,
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "UnionA",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier "set_unionA_a"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_unionA_a",
      unionSetterType = HsPrimType
        HsPrimCInt,
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "UnionA",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier "get_unionA_a"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_unionA_b",
      unionGetterType = HsPrimType
        HsPrimCChar,
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "UnionA",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier "set_unionA_b"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_unionA_b",
      unionSetterType = HsPrimType
        HsPrimCChar,
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "UnionA",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier "get_unionA_b"]]}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "ExA",
      structConstr = HsName
        "@NsConstr"
        "ExA",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "exA_fieldA1",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "UnionA"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_unions.h:5:11",
              structFieldName = NamePair {
                nameC = Name "fieldA1",
                nameHsIdent = HsIdentifier
                  "exA_fieldA1"},
              structFieldType = TypeUnion
                NamePair {
                  nameC = Name "unionA",
                  nameHsIdent = HsIdentifier
                    "UnionA"}
                NameOriginInSource,
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_unions.h:1:8",
            declId = NamePair {
              nameC = Name "exA",
              nameHsIdent = HsIdentifier
                "ExA"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "nested_unions.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "ExA"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "nested_unions.h:5:11",
                  structFieldName = NamePair {
                    nameC = Name "fieldA1",
                    nameHsIdent = HsIdentifier
                      "exA_fieldA1"},
                  structFieldType = TypeUnion
                    NamePair {
                      nameC = Name "unionA",
                      nameHsIdent = HsIdentifier
                        "UnionA"}
                    NameOriginInSource,
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
        [Storable],
      structComment = Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "ExA",
          structConstr = HsName
            "@NsConstr"
            "ExA",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "exA_fieldA1",
              fieldType = HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "UnionA"),
              fieldOrigin = StructField
                StructField {
                  structFieldLoc =
                  "nested_unions.h:5:11",
                  structFieldName = NamePair {
                    nameC = Name "fieldA1",
                    nameHsIdent = HsIdentifier
                      "exA_fieldA1"},
                  structFieldType = TypeUnion
                    NamePair {
                      nameC = Name "unionA",
                      nameHsIdent = HsIdentifier
                        "UnionA"}
                    NameOriginInSource,
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "nested_unions.h:1:8",
                declId = NamePair {
                  nameC = Name "exA",
                  nameHsIdent = HsIdentifier
                    "ExA"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "nested_unions.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "ExA"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc =
                      "nested_unions.h:5:11",
                      structFieldName = NamePair {
                        nameC = Name "fieldA1",
                        nameHsIdent = HsIdentifier
                          "exA_fieldA1"},
                      structFieldType = TypeUnion
                        NamePair {
                          nameC = Name "unionA",
                          nameHsIdent = HsIdentifier
                            "UnionA"}
                        NameOriginInSource,
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
            [Storable],
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
                    "ExA",
                  structConstr = HsName
                    "@NsConstr"
                    "ExA",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "exA_fieldA1",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "UnionA"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "nested_unions.h:5:11",
                          structFieldName = NamePair {
                            nameC = Name "fieldA1",
                            nameHsIdent = HsIdentifier
                              "exA_fieldA1"},
                          structFieldType = TypeUnion
                            NamePair {
                              nameC = Name "unionA",
                              nameHsIdent = HsIdentifier
                                "UnionA"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_unions.h:1:8",
                        declId = NamePair {
                          nameC = Name "exA",
                          nameHsIdent = HsIdentifier
                            "ExA"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "nested_unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "ExA"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "nested_unions.h:5:11",
                              structFieldName = NamePair {
                                nameC = Name "fieldA1",
                                nameHsIdent = HsIdentifier
                                  "exA_fieldA1"},
                              structFieldType = TypeUnion
                                NamePair {
                                  nameC = Name "unionA",
                                  nameHsIdent = HsIdentifier
                                    "UnionA"}
                                NameOriginInSource,
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
                    [Storable],
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
                    "ExA",
                  structConstr = HsName
                    "@NsConstr"
                    "ExA",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "exA_fieldA1",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "UnionA"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "nested_unions.h:5:11",
                          structFieldName = NamePair {
                            nameC = Name "fieldA1",
                            nameHsIdent = HsIdentifier
                              "exA_fieldA1"},
                          structFieldType = TypeUnion
                            NamePair {
                              nameC = Name "unionA",
                              nameHsIdent = HsIdentifier
                                "UnionA"}
                            NameOriginInSource,
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_unions.h:1:8",
                        declId = NamePair {
                          nameC = Name "exA",
                          nameHsIdent = HsIdentifier
                            "ExA"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "nested_unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "ExA"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "nested_unions.h:5:11",
                              structFieldName = NamePair {
                                nameC = Name "fieldA1",
                                nameHsIdent = HsIdentifier
                                  "exA_fieldA1"},
                              structFieldType = TypeUnion
                                NamePair {
                                  nameC = Name "unionA",
                                  nameHsIdent = HsIdentifier
                                    "UnionA"}
                                NameOriginInSource,
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
                    [Storable],
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
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "ExB_fieldB1",
      newtypeConstr = HsName
        "@NsConstr"
        "ExB_fieldB1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_ExB_fieldB1",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "nested_unions.h:9:9",
          declId = NamePair {
            nameC = Name "exB_fieldB1",
            nameHsIdent = HsIdentifier
              "ExB_fieldB1"},
          declOrigin = NameOriginGenerated
            (AnonId "nested_unions.h:9:9"),
          declAliases = [],
          declHeader = "nested_unions.h",
          declComment = Nothing},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "ExB_fieldB1",
              newtypeField = HsName
                "@NsVar"
                "un_ExB_fieldB1"},
            unionSizeof = 4,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldLoc =
                "nested_unions.h:10:21",
                unionFieldName = NamePair {
                  nameC = Name "a",
                  nameHsIdent = HsIdentifier
                    "exB_fieldB1_a"},
                unionFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                unionFieldComment = Nothing},
              UnionField {
                unionFieldLoc =
                "nested_unions.h:11:22",
                unionFieldName = NamePair {
                  nameC = Name "b",
                  nameHsIdent = HsIdentifier
                    "exB_fieldB1_b"},
                unionFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
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
        "ExB_fieldB1",
      deriveInstanceComment =
      Nothing},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_exB_fieldB1_a",
      unionGetterType = HsPrimType
        HsPrimCInt,
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "ExB_fieldB1",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_exB_fieldB1_a"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_exB_fieldB1_a",
      unionSetterType = HsPrimType
        HsPrimCInt,
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "ExB_fieldB1",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "get_exB_fieldB1_a"]]}},
  DeclUnionGetter
    UnionGetter {
      unionGetterName = HsName
        "@NsVar"
        "get_exB_fieldB1_b",
      unionGetterType = HsPrimType
        HsPrimCChar,
      unionGetterConstr = HsName
        "@NsTypeConstr"
        "ExB_fieldB1",
      unionGetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "set_exB_fieldB1_b"]]}},
  DeclUnionSetter
    UnionSetter {
      unionSetterName = HsName
        "@NsVar"
        "set_exB_fieldB1_b",
      unionSetterType = HsPrimType
        HsPrimCChar,
      unionSetterConstr = HsName
        "@NsTypeConstr"
        "ExB_fieldB1",
      unionSetterComment = Just
        Comment {
          commentTitle = Nothing,
          commentOrigin = Nothing,
          commentChildren = [
            Paragraph
              [
                Bold [TextContent "See:"],
                Identifier
                  "get_exB_fieldB1_b"]]}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "ExB",
      structConstr = HsName
        "@NsConstr"
        "ExB",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "exB_fieldB1",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "ExB_fieldB1"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "nested_unions.h:12:11",
              structFieldName = NamePair {
                nameC = Name "fieldB1",
                nameHsIdent = HsIdentifier
                  "exB_fieldB1"},
              structFieldType = TypeUnion
                NamePair {
                  nameC = Name "exB_fieldB1",
                  nameHsIdent = HsIdentifier
                    "ExB_fieldB1"}
                (NameOriginGenerated
                  (AnonId "nested_unions.h:9:9")),
              structFieldOffset = 0,
              structFieldWidth = Nothing,
              structFieldComment = Nothing},
          fieldComment = Nothing}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "nested_unions.h:8:8",
            declId = NamePair {
              nameC = Name "exB",
              nameHsIdent = HsIdentifier
                "ExB"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "nested_unions.h",
            declComment = Nothing},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "ExB"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "nested_unions.h:12:11",
                  structFieldName = NamePair {
                    nameC = Name "fieldB1",
                    nameHsIdent = HsIdentifier
                      "exB_fieldB1"},
                  structFieldType = TypeUnion
                    NamePair {
                      nameC = Name "exB_fieldB1",
                      nameHsIdent = HsIdentifier
                        "ExB_fieldB1"}
                    (NameOriginGenerated
                      (AnonId "nested_unions.h:9:9")),
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
        [Storable],
      structComment = Nothing},
  DeclInstance
    DefineInstance {
      defineInstanceDeclarations =
      InstanceStorable
        Struct {
          structName = HsName
            "@NsTypeConstr"
            "ExB",
          structConstr = HsName
            "@NsConstr"
            "ExB",
          structFields = [
            Field {
              fieldName = HsName
                "@NsVar"
                "exB_fieldB1",
              fieldType = HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "ExB_fieldB1"),
              fieldOrigin = StructField
                StructField {
                  structFieldLoc =
                  "nested_unions.h:12:11",
                  structFieldName = NamePair {
                    nameC = Name "fieldB1",
                    nameHsIdent = HsIdentifier
                      "exB_fieldB1"},
                  structFieldType = TypeUnion
                    NamePair {
                      nameC = Name "exB_fieldB1",
                      nameHsIdent = HsIdentifier
                        "ExB_fieldB1"}
                    (NameOriginGenerated
                      (AnonId "nested_unions.h:9:9")),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing,
                  structFieldComment = Nothing},
              fieldComment = Nothing}],
          structOrigin = Just
            Decl {
              declInfo = DeclInfo {
                declLoc = "nested_unions.h:8:8",
                declId = NamePair {
                  nameC = Name "exB",
                  nameHsIdent = HsIdentifier
                    "ExB"},
                declOrigin = NameOriginInSource,
                declAliases = [],
                declHeader = "nested_unions.h",
                declComment = Nothing},
              declKind = Struct
                Struct {
                  structNames = RecordNames
                    (HsName "@NsConstr" "ExB"),
                  structSizeof = 4,
                  structAlignment = 4,
                  structFields = [
                    StructField {
                      structFieldLoc =
                      "nested_unions.h:12:11",
                      structFieldName = NamePair {
                        nameC = Name "fieldB1",
                        nameHsIdent = HsIdentifier
                          "exB_fieldB1"},
                      structFieldType = TypeUnion
                        NamePair {
                          nameC = Name "exB_fieldB1",
                          nameHsIdent = HsIdentifier
                            "ExB_fieldB1"}
                        (NameOriginGenerated
                          (AnonId "nested_unions.h:9:9")),
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
            [Storable],
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
                    "ExB",
                  structConstr = HsName
                    "@NsConstr"
                    "ExB",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "exB_fieldB1",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "ExB_fieldB1"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "nested_unions.h:12:11",
                          structFieldName = NamePair {
                            nameC = Name "fieldB1",
                            nameHsIdent = HsIdentifier
                              "exB_fieldB1"},
                          structFieldType = TypeUnion
                            NamePair {
                              nameC = Name "exB_fieldB1",
                              nameHsIdent = HsIdentifier
                                "ExB_fieldB1"}
                            (NameOriginGenerated
                              (AnonId "nested_unions.h:9:9")),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_unions.h:8:8",
                        declId = NamePair {
                          nameC = Name "exB",
                          nameHsIdent = HsIdentifier
                            "ExB"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "nested_unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "ExB"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "nested_unions.h:12:11",
                              structFieldName = NamePair {
                                nameC = Name "fieldB1",
                                nameHsIdent = HsIdentifier
                                  "exB_fieldB1"},
                              structFieldType = TypeUnion
                                NamePair {
                                  nameC = Name "exB_fieldB1",
                                  nameHsIdent = HsIdentifier
                                    "ExB_fieldB1"}
                                (NameOriginGenerated
                                  (AnonId "nested_unions.h:9:9")),
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
                    [Storable],
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
                    "ExB",
                  structConstr = HsName
                    "@NsConstr"
                    "ExB",
                  structFields = [
                    Field {
                      fieldName = HsName
                        "@NsVar"
                        "exB_fieldB1",
                      fieldType = HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "ExB_fieldB1"),
                      fieldOrigin = StructField
                        StructField {
                          structFieldLoc =
                          "nested_unions.h:12:11",
                          structFieldName = NamePair {
                            nameC = Name "fieldB1",
                            nameHsIdent = HsIdentifier
                              "exB_fieldB1"},
                          structFieldType = TypeUnion
                            NamePair {
                              nameC = Name "exB_fieldB1",
                              nameHsIdent = HsIdentifier
                                "ExB_fieldB1"}
                            (NameOriginGenerated
                              (AnonId "nested_unions.h:9:9")),
                          structFieldOffset = 0,
                          structFieldWidth = Nothing,
                          structFieldComment = Nothing},
                      fieldComment = Nothing}],
                  structOrigin = Just
                    Decl {
                      declInfo = DeclInfo {
                        declLoc = "nested_unions.h:8:8",
                        declId = NamePair {
                          nameC = Name "exB",
                          nameHsIdent = HsIdentifier
                            "ExB"},
                        declOrigin = NameOriginInSource,
                        declAliases = [],
                        declHeader = "nested_unions.h",
                        declComment = Nothing},
                      declKind = Struct
                        Struct {
                          structNames = RecordNames
                            (HsName "@NsConstr" "ExB"),
                          structSizeof = 4,
                          structAlignment = 4,
                          structFields = [
                            StructField {
                              structFieldLoc =
                              "nested_unions.h:12:11",
                              structFieldName = NamePair {
                                nameC = Name "fieldB1",
                                nameHsIdent = HsIdentifier
                                  "exB_fieldB1"},
                              structFieldType = TypeUnion
                                NamePair {
                                  nameC = Name "exB_fieldB1",
                                  nameHsIdent = HsIdentifier
                                    "ExB_fieldB1"}
                                (NameOriginGenerated
                                  (AnonId "nested_unions.h:9:9")),
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
                    [Storable],
                  structComment = Nothing}
                (Add 1)
                (Seq
                  [
                    PokeByteOff
                      (Idx 2)
                      0
                      (Idx 0)])))},
      defineInstanceComment =
      Nothing}]
