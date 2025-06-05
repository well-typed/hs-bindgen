[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S1_c",
      structConstr = HsName
        "@NsConstr"
        "S1_c",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s1_c_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "anonymous.h:4:9",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "s1_c_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s1_c_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "anonymous.h:5:9",
              structFieldName = NamePair {
                nameC = CName "b",
                nameHsIdent = HsIdentifier
                  "s1_c_b"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "anonymous.h:3:3",
            declId = NamePair {
              nameC = CName "S1_c",
              nameHsIdent = HsIdentifier
                "S1_c"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S1_c"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "anonymous.h:4:9",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "s1_c_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "anonymous.h:5:9",
                  structFieldName = NamePair {
                    nameC = CName "b",
                    nameHsIdent = HsIdentifier
                      "s1_c_b"},
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "S1_c",
        structConstr = HsName
          "@NsConstr"
          "S1_c",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s1_c_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "anonymous.h:4:9",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "s1_c_a"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s1_c_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "anonymous.h:5:9",
                structFieldName = NamePair {
                  nameC = CName "b",
                  nameHsIdent = HsIdentifier
                    "s1_c_b"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "anonymous.h:3:3",
              declId = NamePair {
                nameC = CName "S1_c",
                nameHsIdent = HsIdentifier
                  "S1_c"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "S1_c"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "anonymous.h:4:9",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "s1_c_a"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "anonymous.h:5:9",
                    structFieldName = NamePair {
                      nameC = CName "b",
                      nameHsIdent = HsIdentifier
                        "s1_c_b"},
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
          [Eq, Show, Storable]}
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
                  "S1_c",
                structConstr = HsName
                  "@NsConstr"
                  "S1_c",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_c_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:4:9",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s1_c_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_c_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:5:9",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s1_c_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "anonymous.h:3:3",
                      declId = NamePair {
                        nameC = CName "S1_c",
                        nameHsIdent = HsIdentifier
                          "S1_c"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S1_c"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "anonymous.h:4:9",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s1_c_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "anonymous.h:5:9",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s1_c_b"},
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
                  [Eq, Show, Storable]})
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
                  "S1_c",
                structConstr = HsName
                  "@NsConstr"
                  "S1_c",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_c_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:4:9",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s1_c_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_c_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:5:9",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s1_c_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "anonymous.h:3:3",
                      declId = NamePair {
                        nameC = CName "S1_c",
                        nameHsIdent = HsIdentifier
                          "S1_c"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S1_c"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "anonymous.h:4:9",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s1_c_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "anonymous.h:5:9",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s1_c_b"},
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
                  [Eq, Show, Storable]}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "S1_c"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S1_c"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S1",
      structConstr = HsName
        "@NsConstr"
        "S1",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s1_c",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "S1_c"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "anonymous.h:6:5",
              structFieldName = NamePair {
                nameC = CName "c",
                nameHsIdent = HsIdentifier
                  "s1_c"},
              structFieldType = TypeStruct
                NamePair {
                  nameC = CName "S1_c",
                  nameHsIdent = HsIdentifier
                    "S1_c"},
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s1_d",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "anonymous.h:8:7",
              structFieldName = NamePair {
                nameC = CName "d",
                nameHsIdent = HsIdentifier
                  "s1_d"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "anonymous.h:2:8",
            declId = NamePair {
              nameC = CName "S1",
              nameHsIdent = HsIdentifier
                "S1"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S1"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "anonymous.h:6:5",
                  structFieldName = NamePair {
                    nameC = CName "c",
                    nameHsIdent = HsIdentifier
                      "s1_c"},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = CName "S1_c",
                      nameHsIdent = HsIdentifier
                        "S1_c"},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "anonymous.h:8:7",
                  structFieldName = NamePair {
                    nameC = CName "d",
                    nameHsIdent = HsIdentifier
                      "s1_d"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "S1",
        structConstr = HsName
          "@NsConstr"
          "S1",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s1_c",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "S1_c"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "anonymous.h:6:5",
                structFieldName = NamePair {
                  nameC = CName "c",
                  nameHsIdent = HsIdentifier
                    "s1_c"},
                structFieldType = TypeStruct
                  NamePair {
                    nameC = CName "S1_c",
                    nameHsIdent = HsIdentifier
                      "S1_c"},
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s1_d",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "anonymous.h:8:7",
                structFieldName = NamePair {
                  nameC = CName "d",
                  nameHsIdent = HsIdentifier
                    "s1_d"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "anonymous.h:2:8",
              declId = NamePair {
                nameC = CName "S1",
                nameHsIdent = HsIdentifier
                  "S1"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "S1"),
                structSizeof = 12,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "anonymous.h:6:5",
                    structFieldName = NamePair {
                      nameC = CName "c",
                      nameHsIdent = HsIdentifier
                        "s1_c"},
                    structFieldType = TypeStruct
                      NamePair {
                        nameC = CName "S1_c",
                        nameHsIdent = HsIdentifier
                          "S1_c"},
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "anonymous.h:8:7",
                    structFieldName = NamePair {
                      nameC = CName "d",
                      nameHsIdent = HsIdentifier
                        "s1_d"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
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
          [Eq, Show, Storable]}
      StorableInstance {
        storableSizeOf = 12,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "S1",
                structConstr = HsName
                  "@NsConstr"
                  "S1",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_c",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "S1_c"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:6:5",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "s1_c"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName "S1_c",
                            nameHsIdent = HsIdentifier
                              "S1_c"},
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_d",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:8:7",
                        structFieldName = NamePair {
                          nameC = CName "d",
                          nameHsIdent = HsIdentifier
                            "s1_d"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "anonymous.h:2:8",
                      declId = NamePair {
                        nameC = CName "S1",
                        nameHsIdent = HsIdentifier
                          "S1"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S1"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "anonymous.h:6:5",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "s1_c"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "S1_c",
                                nameHsIdent = HsIdentifier
                                  "S1_c"},
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "anonymous.h:8:7",
                            structFieldName = NamePair {
                              nameC = CName "d",
                              nameHsIdent = HsIdentifier
                                "s1_d"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
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
                  [Eq, Show, Storable]})
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
                  "S1",
                structConstr = HsName
                  "@NsConstr"
                  "S1",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_c",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "S1_c"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:6:5",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "s1_c"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName "S1_c",
                            nameHsIdent = HsIdentifier
                              "S1_c"},
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_d",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:8:7",
                        structFieldName = NamePair {
                          nameC = CName "d",
                          nameHsIdent = HsIdentifier
                            "s1_d"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "anonymous.h:2:8",
                      declId = NamePair {
                        nameC = CName "S1",
                        nameHsIdent = HsIdentifier
                          "S1"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S1"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "anonymous.h:6:5",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "s1_c"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "S1_c",
                                nameHsIdent = HsIdentifier
                                  "S1_c"},
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "anonymous.h:8:7",
                            structFieldName = NamePair {
                              nameC = CName "d",
                              nameHsIdent = HsIdentifier
                                "s1_d"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
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
                  [Eq, Show, Storable]}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "S1"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S1"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S2_inner_deep",
      structConstr = HsName
        "@NsConstr"
        "S2_inner_deep",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_inner_deep_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "anonymous.h:16:11",
              structFieldName = NamePair {
                nameC = CName "b",
                nameHsIdent = HsIdentifier
                  "s2_inner_deep_b"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "anonymous.h:15:5",
            declId = NamePair {
              nameC = CName "S2_inner_deep",
              nameHsIdent = HsIdentifier
                "S2_inner_deep"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "S2_inner_deep"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "anonymous.h:16:11",
                  structFieldName = NamePair {
                    nameC = CName "b",
                    nameHsIdent = HsIdentifier
                      "s2_inner_deep_b"},
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "S2_inner_deep",
        structConstr = HsName
          "@NsConstr"
          "S2_inner_deep",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s2_inner_deep_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "anonymous.h:16:11",
                structFieldName = NamePair {
                  nameC = CName "b",
                  nameHsIdent = HsIdentifier
                    "s2_inner_deep_b"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "anonymous.h:15:5",
              declId = NamePair {
                nameC = CName "S2_inner_deep",
                nameHsIdent = HsIdentifier
                  "S2_inner_deep"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "S2_inner_deep"),
                structSizeof = 4,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "anonymous.h:16:11",
                    structFieldName = NamePair {
                      nameC = CName "b",
                      nameHsIdent = HsIdentifier
                        "s2_inner_deep_b"},
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
          [Eq, Show, Storable]}
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
                  "S2_inner_deep",
                structConstr = HsName
                  "@NsConstr"
                  "S2_inner_deep",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner_deep_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:16:11",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s2_inner_deep_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "anonymous.h:15:5",
                      declId = NamePair {
                        nameC = CName "S2_inner_deep",
                        nameHsIdent = HsIdentifier
                          "S2_inner_deep"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "S2_inner_deep"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "anonymous.h:16:11",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s2_inner_deep_b"},
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
                  [Eq, Show, Storable]})
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
                  "S2_inner_deep",
                structConstr = HsName
                  "@NsConstr"
                  "S2_inner_deep",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner_deep_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:16:11",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s2_inner_deep_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "anonymous.h:15:5",
                      declId = NamePair {
                        nameC = CName "S2_inner_deep",
                        nameHsIdent = HsIdentifier
                          "S2_inner_deep"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "S2_inner_deep"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "anonymous.h:16:11",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s2_inner_deep_b"},
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
                  [Eq, Show, Storable]}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "S2_inner_deep"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "S2_inner_deep"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S2_inner",
      structConstr = HsName
        "@NsConstr"
        "S2_inner",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_inner_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "anonymous.h:14:9",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "s2_inner_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_inner_deep",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "S2_inner_deep"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "anonymous.h:17:7",
              structFieldName = NamePair {
                nameC = CName "deep",
                nameHsIdent = HsIdentifier
                  "s2_inner_deep"},
              structFieldType = TypeStruct
                NamePair {
                  nameC = CName "S2_inner_deep",
                  nameHsIdent = HsIdentifier
                    "S2_inner_deep"},
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "anonymous.h:13:3",
            declId = NamePair {
              nameC = CName "S2_inner",
              nameHsIdent = HsIdentifier
                "S2_inner"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S2_inner"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "anonymous.h:14:9",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "s2_inner_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "anonymous.h:17:7",
                  structFieldName = NamePair {
                    nameC = CName "deep",
                    nameHsIdent = HsIdentifier
                      "s2_inner_deep"},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = CName "S2_inner_deep",
                      nameHsIdent = HsIdentifier
                        "S2_inner_deep"},
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "S2_inner",
        structConstr = HsName
          "@NsConstr"
          "S2_inner",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s2_inner_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "anonymous.h:14:9",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "s2_inner_a"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s2_inner_deep",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "S2_inner_deep"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "anonymous.h:17:7",
                structFieldName = NamePair {
                  nameC = CName "deep",
                  nameHsIdent = HsIdentifier
                    "s2_inner_deep"},
                structFieldType = TypeStruct
                  NamePair {
                    nameC = CName "S2_inner_deep",
                    nameHsIdent = HsIdentifier
                      "S2_inner_deep"},
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "anonymous.h:13:3",
              declId = NamePair {
                nameC = CName "S2_inner",
                nameHsIdent = HsIdentifier
                  "S2_inner"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "S2_inner"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "anonymous.h:14:9",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "s2_inner_a"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "anonymous.h:17:7",
                    structFieldName = NamePair {
                      nameC = CName "deep",
                      nameHsIdent = HsIdentifier
                        "s2_inner_deep"},
                    structFieldType = TypeStruct
                      NamePair {
                        nameC = CName "S2_inner_deep",
                        nameHsIdent = HsIdentifier
                          "S2_inner_deep"},
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
          [Eq, Show, Storable]}
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
                  "S2_inner",
                structConstr = HsName
                  "@NsConstr"
                  "S2_inner",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:14:9",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s2_inner_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner_deep",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "S2_inner_deep"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:17:7",
                        structFieldName = NamePair {
                          nameC = CName "deep",
                          nameHsIdent = HsIdentifier
                            "s2_inner_deep"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName "S2_inner_deep",
                            nameHsIdent = HsIdentifier
                              "S2_inner_deep"},
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "anonymous.h:13:3",
                      declId = NamePair {
                        nameC = CName "S2_inner",
                        nameHsIdent = HsIdentifier
                          "S2_inner"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S2_inner"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "anonymous.h:14:9",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s2_inner_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "anonymous.h:17:7",
                            structFieldName = NamePair {
                              nameC = CName "deep",
                              nameHsIdent = HsIdentifier
                                "s2_inner_deep"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "S2_inner_deep",
                                nameHsIdent = HsIdentifier
                                  "S2_inner_deep"},
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
                  [Eq, Show, Storable]})
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
                  "S2_inner",
                structConstr = HsName
                  "@NsConstr"
                  "S2_inner",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:14:9",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s2_inner_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner_deep",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "S2_inner_deep"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:17:7",
                        structFieldName = NamePair {
                          nameC = CName "deep",
                          nameHsIdent = HsIdentifier
                            "s2_inner_deep"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName "S2_inner_deep",
                            nameHsIdent = HsIdentifier
                              "S2_inner_deep"},
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "anonymous.h:13:3",
                      declId = NamePair {
                        nameC = CName "S2_inner",
                        nameHsIdent = HsIdentifier
                          "S2_inner"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S2_inner"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "anonymous.h:14:9",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s2_inner_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "anonymous.h:17:7",
                            structFieldName = NamePair {
                              nameC = CName "deep",
                              nameHsIdent = HsIdentifier
                                "s2_inner_deep"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "S2_inner_deep",
                                nameHsIdent = HsIdentifier
                                  "S2_inner_deep"},
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
                  [Eq, Show, Storable]}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "S2_inner"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "S2_inner"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S2",
      structConstr = HsName
        "@NsConstr"
        "S2",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_inner",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "S2_inner"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "anonymous.h:18:5",
              structFieldName = NamePair {
                nameC = CName "inner",
                nameHsIdent = HsIdentifier
                  "s2_inner"},
              structFieldType = TypeStruct
                NamePair {
                  nameC = CName "S2_inner",
                  nameHsIdent = HsIdentifier
                    "S2_inner"},
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_d",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "anonymous.h:20:7",
              structFieldName = NamePair {
                nameC = CName "d",
                nameHsIdent = HsIdentifier
                  "s2_d"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "anonymous.h:12:8",
            declId = NamePair {
              nameC = CName "S2",
              nameHsIdent = HsIdentifier
                "S2"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S2"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "anonymous.h:18:5",
                  structFieldName = NamePair {
                    nameC = CName "inner",
                    nameHsIdent = HsIdentifier
                      "s2_inner"},
                  structFieldType = TypeStruct
                    NamePair {
                      nameC = CName "S2_inner",
                      nameHsIdent = HsIdentifier
                        "S2_inner"},
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "anonymous.h:20:7",
                  structFieldName = NamePair {
                    nameC = CName "d",
                    nameHsIdent = HsIdentifier
                      "s2_d"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "S2",
        structConstr = HsName
          "@NsConstr"
          "S2",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s2_inner",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "S2_inner"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "anonymous.h:18:5",
                structFieldName = NamePair {
                  nameC = CName "inner",
                  nameHsIdent = HsIdentifier
                    "s2_inner"},
                structFieldType = TypeStruct
                  NamePair {
                    nameC = CName "S2_inner",
                    nameHsIdent = HsIdentifier
                      "S2_inner"},
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s2_d",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "anonymous.h:20:7",
                structFieldName = NamePair {
                  nameC = CName "d",
                  nameHsIdent = HsIdentifier
                    "s2_d"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "anonymous.h:12:8",
              declId = NamePair {
                nameC = CName "S2",
                nameHsIdent = HsIdentifier
                  "S2"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "S2"),
                structSizeof = 12,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "anonymous.h:18:5",
                    structFieldName = NamePair {
                      nameC = CName "inner",
                      nameHsIdent = HsIdentifier
                        "s2_inner"},
                    structFieldType = TypeStruct
                      NamePair {
                        nameC = CName "S2_inner",
                        nameHsIdent = HsIdentifier
                          "S2_inner"},
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "anonymous.h:20:7",
                    structFieldName = NamePair {
                      nameC = CName "d",
                      nameHsIdent = HsIdentifier
                        "s2_d"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
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
          [Eq, Show, Storable]}
      StorableInstance {
        storableSizeOf = 12,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "S2",
                structConstr = HsName
                  "@NsConstr"
                  "S2",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "S2_inner"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:18:5",
                        structFieldName = NamePair {
                          nameC = CName "inner",
                          nameHsIdent = HsIdentifier
                            "s2_inner"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName "S2_inner",
                            nameHsIdent = HsIdentifier
                              "S2_inner"},
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_d",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:20:7",
                        structFieldName = NamePair {
                          nameC = CName "d",
                          nameHsIdent = HsIdentifier
                            "s2_d"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "anonymous.h:12:8",
                      declId = NamePair {
                        nameC = CName "S2",
                        nameHsIdent = HsIdentifier
                          "S2"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S2"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "anonymous.h:18:5",
                            structFieldName = NamePair {
                              nameC = CName "inner",
                              nameHsIdent = HsIdentifier
                                "s2_inner"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "S2_inner",
                                nameHsIdent = HsIdentifier
                                  "S2_inner"},
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "anonymous.h:20:7",
                            structFieldName = NamePair {
                              nameC = CName "d",
                              nameHsIdent = HsIdentifier
                                "s2_d"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
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
                  [Eq, Show, Storable]})
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
                  "S2",
                structConstr = HsName
                  "@NsConstr"
                  "S2",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "S2_inner"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:18:5",
                        structFieldName = NamePair {
                          nameC = CName "inner",
                          nameHsIdent = HsIdentifier
                            "s2_inner"},
                        structFieldType = TypeStruct
                          NamePair {
                            nameC = CName "S2_inner",
                            nameHsIdent = HsIdentifier
                              "S2_inner"},
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_d",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:20:7",
                        structFieldName = NamePair {
                          nameC = CName "d",
                          nameHsIdent = HsIdentifier
                            "s2_d"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "anonymous.h:12:8",
                      declId = NamePair {
                        nameC = CName "S2",
                        nameHsIdent = HsIdentifier
                          "S2"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S2"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "anonymous.h:18:5",
                            structFieldName = NamePair {
                              nameC = CName "inner",
                              nameHsIdent = HsIdentifier
                                "s2_inner"},
                            structFieldType = TypeStruct
                              NamePair {
                                nameC = CName "S2_inner",
                                nameHsIdent = HsIdentifier
                                  "S2_inner"},
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "anonymous.h:20:7",
                            structFieldName = NamePair {
                              nameC = CName "d",
                              nameHsIdent = HsIdentifier
                                "s2_d"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
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
                  [Eq, Show, Storable]}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "S2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S2"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S3_c",
      structConstr = HsName
        "@NsConstr"
        "S3_c",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s3_c_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "anonymous.h:26:9",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "s3_c_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s3_c_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "anonymous.h:27:9",
              structFieldName = NamePair {
                nameC = CName "b",
                nameHsIdent = HsIdentifier
                  "s3_c_b"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "anonymous.h:25:3",
            declId = NamePair {
              nameC = CName "S3_c",
              nameHsIdent = HsIdentifier
                "S3_c"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S3_c"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "anonymous.h:26:9",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "s3_c_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "anonymous.h:27:9",
                  structFieldName = NamePair {
                    nameC = CName "b",
                    nameHsIdent = HsIdentifier
                      "s3_c_b"},
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "S3_c",
        structConstr = HsName
          "@NsConstr"
          "S3_c",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s3_c_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "anonymous.h:26:9",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "s3_c_a"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s3_c_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "anonymous.h:27:9",
                structFieldName = NamePair {
                  nameC = CName "b",
                  nameHsIdent = HsIdentifier
                    "s3_c_b"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "anonymous.h:25:3",
              declId = NamePair {
                nameC = CName "S3_c",
                nameHsIdent = HsIdentifier
                  "S3_c"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "S3_c"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "anonymous.h:26:9",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "s3_c_a"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "anonymous.h:27:9",
                    structFieldName = NamePair {
                      nameC = CName "b",
                      nameHsIdent = HsIdentifier
                        "s3_c_b"},
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
          [Eq, Show, Storable]}
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
                  "S3_c",
                structConstr = HsName
                  "@NsConstr"
                  "S3_c",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s3_c_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:26:9",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s3_c_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s3_c_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:27:9",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s3_c_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "anonymous.h:25:3",
                      declId = NamePair {
                        nameC = CName "S3_c",
                        nameHsIdent = HsIdentifier
                          "S3_c"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S3_c"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "anonymous.h:26:9",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s3_c_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "anonymous.h:27:9",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s3_c_b"},
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
                  [Eq, Show, Storable]})
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
                  "S3_c",
                structConstr = HsName
                  "@NsConstr"
                  "S3_c",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s3_c_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:26:9",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s3_c_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s3_c_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:27:9",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s3_c_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "anonymous.h:25:3",
                      declId = NamePair {
                        nameC = CName "S3_c",
                        nameHsIdent = HsIdentifier
                          "S3_c"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S3_c"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "anonymous.h:26:9",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s3_c_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "anonymous.h:27:9",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s3_c_b"},
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
                  [Eq, Show, Storable]}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "S3_c"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S3_c"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S3",
      structConstr = HsName
        "@NsConstr"
        "S3",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s3_c",
          fieldType = HsPtr
            (HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "S3_c"))),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "anonymous.h:28:7",
              structFieldName = NamePair {
                nameC = CName "c",
                nameHsIdent = HsIdentifier
                  "s3_c"},
              structFieldType = TypePointer
                (TypePointer
                  (TypeStruct
                    NamePair {
                      nameC = CName "S3_c",
                      nameHsIdent = HsIdentifier
                        "S3_c"})),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s3_d",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "anonymous.h:30:7",
              structFieldName = NamePair {
                nameC = CName "d",
                nameHsIdent = HsIdentifier
                  "s3_d"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "anonymous.h:24:8",
            declId = NamePair {
              nameC = CName "S3",
              nameHsIdent = HsIdentifier
                "S3"}},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S3"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "anonymous.h:28:7",
                  structFieldName = NamePair {
                    nameC = CName "c",
                    nameHsIdent = HsIdentifier
                      "s3_c"},
                  structFieldType = TypePointer
                    (TypePointer
                      (TypeStruct
                        NamePair {
                          nameC = CName "S3_c",
                          nameHsIdent = HsIdentifier
                            "S3_c"})),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "anonymous.h:30:7",
                  structFieldName = NamePair {
                    nameC = CName "d",
                    nameHsIdent = HsIdentifier
                      "s3_d"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "S3",
        structConstr = HsName
          "@NsConstr"
          "S3",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s3_c",
            fieldType = HsPtr
              (HsPtr
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "S3_c"))),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "anonymous.h:28:7",
                structFieldName = NamePair {
                  nameC = CName "c",
                  nameHsIdent = HsIdentifier
                    "s3_c"},
                structFieldType = TypePointer
                  (TypePointer
                    (TypeStruct
                      NamePair {
                        nameC = CName "S3_c",
                        nameHsIdent = HsIdentifier
                          "S3_c"})),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s3_d",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "anonymous.h:30:7",
                structFieldName = NamePair {
                  nameC = CName "d",
                  nameHsIdent = HsIdentifier
                    "s3_d"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "anonymous.h:24:8",
              declId = NamePair {
                nameC = CName "S3",
                nameHsIdent = HsIdentifier
                  "S3"}},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "S3"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "anonymous.h:28:7",
                    structFieldName = NamePair {
                      nameC = CName "c",
                      nameHsIdent = HsIdentifier
                        "s3_c"},
                    structFieldType = TypePointer
                      (TypePointer
                        (TypeStruct
                          NamePair {
                            nameC = CName "S3_c",
                            nameHsIdent = HsIdentifier
                              "S3_c"})),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "anonymous.h:30:7",
                    structFieldName = NamePair {
                      nameC = CName "d",
                      nameHsIdent = HsIdentifier
                        "s3_d"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
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
          [Eq, Show, Storable]}
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
                  "S3",
                structConstr = HsName
                  "@NsConstr"
                  "S3",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s3_c",
                    fieldType = HsPtr
                      (HsPtr
                        (HsTypRef
                          (HsName
                            "@NsTypeConstr"
                            "S3_c"))),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:28:7",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "s3_c"},
                        structFieldType = TypePointer
                          (TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = CName "S3_c",
                                nameHsIdent = HsIdentifier
                                  "S3_c"})),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s3_d",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:30:7",
                        structFieldName = NamePair {
                          nameC = CName "d",
                          nameHsIdent = HsIdentifier
                            "s3_d"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "anonymous.h:24:8",
                      declId = NamePair {
                        nameC = CName "S3",
                        nameHsIdent = HsIdentifier
                          "S3"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S3"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "anonymous.h:28:7",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "s3_c"},
                            structFieldType = TypePointer
                              (TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = CName "S3_c",
                                    nameHsIdent = HsIdentifier
                                      "S3_c"})),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "anonymous.h:30:7",
                            structFieldName = NamePair {
                              nameC = CName "d",
                              nameHsIdent = HsIdentifier
                                "s3_d"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
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
                  [Eq, Show, Storable]})
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
                  "S3",
                structConstr = HsName
                  "@NsConstr"
                  "S3",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s3_c",
                    fieldType = HsPtr
                      (HsPtr
                        (HsTypRef
                          (HsName
                            "@NsTypeConstr"
                            "S3_c"))),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:28:7",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "s3_c"},
                        structFieldType = TypePointer
                          (TypePointer
                            (TypeStruct
                              NamePair {
                                nameC = CName "S3_c",
                                nameHsIdent = HsIdentifier
                                  "S3_c"})),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s3_d",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "anonymous.h:30:7",
                        structFieldName = NamePair {
                          nameC = CName "d",
                          nameHsIdent = HsIdentifier
                            "s3_d"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "anonymous.h:24:8",
                      declId = NamePair {
                        nameC = CName "S3",
                        nameHsIdent = HsIdentifier
                          "S3"}},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S3"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "anonymous.h:28:7",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "s3_c"},
                            structFieldType = TypePointer
                              (TypePointer
                                (TypeStruct
                                  NamePair {
                                    nameC = CName "S3_c",
                                    nameHsIdent = HsIdentifier
                                      "S3_c"})),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "anonymous.h:30:7",
                            structFieldName = NamePair {
                              nameC = CName "d",
                              nameHsIdent = HsIdentifier
                                "s3_d"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
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
                  [Eq, Show, Storable]}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "S3"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S3")]
