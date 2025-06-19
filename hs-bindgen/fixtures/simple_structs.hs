[
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
            "s1_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:3:9",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "s1_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s1_b",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:4:10",
              structFieldName = NamePair {
                nameC = CName "b",
                nameHsIdent = HsIdentifier
                  "s1_b"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:2:8",
            declId = NamePair {
              nameC = CName "S1",
              nameHsIdent = HsIdentifier
                "S1"},
            declOrigin = NameOriginInSource,
            declAliases = []},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S1"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "simple_structs.h:3:9",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "s1_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "simple_structs.h:4:10",
                  structFieldName = NamePair {
                    nameC = CName "b",
                    nameHsIdent = HsIdentifier
                      "s1_b"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
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
          "S1",
        structConstr = HsName
          "@NsConstr"
          "S1",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s1_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:3:9",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "s1_a"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s1_b",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:4:10",
                structFieldName = NamePair {
                  nameC = CName "b",
                  nameHsIdent = HsIdentifier
                    "s1_b"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "simple_structs.h:2:8",
              declId = NamePair {
                nameC = CName "S1",
                nameHsIdent = HsIdentifier
                  "S1"},
              declOrigin = NameOriginInSource,
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "S1"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:3:9",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "s1_a"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:4:10",
                    structFieldName = NamePair {
                      nameC = CName "b",
                      nameHsIdent = HsIdentifier
                        "s1_b"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
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
                  "S1",
                structConstr = HsName
                  "@NsConstr"
                  "S1",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:3:9",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s1_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_b",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:4:10",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s1_b"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:2:8",
                      declId = NamePair {
                        nameC = CName "S1",
                        nameHsIdent = HsIdentifier
                          "S1"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S1"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:3:9",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s1_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:4:10",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s1_b"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
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
                  "S1",
                structConstr = HsName
                  "@NsConstr"
                  "S1",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:3:9",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s1_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_b",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:4:10",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s1_b"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:2:8",
                      declId = NamePair {
                        nameC = CName "S1",
                        nameHsIdent = HsIdentifier
                          "S1"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S1"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:3:9",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s1_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:4:10",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s1_b"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
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
    (HsName "@NsTypeConstr" "S1"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S1"),
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
            "s2_a",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:9:10",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "s2_a"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:10:9",
              structFieldName = NamePair {
                nameC = CName "b",
                nameHsIdent = HsIdentifier
                  "s2_b"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_c",
          fieldType = HsPrimType
            HsPrimCFloat,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:11:11",
              structFieldName = NamePair {
                nameC = CName "c",
                nameHsIdent = HsIdentifier
                  "s2_c"},
              structFieldType = TypePrim
                (PrimFloating PrimFloat),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:8:16",
            declId = NamePair {
              nameC = CName "S2",
              nameHsIdent = HsIdentifier
                "S2"},
            declOrigin = NameOriginInSource,
            declAliases = [CName "S2_t"]},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S2"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "simple_structs.h:9:10",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "s2_a"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "simple_structs.h:10:9",
                  structFieldName = NamePair {
                    nameC = CName "b",
                    nameHsIdent = HsIdentifier
                      "s2_b"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "simple_structs.h:11:11",
                  structFieldName = NamePair {
                    nameC = CName "c",
                    nameHsIdent = HsIdentifier
                      "s2_c"},
                  structFieldType = TypePrim
                    (PrimFloating PrimFloat),
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
              "s2_a",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:9:10",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "s2_a"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s2_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:10:9",
                structFieldName = NamePair {
                  nameC = CName "b",
                  nameHsIdent = HsIdentifier
                    "s2_b"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s2_c",
            fieldType = HsPrimType
              HsPrimCFloat,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:11:11",
                structFieldName = NamePair {
                  nameC = CName "c",
                  nameHsIdent = HsIdentifier
                    "s2_c"},
                structFieldType = TypePrim
                  (PrimFloating PrimFloat),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "simple_structs.h:8:16",
              declId = NamePair {
                nameC = CName "S2",
                nameHsIdent = HsIdentifier
                  "S2"},
              declOrigin = NameOriginInSource,
              declAliases = [CName "S2_t"]},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "S2"),
                structSizeof = 12,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:9:10",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "s2_a"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:10:9",
                    structFieldName = NamePair {
                      nameC = CName "b",
                      nameHsIdent = HsIdentifier
                        "s2_b"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 32,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:11:11",
                    structFieldName = NamePair {
                      nameC = CName "c",
                      nameHsIdent = HsIdentifier
                        "s2_c"},
                    structFieldType = TypePrim
                      (PrimFloating PrimFloat),
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
                      "s2_a",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:9:10",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s2_a"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:10:9",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s2_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_c",
                    fieldType = HsPrimType
                      HsPrimCFloat,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:11:11",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "s2_c"},
                        structFieldType = TypePrim
                          (PrimFloating PrimFloat),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:8:16",
                      declId = NamePair {
                        nameC = CName "S2",
                        nameHsIdent = HsIdentifier
                          "S2"},
                      declOrigin = NameOriginInSource,
                      declAliases = [CName "S2_t"]},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S2"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:9:10",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s2_a"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:10:9",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s2_b"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:11:11",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "s2_c"},
                            structFieldType = TypePrim
                              (PrimFloating PrimFloat),
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
              PeekByteOff (Idx 0) 4,
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
                      "s2_a",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:9:10",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s2_a"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:10:9",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s2_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_c",
                    fieldType = HsPrimType
                      HsPrimCFloat,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:11:11",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "s2_c"},
                        structFieldType = TypePrim
                          (PrimFloating PrimFloat),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:8:16",
                      declId = NamePair {
                        nameC = CName "S2",
                        nameHsIdent = HsIdentifier
                          "S2"},
                      declOrigin = NameOriginInSource,
                      declAliases = [CName "S2_t"]},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S2"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:9:10",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s2_a"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:10:9",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s2_b"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:11:11",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "s2_c"},
                            structFieldType = TypePrim
                              (PrimFloating PrimFloat),
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
              (Add 3)
              (Seq
                [
                  PokeByteOff (Idx 4) 0 (Idx 0),
                  PokeByteOff (Idx 4) 4 (Idx 1),
                  PokeByteOff
                    (Idx 4)
                    8
                    (Idx 2)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "S2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S2"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "S2_t",
      newtypeConstr = HsName
        "@NsConstr"
        "S2_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_S2_t",
        fieldType = HsTypRef
          (HsName "@NsTypeConstr" "S2"),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "simple_structs.h:12:3",
          declId = NamePair {
            nameC = CName "S2_t",
            nameHsIdent = HsIdentifier
              "S2_t"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "S2_t",
              newtypeField = HsName
                "@NsVar"
                "un_S2_t"},
            typedefType = TypeStruct
              NamePair {
                nameC = CName "S2",
                nameHsIdent = HsIdentifier "S2"}
              NameOriginInSource},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "S2_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S2_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "S2_t"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S3_t",
      structConstr = HsName
        "@NsConstr"
        "S3_t",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s3_t_a",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:16:10",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "s3_t_a"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:15:9",
            declId = NamePair {
              nameC = CName "S3_t",
              nameHsIdent = HsIdentifier
                "S3_t"},
            declOrigin = NameOriginGenerated
              (AnonId
                "simple_structs.h:15:9"),
            declAliases = [CName "S3_t"]},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S3_t"),
              structSizeof = 1,
              structAlignment = 1,
              structFields = [
                StructField {
                  structFieldLoc =
                  "simple_structs.h:16:10",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "s3_t_a"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
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
          "S3_t",
        structConstr = HsName
          "@NsConstr"
          "S3_t",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s3_t_a",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:16:10",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "s3_t_a"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "simple_structs.h:15:9",
              declId = NamePair {
                nameC = CName "S3_t",
                nameHsIdent = HsIdentifier
                  "S3_t"},
              declOrigin = NameOriginGenerated
                (AnonId
                  "simple_structs.h:15:9"),
              declAliases = [CName "S3_t"]},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "S3_t"),
                structSizeof = 1,
                structAlignment = 1,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:16:10",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "s3_t_a"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
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
        storableSizeOf = 1,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "S3_t",
                structConstr = HsName
                  "@NsConstr"
                  "S3_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s3_t_a",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:16:10",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s3_t_a"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:15:9",
                      declId = NamePair {
                        nameC = CName "S3_t",
                        nameHsIdent = HsIdentifier
                          "S3_t"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "simple_structs.h:15:9"),
                      declAliases = [CName "S3_t"]},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S3_t"),
                        structSizeof = 1,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:16:10",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s3_t_a"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
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
                  "S3_t",
                structConstr = HsName
                  "@NsConstr"
                  "S3_t",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s3_t_a",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:16:10",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s3_t_a"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:15:9",
                      declId = NamePair {
                        nameC = CName "S3_t",
                        nameHsIdent = HsIdentifier
                          "S3_t"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "simple_structs.h:15:9"),
                      declAliases = [CName "S3_t"]},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S3_t"),
                        structSizeof = 1,
                        structAlignment = 1,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:16:10",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s3_t_a"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
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
    (HsName "@NsTypeConstr" "S3_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S3_t"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S4",
      structConstr = HsName
        "@NsConstr"
        "S4",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s4_b",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:20:10",
              structFieldName = NamePair {
                nameC = CName "b",
                nameHsIdent = HsIdentifier
                  "s4_b"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s4_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:21:9",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "s4_a"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s4_c",
          fieldType = HsPtr
            (HsPrimType HsPrimCInt),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:22:10",
              structFieldName = NamePair {
                nameC = CName "c",
                nameHsIdent = HsIdentifier
                  "s4_c"},
              structFieldType = TypePointer
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:19:8",
            declId = NamePair {
              nameC = CName "S4",
              nameHsIdent = HsIdentifier
                "S4"},
            declOrigin = NameOriginInSource,
            declAliases = []},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S4"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "simple_structs.h:20:10",
                  structFieldName = NamePair {
                    nameC = CName "b",
                    nameHsIdent = HsIdentifier
                      "s4_b"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "simple_structs.h:21:9",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "s4_a"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "simple_structs.h:22:10",
                  structFieldName = NamePair {
                    nameC = CName "c",
                    nameHsIdent = HsIdentifier
                      "s4_c"},
                  structFieldType = TypePointer
                    (TypePrim
                      (PrimIntegral PrimInt Signed)),
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
          "S4",
        structConstr = HsName
          "@NsConstr"
          "S4",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s4_b",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:20:10",
                structFieldName = NamePair {
                  nameC = CName "b",
                  nameHsIdent = HsIdentifier
                    "s4_b"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s4_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:21:9",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "s4_a"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s4_c",
            fieldType = HsPtr
              (HsPrimType HsPrimCInt),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:22:10",
                structFieldName = NamePair {
                  nameC = CName "c",
                  nameHsIdent = HsIdentifier
                    "s4_c"},
                structFieldType = TypePointer
                  (TypePrim
                    (PrimIntegral PrimInt Signed)),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "simple_structs.h:19:8",
              declId = NamePair {
                nameC = CName "S4",
                nameHsIdent = HsIdentifier
                  "S4"},
              declOrigin = NameOriginInSource,
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "S4"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:20:10",
                    structFieldName = NamePair {
                      nameC = CName "b",
                      nameHsIdent = HsIdentifier
                        "s4_b"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:21:9",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "s4_a"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 32,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:22:10",
                    structFieldName = NamePair {
                      nameC = CName "c",
                      nameHsIdent = HsIdentifier
                        "s4_c"},
                    structFieldType = TypePointer
                      (TypePrim
                        (PrimIntegral PrimInt Signed)),
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
                  "S4",
                structConstr = HsName
                  "@NsConstr"
                  "S4",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s4_b",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:20:10",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s4_b"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s4_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:21:9",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s4_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s4_c",
                    fieldType = HsPtr
                      (HsPrimType HsPrimCInt),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:22:10",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "s4_c"},
                        structFieldType = TypePointer
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:19:8",
                      declId = NamePair {
                        nameC = CName "S4",
                        nameHsIdent = HsIdentifier
                          "S4"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S4"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:20:10",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s4_b"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:21:9",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s4_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:22:10",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "s4_c"},
                            structFieldType = TypePointer
                              (TypePrim
                                (PrimIntegral PrimInt Signed)),
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
              PeekByteOff (Idx 0) 4,
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
                  "S4",
                structConstr = HsName
                  "@NsConstr"
                  "S4",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s4_b",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:20:10",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s4_b"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s4_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:21:9",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s4_a"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s4_c",
                    fieldType = HsPtr
                      (HsPrimType HsPrimCInt),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:22:10",
                        structFieldName = NamePair {
                          nameC = CName "c",
                          nameHsIdent = HsIdentifier
                            "s4_c"},
                        structFieldType = TypePointer
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:19:8",
                      declId = NamePair {
                        nameC = CName "S4",
                        nameHsIdent = HsIdentifier
                          "S4"},
                      declOrigin = NameOriginInSource,
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S4"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:20:10",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s4_b"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:21:9",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s4_a"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:22:10",
                            structFieldName = NamePair {
                              nameC = CName "c",
                              nameHsIdent = HsIdentifier
                                "s4_c"},
                            structFieldType = TypePointer
                              (TypePrim
                                (PrimIntegral PrimInt Signed)),
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
              (Add 3)
              (Seq
                [
                  PokeByteOff (Idx 4) 0 (Idx 0),
                  PokeByteOff (Idx 4) 4 (Idx 1),
                  PokeByteOff
                    (Idx 4)
                    8
                    (Idx 2)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "S4"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S4"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S5",
      structConstr = HsName
        "@NsConstr"
        "S5",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s5_a",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:27:10",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "s5_a"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s5_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:28:9",
              structFieldName = NamePair {
                nameC = CName "b",
                nameHsIdent = HsIdentifier
                  "s5_b"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:26:16",
            declId = NamePair {
              nameC = CName "S5",
              nameHsIdent = HsIdentifier
                "S5"},
            declOrigin = NameOriginInSource,
            declAliases = [CName "S5"]},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S5"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "simple_structs.h:27:10",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "s5_a"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "simple_structs.h:28:9",
                  structFieldName = NamePair {
                    nameC = CName "b",
                    nameHsIdent = HsIdentifier
                      "s5_b"},
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
          "S5",
        structConstr = HsName
          "@NsConstr"
          "S5",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s5_a",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:27:10",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "s5_a"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s5_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:28:9",
                structFieldName = NamePair {
                  nameC = CName "b",
                  nameHsIdent = HsIdentifier
                    "s5_b"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "simple_structs.h:26:16",
              declId = NamePair {
                nameC = CName "S5",
                nameHsIdent = HsIdentifier
                  "S5"},
              declOrigin = NameOriginInSource,
              declAliases = [CName "S5"]},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "S5"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:27:10",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "s5_a"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:28:9",
                    structFieldName = NamePair {
                      nameC = CName "b",
                      nameHsIdent = HsIdentifier
                        "s5_b"},
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
                  "S5",
                structConstr = HsName
                  "@NsConstr"
                  "S5",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s5_a",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:27:10",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s5_a"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s5_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:28:9",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s5_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:26:16",
                      declId = NamePair {
                        nameC = CName "S5",
                        nameHsIdent = HsIdentifier
                          "S5"},
                      declOrigin = NameOriginInSource,
                      declAliases = [CName "S5"]},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S5"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:27:10",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s5_a"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:28:9",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s5_b"},
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
                  "S5",
                structConstr = HsName
                  "@NsConstr"
                  "S5",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s5_a",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:27:10",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s5_a"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s5_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:28:9",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s5_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:26:16",
                      declId = NamePair {
                        nameC = CName "S5",
                        nameHsIdent = HsIdentifier
                          "S5"},
                      declOrigin = NameOriginInSource,
                      declAliases = [CName "S5"]},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S5"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:27:10",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s5_a"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:28:9",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s5_b"},
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
    (HsName "@NsTypeConstr" "S5"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S5"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S6",
      structConstr = HsName
        "@NsConstr"
        "S6",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s6_a",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:31:18",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "s6_a"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s6_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:31:25",
              structFieldName = NamePair {
                nameC = CName "b",
                nameHsIdent = HsIdentifier
                  "s6_b"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:31:8",
            declId = NamePair {
              nameC = CName "S6",
              nameHsIdent = HsIdentifier
                "S6"},
            declOrigin = NameOriginInSource,
            declAliases = [CName "S6"]},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "S6"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "simple_structs.h:31:18",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "s6_a"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "simple_structs.h:31:25",
                  structFieldName = NamePair {
                    nameC = CName "b",
                    nameHsIdent = HsIdentifier
                      "s6_b"},
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
          "S6",
        structConstr = HsName
          "@NsConstr"
          "S6",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s6_a",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:31:18",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "s6_a"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s6_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:31:25",
                structFieldName = NamePair {
                  nameC = CName "b",
                  nameHsIdent = HsIdentifier
                    "s6_b"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "simple_structs.h:31:8",
              declId = NamePair {
                nameC = CName "S6",
                nameHsIdent = HsIdentifier
                  "S6"},
              declOrigin = NameOriginInSource,
              declAliases = [CName "S6"]},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "S6"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:31:18",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "s6_a"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:31:25",
                    structFieldName = NamePair {
                      nameC = CName "b",
                      nameHsIdent = HsIdentifier
                        "s6_b"},
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
                  "S6",
                structConstr = HsName
                  "@NsConstr"
                  "S6",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s6_a",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:31:18",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s6_a"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s6_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:31:25",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s6_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:31:8",
                      declId = NamePair {
                        nameC = CName "S6",
                        nameHsIdent = HsIdentifier
                          "S6"},
                      declOrigin = NameOriginInSource,
                      declAliases = [CName "S6"]},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S6"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:31:18",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s6_a"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:31:25",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s6_b"},
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
                  "S6",
                structConstr = HsName
                  "@NsConstr"
                  "S6",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s6_a",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:31:18",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s6_a"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s6_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:31:25",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s6_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:31:8",
                      declId = NamePair {
                        nameC = CName "S6",
                        nameHsIdent = HsIdentifier
                          "S6"},
                      declOrigin = NameOriginInSource,
                      declAliases = [CName "S6"]},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "S6"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:31:18",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s6_a"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:31:25",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s6_b"},
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
    (HsName "@NsTypeConstr" "S6"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S6"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S7a_Deref",
      structConstr = HsName
        "@NsConstr"
        "S7a_Deref",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s7a_Deref_a",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:34:23",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "s7a_Deref_a"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s7a_Deref_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:34:30",
              structFieldName = NamePair {
                nameC = CName "b",
                nameHsIdent = HsIdentifier
                  "s7a_Deref_b"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:34:9",
            declId = NamePair {
              nameC = CName "S7a_Deref",
              nameHsIdent = HsIdentifier
                "S7a_Deref"},
            declOrigin = NameOriginGenerated
              (AnonId
                "simple_structs.h:34:9"),
            declAliases = []},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "S7a_Deref"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "simple_structs.h:34:23",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "s7a_Deref_a"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "simple_structs.h:34:30",
                  structFieldName = NamePair {
                    nameC = CName "b",
                    nameHsIdent = HsIdentifier
                      "s7a_Deref_b"},
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
          "S7a_Deref",
        structConstr = HsName
          "@NsConstr"
          "S7a_Deref",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s7a_Deref_a",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:34:23",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "s7a_Deref_a"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s7a_Deref_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:34:30",
                structFieldName = NamePair {
                  nameC = CName "b",
                  nameHsIdent = HsIdentifier
                    "s7a_Deref_b"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "simple_structs.h:34:9",
              declId = NamePair {
                nameC = CName "S7a_Deref",
                nameHsIdent = HsIdentifier
                  "S7a_Deref"},
              declOrigin = NameOriginGenerated
                (AnonId
                  "simple_structs.h:34:9"),
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "S7a_Deref"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:34:23",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "s7a_Deref_a"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:34:30",
                    structFieldName = NamePair {
                      nameC = CName "b",
                      nameHsIdent = HsIdentifier
                        "s7a_Deref_b"},
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
                  "S7a_Deref",
                structConstr = HsName
                  "@NsConstr"
                  "S7a_Deref",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s7a_Deref_a",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:34:23",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s7a_Deref_a"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s7a_Deref_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:34:30",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s7a_Deref_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:34:9",
                      declId = NamePair {
                        nameC = CName "S7a_Deref",
                        nameHsIdent = HsIdentifier
                          "S7a_Deref"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "simple_structs.h:34:9"),
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "S7a_Deref"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:34:23",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s7a_Deref_a"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:34:30",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s7a_Deref_b"},
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
                  "S7a_Deref",
                structConstr = HsName
                  "@NsConstr"
                  "S7a_Deref",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s7a_Deref_a",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:34:23",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s7a_Deref_a"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s7a_Deref_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:34:30",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s7a_Deref_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:34:9",
                      declId = NamePair {
                        nameC = CName "S7a_Deref",
                        nameHsIdent = HsIdentifier
                          "S7a_Deref"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "simple_structs.h:34:9"),
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "S7a_Deref"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:34:23",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s7a_Deref_a"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:34:30",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s7a_Deref_b"},
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
    (HsName
      "@NsTypeConstr"
      "S7a_Deref"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "S7a_Deref"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "S7a",
      newtypeConstr = HsName
        "@NsConstr"
        "S7a",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_S7a",
        fieldType = HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "S7a_Deref")),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "simple_structs.h:34:36",
          declId = NamePair {
            nameC = CName "S7a",
            nameHsIdent = HsIdentifier
              "S7a"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "S7a",
              newtypeField = HsName
                "@NsVar"
                "un_S7a"},
            typedefType = TypePointer
              (TypeStruct
                NamePair {
                  nameC = CName "S7a_Deref",
                  nameHsIdent = HsIdentifier
                    "S7a_Deref"}
                (NameOriginGenerated
                  (AnonId
                    "simple_structs.h:34:9")))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "S7a"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S7a"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "S7a"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "S7a"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S7b_Deref",
      structConstr = HsName
        "@NsConstr"
        "S7b_Deref",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s7b_Deref_a",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:35:23",
              structFieldName = NamePair {
                nameC = CName "a",
                nameHsIdent = HsIdentifier
                  "s7b_Deref_a"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s7b_Deref_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "simple_structs.h:35:30",
              structFieldName = NamePair {
                nameC = CName "b",
                nameHsIdent = HsIdentifier
                  "s7b_Deref_b"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc =
            "simple_structs.h:35:9",
            declId = NamePair {
              nameC = CName "S7b_Deref",
              nameHsIdent = HsIdentifier
                "S7b_Deref"},
            declOrigin = NameOriginGenerated
              (AnonId
                "simple_structs.h:35:9"),
            declAliases = []},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "S7b_Deref"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "simple_structs.h:35:23",
                  structFieldName = NamePair {
                    nameC = CName "a",
                    nameHsIdent = HsIdentifier
                      "s7b_Deref_a"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "simple_structs.h:35:30",
                  structFieldName = NamePair {
                    nameC = CName "b",
                    nameHsIdent = HsIdentifier
                      "s7b_Deref_b"},
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
          "S7b_Deref",
        structConstr = HsName
          "@NsConstr"
          "S7b_Deref",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s7b_Deref_a",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:35:23",
                structFieldName = NamePair {
                  nameC = CName "a",
                  nameHsIdent = HsIdentifier
                    "s7b_Deref_a"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s7b_Deref_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "simple_structs.h:35:30",
                structFieldName = NamePair {
                  nameC = CName "b",
                  nameHsIdent = HsIdentifier
                    "s7b_Deref_b"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc =
              "simple_structs.h:35:9",
              declId = NamePair {
                nameC = CName "S7b_Deref",
                nameHsIdent = HsIdentifier
                  "S7b_Deref"},
              declOrigin = NameOriginGenerated
                (AnonId
                  "simple_structs.h:35:9"),
              declAliases = []},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "S7b_Deref"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:35:23",
                    structFieldName = NamePair {
                      nameC = CName "a",
                      nameHsIdent = HsIdentifier
                        "s7b_Deref_a"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "simple_structs.h:35:30",
                    structFieldName = NamePair {
                      nameC = CName "b",
                      nameHsIdent = HsIdentifier
                        "s7b_Deref_b"},
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
                  "S7b_Deref",
                structConstr = HsName
                  "@NsConstr"
                  "S7b_Deref",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s7b_Deref_a",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:35:23",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s7b_Deref_a"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s7b_Deref_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:35:30",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s7b_Deref_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:35:9",
                      declId = NamePair {
                        nameC = CName "S7b_Deref",
                        nameHsIdent = HsIdentifier
                          "S7b_Deref"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "simple_structs.h:35:9"),
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "S7b_Deref"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:35:23",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s7b_Deref_a"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:35:30",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s7b_Deref_b"},
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
                  "S7b_Deref",
                structConstr = HsName
                  "@NsConstr"
                  "S7b_Deref",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s7b_Deref_a",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:35:23",
                        structFieldName = NamePair {
                          nameC = CName "a",
                          nameHsIdent = HsIdentifier
                            "s7b_Deref_a"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s7b_Deref_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "simple_structs.h:35:30",
                        structFieldName = NamePair {
                          nameC = CName "b",
                          nameHsIdent = HsIdentifier
                            "s7b_Deref_b"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc =
                      "simple_structs.h:35:9",
                      declId = NamePair {
                        nameC = CName "S7b_Deref",
                        nameHsIdent = HsIdentifier
                          "S7b_Deref"},
                      declOrigin = NameOriginGenerated
                        (AnonId
                          "simple_structs.h:35:9"),
                      declAliases = []},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "S7b_Deref"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:35:23",
                            structFieldName = NamePair {
                              nameC = CName "a",
                              nameHsIdent = HsIdentifier
                                "s7b_Deref_a"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "simple_structs.h:35:30",
                            structFieldName = NamePair {
                              nameC = CName "b",
                              nameHsIdent = HsIdentifier
                                "s7b_Deref_b"},
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
    (HsName
      "@NsTypeConstr"
      "S7b_Deref"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "S7b_Deref"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "S7b",
      newtypeConstr = HsName
        "@NsConstr"
        "S7b",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_S7b",
        fieldType = HsPtr
          (HsPtr
            (HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "S7b_Deref")))),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "simple_structs.h:35:38",
          declId = NamePair {
            nameC = CName "S7b",
            nameHsIdent = HsIdentifier
              "S7b"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "S7b",
              newtypeField = HsName
                "@NsVar"
                "un_S7b"},
            typedefType = TypePointer
              (TypePointer
                (TypePointer
                  (TypeStruct
                    NamePair {
                      nameC = CName "S7b_Deref",
                      nameHsIdent = HsIdentifier
                        "S7b_Deref"}
                    (NameOriginGenerated
                      (AnonId
                        "simple_structs.h:35:9")))))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "S7b"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S7b"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "S7b"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "S7b")]
