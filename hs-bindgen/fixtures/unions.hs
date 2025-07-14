[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Dim2",
      structConstr = HsName
        "@NsConstr"
        "Dim2",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "dim2_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "unions.h:2:9",
              structFieldName = NamePair {
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "dim2_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "dim2_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "unions.h:3:9",
              structFieldName = NamePair {
                nameC = Name "y",
                nameHsIdent = HsIdentifier
                  "dim2_y"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:1:8",
            declId = NamePair {
              nameC = Name "Dim2",
              nameHsIdent = HsIdentifier
                "Dim2"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "unions.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Dim2"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc = "unions.h:2:9",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "dim2_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc = "unions.h:3:9",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "dim2_y"},
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
          "Dim2",
        structConstr = HsName
          "@NsConstr"
          "Dim2",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "dim2_x",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc = "unions.h:2:9",
                structFieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "dim2_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "dim2_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc = "unions.h:3:9",
                structFieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "dim2_y"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "unions.h:1:8",
              declId = NamePair {
                nameC = Name "Dim2",
                nameHsIdent = HsIdentifier
                  "Dim2"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "unions.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Dim2"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc = "unions.h:2:9",
                    structFieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "dim2_x"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc = "unions.h:3:9",
                    structFieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "dim2_y"},
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
                  "Dim2",
                structConstr = HsName
                  "@NsConstr"
                  "Dim2",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim2_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "unions.h:2:9",
                        structFieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "dim2_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim2_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "unions.h:3:9",
                        structFieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "dim2_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "unions.h:1:8",
                      declId = NamePair {
                        nameC = Name "Dim2",
                        nameHsIdent = HsIdentifier
                          "Dim2"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "unions.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Dim2"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc = "unions.h:2:9",
                            structFieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "dim2_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "unions.h:3:9",
                            structFieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "dim2_y"},
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
                  "Dim2",
                structConstr = HsName
                  "@NsConstr"
                  "Dim2",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim2_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "unions.h:2:9",
                        structFieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "dim2_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim2_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "unions.h:3:9",
                        structFieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "dim2_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "unions.h:1:8",
                      declId = NamePair {
                        nameC = Name "Dim2",
                        nameHsIdent = HsIdentifier
                          "Dim2"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "unions.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Dim2"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc = "unions.h:2:9",
                            structFieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "dim2_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "unions.h:3:9",
                            structFieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "dim2_y"},
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
    (HsName "@NsTypeConstr" "Dim2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Dim2"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Dim3",
      structConstr = HsName
        "@NsConstr"
        "Dim3",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "dim3_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "unions.h:7:9",
              structFieldName = NamePair {
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "dim3_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "dim3_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "unions.h:8:9",
              structFieldName = NamePair {
                nameC = Name "y",
                nameHsIdent = HsIdentifier
                  "dim3_y"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "dim3_z",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc = "unions.h:9:9",
              structFieldName = NamePair {
                nameC = Name "z",
                nameHsIdent = HsIdentifier
                  "dim3_z"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:6:8",
            declId = NamePair {
              nameC = Name "Dim3",
              nameHsIdent = HsIdentifier
                "Dim3"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "unions.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Dim3"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc = "unions.h:7:9",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "dim3_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc = "unions.h:8:9",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "dim3_y"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc = "unions.h:9:9",
                  structFieldName = NamePair {
                    nameC = Name "z",
                    nameHsIdent = HsIdentifier
                      "dim3_z"},
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
          "Dim3",
        structConstr = HsName
          "@NsConstr"
          "Dim3",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "dim3_x",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc = "unions.h:7:9",
                structFieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "dim3_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "dim3_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc = "unions.h:8:9",
                structFieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "dim3_y"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "dim3_z",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc = "unions.h:9:9",
                structFieldName = NamePair {
                  nameC = Name "z",
                  nameHsIdent = HsIdentifier
                    "dim3_z"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "unions.h:6:8",
              declId = NamePair {
                nameC = Name "Dim3",
                nameHsIdent = HsIdentifier
                  "Dim3"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "unions.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Dim3"),
                structSizeof = 12,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc = "unions.h:7:9",
                    structFieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "dim3_x"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc = "unions.h:8:9",
                    structFieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "dim3_y"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 32,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc = "unions.h:9:9",
                    structFieldName = NamePair {
                      nameC = Name "z",
                      nameHsIdent = HsIdentifier
                        "dim3_z"},
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
                  "Dim3",
                structConstr = HsName
                  "@NsConstr"
                  "Dim3",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim3_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "unions.h:7:9",
                        structFieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "dim3_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim3_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "unions.h:8:9",
                        structFieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "dim3_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim3_z",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "unions.h:9:9",
                        structFieldName = NamePair {
                          nameC = Name "z",
                          nameHsIdent = HsIdentifier
                            "dim3_z"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "unions.h:6:8",
                      declId = NamePair {
                        nameC = Name "Dim3",
                        nameHsIdent = HsIdentifier
                          "Dim3"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "unions.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Dim3"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc = "unions.h:7:9",
                            structFieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "dim3_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "unions.h:8:9",
                            structFieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "dim3_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "unions.h:9:9",
                            structFieldName = NamePair {
                              nameC = Name "z",
                              nameHsIdent = HsIdentifier
                                "dim3_z"},
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
                  "Dim3",
                structConstr = HsName
                  "@NsConstr"
                  "Dim3",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim3_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "unions.h:7:9",
                        structFieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "dim3_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim3_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "unions.h:8:9",
                        structFieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "dim3_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim3_z",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc = "unions.h:9:9",
                        structFieldName = NamePair {
                          nameC = Name "z",
                          nameHsIdent = HsIdentifier
                            "dim3_z"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "unions.h:6:8",
                      declId = NamePair {
                        nameC = Name "Dim3",
                        nameHsIdent = HsIdentifier
                          "Dim3"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "unions.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Dim3"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc = "unions.h:7:9",
                            structFieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "dim3_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "unions.h:8:9",
                            structFieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "dim3_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc = "unions.h:9:9",
                            structFieldName = NamePair {
                              nameC = Name "z",
                              nameHsIdent = HsIdentifier
                                "dim3_z"},
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
    (HsName "@NsTypeConstr" "Dim3"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Dim3"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "DimPayload",
      newtypeConstr = HsName
        "@NsConstr"
        "DimPayload",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_DimPayload",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "unions.h:12:7",
          declId = NamePair {
            nameC = Name "DimPayload",
            nameHsIdent = HsIdentifier
              "DimPayload"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "unions.h"},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "DimPayload",
              newtypeField = HsName
                "@NsVar"
                "un_DimPayload"},
            unionSizeof = 8,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldLoc =
                "unions.h:13:17",
                unionFieldName = NamePair {
                  nameC = Name "dim2",
                  nameHsIdent = HsIdentifier
                    "dimPayload_dim2"},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "Dim2",
                    nameHsIdent = HsIdentifier
                      "Dim2"}
                  NameOriginInSource},
              UnionField {
                unionFieldLoc =
                "unions.h:14:17",
                unionFieldName = NamePair {
                  nameC = Name "dim3",
                  nameHsIdent = HsIdentifier
                    "dimPayload_dim3"},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "Dim2",
                    nameHsIdent = HsIdentifier
                      "Dim2"}
                  NameOriginInSource}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Storable]},
  DeclNewtypeInstance
    (DeriveVia
      (HsSizedByteArray 8 4))
    Storable
    (HsName
      "@NsTypeConstr"
      "DimPayload"),
  DeclUnionGetter
    (HsName
      "@NsTypeConstr"
      "DimPayload")
    (HsTypRef
      (HsName "@NsTypeConstr" "Dim2"))
    (HsName
      "@NsVar"
      "get_dimPayload_dim2"),
  DeclUnionSetter
    (HsName
      "@NsTypeConstr"
      "DimPayload")
    (HsTypRef
      (HsName "@NsTypeConstr" "Dim2"))
    (HsName
      "@NsVar"
      "set_dimPayload_dim2"),
  DeclUnionGetter
    (HsName
      "@NsTypeConstr"
      "DimPayload")
    (HsTypRef
      (HsName "@NsTypeConstr" "Dim2"))
    (HsName
      "@NsVar"
      "get_dimPayload_dim3"),
  DeclUnionSetter
    (HsName
      "@NsTypeConstr"
      "DimPayload")
    (HsTypRef
      (HsName "@NsTypeConstr" "Dim2"))
    (HsName
      "@NsVar"
      "set_dimPayload_dim3"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Dim",
      structConstr = HsName
        "@NsConstr"
        "Dim",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "dim_tag",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "unions.h:18:9",
              structFieldName = NamePair {
                nameC = Name "tag",
                nameHsIdent = HsIdentifier
                  "dim_tag"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "dim_payload",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "DimPayload"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "unions.h:19:22",
              structFieldName = NamePair {
                nameC = Name "payload",
                nameHsIdent = HsIdentifier
                  "dim_payload"},
              structFieldType = TypeUnion
                NamePair {
                  nameC = Name "DimPayload",
                  nameHsIdent = HsIdentifier
                    "DimPayload"}
                NameOriginInSource,
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:17:8",
            declId = NamePair {
              nameC = Name "Dim",
              nameHsIdent = HsIdentifier
                "Dim"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "unions.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Dim"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "unions.h:18:9",
                  structFieldName = NamePair {
                    nameC = Name "tag",
                    nameHsIdent = HsIdentifier
                      "dim_tag"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "unions.h:19:22",
                  structFieldName = NamePair {
                    nameC = Name "payload",
                    nameHsIdent = HsIdentifier
                      "dim_payload"},
                  structFieldType = TypeUnion
                    NamePair {
                      nameC = Name "DimPayload",
                      nameHsIdent = HsIdentifier
                        "DimPayload"}
                    NameOriginInSource,
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
        [Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Dim",
        structConstr = HsName
          "@NsConstr"
          "Dim",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "dim_tag",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "unions.h:18:9",
                structFieldName = NamePair {
                  nameC = Name "tag",
                  nameHsIdent = HsIdentifier
                    "dim_tag"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "dim_payload",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "DimPayload"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "unions.h:19:22",
                structFieldName = NamePair {
                  nameC = Name "payload",
                  nameHsIdent = HsIdentifier
                    "dim_payload"},
                structFieldType = TypeUnion
                  NamePair {
                    nameC = Name "DimPayload",
                    nameHsIdent = HsIdentifier
                      "DimPayload"}
                  NameOriginInSource,
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "unions.h:17:8",
              declId = NamePair {
                nameC = Name "Dim",
                nameHsIdent = HsIdentifier
                  "Dim"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "unions.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Dim"),
                structSizeof = 12,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "unions.h:18:9",
                    structFieldName = NamePair {
                      nameC = Name "tag",
                      nameHsIdent = HsIdentifier
                        "dim_tag"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "unions.h:19:22",
                    structFieldName = NamePair {
                      nameC = Name "payload",
                      nameHsIdent = HsIdentifier
                        "dim_payload"},
                    structFieldType = TypeUnion
                      NamePair {
                        nameC = Name "DimPayload",
                        nameHsIdent = HsIdentifier
                          "DimPayload"}
                      NameOriginInSource,
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
          [Storable]}
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
                  "Dim",
                structConstr = HsName
                  "@NsConstr"
                  "Dim",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim_tag",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:18:9",
                        structFieldName = NamePair {
                          nameC = Name "tag",
                          nameHsIdent = HsIdentifier
                            "dim_tag"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim_payload",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "DimPayload"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:19:22",
                        structFieldName = NamePair {
                          nameC = Name "payload",
                          nameHsIdent = HsIdentifier
                            "dim_payload"},
                        structFieldType = TypeUnion
                          NamePair {
                            nameC = Name "DimPayload",
                            nameHsIdent = HsIdentifier
                              "DimPayload"}
                          NameOriginInSource,
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "unions.h:17:8",
                      declId = NamePair {
                        nameC = Name "Dim",
                        nameHsIdent = HsIdentifier
                          "Dim"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "unions.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Dim"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "unions.h:18:9",
                            structFieldName = NamePair {
                              nameC = Name "tag",
                              nameHsIdent = HsIdentifier
                                "dim_tag"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "unions.h:19:22",
                            structFieldName = NamePair {
                              nameC = Name "payload",
                              nameHsIdent = HsIdentifier
                                "dim_payload"},
                            structFieldType = TypeUnion
                              NamePair {
                                nameC = Name "DimPayload",
                                nameHsIdent = HsIdentifier
                                  "DimPayload"}
                              NameOriginInSource,
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
                  [Storable]})
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
                  "Dim",
                structConstr = HsName
                  "@NsConstr"
                  "Dim",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim_tag",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:18:9",
                        structFieldName = NamePair {
                          nameC = Name "tag",
                          nameHsIdent = HsIdentifier
                            "dim_tag"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim_payload",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "DimPayload"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:19:22",
                        structFieldName = NamePair {
                          nameC = Name "payload",
                          nameHsIdent = HsIdentifier
                            "dim_payload"},
                        structFieldType = TypeUnion
                          NamePair {
                            nameC = Name "DimPayload",
                            nameHsIdent = HsIdentifier
                              "DimPayload"}
                          NameOriginInSource,
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "unions.h:17:8",
                      declId = NamePair {
                        nameC = Name "Dim",
                        nameHsIdent = HsIdentifier
                          "Dim"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "unions.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Dim"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "unions.h:18:9",
                            structFieldName = NamePair {
                              nameC = Name "tag",
                              nameHsIdent = HsIdentifier
                                "dim_tag"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "unions.h:19:22",
                            structFieldName = NamePair {
                              nameC = Name "payload",
                              nameHsIdent = HsIdentifier
                                "dim_payload"},
                            structFieldType = TypeUnion
                              NamePair {
                                nameC = Name "DimPayload",
                                nameHsIdent = HsIdentifier
                                  "DimPayload"}
                              NameOriginInSource,
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
                  [Storable]}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "DimPayloadB",
      newtypeConstr = HsName
        "@NsConstr"
        "DimPayloadB",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_DimPayloadB",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "unions.h:23:15",
          declId = NamePair {
            nameC = Name "DimPayloadB",
            nameHsIdent = HsIdentifier
              "DimPayloadB"},
          declOrigin = NameOriginInSource,
          declAliases = [
            Name "DimPayloadB"],
          declHeader = "unions.h"},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "DimPayloadB",
              newtypeField = HsName
                "@NsVar"
                "un_DimPayloadB"},
            unionSizeof = 8,
            unionAlignment = 4,
            unionFields = [
              UnionField {
                unionFieldLoc =
                "unions.h:24:17",
                unionFieldName = NamePair {
                  nameC = Name "dim2",
                  nameHsIdent = HsIdentifier
                    "dimPayloadB_dim2"},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "Dim2",
                    nameHsIdent = HsIdentifier
                      "Dim2"}
                  NameOriginInSource},
              UnionField {
                unionFieldLoc =
                "unions.h:25:17",
                unionFieldName = NamePair {
                  nameC = Name "dim3",
                  nameHsIdent = HsIdentifier
                    "dimPayloadB_dim3"},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "Dim2",
                    nameHsIdent = HsIdentifier
                      "Dim2"}
                  NameOriginInSource}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Storable]},
  DeclNewtypeInstance
    (DeriveVia
      (HsSizedByteArray 8 4))
    Storable
    (HsName
      "@NsTypeConstr"
      "DimPayloadB"),
  DeclUnionGetter
    (HsName
      "@NsTypeConstr"
      "DimPayloadB")
    (HsTypRef
      (HsName "@NsTypeConstr" "Dim2"))
    (HsName
      "@NsVar"
      "get_dimPayloadB_dim2"),
  DeclUnionSetter
    (HsName
      "@NsTypeConstr"
      "DimPayloadB")
    (HsTypRef
      (HsName "@NsTypeConstr" "Dim2"))
    (HsName
      "@NsVar"
      "set_dimPayloadB_dim2"),
  DeclUnionGetter
    (HsName
      "@NsTypeConstr"
      "DimPayloadB")
    (HsTypRef
      (HsName "@NsTypeConstr" "Dim2"))
    (HsName
      "@NsVar"
      "get_dimPayloadB_dim3"),
  DeclUnionSetter
    (HsName
      "@NsTypeConstr"
      "DimPayloadB")
    (HsTypRef
      (HsName "@NsTypeConstr" "Dim2"))
    (HsName
      "@NsVar"
      "set_dimPayloadB_dim3"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "DimB",
      structConstr = HsName
        "@NsConstr"
        "DimB",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "dimB_tag",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "unions.h:29:9",
              structFieldName = NamePair {
                nameC = Name "tag",
                nameHsIdent = HsIdentifier
                  "dimB_tag"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "dimB_payload",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "DimPayloadB"),
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "unions.h:30:17",
              structFieldName = NamePair {
                nameC = Name "payload",
                nameHsIdent = HsIdentifier
                  "dimB_payload"},
              structFieldType = TypeTypedef
                (TypedefSquashed
                  (Name "DimPayloadB")
                  (TypeUnion
                    NamePair {
                      nameC = Name "DimPayloadB",
                      nameHsIdent = HsIdentifier
                        "DimPayloadB"}
                    NameOriginInSource)),
              structFieldOffset = 32,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:28:8",
            declId = NamePair {
              nameC = Name "DimB",
              nameHsIdent = HsIdentifier
                "DimB"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "unions.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "DimB"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "unions.h:29:9",
                  structFieldName = NamePair {
                    nameC = Name "tag",
                    nameHsIdent = HsIdentifier
                      "dimB_tag"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "unions.h:30:17",
                  structFieldName = NamePair {
                    nameC = Name "payload",
                    nameHsIdent = HsIdentifier
                      "dimB_payload"},
                  structFieldType = TypeTypedef
                    (TypedefSquashed
                      (Name "DimPayloadB")
                      (TypeUnion
                        NamePair {
                          nameC = Name "DimPayloadB",
                          nameHsIdent = HsIdentifier
                            "DimPayloadB"}
                        NameOriginInSource)),
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
        [Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "DimB",
        structConstr = HsName
          "@NsConstr"
          "DimB",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "dimB_tag",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "unions.h:29:9",
                structFieldName = NamePair {
                  nameC = Name "tag",
                  nameHsIdent = HsIdentifier
                    "dimB_tag"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "dimB_payload",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "DimPayloadB"),
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "unions.h:30:17",
                structFieldName = NamePair {
                  nameC = Name "payload",
                  nameHsIdent = HsIdentifier
                    "dimB_payload"},
                structFieldType = TypeTypedef
                  (TypedefSquashed
                    (Name "DimPayloadB")
                    (TypeUnion
                      NamePair {
                        nameC = Name "DimPayloadB",
                        nameHsIdent = HsIdentifier
                          "DimPayloadB"}
                      NameOriginInSource)),
                structFieldOffset = 32,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "unions.h:28:8",
              declId = NamePair {
                nameC = Name "DimB",
                nameHsIdent = HsIdentifier
                  "DimB"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "unions.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "DimB"),
                structSizeof = 12,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "unions.h:29:9",
                    structFieldName = NamePair {
                      nameC = Name "tag",
                      nameHsIdent = HsIdentifier
                        "dimB_tag"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "unions.h:30:17",
                    structFieldName = NamePair {
                      nameC = Name "payload",
                      nameHsIdent = HsIdentifier
                        "dimB_payload"},
                    structFieldType = TypeTypedef
                      (TypedefSquashed
                        (Name "DimPayloadB")
                        (TypeUnion
                          NamePair {
                            nameC = Name "DimPayloadB",
                            nameHsIdent = HsIdentifier
                              "DimPayloadB"}
                          NameOriginInSource)),
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
          [Storable]}
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
                  "DimB",
                structConstr = HsName
                  "@NsConstr"
                  "DimB",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dimB_tag",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:29:9",
                        structFieldName = NamePair {
                          nameC = Name "tag",
                          nameHsIdent = HsIdentifier
                            "dimB_tag"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dimB_payload",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "DimPayloadB"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:30:17",
                        structFieldName = NamePair {
                          nameC = Name "payload",
                          nameHsIdent = HsIdentifier
                            "dimB_payload"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "DimPayloadB")
                            (TypeUnion
                              NamePair {
                                nameC = Name "DimPayloadB",
                                nameHsIdent = HsIdentifier
                                  "DimPayloadB"}
                              NameOriginInSource)),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "unions.h:28:8",
                      declId = NamePair {
                        nameC = Name "DimB",
                        nameHsIdent = HsIdentifier
                          "DimB"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "unions.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "DimB"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "unions.h:29:9",
                            structFieldName = NamePair {
                              nameC = Name "tag",
                              nameHsIdent = HsIdentifier
                                "dimB_tag"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "unions.h:30:17",
                            structFieldName = NamePair {
                              nameC = Name "payload",
                              nameHsIdent = HsIdentifier
                                "dimB_payload"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "DimPayloadB")
                                (TypeUnion
                                  NamePair {
                                    nameC = Name "DimPayloadB",
                                    nameHsIdent = HsIdentifier
                                      "DimPayloadB"}
                                  NameOriginInSource)),
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
                  [Storable]})
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
                  "DimB",
                structConstr = HsName
                  "@NsConstr"
                  "DimB",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dimB_tag",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:29:9",
                        structFieldName = NamePair {
                          nameC = Name "tag",
                          nameHsIdent = HsIdentifier
                            "dimB_tag"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dimB_payload",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "DimPayloadB"),
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:30:17",
                        structFieldName = NamePair {
                          nameC = Name "payload",
                          nameHsIdent = HsIdentifier
                            "dimB_payload"},
                        structFieldType = TypeTypedef
                          (TypedefSquashed
                            (Name "DimPayloadB")
                            (TypeUnion
                              NamePair {
                                nameC = Name "DimPayloadB",
                                nameHsIdent = HsIdentifier
                                  "DimPayloadB"}
                              NameOriginInSource)),
                        structFieldOffset = 32,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "unions.h:28:8",
                      declId = NamePair {
                        nameC = Name "DimB",
                        nameHsIdent = HsIdentifier
                          "DimB"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "unions.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "DimB"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "unions.h:29:9",
                            structFieldName = NamePair {
                              nameC = Name "tag",
                              nameHsIdent = HsIdentifier
                                "dimB_tag"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "unions.h:30:17",
                            structFieldName = NamePair {
                              nameC = Name "payload",
                              nameHsIdent = HsIdentifier
                                "dimB_payload"},
                            structFieldType = TypeTypedef
                              (TypedefSquashed
                                (Name "DimPayloadB")
                                (TypeUnion
                                  NamePair {
                                    nameC = Name "DimPayloadB",
                                    nameHsIdent = HsIdentifier
                                      "DimPayloadB"}
                                  NameOriginInSource)),
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
                  [Storable]}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "AnonA_xy",
      structConstr = HsName
        "@NsConstr"
        "AnonA_xy",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "anonA_xy_x",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "unions.h:35:21",
              structFieldName = NamePair {
                nameC = Name "x",
                nameHsIdent = HsIdentifier
                  "anonA_xy_x"},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "anonA_xy_y",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "unions.h:35:31",
              structFieldName = NamePair {
                nameC = Name "y",
                nameHsIdent = HsIdentifier
                  "anonA_xy_y"},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:35:5",
            declId = NamePair {
              nameC = Name "AnonA_xy",
              nameHsIdent = HsIdentifier
                "AnonA_xy"},
            declOrigin = NameOriginGenerated
              (AnonId "unions.h:35:5"),
            declAliases = [],
            declHeader = "unions.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "AnonA_xy"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "unions.h:35:21",
                  structFieldName = NamePair {
                    nameC = Name "x",
                    nameHsIdent = HsIdentifier
                      "anonA_xy_x"},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "unions.h:35:31",
                  structFieldName = NamePair {
                    nameC = Name "y",
                    nameHsIdent = HsIdentifier
                      "anonA_xy_y"},
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "AnonA_xy",
        structConstr = HsName
          "@NsConstr"
          "AnonA_xy",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "anonA_xy_x",
            fieldType = HsPrimType
              HsPrimCDouble,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "unions.h:35:21",
                structFieldName = NamePair {
                  nameC = Name "x",
                  nameHsIdent = HsIdentifier
                    "anonA_xy_x"},
                structFieldType = TypePrim
                  (PrimFloating PrimDouble),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "anonA_xy_y",
            fieldType = HsPrimType
              HsPrimCDouble,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "unions.h:35:31",
                structFieldName = NamePair {
                  nameC = Name "y",
                  nameHsIdent = HsIdentifier
                    "anonA_xy_y"},
                structFieldType = TypePrim
                  (PrimFloating PrimDouble),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "unions.h:35:5",
              declId = NamePair {
                nameC = Name "AnonA_xy",
                nameHsIdent = HsIdentifier
                  "AnonA_xy"},
              declOrigin = NameOriginGenerated
                (AnonId "unions.h:35:5"),
              declAliases = [],
              declHeader = "unions.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "AnonA_xy"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "unions.h:35:21",
                    structFieldName = NamePair {
                      nameC = Name "x",
                      nameHsIdent = HsIdentifier
                        "anonA_xy_x"},
                    structFieldType = TypePrim
                      (PrimFloating PrimDouble),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "unions.h:35:31",
                    structFieldName = NamePair {
                      nameC = Name "y",
                      nameHsIdent = HsIdentifier
                        "anonA_xy_y"},
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
                  "AnonA_xy",
                structConstr = HsName
                  "@NsConstr"
                  "AnonA_xy",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "anonA_xy_x",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:35:21",
                        structFieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "anonA_xy_x"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "anonA_xy_y",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:35:31",
                        structFieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "anonA_xy_y"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "unions.h:35:5",
                      declId = NamePair {
                        nameC = Name "AnonA_xy",
                        nameHsIdent = HsIdentifier
                          "AnonA_xy"},
                      declOrigin = NameOriginGenerated
                        (AnonId "unions.h:35:5"),
                      declAliases = [],
                      declHeader = "unions.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "AnonA_xy"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "unions.h:35:21",
                            structFieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "anonA_xy_x"},
                            structFieldType = TypePrim
                              (PrimFloating PrimDouble),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "unions.h:35:31",
                            structFieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "anonA_xy_y"},
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
                  "AnonA_xy",
                structConstr = HsName
                  "@NsConstr"
                  "AnonA_xy",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "anonA_xy_x",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:35:21",
                        structFieldName = NamePair {
                          nameC = Name "x",
                          nameHsIdent = HsIdentifier
                            "anonA_xy_x"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "anonA_xy_y",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:35:31",
                        structFieldName = NamePair {
                          nameC = Name "y",
                          nameHsIdent = HsIdentifier
                            "anonA_xy_y"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "unions.h:35:5",
                      declId = NamePair {
                        nameC = Name "AnonA_xy",
                        nameHsIdent = HsIdentifier
                          "AnonA_xy"},
                      declOrigin = NameOriginGenerated
                        (AnonId "unions.h:35:5"),
                      declAliases = [],
                      declHeader = "unions.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "AnonA_xy"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "unions.h:35:21",
                            structFieldName = NamePair {
                              nameC = Name "x",
                              nameHsIdent = HsIdentifier
                                "anonA_xy_x"},
                            structFieldType = TypePrim
                              (PrimFloating PrimDouble),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "unions.h:35:31",
                            structFieldName = NamePair {
                              nameC = Name "y",
                              nameHsIdent = HsIdentifier
                                "anonA_xy_y"},
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
    (HsName
      "@NsTypeConstr"
      "AnonA_xy"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "AnonA_xy"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "AnonA_polar",
      structConstr = HsName
        "@NsConstr"
        "AnonA_polar",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "anonA_polar_r",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "unions.h:36:21",
              structFieldName = NamePair {
                nameC = Name "r",
                nameHsIdent = HsIdentifier
                  "anonA_polar_r"},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "anonA_polar_p",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "unions.h:36:31",
              structFieldName = NamePair {
                nameC = Name "p",
                nameHsIdent = HsIdentifier
                  "anonA_polar_p"},
              structFieldType = TypePrim
                (PrimFloating PrimDouble),
              structFieldOffset = 64,
              structFieldWidth = Nothing}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "unions.h:36:5",
            declId = NamePair {
              nameC = Name "AnonA_polar",
              nameHsIdent = HsIdentifier
                "AnonA_polar"},
            declOrigin = NameOriginGenerated
              (AnonId "unions.h:36:5"),
            declAliases = [],
            declHeader = "unions.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "AnonA_polar"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "unions.h:36:21",
                  structFieldName = NamePair {
                    nameC = Name "r",
                    nameHsIdent = HsIdentifier
                      "anonA_polar_r"},
                  structFieldType = TypePrim
                    (PrimFloating PrimDouble),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "unions.h:36:31",
                  structFieldName = NamePair {
                    nameC = Name "p",
                    nameHsIdent = HsIdentifier
                      "anonA_polar_p"},
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
        [Eq, Show, Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "AnonA_polar",
        structConstr = HsName
          "@NsConstr"
          "AnonA_polar",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "anonA_polar_r",
            fieldType = HsPrimType
              HsPrimCDouble,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "unions.h:36:21",
                structFieldName = NamePair {
                  nameC = Name "r",
                  nameHsIdent = HsIdentifier
                    "anonA_polar_r"},
                structFieldType = TypePrim
                  (PrimFloating PrimDouble),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "anonA_polar_p",
            fieldType = HsPrimType
              HsPrimCDouble,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "unions.h:36:31",
                structFieldName = NamePair {
                  nameC = Name "p",
                  nameHsIdent = HsIdentifier
                    "anonA_polar_p"},
                structFieldType = TypePrim
                  (PrimFloating PrimDouble),
                structFieldOffset = 64,
                structFieldWidth = Nothing}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "unions.h:36:5",
              declId = NamePair {
                nameC = Name "AnonA_polar",
                nameHsIdent = HsIdentifier
                  "AnonA_polar"},
              declOrigin = NameOriginGenerated
                (AnonId "unions.h:36:5"),
              declAliases = [],
              declHeader = "unions.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "AnonA_polar"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "unions.h:36:21",
                    structFieldName = NamePair {
                      nameC = Name "r",
                      nameHsIdent = HsIdentifier
                        "anonA_polar_r"},
                    structFieldType = TypePrim
                      (PrimFloating PrimDouble),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "unions.h:36:31",
                    structFieldName = NamePair {
                      nameC = Name "p",
                      nameHsIdent = HsIdentifier
                        "anonA_polar_p"},
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
                  "AnonA_polar",
                structConstr = HsName
                  "@NsConstr"
                  "AnonA_polar",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "anonA_polar_r",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:36:21",
                        structFieldName = NamePair {
                          nameC = Name "r",
                          nameHsIdent = HsIdentifier
                            "anonA_polar_r"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "anonA_polar_p",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:36:31",
                        structFieldName = NamePair {
                          nameC = Name "p",
                          nameHsIdent = HsIdentifier
                            "anonA_polar_p"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "unions.h:36:5",
                      declId = NamePair {
                        nameC = Name "AnonA_polar",
                        nameHsIdent = HsIdentifier
                          "AnonA_polar"},
                      declOrigin = NameOriginGenerated
                        (AnonId "unions.h:36:5"),
                      declAliases = [],
                      declHeader = "unions.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "AnonA_polar"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "unions.h:36:21",
                            structFieldName = NamePair {
                              nameC = Name "r",
                              nameHsIdent = HsIdentifier
                                "anonA_polar_r"},
                            structFieldType = TypePrim
                              (PrimFloating PrimDouble),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "unions.h:36:31",
                            structFieldName = NamePair {
                              nameC = Name "p",
                              nameHsIdent = HsIdentifier
                                "anonA_polar_p"},
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
                  "AnonA_polar",
                structConstr = HsName
                  "@NsConstr"
                  "AnonA_polar",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "anonA_polar_r",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:36:21",
                        structFieldName = NamePair {
                          nameC = Name "r",
                          nameHsIdent = HsIdentifier
                            "anonA_polar_r"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "anonA_polar_p",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "unions.h:36:31",
                        structFieldName = NamePair {
                          nameC = Name "p",
                          nameHsIdent = HsIdentifier
                            "anonA_polar_p"},
                        structFieldType = TypePrim
                          (PrimFloating PrimDouble),
                        structFieldOffset = 64,
                        structFieldWidth = Nothing}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "unions.h:36:5",
                      declId = NamePair {
                        nameC = Name "AnonA_polar",
                        nameHsIdent = HsIdentifier
                          "AnonA_polar"},
                      declOrigin = NameOriginGenerated
                        (AnonId "unions.h:36:5"),
                      declAliases = [],
                      declHeader = "unions.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "AnonA_polar"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "unions.h:36:21",
                            structFieldName = NamePair {
                              nameC = Name "r",
                              nameHsIdent = HsIdentifier
                                "anonA_polar_r"},
                            structFieldType = TypePrim
                              (PrimFloating PrimDouble),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "unions.h:36:31",
                            structFieldName = NamePair {
                              nameC = Name "p",
                              nameHsIdent = HsIdentifier
                                "anonA_polar_p"},
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
    (HsName
      "@NsTypeConstr"
      "AnonA_polar"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "AnonA_polar"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "AnonA",
      newtypeConstr = HsName
        "@NsConstr"
        "AnonA",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_AnonA",
        fieldType = HsByteArray,
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "unions.h:34:7",
          declId = NamePair {
            nameC = Name "AnonA",
            nameHsIdent = HsIdentifier
              "AnonA"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "unions.h"},
        declKind = Union
          Union {
            unionNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "AnonA",
              newtypeField = HsName
                "@NsVar"
                "un_AnonA"},
            unionSizeof = 16,
            unionAlignment = 8,
            unionFields = [
              UnionField {
                unionFieldLoc =
                "unions.h:35:36",
                unionFieldName = NamePair {
                  nameC = Name "xy",
                  nameHsIdent = HsIdentifier
                    "anonA_xy"},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "AnonA_xy",
                    nameHsIdent = HsIdentifier
                      "AnonA_xy"}
                  (NameOriginGenerated
                    (AnonId "unions.h:35:5"))},
              UnionField {
                unionFieldLoc =
                "unions.h:36:36",
                unionFieldName = NamePair {
                  nameC = Name "polar",
                  nameHsIdent = HsIdentifier
                    "anonA_polar"},
                unionFieldType = TypeStruct
                  NamePair {
                    nameC = Name "AnonA_polar",
                    nameHsIdent = HsIdentifier
                      "AnonA_polar"}
                  (NameOriginGenerated
                    (AnonId "unions.h:36:5"))}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Storable]},
  DeclNewtypeInstance
    (DeriveVia
      (HsSizedByteArray 16 8))
    Storable
    (HsName
      "@NsTypeConstr"
      "AnonA"),
  DeclUnionGetter
    (HsName "@NsTypeConstr" "AnonA")
    (HsTypRef
      (HsName
        "@NsTypeConstr"
        "AnonA_xy"))
    (HsName
      "@NsVar"
      "get_anonA_xy"),
  DeclUnionSetter
    (HsName "@NsTypeConstr" "AnonA")
    (HsTypRef
      (HsName
        "@NsTypeConstr"
        "AnonA_xy"))
    (HsName
      "@NsVar"
      "set_anonA_xy"),
  DeclUnionGetter
    (HsName "@NsTypeConstr" "AnonA")
    (HsTypRef
      (HsName
        "@NsTypeConstr"
        "AnonA_polar"))
    (HsName
      "@NsVar"
      "get_anonA_polar"),
  DeclUnionSetter
    (HsName "@NsTypeConstr" "AnonA")
    (HsTypRef
      (HsName
        "@NsTypeConstr"
        "AnonA_polar"))
    (HsName
      "@NsVar"
      "set_anonA_polar")]
