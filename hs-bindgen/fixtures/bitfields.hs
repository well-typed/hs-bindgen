[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Flags",
      structConstr = HsName
        "@NsConstr"
        "Flags",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "flags_fieldX",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:2:10",
              structFieldName = NamePair {
                nameC = CName "fieldX",
                nameHsIdent = HsIdentifier
                  "flags_fieldX"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 0,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "flags_flagA",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:3:9",
              structFieldName = NamePair {
                nameC = CName "flagA",
                nameHsIdent = HsIdentifier
                  "flags_flagA"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 8,
              structFieldWidth = Just 1}},
        Field {
          fieldName = HsName
            "@NsVar"
            "flags_flagB",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:4:9",
              structFieldName = NamePair {
                nameC = CName "flagB",
                nameHsIdent = HsIdentifier
                  "flags_flagB"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 9,
              structFieldWidth = Just 1}},
        Field {
          fieldName = HsName
            "@NsVar"
            "flags_flagC",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:5:9",
              structFieldName = NamePair {
                nameC = CName "flagC",
                nameHsIdent = HsIdentifier
                  "flags_flagC"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 10,
              structFieldWidth = Just 1}},
        Field {
          fieldName = HsName
            "@NsVar"
            "flags_fieldY",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:6:10",
              structFieldName = NamePair {
                nameC = CName "fieldY",
                nameHsIdent = HsIdentifier
                  "flags_fieldY"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              structFieldOffset = 16,
              structFieldWidth = Nothing}},
        Field {
          fieldName = HsName
            "@NsVar"
            "flags_bits",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:7:9",
              structFieldName = NamePair {
                nameC = CName "bits",
                nameHsIdent = HsIdentifier
                  "flags_bits"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 24,
              structFieldWidth = Just 2}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bitfields.h:1:8",
            declId = NamePair {
              nameC = CName "flags",
              nameHsIdent = HsIdentifier
                "Flags"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "bitfields.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "Flags"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "bitfields.h:2:10",
                  structFieldName = NamePair {
                    nameC = CName "fieldX",
                    nameHsIdent = HsIdentifier
                      "flags_fieldX"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 0,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "bitfields.h:3:9",
                  structFieldName = NamePair {
                    nameC = CName "flagA",
                    nameHsIdent = HsIdentifier
                      "flags_flagA"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 8,
                  structFieldWidth = Just 1},
                StructField {
                  structFieldLoc =
                  "bitfields.h:4:9",
                  structFieldName = NamePair {
                    nameC = CName "flagB",
                    nameHsIdent = HsIdentifier
                      "flags_flagB"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 9,
                  structFieldWidth = Just 1},
                StructField {
                  structFieldLoc =
                  "bitfields.h:5:9",
                  structFieldName = NamePair {
                    nameC = CName "flagC",
                    nameHsIdent = HsIdentifier
                      "flags_flagC"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 10,
                  structFieldWidth = Just 1},
                StructField {
                  structFieldLoc =
                  "bitfields.h:6:10",
                  structFieldName = NamePair {
                    nameC = CName "fieldY",
                    nameHsIdent = HsIdentifier
                      "flags_fieldY"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignImplicit
                        (Just Signed))),
                  structFieldOffset = 16,
                  structFieldWidth = Nothing},
                StructField {
                  structFieldLoc =
                  "bitfields.h:7:9",
                  structFieldName = NamePair {
                    nameC = CName "bits",
                    nameHsIdent = HsIdentifier
                      "flags_bits"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 24,
                  structFieldWidth = Just 2}],
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
          "Flags",
        structConstr = HsName
          "@NsConstr"
          "Flags",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "flags_fieldX",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:2:10",
                structFieldName = NamePair {
                  nameC = CName "fieldX",
                  nameHsIdent = HsIdentifier
                    "flags_fieldX"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 0,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "flags_flagA",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:3:9",
                structFieldName = NamePair {
                  nameC = CName "flagA",
                  nameHsIdent = HsIdentifier
                    "flags_flagA"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 8,
                structFieldWidth = Just 1}},
          Field {
            fieldName = HsName
              "@NsVar"
              "flags_flagB",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:4:9",
                structFieldName = NamePair {
                  nameC = CName "flagB",
                  nameHsIdent = HsIdentifier
                    "flags_flagB"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 9,
                structFieldWidth = Just 1}},
          Field {
            fieldName = HsName
              "@NsVar"
              "flags_flagC",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:5:9",
                structFieldName = NamePair {
                  nameC = CName "flagC",
                  nameHsIdent = HsIdentifier
                    "flags_flagC"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 10,
                structFieldWidth = Just 1}},
          Field {
            fieldName = HsName
              "@NsVar"
              "flags_fieldY",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:6:10",
                structFieldName = NamePair {
                  nameC = CName "fieldY",
                  nameHsIdent = HsIdentifier
                    "flags_fieldY"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                structFieldOffset = 16,
                structFieldWidth = Nothing}},
          Field {
            fieldName = HsName
              "@NsVar"
              "flags_bits",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:7:9",
                structFieldName = NamePair {
                  nameC = CName "bits",
                  nameHsIdent = HsIdentifier
                    "flags_bits"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 24,
                structFieldWidth = Just 2}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "bitfields.h:1:8",
              declId = NamePair {
                nameC = CName "flags",
                nameHsIdent = HsIdentifier
                  "Flags"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "bitfields.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "Flags"),
                structSizeof = 4,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "bitfields.h:2:10",
                    structFieldName = NamePair {
                      nameC = CName "fieldX",
                      nameHsIdent = HsIdentifier
                        "flags_fieldX"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 0,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "bitfields.h:3:9",
                    structFieldName = NamePair {
                      nameC = CName "flagA",
                      nameHsIdent = HsIdentifier
                        "flags_flagA"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 8,
                    structFieldWidth = Just 1},
                  StructField {
                    structFieldLoc =
                    "bitfields.h:4:9",
                    structFieldName = NamePair {
                      nameC = CName "flagB",
                      nameHsIdent = HsIdentifier
                        "flags_flagB"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 9,
                    structFieldWidth = Just 1},
                  StructField {
                    structFieldLoc =
                    "bitfields.h:5:9",
                    structFieldName = NamePair {
                      nameC = CName "flagC",
                      nameHsIdent = HsIdentifier
                        "flags_flagC"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 10,
                    structFieldWidth = Just 1},
                  StructField {
                    structFieldLoc =
                    "bitfields.h:6:10",
                    structFieldName = NamePair {
                      nameC = CName "fieldY",
                      nameHsIdent = HsIdentifier
                        "flags_fieldY"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignImplicit
                          (Just Signed))),
                    structFieldOffset = 16,
                    structFieldWidth = Nothing},
                  StructField {
                    structFieldLoc =
                    "bitfields.h:7:9",
                    structFieldName = NamePair {
                      nameC = CName "bits",
                      nameHsIdent = HsIdentifier
                        "flags_bits"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 24,
                    structFieldWidth = Just 2}],
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
                  "Flags",
                structConstr = HsName
                  "@NsConstr"
                  "Flags",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_fieldX",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:2:10",
                        structFieldName = NamePair {
                          nameC = CName "fieldX",
                          nameHsIdent = HsIdentifier
                            "flags_fieldX"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_flagA",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:3:9",
                        structFieldName = NamePair {
                          nameC = CName "flagA",
                          nameHsIdent = HsIdentifier
                            "flags_flagA"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 8,
                        structFieldWidth = Just 1}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_flagB",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:4:9",
                        structFieldName = NamePair {
                          nameC = CName "flagB",
                          nameHsIdent = HsIdentifier
                            "flags_flagB"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 9,
                        structFieldWidth = Just 1}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_flagC",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:5:9",
                        structFieldName = NamePair {
                          nameC = CName "flagC",
                          nameHsIdent = HsIdentifier
                            "flags_flagC"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 10,
                        structFieldWidth = Just 1}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_fieldY",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:6:10",
                        structFieldName = NamePair {
                          nameC = CName "fieldY",
                          nameHsIdent = HsIdentifier
                            "flags_fieldY"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 16,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_bits",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:7:9",
                        structFieldName = NamePair {
                          nameC = CName "bits",
                          nameHsIdent = HsIdentifier
                            "flags_bits"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 24,
                        structFieldWidth = Just 2}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bitfields.h:1:8",
                      declId = NamePair {
                        nameC = CName "flags",
                        nameHsIdent = HsIdentifier
                          "Flags"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "bitfields.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Flags"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "bitfields.h:2:10",
                            structFieldName = NamePair {
                              nameC = CName "fieldX",
                              nameHsIdent = HsIdentifier
                                "flags_fieldX"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:3:9",
                            structFieldName = NamePair {
                              nameC = CName "flagA",
                              nameHsIdent = HsIdentifier
                                "flags_flagA"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 8,
                            structFieldWidth = Just 1},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:4:9",
                            structFieldName = NamePair {
                              nameC = CName "flagB",
                              nameHsIdent = HsIdentifier
                                "flags_flagB"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 9,
                            structFieldWidth = Just 1},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:5:9",
                            structFieldName = NamePair {
                              nameC = CName "flagC",
                              nameHsIdent = HsIdentifier
                                "flags_flagC"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 10,
                            structFieldWidth = Just 1},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:6:10",
                            structFieldName = NamePair {
                              nameC = CName "fieldY",
                              nameHsIdent = HsIdentifier
                                "flags_fieldY"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 16,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:7:9",
                            structFieldName = NamePair {
                              nameC = CName "bits",
                              nameHsIdent = HsIdentifier
                                "flags_bits"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 24,
                            structFieldWidth = Just 2}],
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
              PeekBitOffWidth (Idx 0) 8 1,
              PeekBitOffWidth (Idx 0) 9 1,
              PeekBitOffWidth (Idx 0) 10 1,
              PeekByteOff (Idx 0) 2,
              PeekBitOffWidth (Idx 0) 24 2]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Flags",
                structConstr = HsName
                  "@NsConstr"
                  "Flags",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_fieldX",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:2:10",
                        structFieldName = NamePair {
                          nameC = CName "fieldX",
                          nameHsIdent = HsIdentifier
                            "flags_fieldX"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 0,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_flagA",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:3:9",
                        structFieldName = NamePair {
                          nameC = CName "flagA",
                          nameHsIdent = HsIdentifier
                            "flags_flagA"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 8,
                        structFieldWidth = Just 1}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_flagB",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:4:9",
                        structFieldName = NamePair {
                          nameC = CName "flagB",
                          nameHsIdent = HsIdentifier
                            "flags_flagB"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 9,
                        structFieldWidth = Just 1}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_flagC",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:5:9",
                        structFieldName = NamePair {
                          nameC = CName "flagC",
                          nameHsIdent = HsIdentifier
                            "flags_flagC"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 10,
                        structFieldWidth = Just 1}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_fieldY",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:6:10",
                        structFieldName = NamePair {
                          nameC = CName "fieldY",
                          nameHsIdent = HsIdentifier
                            "flags_fieldY"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        structFieldOffset = 16,
                        structFieldWidth = Nothing}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_bits",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:7:9",
                        structFieldName = NamePair {
                          nameC = CName "bits",
                          nameHsIdent = HsIdentifier
                            "flags_bits"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 24,
                        structFieldWidth = Just 2}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bitfields.h:1:8",
                      declId = NamePair {
                        nameC = CName "flags",
                        nameHsIdent = HsIdentifier
                          "Flags"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "bitfields.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "Flags"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "bitfields.h:2:10",
                            structFieldName = NamePair {
                              nameC = CName "fieldX",
                              nameHsIdent = HsIdentifier
                                "flags_fieldX"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 0,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:3:9",
                            structFieldName = NamePair {
                              nameC = CName "flagA",
                              nameHsIdent = HsIdentifier
                                "flags_flagA"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 8,
                            structFieldWidth = Just 1},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:4:9",
                            structFieldName = NamePair {
                              nameC = CName "flagB",
                              nameHsIdent = HsIdentifier
                                "flags_flagB"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 9,
                            structFieldWidth = Just 1},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:5:9",
                            structFieldName = NamePair {
                              nameC = CName "flagC",
                              nameHsIdent = HsIdentifier
                                "flags_flagC"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 10,
                            structFieldWidth = Just 1},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:6:10",
                            structFieldName = NamePair {
                              nameC = CName "fieldY",
                              nameHsIdent = HsIdentifier
                                "flags_fieldY"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignImplicit
                                  (Just Signed))),
                            structFieldOffset = 16,
                            structFieldWidth = Nothing},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:7:9",
                            structFieldName = NamePair {
                              nameC = CName "bits",
                              nameHsIdent = HsIdentifier
                                "flags_bits"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 24,
                            structFieldWidth = Just 2}],
                        structFlam = Nothing},
                    declSpec = DeclSpec
                      TypeSpec {
                        typeSpecModule = Nothing,
                        typeSpecIdentifier = Nothing,
                        typeSpecInstances = Map.fromList
                          []}},
                structInstances = Set.fromList
                  [Eq, Show, Storable]}
              (Add 6)
              (Seq
                [
                  PokeByteOff (Idx 7) 0 (Idx 0),
                  PokeBitOffWidth
                    (Idx 7)
                    8
                    1
                    (Idx 1),
                  PokeBitOffWidth
                    (Idx 7)
                    9
                    1
                    (Idx 2),
                  PokeBitOffWidth
                    (Idx 7)
                    10
                    1
                    (Idx 3),
                  PokeByteOff (Idx 7) 2 (Idx 4),
                  PokeBitOffWidth
                    (Idx 7)
                    24
                    2
                    (Idx 5)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Flags"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Flags"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Overflow32",
      structConstr = HsName
        "@NsConstr"
        "Overflow32",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow32_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:13:9",
              structFieldName = NamePair {
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "overflow32_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 0,
              structFieldWidth = Just 17}},
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow32_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:14:9",
              structFieldName = NamePair {
                nameC = CName "y",
                nameHsIdent = HsIdentifier
                  "overflow32_y"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Just 17}},
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow32_z",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:15:9",
              structFieldName = NamePair {
                nameC = CName "z",
                nameHsIdent = HsIdentifier
                  "overflow32_z"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 64,
              structFieldWidth = Just 17}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bitfields.h:12:8",
            declId = NamePair {
              nameC = CName "overflow32",
              nameHsIdent = HsIdentifier
                "Overflow32"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "bitfields.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Overflow32"),
              structSizeof = 12,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "bitfields.h:13:9",
                  structFieldName = NamePair {
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "overflow32_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Just 17},
                StructField {
                  structFieldLoc =
                  "bitfields.h:14:9",
                  structFieldName = NamePair {
                    nameC = CName "y",
                    nameHsIdent = HsIdentifier
                      "overflow32_y"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Just 17},
                StructField {
                  structFieldLoc =
                  "bitfields.h:15:9",
                  structFieldName = NamePair {
                    nameC = CName "z",
                    nameHsIdent = HsIdentifier
                      "overflow32_z"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Just 17}],
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
          "Overflow32",
        structConstr = HsName
          "@NsConstr"
          "Overflow32",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow32_x",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:13:9",
                structFieldName = NamePair {
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "overflow32_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 0,
                structFieldWidth = Just 17}},
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow32_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:14:9",
                structFieldName = NamePair {
                  nameC = CName "y",
                  nameHsIdent = HsIdentifier
                    "overflow32_y"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Just 17}},
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow32_z",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:15:9",
                structFieldName = NamePair {
                  nameC = CName "z",
                  nameHsIdent = HsIdentifier
                    "overflow32_z"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 64,
                structFieldWidth = Just 17}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "bitfields.h:12:8",
              declId = NamePair {
                nameC = CName "overflow32",
                nameHsIdent = HsIdentifier
                  "Overflow32"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "bitfields.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Overflow32"),
                structSizeof = 12,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "bitfields.h:13:9",
                    structFieldName = NamePair {
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "overflow32_x"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Just 17},
                  StructField {
                    structFieldLoc =
                    "bitfields.h:14:9",
                    structFieldName = NamePair {
                      nameC = CName "y",
                      nameHsIdent = HsIdentifier
                        "overflow32_y"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 32,
                    structFieldWidth = Just 17},
                  StructField {
                    structFieldLoc =
                    "bitfields.h:15:9",
                    structFieldName = NamePair {
                      nameC = CName "z",
                      nameHsIdent = HsIdentifier
                        "overflow32_z"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 64,
                    structFieldWidth = Just 17}],
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
                  "Overflow32",
                structConstr = HsName
                  "@NsConstr"
                  "Overflow32",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:13:9",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "overflow32_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Just 17}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:14:9",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "overflow32_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Just 17}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32_z",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:15:9",
                        structFieldName = NamePair {
                          nameC = CName "z",
                          nameHsIdent = HsIdentifier
                            "overflow32_z"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Just 17}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bitfields.h:12:8",
                      declId = NamePair {
                        nameC = CName "overflow32",
                        nameHsIdent = HsIdentifier
                          "Overflow32"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "bitfields.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Overflow32"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "bitfields.h:13:9",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "overflow32_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Just 17},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:14:9",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "overflow32_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Just 17},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:15:9",
                            structFieldName = NamePair {
                              nameC = CName "z",
                              nameHsIdent = HsIdentifier
                                "overflow32_z"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 64,
                            structFieldWidth = Just 17}],
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
              PeekBitOffWidth (Idx 0) 0 17,
              PeekBitOffWidth (Idx 0) 32 17,
              PeekBitOffWidth (Idx 0) 64 17]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Overflow32",
                structConstr = HsName
                  "@NsConstr"
                  "Overflow32",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:13:9",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "overflow32_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Just 17}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:14:9",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "overflow32_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Just 17}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32_z",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:15:9",
                        structFieldName = NamePair {
                          nameC = CName "z",
                          nameHsIdent = HsIdentifier
                            "overflow32_z"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Just 17}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bitfields.h:12:8",
                      declId = NamePair {
                        nameC = CName "overflow32",
                        nameHsIdent = HsIdentifier
                          "Overflow32"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "bitfields.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Overflow32"),
                        structSizeof = 12,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "bitfields.h:13:9",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "overflow32_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Just 17},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:14:9",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "overflow32_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Just 17},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:15:9",
                            structFieldName = NamePair {
                              nameC = CName "z",
                              nameHsIdent = HsIdentifier
                                "overflow32_z"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 64,
                            structFieldWidth = Just 17}],
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
                  PokeBitOffWidth
                    (Idx 4)
                    0
                    17
                    (Idx 0),
                  PokeBitOffWidth
                    (Idx 4)
                    32
                    17
                    (Idx 1),
                  PokeBitOffWidth
                    (Idx 4)
                    64
                    17
                    (Idx 2)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Overflow32"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Overflow32"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Overflow32b",
      structConstr = HsName
        "@NsConstr"
        "Overflow32b",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow32b_x",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:19:10",
              structFieldName = NamePair {
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "overflow32b_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 0,
              structFieldWidth = Just 17}},
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow32b_y",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:20:10",
              structFieldName = NamePair {
                nameC = CName "y",
                nameHsIdent = HsIdentifier
                  "overflow32b_y"},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 17,
              structFieldWidth = Just 17}},
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow32b_z",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:21:10",
              structFieldName = NamePair {
                nameC = CName "z",
                nameHsIdent = HsIdentifier
                  "overflow32b_z"},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 34,
              structFieldWidth = Just 17}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bitfields.h:18:8",
            declId = NamePair {
              nameC = CName "overflow32b",
              nameHsIdent = HsIdentifier
                "Overflow32b"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "bitfields.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Overflow32b"),
              structSizeof = 8,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "bitfields.h:19:10",
                  structFieldName = NamePair {
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "overflow32b_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Just 17},
                StructField {
                  structFieldLoc =
                  "bitfields.h:20:10",
                  structFieldName = NamePair {
                    nameC = CName "y",
                    nameHsIdent = HsIdentifier
                      "overflow32b_y"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 17,
                  structFieldWidth = Just 17},
                StructField {
                  structFieldLoc =
                  "bitfields.h:21:10",
                  structFieldName = NamePair {
                    nameC = CName "z",
                    nameHsIdent = HsIdentifier
                      "overflow32b_z"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 34,
                  structFieldWidth = Just 17}],
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
          "Overflow32b",
        structConstr = HsName
          "@NsConstr"
          "Overflow32b",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow32b_x",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:19:10",
                structFieldName = NamePair {
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "overflow32b_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                structFieldOffset = 0,
                structFieldWidth = Just 17}},
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow32b_y",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:20:10",
                structFieldName = NamePair {
                  nameC = CName "y",
                  nameHsIdent = HsIdentifier
                    "overflow32b_y"},
                structFieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                structFieldOffset = 17,
                structFieldWidth = Just 17}},
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow32b_z",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:21:10",
                structFieldName = NamePair {
                  nameC = CName "z",
                  nameHsIdent = HsIdentifier
                    "overflow32b_z"},
                structFieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                structFieldOffset = 34,
                structFieldWidth = Just 17}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "bitfields.h:18:8",
              declId = NamePair {
                nameC = CName "overflow32b",
                nameHsIdent = HsIdentifier
                  "Overflow32b"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "bitfields.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Overflow32b"),
                structSizeof = 8,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "bitfields.h:19:10",
                    structFieldName = NamePair {
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "overflow32b_x"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimLong Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Just 17},
                  StructField {
                    structFieldLoc =
                    "bitfields.h:20:10",
                    structFieldName = NamePair {
                      nameC = CName "y",
                      nameHsIdent = HsIdentifier
                        "overflow32b_y"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimLong Signed),
                    structFieldOffset = 17,
                    structFieldWidth = Just 17},
                  StructField {
                    structFieldLoc =
                    "bitfields.h:21:10",
                    structFieldName = NamePair {
                      nameC = CName "z",
                      nameHsIdent = HsIdentifier
                        "overflow32b_z"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimLong Signed),
                    structFieldOffset = 34,
                    structFieldWidth = Just 17}],
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
        storableAlignment = 8,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Overflow32b",
                structConstr = HsName
                  "@NsConstr"
                  "Overflow32b",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32b_x",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:19:10",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "overflow32b_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Just 17}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32b_y",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:20:10",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "overflow32b_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 17,
                        structFieldWidth = Just 17}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32b_z",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:21:10",
                        structFieldName = NamePair {
                          nameC = CName "z",
                          nameHsIdent = HsIdentifier
                            "overflow32b_z"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 34,
                        structFieldWidth = Just 17}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bitfields.h:18:8",
                      declId = NamePair {
                        nameC = CName "overflow32b",
                        nameHsIdent = HsIdentifier
                          "Overflow32b"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "bitfields.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Overflow32b"),
                        structSizeof = 8,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "bitfields.h:19:10",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "overflow32b_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Just 17},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:20:10",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "overflow32b_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 17,
                            structFieldWidth = Just 17},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:21:10",
                            structFieldName = NamePair {
                              nameC = CName "z",
                              nameHsIdent = HsIdentifier
                                "overflow32b_z"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 34,
                            structFieldWidth = Just 17}],
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
              PeekBitOffWidth (Idx 0) 0 17,
              PeekBitOffWidth (Idx 0) 17 17,
              PeekBitOffWidth (Idx 0) 34 17]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Overflow32b",
                structConstr = HsName
                  "@NsConstr"
                  "Overflow32b",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32b_x",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:19:10",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "overflow32b_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Just 17}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32b_y",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:20:10",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "overflow32b_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 17,
                        structFieldWidth = Just 17}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32b_z",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:21:10",
                        structFieldName = NamePair {
                          nameC = CName "z",
                          nameHsIdent = HsIdentifier
                            "overflow32b_z"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 34,
                        structFieldWidth = Just 17}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bitfields.h:18:8",
                      declId = NamePair {
                        nameC = CName "overflow32b",
                        nameHsIdent = HsIdentifier
                          "Overflow32b"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "bitfields.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Overflow32b"),
                        structSizeof = 8,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "bitfields.h:19:10",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "overflow32b_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Just 17},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:20:10",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "overflow32b_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 17,
                            structFieldWidth = Just 17},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:21:10",
                            structFieldName = NamePair {
                              nameC = CName "z",
                              nameHsIdent = HsIdentifier
                                "overflow32b_z"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 34,
                            structFieldWidth = Just 17}],
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
                  PokeBitOffWidth
                    (Idx 4)
                    0
                    17
                    (Idx 0),
                  PokeBitOffWidth
                    (Idx 4)
                    17
                    17
                    (Idx 1),
                  PokeBitOffWidth
                    (Idx 4)
                    34
                    17
                    (Idx 2)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Overflow32b"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Overflow32b"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Overflow32c",
      structConstr = HsName
        "@NsConstr"
        "Overflow32c",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow32c_x",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:25:10",
              structFieldName = NamePair {
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "overflow32c_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 0,
              structFieldWidth = Just 17}},
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow32c_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:26:10",
              structFieldName = NamePair {
                nameC = CName "y",
                nameHsIdent = HsIdentifier
                  "overflow32c_y"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Just 17}},
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow32c_z",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:27:10",
              structFieldName = NamePair {
                nameC = CName "z",
                nameHsIdent = HsIdentifier
                  "overflow32c_z"},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 64,
              structFieldWidth = Just 17}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bitfields.h:24:8",
            declId = NamePair {
              nameC = CName "overflow32c",
              nameHsIdent = HsIdentifier
                "Overflow32c"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "bitfields.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Overflow32c"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "bitfields.h:25:10",
                  structFieldName = NamePair {
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "overflow32c_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Just 17},
                StructField {
                  structFieldLoc =
                  "bitfields.h:26:10",
                  structFieldName = NamePair {
                    nameC = CName "y",
                    nameHsIdent = HsIdentifier
                      "overflow32c_y"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Just 17},
                StructField {
                  structFieldLoc =
                  "bitfields.h:27:10",
                  structFieldName = NamePair {
                    nameC = CName "z",
                    nameHsIdent = HsIdentifier
                      "overflow32c_z"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Just 17}],
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
          "Overflow32c",
        structConstr = HsName
          "@NsConstr"
          "Overflow32c",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow32c_x",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:25:10",
                structFieldName = NamePair {
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "overflow32c_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                structFieldOffset = 0,
                structFieldWidth = Just 17}},
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow32c_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:26:10",
                structFieldName = NamePair {
                  nameC = CName "y",
                  nameHsIdent = HsIdentifier
                    "overflow32c_y"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Just 17}},
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow32c_z",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:27:10",
                structFieldName = NamePair {
                  nameC = CName "z",
                  nameHsIdent = HsIdentifier
                    "overflow32c_z"},
                structFieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                structFieldOffset = 64,
                structFieldWidth = Just 17}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "bitfields.h:24:8",
              declId = NamePair {
                nameC = CName "overflow32c",
                nameHsIdent = HsIdentifier
                  "Overflow32c"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "bitfields.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Overflow32c"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "bitfields.h:25:10",
                    structFieldName = NamePair {
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "overflow32c_x"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimLong Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Just 17},
                  StructField {
                    structFieldLoc =
                    "bitfields.h:26:10",
                    structFieldName = NamePair {
                      nameC = CName "y",
                      nameHsIdent = HsIdentifier
                        "overflow32c_y"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 32,
                    structFieldWidth = Just 17},
                  StructField {
                    structFieldLoc =
                    "bitfields.h:27:10",
                    structFieldName = NamePair {
                      nameC = CName "z",
                      nameHsIdent = HsIdentifier
                        "overflow32c_z"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimLong Signed),
                    structFieldOffset = 64,
                    structFieldWidth = Just 17}],
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
                  "Overflow32c",
                structConstr = HsName
                  "@NsConstr"
                  "Overflow32c",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32c_x",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:25:10",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "overflow32c_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Just 17}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32c_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:26:10",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "overflow32c_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Just 17}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32c_z",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:27:10",
                        structFieldName = NamePair {
                          nameC = CName "z",
                          nameHsIdent = HsIdentifier
                            "overflow32c_z"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Just 17}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bitfields.h:24:8",
                      declId = NamePair {
                        nameC = CName "overflow32c",
                        nameHsIdent = HsIdentifier
                          "Overflow32c"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "bitfields.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Overflow32c"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "bitfields.h:25:10",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "overflow32c_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Just 17},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:26:10",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "overflow32c_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Just 17},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:27:10",
                            structFieldName = NamePair {
                              nameC = CName "z",
                              nameHsIdent = HsIdentifier
                                "overflow32c_z"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 64,
                            structFieldWidth = Just 17}],
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
              PeekBitOffWidth (Idx 0) 0 17,
              PeekBitOffWidth (Idx 0) 32 17,
              PeekBitOffWidth (Idx 0) 64 17]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Overflow32c",
                structConstr = HsName
                  "@NsConstr"
                  "Overflow32c",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32c_x",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:25:10",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "overflow32c_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Just 17}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32c_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:26:10",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "overflow32c_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Just 17}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32c_z",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:27:10",
                        structFieldName = NamePair {
                          nameC = CName "z",
                          nameHsIdent = HsIdentifier
                            "overflow32c_z"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Just 17}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bitfields.h:24:8",
                      declId = NamePair {
                        nameC = CName "overflow32c",
                        nameHsIdent = HsIdentifier
                          "Overflow32c"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "bitfields.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Overflow32c"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "bitfields.h:25:10",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "overflow32c_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Just 17},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:26:10",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "overflow32c_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Just 17},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:27:10",
                            structFieldName = NamePair {
                              nameC = CName "z",
                              nameHsIdent = HsIdentifier
                                "overflow32c_z"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 64,
                            structFieldWidth = Just 17}],
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
                  PokeBitOffWidth
                    (Idx 4)
                    0
                    17
                    (Idx 0),
                  PokeBitOffWidth
                    (Idx 4)
                    32
                    17
                    (Idx 1),
                  PokeBitOffWidth
                    (Idx 4)
                    64
                    17
                    (Idx 2)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Overflow32c"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Overflow32c"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Overflow64",
      structConstr = HsName
        "@NsConstr"
        "Overflow64",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow64_x",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:31:10",
              structFieldName = NamePair {
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "overflow64_x"},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 0,
              structFieldWidth = Just 33}},
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow64_y",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:32:10",
              structFieldName = NamePair {
                nameC = CName "y",
                nameHsIdent = HsIdentifier
                  "overflow64_y"},
              structFieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              structFieldOffset = 64,
              structFieldWidth = Just 33}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bitfields.h:30:8",
            declId = NamePair {
              nameC = CName "overflow64",
              nameHsIdent = HsIdentifier
                "Overflow64"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "bitfields.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName
                  "@NsConstr"
                  "Overflow64"),
              structSizeof = 16,
              structAlignment = 8,
              structFields = [
                StructField {
                  structFieldLoc =
                  "bitfields.h:31:10",
                  structFieldName = NamePair {
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "overflow64_x"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 0,
                  structFieldWidth = Just 33},
                StructField {
                  structFieldLoc =
                  "bitfields.h:32:10",
                  structFieldName = NamePair {
                    nameC = CName "y",
                    nameHsIdent = HsIdentifier
                      "overflow64_y"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  structFieldOffset = 64,
                  structFieldWidth = Just 33}],
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
          "Overflow64",
        structConstr = HsName
          "@NsConstr"
          "Overflow64",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow64_x",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:31:10",
                structFieldName = NamePair {
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "overflow64_x"},
                structFieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                structFieldOffset = 0,
                structFieldWidth = Just 33}},
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow64_y",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:32:10",
                structFieldName = NamePair {
                  nameC = CName "y",
                  nameHsIdent = HsIdentifier
                    "overflow64_y"},
                structFieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                structFieldOffset = 64,
                structFieldWidth = Just 33}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "bitfields.h:30:8",
              declId = NamePair {
                nameC = CName "overflow64",
                nameHsIdent = HsIdentifier
                  "Overflow64"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "bitfields.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName
                    "@NsConstr"
                    "Overflow64"),
                structSizeof = 16,
                structAlignment = 8,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "bitfields.h:31:10",
                    structFieldName = NamePair {
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "overflow64_x"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimLong Signed),
                    structFieldOffset = 0,
                    structFieldWidth = Just 33},
                  StructField {
                    structFieldLoc =
                    "bitfields.h:32:10",
                    structFieldName = NamePair {
                      nameC = CName "y",
                      nameHsIdent = HsIdentifier
                        "overflow64_y"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimLong Signed),
                    structFieldOffset = 64,
                    structFieldWidth = Just 33}],
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
                  "Overflow64",
                structConstr = HsName
                  "@NsConstr"
                  "Overflow64",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow64_x",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:31:10",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "overflow64_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Just 33}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow64_y",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:32:10",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "overflow64_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Just 33}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bitfields.h:30:8",
                      declId = NamePair {
                        nameC = CName "overflow64",
                        nameHsIdent = HsIdentifier
                          "Overflow64"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "bitfields.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Overflow64"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "bitfields.h:31:10",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "overflow64_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Just 33},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:32:10",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "overflow64_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 64,
                            structFieldWidth = Just 33}],
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
              PeekBitOffWidth (Idx 0) 0 33,
              PeekBitOffWidth (Idx 0) 64 33]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Overflow64",
                structConstr = HsName
                  "@NsConstr"
                  "Overflow64",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow64_x",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:31:10",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "overflow64_x"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 0,
                        structFieldWidth = Just 33}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow64_y",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:32:10",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "overflow64_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        structFieldOffset = 64,
                        structFieldWidth = Just 33}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bitfields.h:30:8",
                      declId = NamePair {
                        nameC = CName "overflow64",
                        nameHsIdent = HsIdentifier
                          "Overflow64"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "bitfields.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName
                            "@NsConstr"
                            "Overflow64"),
                        structSizeof = 16,
                        structAlignment = 8,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "bitfields.h:31:10",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "overflow64_x"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 0,
                            structFieldWidth = Just 33},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:32:10",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "overflow64_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimLong Signed),
                            structFieldOffset = 64,
                            structFieldWidth = Just 33}],
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
                  PokeBitOffWidth
                    (Idx 3)
                    0
                    33
                    (Idx 0),
                  PokeBitOffWidth
                    (Idx 3)
                    64
                    33
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Overflow64"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Overflow64"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "AlignA",
      structConstr = HsName
        "@NsConstr"
        "AlignA",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "alignA_x",
          fieldType = HsPrimType
            HsPrimCUChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:37:16",
              structFieldName = NamePair {
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "alignA_x"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignExplicit Unsigned)),
              structFieldOffset = 0,
              structFieldWidth = Just 1}},
        Field {
          fieldName = HsName
            "@NsVar"
            "alignA_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:38:6",
              structFieldName = NamePair {
                nameC = CName "y",
                nameHsIdent = HsIdentifier
                  "alignA_y"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 1,
              structFieldWidth = Just 10}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bitfields.h:36:8",
            declId = NamePair {
              nameC = CName "alignA",
              nameHsIdent = HsIdentifier
                "AlignA"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "bitfields.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "AlignA"),
              structSizeof = 4,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "bitfields.h:37:16",
                  structFieldName = NamePair {
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "alignA_x"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignExplicit Unsigned)),
                  structFieldOffset = 0,
                  structFieldWidth = Just 1},
                StructField {
                  structFieldLoc =
                  "bitfields.h:38:6",
                  structFieldName = NamePair {
                    nameC = CName "y",
                    nameHsIdent = HsIdentifier
                      "alignA_y"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 1,
                  structFieldWidth = Just 10}],
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
          "AlignA",
        structConstr = HsName
          "@NsConstr"
          "AlignA",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "alignA_x",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:37:16",
                structFieldName = NamePair {
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "alignA_x"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignExplicit Unsigned)),
                structFieldOffset = 0,
                structFieldWidth = Just 1}},
          Field {
            fieldName = HsName
              "@NsVar"
              "alignA_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:38:6",
                structFieldName = NamePair {
                  nameC = CName "y",
                  nameHsIdent = HsIdentifier
                    "alignA_y"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 1,
                structFieldWidth = Just 10}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "bitfields.h:36:8",
              declId = NamePair {
                nameC = CName "alignA",
                nameHsIdent = HsIdentifier
                  "AlignA"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "bitfields.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "AlignA"),
                structSizeof = 4,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "bitfields.h:37:16",
                    structFieldName = NamePair {
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "alignA_x"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignExplicit Unsigned)),
                    structFieldOffset = 0,
                    structFieldWidth = Just 1},
                  StructField {
                    structFieldLoc =
                    "bitfields.h:38:6",
                    structFieldName = NamePair {
                      nameC = CName "y",
                      nameHsIdent = HsIdentifier
                        "alignA_y"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 1,
                    structFieldWidth = Just 10}],
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
                  "AlignA",
                structConstr = HsName
                  "@NsConstr"
                  "AlignA",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "alignA_x",
                    fieldType = HsPrimType
                      HsPrimCUChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:37:16",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "alignA_x"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Unsigned)),
                        structFieldOffset = 0,
                        structFieldWidth = Just 1}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "alignA_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:38:6",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "alignA_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 1,
                        structFieldWidth = Just 10}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bitfields.h:36:8",
                      declId = NamePair {
                        nameC = CName "alignA",
                        nameHsIdent = HsIdentifier
                          "AlignA"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "bitfields.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "AlignA"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "bitfields.h:37:16",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "alignA_x"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignExplicit Unsigned)),
                            structFieldOffset = 0,
                            structFieldWidth = Just 1},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:38:6",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "alignA_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 1,
                            structFieldWidth = Just 10}],
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
              PeekBitOffWidth (Idx 0) 0 1,
              PeekBitOffWidth (Idx 0) 1 10]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "AlignA",
                structConstr = HsName
                  "@NsConstr"
                  "AlignA",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "alignA_x",
                    fieldType = HsPrimType
                      HsPrimCUChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:37:16",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "alignA_x"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Unsigned)),
                        structFieldOffset = 0,
                        structFieldWidth = Just 1}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "alignA_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:38:6",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "alignA_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 1,
                        structFieldWidth = Just 10}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bitfields.h:36:8",
                      declId = NamePair {
                        nameC = CName "alignA",
                        nameHsIdent = HsIdentifier
                          "AlignA"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "bitfields.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "AlignA"),
                        structSizeof = 4,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "bitfields.h:37:16",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "alignA_x"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignExplicit Unsigned)),
                            structFieldOffset = 0,
                            structFieldWidth = Just 1},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:38:6",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "alignA_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 1,
                            structFieldWidth = Just 10}],
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
                  PokeBitOffWidth
                    (Idx 3)
                    0
                    1
                    (Idx 0),
                  PokeBitOffWidth
                    (Idx 3)
                    1
                    10
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "AlignA"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "AlignA"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "AlignB",
      structConstr = HsName
        "@NsConstr"
        "AlignB",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "alignB_x",
          fieldType = HsPrimType
            HsPrimCUChar,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:42:16",
              structFieldName = NamePair {
                nameC = CName "x",
                nameHsIdent = HsIdentifier
                  "alignB_x"},
              structFieldType = TypePrim
                (PrimChar
                  (PrimSignExplicit Unsigned)),
              structFieldOffset = 0,
              structFieldWidth = Just 7}},
        Field {
          fieldName = HsName
            "@NsVar"
            "alignB_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin = StructField
            StructField {
              structFieldLoc =
              "bitfields.h:43:6",
              structFieldName = NamePair {
                nameC = CName "y",
                nameHsIdent = HsIdentifier
                  "alignB_y"},
              structFieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              structFieldOffset = 32,
              structFieldWidth = Just 31}}],
      structOrigin = Just
        Decl {
          declInfo = DeclInfo {
            declLoc = "bitfields.h:41:8",
            declId = NamePair {
              nameC = CName "alignB",
              nameHsIdent = HsIdentifier
                "AlignB"},
            declOrigin = NameOriginInSource,
            declAliases = [],
            declHeader = "bitfields.h"},
          declKind = Struct
            Struct {
              structNames = RecordNames
                (HsName "@NsConstr" "AlignB"),
              structSizeof = 8,
              structAlignment = 4,
              structFields = [
                StructField {
                  structFieldLoc =
                  "bitfields.h:42:16",
                  structFieldName = NamePair {
                    nameC = CName "x",
                    nameHsIdent = HsIdentifier
                      "alignB_x"},
                  structFieldType = TypePrim
                    (PrimChar
                      (PrimSignExplicit Unsigned)),
                  structFieldOffset = 0,
                  structFieldWidth = Just 7},
                StructField {
                  structFieldLoc =
                  "bitfields.h:43:6",
                  structFieldName = NamePair {
                    nameC = CName "y",
                    nameHsIdent = HsIdentifier
                      "alignB_y"},
                  structFieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  structFieldOffset = 32,
                  structFieldWidth = Just 31}],
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
          "AlignB",
        structConstr = HsName
          "@NsConstr"
          "AlignB",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "alignB_x",
            fieldType = HsPrimType
              HsPrimCUChar,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:42:16",
                structFieldName = NamePair {
                  nameC = CName "x",
                  nameHsIdent = HsIdentifier
                    "alignB_x"},
                structFieldType = TypePrim
                  (PrimChar
                    (PrimSignExplicit Unsigned)),
                structFieldOffset = 0,
                structFieldWidth = Just 7}},
          Field {
            fieldName = HsName
              "@NsVar"
              "alignB_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = StructField
              StructField {
                structFieldLoc =
                "bitfields.h:43:6",
                structFieldName = NamePair {
                  nameC = CName "y",
                  nameHsIdent = HsIdentifier
                    "alignB_y"},
                structFieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                structFieldOffset = 32,
                structFieldWidth = Just 31}}],
        structOrigin = Just
          Decl {
            declInfo = DeclInfo {
              declLoc = "bitfields.h:41:8",
              declId = NamePair {
                nameC = CName "alignB",
                nameHsIdent = HsIdentifier
                  "AlignB"},
              declOrigin = NameOriginInSource,
              declAliases = [],
              declHeader = "bitfields.h"},
            declKind = Struct
              Struct {
                structNames = RecordNames
                  (HsName "@NsConstr" "AlignB"),
                structSizeof = 8,
                structAlignment = 4,
                structFields = [
                  StructField {
                    structFieldLoc =
                    "bitfields.h:42:16",
                    structFieldName = NamePair {
                      nameC = CName "x",
                      nameHsIdent = HsIdentifier
                        "alignB_x"},
                    structFieldType = TypePrim
                      (PrimChar
                        (PrimSignExplicit Unsigned)),
                    structFieldOffset = 0,
                    structFieldWidth = Just 7},
                  StructField {
                    structFieldLoc =
                    "bitfields.h:43:6",
                    structFieldName = NamePair {
                      nameC = CName "y",
                      nameHsIdent = HsIdentifier
                        "alignB_y"},
                    structFieldType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    structFieldOffset = 32,
                    structFieldWidth = Just 31}],
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
                  "AlignB",
                structConstr = HsName
                  "@NsConstr"
                  "AlignB",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "alignB_x",
                    fieldType = HsPrimType
                      HsPrimCUChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:42:16",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "alignB_x"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Unsigned)),
                        structFieldOffset = 0,
                        structFieldWidth = Just 7}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "alignB_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:43:6",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "alignB_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Just 31}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bitfields.h:41:8",
                      declId = NamePair {
                        nameC = CName "alignB",
                        nameHsIdent = HsIdentifier
                          "AlignB"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "bitfields.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "AlignB"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "bitfields.h:42:16",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "alignB_x"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignExplicit Unsigned)),
                            structFieldOffset = 0,
                            structFieldWidth = Just 7},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:43:6",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "alignB_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Just 31}],
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
              PeekBitOffWidth (Idx 0) 0 7,
              PeekBitOffWidth (Idx 0) 32 31]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "AlignB",
                structConstr = HsName
                  "@NsConstr"
                  "AlignB",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "alignB_x",
                    fieldType = HsPrimType
                      HsPrimCUChar,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:42:16",
                        structFieldName = NamePair {
                          nameC = CName "x",
                          nameHsIdent = HsIdentifier
                            "alignB_x"},
                        structFieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Unsigned)),
                        structFieldOffset = 0,
                        structFieldWidth = Just 7}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "alignB_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = StructField
                      StructField {
                        structFieldLoc =
                        "bitfields.h:43:6",
                        structFieldName = NamePair {
                          nameC = CName "y",
                          nameHsIdent = HsIdentifier
                            "alignB_y"},
                        structFieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        structFieldOffset = 32,
                        structFieldWidth = Just 31}}],
                structOrigin = Just
                  Decl {
                    declInfo = DeclInfo {
                      declLoc = "bitfields.h:41:8",
                      declId = NamePair {
                        nameC = CName "alignB",
                        nameHsIdent = HsIdentifier
                          "AlignB"},
                      declOrigin = NameOriginInSource,
                      declAliases = [],
                      declHeader = "bitfields.h"},
                    declKind = Struct
                      Struct {
                        structNames = RecordNames
                          (HsName "@NsConstr" "AlignB"),
                        structSizeof = 8,
                        structAlignment = 4,
                        structFields = [
                          StructField {
                            structFieldLoc =
                            "bitfields.h:42:16",
                            structFieldName = NamePair {
                              nameC = CName "x",
                              nameHsIdent = HsIdentifier
                                "alignB_x"},
                            structFieldType = TypePrim
                              (PrimChar
                                (PrimSignExplicit Unsigned)),
                            structFieldOffset = 0,
                            structFieldWidth = Just 7},
                          StructField {
                            structFieldLoc =
                            "bitfields.h:43:6",
                            structFieldName = NamePair {
                              nameC = CName "y",
                              nameHsIdent = HsIdentifier
                                "alignB_y"},
                            structFieldType = TypePrim
                              (PrimIntegral PrimInt Signed),
                            structFieldOffset = 32,
                            structFieldWidth = Just 31}],
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
                  PokeBitOffWidth
                    (Idx 3)
                    0
                    7
                    (Idx 0),
                  PokeBitOffWidth
                    (Idx 3)
                    32
                    31
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "AlignB"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "AlignB")]
