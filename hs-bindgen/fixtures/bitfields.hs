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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "fieldX",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              fieldSourceLoc =
              "bitfields.h:2:10"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "flags_flagA",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "flagA",
              fieldOffset = 8,
              fieldWidth = Just 1,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:3:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "flags_flagB",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "flagB",
              fieldOffset = 9,
              fieldWidth = Just 1,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:4:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "flags_flagC",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "flagC",
              fieldOffset = 10,
              fieldWidth = Just 1,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:5:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "flags_fieldY",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "fieldY",
              fieldOffset = 16,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              fieldSourceLoc =
              "bitfields.h:6:10"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "flags_bits",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "bits",
              fieldOffset = 24,
              fieldWidth = Just 2,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:7:9"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "flags"),
          structAliases = [],
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "fieldX",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              fieldSourceLoc =
              "bitfields.h:2:10"},
            StructField {
              fieldName = CName "flagA",
              fieldOffset = 8,
              fieldWidth = Just 1,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:3:9"},
            StructField {
              fieldName = CName "flagB",
              fieldOffset = 9,
              fieldWidth = Just 1,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:4:9"},
            StructField {
              fieldName = CName "flagC",
              fieldOffset = 10,
              fieldWidth = Just 1,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:5:9"},
            StructField {
              fieldName = CName "fieldY",
              fieldOffset = 16,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar
                  (PrimSignImplicit
                    (Just Signed))),
              fieldSourceLoc =
              "bitfields.h:6:10"},
            StructField {
              fieldName = CName "bits",
              fieldOffset = 24,
              fieldWidth = Just 2,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:7:9"}],
          structFlam = Nothing,
          structSourceLoc =
          "bitfields.h:1:8"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "fieldX",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                fieldSourceLoc =
                "bitfields.h:2:10"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "flags_flagA",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "flagA",
                fieldOffset = 8,
                fieldWidth = Just 1,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:3:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "flags_flagB",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "flagB",
                fieldOffset = 9,
                fieldWidth = Just 1,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:4:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "flags_flagC",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "flagC",
                fieldOffset = 10,
                fieldWidth = Just 1,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:5:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "flags_fieldY",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "fieldY",
                fieldOffset = 16,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                fieldSourceLoc =
                "bitfields.h:6:10"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "flags_bits",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "bits",
                fieldOffset = 24,
                fieldWidth = Just 2,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:7:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "flags"),
            structAliases = [],
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "fieldX",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                fieldSourceLoc =
                "bitfields.h:2:10"},
              StructField {
                fieldName = CName "flagA",
                fieldOffset = 8,
                fieldWidth = Just 1,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:3:9"},
              StructField {
                fieldName = CName "flagB",
                fieldOffset = 9,
                fieldWidth = Just 1,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:4:9"},
              StructField {
                fieldName = CName "flagC",
                fieldOffset = 10,
                fieldWidth = Just 1,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:5:9"},
              StructField {
                fieldName = CName "fieldY",
                fieldOffset = 16,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar
                    (PrimSignImplicit
                      (Just Signed))),
                fieldSourceLoc =
                "bitfields.h:6:10"},
              StructField {
                fieldName = CName "bits",
                fieldOffset = 24,
                fieldWidth = Just 2,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:7:9"}],
            structFlam = Nothing,
            structSourceLoc =
            "bitfields.h:1:8"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "fieldX",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        fieldSourceLoc =
                        "bitfields.h:2:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_flagA",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "flagA",
                        fieldOffset = 8,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:3:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_flagB",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "flagB",
                        fieldOffset = 9,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:4:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_flagC",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "flagC",
                        fieldOffset = 10,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:5:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_fieldY",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "fieldY",
                        fieldOffset = 16,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        fieldSourceLoc =
                        "bitfields.h:6:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_bits",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "bits",
                        fieldOffset = 24,
                        fieldWidth = Just 2,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:7:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "flags"),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "fieldX",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        fieldSourceLoc =
                        "bitfields.h:2:10"},
                      StructField {
                        fieldName = CName "flagA",
                        fieldOffset = 8,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:3:9"},
                      StructField {
                        fieldName = CName "flagB",
                        fieldOffset = 9,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:4:9"},
                      StructField {
                        fieldName = CName "flagC",
                        fieldOffset = 10,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:5:9"},
                      StructField {
                        fieldName = CName "fieldY",
                        fieldOffset = 16,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        fieldSourceLoc =
                        "bitfields.h:6:10"},
                      StructField {
                        fieldName = CName "bits",
                        fieldOffset = 24,
                        fieldWidth = Just 2,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:7:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bitfields.h:1:8"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "fieldX",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        fieldSourceLoc =
                        "bitfields.h:2:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_flagA",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "flagA",
                        fieldOffset = 8,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:3:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_flagB",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "flagB",
                        fieldOffset = 9,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:4:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_flagC",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "flagC",
                        fieldOffset = 10,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:5:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_fieldY",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "fieldY",
                        fieldOffset = 16,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        fieldSourceLoc =
                        "bitfields.h:6:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "flags_bits",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "bits",
                        fieldOffset = 24,
                        fieldWidth = Just 2,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:7:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "flags"),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "fieldX",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        fieldSourceLoc =
                        "bitfields.h:2:10"},
                      StructField {
                        fieldName = CName "flagA",
                        fieldOffset = 8,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:3:9"},
                      StructField {
                        fieldName = CName "flagB",
                        fieldOffset = 9,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:4:9"},
                      StructField {
                        fieldName = CName "flagC",
                        fieldOffset = 10,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:5:9"},
                      StructField {
                        fieldName = CName "fieldY",
                        fieldOffset = 16,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignImplicit
                              (Just Signed))),
                        fieldSourceLoc =
                        "bitfields.h:6:10"},
                      StructField {
                        fieldName = CName "bits",
                        fieldOffset = 24,
                        fieldWidth = Just 2,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:7:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bitfields.h:1:8"}}
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:13:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow32_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:14:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow32_z",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "z",
              fieldOffset = 64,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:15:9"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "overflow32"),
          structAliases = [],
          structSizeof = 12,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:13:9"},
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:14:9"},
            StructField {
              fieldName = CName "z",
              fieldOffset = 64,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:15:9"}],
          structFlam = Nothing,
          structSourceLoc =
          "bitfields.h:12:8"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:13:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow32_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:14:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow32_z",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "z",
                fieldOffset = 64,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:15:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "overflow32"),
            structAliases = [],
            structSizeof = 12,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:13:9"},
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:14:9"},
              StructField {
                fieldName = CName "z",
                fieldOffset = 64,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:15:9"}],
            structFlam = Nothing,
            structSourceLoc =
            "bitfields.h:12:8"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:13:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:14:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32_z",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 64,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:15:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "overflow32"),
                    structAliases = [],
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:13:9"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:14:9"},
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 64,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:15:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bitfields.h:12:8"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:13:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:14:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32_z",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 64,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:15:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "overflow32"),
                    structAliases = [],
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:13:9"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:14:9"},
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 64,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:15:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bitfields.h:12:8"}}
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc =
              "bitfields.h:19:10"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow32b_y",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "y",
              fieldOffset = 17,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc =
              "bitfields.h:20:10"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow32b_z",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "z",
              fieldOffset = 34,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc =
              "bitfields.h:21:10"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "overflow32b"),
          structAliases = [],
          structSizeof = 8,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc =
              "bitfields.h:19:10"},
            StructField {
              fieldName = CName "y",
              fieldOffset = 17,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc =
              "bitfields.h:20:10"},
            StructField {
              fieldName = CName "z",
              fieldOffset = 34,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc =
              "bitfields.h:21:10"}],
          structFlam = Nothing,
          structSourceLoc =
          "bitfields.h:18:8"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "bitfields.h:19:10"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow32b_y",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "y",
                fieldOffset = 17,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "bitfields.h:20:10"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow32b_z",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "z",
                fieldOffset = 34,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "bitfields.h:21:10"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "overflow32b"),
            structAliases = [],
            structSizeof = 8,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "bitfields.h:19:10"},
              StructField {
                fieldName = CName "y",
                fieldOffset = 17,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "bitfields.h:20:10"},
              StructField {
                fieldName = CName "z",
                fieldOffset = 34,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "bitfields.h:21:10"}],
            structFlam = Nothing,
            structSourceLoc =
            "bitfields.h:18:8"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:19:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32b_y",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 17,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:20:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32b_z",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 34,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:21:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "overflow32b"),
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:19:10"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 17,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:20:10"},
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 34,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:21:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bitfields.h:18:8"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:19:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32b_y",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 17,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:20:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32b_z",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 34,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:21:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "overflow32b"),
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:19:10"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 17,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:20:10"},
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 34,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:21:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bitfields.h:18:8"}}
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc =
              "bitfields.h:25:10"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow32c_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:26:10"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow32c_z",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "z",
              fieldOffset = 64,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc =
              "bitfields.h:27:10"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "overflow32c"),
          structAliases = [],
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc =
              "bitfields.h:25:10"},
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:26:10"},
            StructField {
              fieldName = CName "z",
              fieldOffset = 64,
              fieldWidth = Just 17,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc =
              "bitfields.h:27:10"}],
          structFlam = Nothing,
          structSourceLoc =
          "bitfields.h:24:8"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "bitfields.h:25:10"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow32c_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:26:10"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow32c_z",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "z",
                fieldOffset = 64,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "bitfields.h:27:10"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "overflow32c"),
            structAliases = [],
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "bitfields.h:25:10"},
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:26:10"},
              StructField {
                fieldName = CName "z",
                fieldOffset = 64,
                fieldWidth = Just 17,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "bitfields.h:27:10"}],
            structFlam = Nothing,
            structSourceLoc =
            "bitfields.h:24:8"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:25:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32c_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:26:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32c_z",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 64,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:27:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "overflow32c"),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:25:10"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:26:10"},
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 64,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:27:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bitfields.h:24:8"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:25:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32c_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:26:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow32c_z",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 64,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:27:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "overflow32c"),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:25:10"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:26:10"},
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 64,
                        fieldWidth = Just 17,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:27:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bitfields.h:24:8"}}
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Just 33,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc =
              "bitfields.h:31:10"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "overflow64_y",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "y",
              fieldOffset = 64,
              fieldWidth = Just 33,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc =
              "bitfields.h:32:10"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "overflow64"),
          structAliases = [],
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Just 33,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc =
              "bitfields.h:31:10"},
            StructField {
              fieldName = CName "y",
              fieldOffset = 64,
              fieldWidth = Just 33,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc =
              "bitfields.h:32:10"}],
          structFlam = Nothing,
          structSourceLoc =
          "bitfields.h:30:8"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Just 33,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "bitfields.h:31:10"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "overflow64_y",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "y",
                fieldOffset = 64,
                fieldWidth = Just 33,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "bitfields.h:32:10"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "overflow64"),
            structAliases = [],
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Just 33,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "bitfields.h:31:10"},
              StructField {
                fieldName = CName "y",
                fieldOffset = 64,
                fieldWidth = Just 33,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "bitfields.h:32:10"}],
            structFlam = Nothing,
            structSourceLoc =
            "bitfields.h:30:8"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 33,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:31:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow64_y",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 64,
                        fieldWidth = Just 33,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:32:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "overflow64"),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 33,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:31:10"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 64,
                        fieldWidth = Just 33,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:32:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bitfields.h:30:8"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 33,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:31:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "overflow64_y",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 64,
                        fieldWidth = Just 33,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:32:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "overflow64"),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 33,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:31:10"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 64,
                        fieldWidth = Just 33,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "bitfields.h:32:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bitfields.h:30:8"}}
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Just 1,
              fieldType = TypePrim
                (PrimChar
                  (PrimSignExplicit Unsigned)),
              fieldSourceLoc =
              "bitfields.h:37:16"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "alignA_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "y",
              fieldOffset = 1,
              fieldWidth = Just 10,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:38:6"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "alignA"),
          structAliases = [],
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Just 1,
              fieldType = TypePrim
                (PrimChar
                  (PrimSignExplicit Unsigned)),
              fieldSourceLoc =
              "bitfields.h:37:16"},
            StructField {
              fieldName = CName "y",
              fieldOffset = 1,
              fieldWidth = Just 10,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:38:6"}],
          structFlam = Nothing,
          structSourceLoc =
          "bitfields.h:36:8"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Just 1,
                fieldType = TypePrim
                  (PrimChar
                    (PrimSignExplicit Unsigned)),
                fieldSourceLoc =
                "bitfields.h:37:16"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "alignA_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "y",
                fieldOffset = 1,
                fieldWidth = Just 10,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:38:6"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "alignA"),
            structAliases = [],
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Just 1,
                fieldType = TypePrim
                  (PrimChar
                    (PrimSignExplicit Unsigned)),
                fieldSourceLoc =
                "bitfields.h:37:16"},
              StructField {
                fieldName = CName "y",
                fieldOffset = 1,
                fieldWidth = Just 10,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:38:6"}],
            structFlam = Nothing,
            structSourceLoc =
            "bitfields.h:36:8"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Unsigned)),
                        fieldSourceLoc =
                        "bitfields.h:37:16"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "alignA_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 1,
                        fieldWidth = Just 10,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:38:6"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "alignA"),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Unsigned)),
                        fieldSourceLoc =
                        "bitfields.h:37:16"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 1,
                        fieldWidth = Just 10,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:38:6"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bitfields.h:36:8"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Unsigned)),
                        fieldSourceLoc =
                        "bitfields.h:37:16"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "alignA_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 1,
                        fieldWidth = Just 10,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:38:6"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "alignA"),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 1,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Unsigned)),
                        fieldSourceLoc =
                        "bitfields.h:37:16"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 1,
                        fieldWidth = Just 10,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:38:6"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bitfields.h:36:8"}}
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Just 7,
              fieldType = TypePrim
                (PrimChar
                  (PrimSignExplicit Unsigned)),
              fieldSourceLoc =
              "bitfields.h:42:16"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "alignB_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldWidth = Just 31,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:43:6"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "alignB"),
          structAliases = [],
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Just 7,
              fieldType = TypePrim
                (PrimChar
                  (PrimSignExplicit Unsigned)),
              fieldSourceLoc =
              "bitfields.h:42:16"},
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldWidth = Just 31,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "bitfields.h:43:6"}],
          structFlam = Nothing,
          structSourceLoc =
          "bitfields.h:41:8"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Just 7,
                fieldType = TypePrim
                  (PrimChar
                    (PrimSignExplicit Unsigned)),
                fieldSourceLoc =
                "bitfields.h:42:16"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "alignB_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldWidth = Just 31,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:43:6"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "alignB"),
            structAliases = [],
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Just 7,
                fieldType = TypePrim
                  (PrimChar
                    (PrimSignExplicit Unsigned)),
                fieldSourceLoc =
                "bitfields.h:42:16"},
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldWidth = Just 31,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "bitfields.h:43:6"}],
            structFlam = Nothing,
            structSourceLoc =
            "bitfields.h:41:8"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 7,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Unsigned)),
                        fieldSourceLoc =
                        "bitfields.h:42:16"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "alignB_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Just 31,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:43:6"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "alignB"),
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 7,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Unsigned)),
                        fieldSourceLoc =
                        "bitfields.h:42:16"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Just 31,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:43:6"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bitfields.h:41:8"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 7,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Unsigned)),
                        fieldSourceLoc =
                        "bitfields.h:42:16"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "alignB_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Just 31,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:43:6"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "alignB"),
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Just 7,
                        fieldType = TypePrim
                          (PrimChar
                            (PrimSignExplicit Unsigned)),
                        fieldSourceLoc =
                        "bitfields.h:42:16"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Just 31,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "bitfields.h:43:6"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bitfields.h:41:8"}}
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
