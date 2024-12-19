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
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/bitfields.h:2:10"}},
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
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/bitfields.h:6:10"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "flags"))
            DeclPathTop,
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "fieldX",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/bitfields.h:2:10"},
            StructField {
              fieldName = CName "fieldY",
              fieldOffset = 16,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/bitfields.h:6:10"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/bitfields.h:1:8",
          structBitfields = [
            _×_
              StructField {
                fieldName = CName "flagA",
                fieldOffset = 8,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/bitfields.h:3:9"}
              1,
            _×_
              StructField {
                fieldName = CName "flagB",
                fieldOffset = 9,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/bitfields.h:4:9"}
              1,
            _×_
              StructField {
                fieldName = CName "flagC",
                fieldOffset = 10,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/bitfields.h:5:9"}
              1,
            _×_
              StructField {
                fieldName = CName "bits",
                fieldOffset = 24,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/bitfields.h:7:9"}
              2]}},
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
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/bitfields.h:2:10"}},
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
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/bitfields.h:6:10"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "flags"))
              DeclPathTop,
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "fieldX",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/bitfields.h:2:10"},
              StructField {
                fieldName = CName "fieldY",
                fieldOffset = 16,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/bitfields.h:6:10"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/bitfields.h:1:8",
            structBitfields = [
              _×_
                StructField {
                  fieldName = CName "flagA",
                  fieldOffset = 8,
                  fieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:3:9"}
                1,
              _×_
                StructField {
                  fieldName = CName "flagB",
                  fieldOffset = 9,
                  fieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:4:9"}
                1,
              _×_
                StructField {
                  fieldName = CName "flagC",
                  fieldOffset = 10,
                  fieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:5:9"}
                1,
              _×_
                StructField {
                  fieldName = CName "bits",
                  fieldOffset = 24,
                  fieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:7:9"}
                2]}}
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/bitfields.h:2:10"}},
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/bitfields.h:6:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "flags"))
                      DeclPathTop,
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "fieldX",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/bitfields.h:2:10"},
                      StructField {
                        fieldName = CName "fieldY",
                        fieldOffset = 16,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/bitfields.h:6:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/bitfields.h:1:8",
                    structBitfields = [
                      _×_
                        StructField {
                          fieldName = CName "flagA",
                          fieldOffset = 8,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:3:9"}
                        1,
                      _×_
                        StructField {
                          fieldName = CName "flagB",
                          fieldOffset = 9,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:4:9"}
                        1,
                      _×_
                        StructField {
                          fieldName = CName "flagC",
                          fieldOffset = 10,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:5:9"}
                        1,
                      _×_
                        StructField {
                          fieldName = CName "bits",
                          fieldOffset = 24,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:7:9"}
                        2]}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 2]),
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/bitfields.h:2:10"}},
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/bitfields.h:6:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "flags"))
                      DeclPathTop,
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "fieldX",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/bitfields.h:2:10"},
                      StructField {
                        fieldName = CName "fieldY",
                        fieldOffset = 16,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/bitfields.h:6:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/bitfields.h:1:8",
                    structBitfields = [
                      _×_
                        StructField {
                          fieldName = CName "flagA",
                          fieldOffset = 8,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:3:9"}
                        1,
                      _×_
                        StructField {
                          fieldName = CName "flagB",
                          fieldOffset = 9,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:4:9"}
                        1,
                      _×_
                        StructField {
                          fieldName = CName "flagC",
                          fieldOffset = 10,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:5:9"}
                        1,
                      _×_
                        StructField {
                          fieldName = CName "bits",
                          fieldOffset = 24,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:7:9"}
                        2]}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    2
                    (Idx 1)])))}),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Overflow32",
      structConstr = HsName
        "@NsConstr"
        "Overflow32",
      structFields = [],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag
              (CName "overflow32"))
            DeclPathTop,
          structSizeof = 12,
          structAlignment = 4,
          structFields = [],
          structFlam = Nothing,
          structSourceLoc =
          "examples/bitfields.h:12:8",
          structBitfields = [
            _×_
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/bitfields.h:13:9"}
              17,
            _×_
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/bitfields.h:14:9"}
              17,
            _×_
              StructField {
                fieldName = CName "z",
                fieldOffset = 64,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/bitfields.h:15:9"}
              17]}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Overflow32",
        structConstr = HsName
          "@NsConstr"
          "Overflow32",
        structFields = [],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag
                (CName "overflow32"))
              DeclPathTop,
            structSizeof = 12,
            structAlignment = 4,
            structFields = [],
            structFlam = Nothing,
            structSourceLoc =
            "examples/bitfields.h:12:8",
            structBitfields = [
              _×_
                StructField {
                  fieldName = CName "x",
                  fieldOffset = 0,
                  fieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:13:9"}
                17,
              _×_
                StructField {
                  fieldName = CName "y",
                  fieldOffset = 32,
                  fieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:14:9"}
                17,
              _×_
                StructField {
                  fieldName = CName "z",
                  fieldOffset = 64,
                  fieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:15:9"}
                17]}}
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
                structFields = [],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "overflow32"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/bitfields.h:12:8",
                    structBitfields = [
                      _×_
                        StructField {
                          fieldName = CName "x",
                          fieldOffset = 0,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:13:9"}
                        17,
                      _×_
                        StructField {
                          fieldName = CName "y",
                          fieldOffset = 32,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:14:9"}
                        17,
                      _×_
                        StructField {
                          fieldName = CName "z",
                          fieldOffset = 64,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:15:9"}
                        17]}})
            []),
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
                structFields = [],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "overflow32"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/bitfields.h:12:8",
                    structBitfields = [
                      _×_
                        StructField {
                          fieldName = CName "x",
                          fieldOffset = 0,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:13:9"}
                        17,
                      _×_
                        StructField {
                          fieldName = CName "y",
                          fieldOffset = 32,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:14:9"}
                        17,
                      _×_
                        StructField {
                          fieldName = CName "z",
                          fieldOffset = 64,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:15:9"}
                        17]}}
              (Add 0)
              (Seq [])))}),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Overflow32b",
      structConstr = HsName
        "@NsConstr"
        "Overflow32b",
      structFields = [],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag
              (CName "overflow32b"))
            DeclPathTop,
          structSizeof = 8,
          structAlignment = 8,
          structFields = [],
          structFlam = Nothing,
          structSourceLoc =
          "examples/bitfields.h:18:8",
          structBitfields = [
            _×_
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "examples/bitfields.h:19:10"}
              17,
            _×_
              StructField {
                fieldName = CName "y",
                fieldOffset = 17,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "examples/bitfields.h:20:10"}
              17,
            _×_
              StructField {
                fieldName = CName "z",
                fieldOffset = 34,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "examples/bitfields.h:21:10"}
              17]}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Overflow32b",
        structConstr = HsName
          "@NsConstr"
          "Overflow32b",
        structFields = [],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag
                (CName "overflow32b"))
              DeclPathTop,
            structSizeof = 8,
            structAlignment = 8,
            structFields = [],
            structFlam = Nothing,
            structSourceLoc =
            "examples/bitfields.h:18:8",
            structBitfields = [
              _×_
                StructField {
                  fieldName = CName "x",
                  fieldOffset = 0,
                  fieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:19:10"}
                17,
              _×_
                StructField {
                  fieldName = CName "y",
                  fieldOffset = 17,
                  fieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:20:10"}
                17,
              _×_
                StructField {
                  fieldName = CName "z",
                  fieldOffset = 34,
                  fieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:21:10"}
                17]}}
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
                structFields = [],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "overflow32b"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 8,
                    structFields = [],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/bitfields.h:18:8",
                    structBitfields = [
                      _×_
                        StructField {
                          fieldName = CName "x",
                          fieldOffset = 0,
                          fieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:19:10"}
                        17,
                      _×_
                        StructField {
                          fieldName = CName "y",
                          fieldOffset = 17,
                          fieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:20:10"}
                        17,
                      _×_
                        StructField {
                          fieldName = CName "z",
                          fieldOffset = 34,
                          fieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:21:10"}
                        17]}})
            []),
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
                structFields = [],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "overflow32b"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 8,
                    structFields = [],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/bitfields.h:18:8",
                    structBitfields = [
                      _×_
                        StructField {
                          fieldName = CName "x",
                          fieldOffset = 0,
                          fieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:19:10"}
                        17,
                      _×_
                        StructField {
                          fieldName = CName "y",
                          fieldOffset = 17,
                          fieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:20:10"}
                        17,
                      _×_
                        StructField {
                          fieldName = CName "z",
                          fieldOffset = 34,
                          fieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:21:10"}
                        17]}}
              (Add 0)
              (Seq [])))}),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Overflow32c",
      structConstr = HsName
        "@NsConstr"
        "Overflow32c",
      structFields = [],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag
              (CName "overflow32c"))
            DeclPathTop,
          structSizeof = 16,
          structAlignment = 8,
          structFields = [],
          structFlam = Nothing,
          structSourceLoc =
          "examples/bitfields.h:24:8",
          structBitfields = [
            _×_
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "examples/bitfields.h:25:10"}
              17,
            _×_
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/bitfields.h:26:10"}
              17,
            _×_
              StructField {
                fieldName = CName "z",
                fieldOffset = 64,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "examples/bitfields.h:27:10"}
              17]}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Overflow32c",
        structConstr = HsName
          "@NsConstr"
          "Overflow32c",
        structFields = [],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag
                (CName "overflow32c"))
              DeclPathTop,
            structSizeof = 16,
            structAlignment = 8,
            structFields = [],
            structFlam = Nothing,
            structSourceLoc =
            "examples/bitfields.h:24:8",
            structBitfields = [
              _×_
                StructField {
                  fieldName = CName "x",
                  fieldOffset = 0,
                  fieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:25:10"}
                17,
              _×_
                StructField {
                  fieldName = CName "y",
                  fieldOffset = 32,
                  fieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:26:10"}
                17,
              _×_
                StructField {
                  fieldName = CName "z",
                  fieldOffset = 64,
                  fieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:27:10"}
                17]}}
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
                structFields = [],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "overflow32c"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/bitfields.h:24:8",
                    structBitfields = [
                      _×_
                        StructField {
                          fieldName = CName "x",
                          fieldOffset = 0,
                          fieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:25:10"}
                        17,
                      _×_
                        StructField {
                          fieldName = CName "y",
                          fieldOffset = 32,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:26:10"}
                        17,
                      _×_
                        StructField {
                          fieldName = CName "z",
                          fieldOffset = 64,
                          fieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:27:10"}
                        17]}})
            []),
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
                structFields = [],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "overflow32c"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/bitfields.h:24:8",
                    structBitfields = [
                      _×_
                        StructField {
                          fieldName = CName "x",
                          fieldOffset = 0,
                          fieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:25:10"}
                        17,
                      _×_
                        StructField {
                          fieldName = CName "y",
                          fieldOffset = 32,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:26:10"}
                        17,
                      _×_
                        StructField {
                          fieldName = CName "z",
                          fieldOffset = 64,
                          fieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:27:10"}
                        17]}}
              (Add 0)
              (Seq [])))}),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Overflow64",
      structConstr = HsName
        "@NsConstr"
        "Overflow64",
      structFields = [],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag
              (CName "overflow64"))
            DeclPathTop,
          structSizeof = 16,
          structAlignment = 8,
          structFields = [],
          structFlam = Nothing,
          structSourceLoc =
          "examples/bitfields.h:30:8",
          structBitfields = [
            _×_
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "examples/bitfields.h:31:10"}
              33,
            _×_
              StructField {
                fieldName = CName "y",
                fieldOffset = 64,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "examples/bitfields.h:32:10"}
              33]}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Overflow64",
        structConstr = HsName
          "@NsConstr"
          "Overflow64",
        structFields = [],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag
                (CName "overflow64"))
              DeclPathTop,
            structSizeof = 16,
            structAlignment = 8,
            structFields = [],
            structFlam = Nothing,
            structSourceLoc =
            "examples/bitfields.h:30:8",
            structBitfields = [
              _×_
                StructField {
                  fieldName = CName "x",
                  fieldOffset = 0,
                  fieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:31:10"}
                33,
              _×_
                StructField {
                  fieldName = CName "y",
                  fieldOffset = 64,
                  fieldType = TypePrim
                    (PrimIntegral PrimLong Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:32:10"}
                33]}}
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
                structFields = [],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "overflow64"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/bitfields.h:30:8",
                    structBitfields = [
                      _×_
                        StructField {
                          fieldName = CName "x",
                          fieldOffset = 0,
                          fieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:31:10"}
                        33,
                      _×_
                        StructField {
                          fieldName = CName "y",
                          fieldOffset = 64,
                          fieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:32:10"}
                        33]}})
            []),
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
                structFields = [],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "overflow64"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/bitfields.h:30:8",
                    structBitfields = [
                      _×_
                        StructField {
                          fieldName = CName "x",
                          fieldOffset = 0,
                          fieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:31:10"}
                        33,
                      _×_
                        StructField {
                          fieldName = CName "y",
                          fieldOffset = 64,
                          fieldType = TypePrim
                            (PrimIntegral PrimLong Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:32:10"}
                        33]}}
              (Add 0)
              (Seq [])))}),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "AlignA",
      structConstr = HsName
        "@NsConstr"
        "AlignA",
      structFields = [],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "alignA"))
            DeclPathTop,
          structSizeof = 4,
          structAlignment = 4,
          structFields = [],
          structFlam = Nothing,
          structSourceLoc =
          "examples/bitfields.h:36:8",
          structBitfields = [
            _×_
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimChar (Just Unsigned)),
                fieldSourceLoc =
                "examples/bitfields.h:37:16"}
              1,
            _×_
              StructField {
                fieldName = CName "y",
                fieldOffset = 1,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/bitfields.h:38:6"}
              10]}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "AlignA",
        structConstr = HsName
          "@NsConstr"
          "AlignA",
        structFields = [],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "alignA"))
              DeclPathTop,
            structSizeof = 4,
            structAlignment = 4,
            structFields = [],
            structFlam = Nothing,
            structSourceLoc =
            "examples/bitfields.h:36:8",
            structBitfields = [
              _×_
                StructField {
                  fieldName = CName "x",
                  fieldOffset = 0,
                  fieldType = TypePrim
                    (PrimChar (Just Unsigned)),
                  fieldSourceLoc =
                  "examples/bitfields.h:37:16"}
                1,
              _×_
                StructField {
                  fieldName = CName "y",
                  fieldOffset = 1,
                  fieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:38:6"}
                10]}}
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
                structFields = [],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "alignA"))
                      DeclPathTop,
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/bitfields.h:36:8",
                    structBitfields = [
                      _×_
                        StructField {
                          fieldName = CName "x",
                          fieldOffset = 0,
                          fieldType = TypePrim
                            (PrimChar (Just Unsigned)),
                          fieldSourceLoc =
                          "examples/bitfields.h:37:16"}
                        1,
                      _×_
                        StructField {
                          fieldName = CName "y",
                          fieldOffset = 1,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:38:6"}
                        10]}})
            []),
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
                structFields = [],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "alignA"))
                      DeclPathTop,
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/bitfields.h:36:8",
                    structBitfields = [
                      _×_
                        StructField {
                          fieldName = CName "x",
                          fieldOffset = 0,
                          fieldType = TypePrim
                            (PrimChar (Just Unsigned)),
                          fieldSourceLoc =
                          "examples/bitfields.h:37:16"}
                        1,
                      _×_
                        StructField {
                          fieldName = CName "y",
                          fieldOffset = 1,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:38:6"}
                        10]}}
              (Add 0)
              (Seq [])))}),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "AlignB",
      structConstr = HsName
        "@NsConstr"
        "AlignB",
      structFields = [],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "alignB"))
            DeclPathTop,
          structSizeof = 8,
          structAlignment = 4,
          structFields = [],
          structFlam = Nothing,
          structSourceLoc =
          "examples/bitfields.h:41:8",
          structBitfields = [
            _×_
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimChar (Just Unsigned)),
                fieldSourceLoc =
                "examples/bitfields.h:42:16"}
              7,
            _×_
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/bitfields.h:43:6"}
              31]}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "AlignB",
        structConstr = HsName
          "@NsConstr"
          "AlignB",
        structFields = [],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "alignB"))
              DeclPathTop,
            structSizeof = 8,
            structAlignment = 4,
            structFields = [],
            structFlam = Nothing,
            structSourceLoc =
            "examples/bitfields.h:41:8",
            structBitfields = [
              _×_
                StructField {
                  fieldName = CName "x",
                  fieldOffset = 0,
                  fieldType = TypePrim
                    (PrimChar (Just Unsigned)),
                  fieldSourceLoc =
                  "examples/bitfields.h:42:16"}
                7,
              _×_
                StructField {
                  fieldName = CName "y",
                  fieldOffset = 32,
                  fieldType = TypePrim
                    (PrimIntegral PrimInt Signed),
                  fieldSourceLoc =
                  "examples/bitfields.h:43:6"}
                31]}}
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
                structFields = [],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "alignB"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/bitfields.h:41:8",
                    structBitfields = [
                      _×_
                        StructField {
                          fieldName = CName "x",
                          fieldOffset = 0,
                          fieldType = TypePrim
                            (PrimChar (Just Unsigned)),
                          fieldSourceLoc =
                          "examples/bitfields.h:42:16"}
                        7,
                      _×_
                        StructField {
                          fieldName = CName "y",
                          fieldOffset = 32,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:43:6"}
                        31]}})
            []),
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
                structFields = [],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "alignB"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/bitfields.h:41:8",
                    structBitfields = [
                      _×_
                        StructField {
                          fieldName = CName "x",
                          fieldOffset = 0,
                          fieldType = TypePrim
                            (PrimChar (Just Unsigned)),
                          fieldSourceLoc =
                          "examples/bitfields.h:42:16"}
                        7,
                      _×_
                        StructField {
                          fieldName = CName "y",
                          fieldOffset = 32,
                          fieldType = TypePrim
                            (PrimIntegral PrimInt Signed),
                          fieldSourceLoc =
                          "examples/bitfields.h:43:6"}
                        31]}}
              (Add 0)
              (Seq [])))})]
