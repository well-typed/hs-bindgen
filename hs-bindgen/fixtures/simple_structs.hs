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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "simple_structs.h:3:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s1_b",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:4:10"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            (DeclNameTag (CName "S1"))
            DeclPathTop,
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "simple_structs.h:3:9"},
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:4:10"}],
          structFlam = Nothing,
          structSourceLoc =
          "simple_structs.h:2:8"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "simple_structs.h:3:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s1_b",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:4:10"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "S1"))
              DeclPathTop,
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "simple_structs.h:3:9"},
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:4:10"}],
            structFlam = Nothing,
            structSourceLoc =
            "simple_structs.h:2:8"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:3:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_b",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:4:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "S1"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:3:9"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:4:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:2:8"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:3:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_b",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:4:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "S1"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:3:9"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:4:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:2:8"}}
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:9:10"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "simple_structs.h:10:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_c",
          fieldType = HsPrimType
            HsPrimCFloat,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "c",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimFloating PrimFloat),
              fieldSourceLoc =
              "simple_structs.h:11:11"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            (DeclNameTag (CName "S2"))
            DeclPathTop,
          structSizeof = 12,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:9:10"},
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "simple_structs.h:10:9"},
            StructField {
              fieldName = CName "c",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimFloating PrimFloat),
              fieldSourceLoc =
              "simple_structs.h:11:11"}],
          structFlam = Nothing,
          structSourceLoc =
          "simple_structs.h:8:16"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:9:10"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s2_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "simple_structs.h:10:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s2_c",
            fieldType = HsPrimType
              HsPrimCFloat,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "c",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimFloating PrimFloat),
                fieldSourceLoc =
                "simple_structs.h:11:11"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "S2"))
              DeclPathTop,
            structSizeof = 12,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:9:10"},
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "simple_structs.h:10:9"},
              StructField {
                fieldName = CName "c",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimFloating PrimFloat),
                fieldSourceLoc =
                "simple_structs.h:11:11"}],
            structFlam = Nothing,
            structSourceLoc =
            "simple_structs.h:8:16"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:9:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:10:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_c",
                    fieldType = HsPrimType
                      HsPrimCFloat,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "simple_structs.h:11:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "S2"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:9:10"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:10:9"},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "simple_structs.h:11:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:8:16"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:9:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:10:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_c",
                    fieldType = HsPrimType
                      HsPrimCFloat,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "simple_structs.h:11:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "S2"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:9:10"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:10:9"},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "simple_structs.h:11:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:8:16"}}
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
          "unS2_t",
        fieldType = HsTypRef
          (HsName "@NsTypeConstr" "S2"),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "S2_t",
          typedefType = TypeStruct
            (DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "S2"))
              DeclPathTop),
          typedefSourceLoc =
          "simple_structs.h:12:3"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:16:10"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            (DeclNameTypedef (CName "S3_t"))
            DeclPathTop,
          structSizeof = 1,
          structAlignment = 1,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:16:10"}],
          structFlam = Nothing,
          structSourceLoc =
          "simple_structs.h:15:9"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:16:10"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTypedef (CName "S3_t"))
              DeclPathTop,
            structSizeof = 1,
            structAlignment = 1,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:16:10"}],
            structFlam = Nothing,
            structSourceLoc =
            "simple_structs.h:15:9"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:16:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTypedef (CName "S3_t"))
                      DeclPathTop,
                    structSizeof = 1,
                    structAlignment = 1,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:16:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:15:9"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:16:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTypedef (CName "S3_t"))
                      DeclPathTop,
                    structSizeof = 1,
                    structAlignment = 1,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:16:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:15:9"}}
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "b",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:20:10"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s4_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "a",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "simple_structs.h:21:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s4_c",
          fieldType = HsPtr
            (HsPrimType HsPrimCInt),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "c",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              fieldSourceLoc =
              "simple_structs.h:22:10"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            (DeclNameTag (CName "S4"))
            DeclPathTop,
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "b",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:20:10"},
            StructField {
              fieldName = CName "a",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "simple_structs.h:21:9"},
            StructField {
              fieldName = CName "c",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              fieldSourceLoc =
              "simple_structs.h:22:10"}],
          structFlam = Nothing,
          structSourceLoc =
          "simple_structs.h:19:8"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "b",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:20:10"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s4_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "a",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "simple_structs.h:21:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s4_c",
            fieldType = HsPtr
              (HsPrimType HsPrimCInt),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "c",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypePrim
                    (PrimIntegral PrimInt Signed)),
                fieldSourceLoc =
                "simple_structs.h:22:10"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "S4"))
              DeclPathTop,
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "b",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:20:10"},
              StructField {
                fieldName = CName "a",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "simple_structs.h:21:9"},
              StructField {
                fieldName = CName "c",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypePrim
                    (PrimIntegral PrimInt Signed)),
                fieldSourceLoc =
                "simple_structs.h:22:10"}],
            structFlam = Nothing,
            structSourceLoc =
            "simple_structs.h:19:8"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:20:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s4_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:21:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s4_c",
                    fieldType = HsPtr
                      (HsPrimType HsPrimCInt),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        fieldSourceLoc =
                        "simple_structs.h:22:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "S4"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:20:10"},
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:21:9"},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        fieldSourceLoc =
                        "simple_structs.h:22:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:19:8"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:20:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s4_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:21:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s4_c",
                    fieldType = HsPtr
                      (HsPrimType HsPrimCInt),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        fieldSourceLoc =
                        "simple_structs.h:22:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "S4"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:20:10"},
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:21:9"},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        fieldSourceLoc =
                        "simple_structs.h:22:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:19:8"}}
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:27:10"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s5_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "simple_structs.h:28:9"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            (DeclNameTag (CName "S5"))
            DeclPathTop,
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:27:10"},
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "simple_structs.h:28:9"}],
          structFlam = Nothing,
          structSourceLoc =
          "simple_structs.h:26:16"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:27:10"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s5_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "simple_structs.h:28:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "S5"))
              DeclPathTop,
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:27:10"},
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "simple_structs.h:28:9"}],
            structFlam = Nothing,
            structSourceLoc =
            "simple_structs.h:26:16"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:27:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s5_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:28:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "S5"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:27:10"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:28:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:26:16"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:27:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s5_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:28:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "S5"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:27:10"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:28:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:26:16"}}
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:31:18"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s6_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "simple_structs.h:31:25"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            (DeclNameTag (CName "S6"))
            DeclPathTop,
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:31:18"},
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "simple_structs.h:31:25"}],
          structFlam = Nothing,
          structSourceLoc =
          "simple_structs.h:31:8"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:31:18"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s6_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "simple_structs.h:31:25"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "S6"))
              DeclPathTop,
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:31:18"},
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "simple_structs.h:31:25"}],
            structFlam = Nothing,
            structSourceLoc =
            "simple_structs.h:31:8"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:31:18"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s6_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:31:25"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "S6"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:31:18"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:31:25"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:31:8"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:31:18"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s6_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:31:25"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "S6"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:31:18"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:31:25"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:31:8"}}
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:34:23"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s7a_Deref_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "simple_structs.h:34:30"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            DeclNameNone
            (DeclPathPtr
              (DeclPathConstr
                DeclConstrStruct
                (DeclNameTypedef (CName "S7a"))
                DeclPathTop)),
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:34:23"},
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "simple_structs.h:34:30"}],
          structFlam = Nothing,
          structSourceLoc =
          "simple_structs.h:34:9"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:34:23"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s7a_Deref_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "simple_structs.h:34:30"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              DeclNameNone
              (DeclPathPtr
                (DeclPathConstr
                  DeclConstrStruct
                  (DeclNameTypedef (CName "S7a"))
                  DeclPathTop)),
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:34:23"},
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "simple_structs.h:34:30"}],
            structFlam = Nothing,
            structSourceLoc =
            "simple_structs.h:34:9"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:34:23"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s7a_Deref_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:34:30"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      DeclNameNone
                      (DeclPathPtr
                        (DeclPathConstr
                          DeclConstrStruct
                          (DeclNameTypedef (CName "S7a"))
                          DeclPathTop)),
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:34:23"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:34:30"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:34:9"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:34:23"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s7a_Deref_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:34:30"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      DeclNameNone
                      (DeclPathPtr
                        (DeclPathConstr
                          DeclConstrStruct
                          (DeclNameTypedef (CName "S7a"))
                          DeclPathTop)),
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:34:23"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:34:30"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:34:9"}}
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
          "unS7a",
        fieldType = HsPtr
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "S7a_Deref")),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "S7a",
          typedefType = TypePointer
            (TypeStruct
              (DeclPathConstr
                DeclConstrStruct
                DeclNameNone
                (DeclPathPtr
                  (DeclPathConstr
                    DeclConstrStruct
                    (DeclNameTypedef (CName "S7a"))
                    DeclPathTop)))),
          typedefSourceLoc =
          "simple_structs.h:34:36"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "S7a"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S7b_Deref_Deref_Deref",
      structConstr = HsName
        "@NsConstr"
        "S7b_Deref_Deref_Deref",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s7b_Deref_Deref_Deref_a",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:35:23"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s7b_Deref_Deref_Deref_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "simple_structs.h:35:30"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            DeclNameNone
            (DeclPathPtr
              (DeclPathPtr
                (DeclPathPtr
                  (DeclPathConstr
                    DeclConstrStruct
                    (DeclNameTypedef (CName "S7b"))
                    DeclPathTop)))),
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "simple_structs.h:35:23"},
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "simple_structs.h:35:30"}],
          structFlam = Nothing,
          structSourceLoc =
          "simple_structs.h:35:9"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "S7b_Deref_Deref_Deref",
        structConstr = HsName
          "@NsConstr"
          "S7b_Deref_Deref_Deref",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s7b_Deref_Deref_Deref_a",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:35:23"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s7b_Deref_Deref_Deref_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "simple_structs.h:35:30"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              DeclNameNone
              (DeclPathPtr
                (DeclPathPtr
                  (DeclPathPtr
                    (DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTypedef (CName "S7b"))
                      DeclPathTop)))),
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "simple_structs.h:35:23"},
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "simple_structs.h:35:30"}],
            structFlam = Nothing,
            structSourceLoc =
            "simple_structs.h:35:9"}}
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
                  "S7b_Deref_Deref_Deref",
                structConstr = HsName
                  "@NsConstr"
                  "S7b_Deref_Deref_Deref",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s7b_Deref_Deref_Deref_a",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:35:23"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s7b_Deref_Deref_Deref_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:35:30"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      DeclNameNone
                      (DeclPathPtr
                        (DeclPathPtr
                          (DeclPathPtr
                            (DeclPathConstr
                              DeclConstrStruct
                              (DeclNameTypedef (CName "S7b"))
                              DeclPathTop)))),
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:35:23"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:35:30"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:35:9"}})
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
                  "S7b_Deref_Deref_Deref",
                structConstr = HsName
                  "@NsConstr"
                  "S7b_Deref_Deref_Deref",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s7b_Deref_Deref_Deref_a",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:35:23"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s7b_Deref_Deref_Deref_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:35:30"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      DeclNameNone
                      (DeclPathPtr
                        (DeclPathPtr
                          (DeclPathPtr
                            (DeclPathConstr
                              DeclConstrStruct
                              (DeclNameTypedef (CName "S7b"))
                              DeclPathTop)))),
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "simple_structs.h:35:23"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "simple_structs.h:35:30"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "simple_structs.h:35:9"}}
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
      "S7b_Deref_Deref_Deref"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "S7b_Deref_Deref_Deref"),
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
          "unS7b",
        fieldType = HsPtr
          (HsPtr
            (HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "S7b_Deref_Deref_Deref")))),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "S7b",
          typedefType = TypePointer
            (TypePointer
              (TypePointer
                (TypeStruct
                  (DeclPathConstr
                    DeclConstrStruct
                    DeclNameNone
                    (DeclPathPtr
                      (DeclPathPtr
                        (DeclPathPtr
                          (DeclPathConstr
                            DeclConstrStruct
                            (DeclNameTypedef (CName "S7b"))
                            DeclPathTop)))))))),
          typedefSourceLoc =
          "simple_structs.h:35:38"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "S7b")]
