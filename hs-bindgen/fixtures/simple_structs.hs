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
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/simple_structs.h:3:9"}},
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
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/simple_structs.h:4:10"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "S1"))
            DeclPathTop,
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/simple_structs.h:3:9"},
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/simple_structs.h:4:10"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/simple_structs.h:2:8",
          structBitfields = []}},
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
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/simple_structs.h:3:9"}},
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
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/simple_structs.h:4:10"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "S1"))
              DeclPathTop,
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/simple_structs.h:3:9"},
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/simple_structs.h:4:10"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/simple_structs.h:2:8",
            structBitfields = []}}
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
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:3:9"}},
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:4:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "S1"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:3:9"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:4:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/simple_structs.h:2:8",
                    structBitfields = []}})
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
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:3:9"}},
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:4:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "S1"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:3:9"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:4:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/simple_structs.h:2:8",
                    structBitfields = []}}
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
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/simple_structs.h:9:10"}},
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
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/simple_structs.h:10:9"}},
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
              fieldType = TypePrim
                (PrimFloating PrimFloat),
              fieldSourceLoc =
              "examples/simple_structs.h:11:11"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "S2"))
            DeclPathTop,
          structSizeof = 12,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/simple_structs.h:9:10"},
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/simple_structs.h:10:9"},
            StructField {
              fieldName = CName "c",
              fieldOffset = 64,
              fieldType = TypePrim
                (PrimFloating PrimFloat),
              fieldSourceLoc =
              "examples/simple_structs.h:11:11"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/simple_structs.h:8:16",
          structBitfields = []}},
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
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/simple_structs.h:9:10"}},
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
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/simple_structs.h:10:9"}},
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
                fieldType = TypePrim
                  (PrimFloating PrimFloat),
                fieldSourceLoc =
                "examples/simple_structs.h:11:11"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "S2"))
              DeclPathTop,
            structSizeof = 12,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/simple_structs.h:9:10"},
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/simple_structs.h:10:9"},
              StructField {
                fieldName = CName "c",
                fieldOffset = 64,
                fieldType = TypePrim
                  (PrimFloating PrimFloat),
                fieldSourceLoc =
                "examples/simple_structs.h:11:11"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/simple_structs.h:8:16",
            structBitfields = []}}
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:9:10"}},
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
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:10:9"}},
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
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "examples/simple_structs.h:11:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "S2"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:9:10"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:10:9"},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "examples/simple_structs.h:11:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/simple_structs.h:8:16",
                    structBitfields = []}})
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:9:10"}},
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
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:10:9"}},
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
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "examples/simple_structs.h:11:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "S2"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:9:10"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:10:9"},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "examples/simple_structs.h:11:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/simple_structs.h:8:16",
                    structBitfields = []}}
              (Add 3)
              (Seq
                [
                  PokeByteOff (Idx 4) 0 (Idx 0),
                  PokeByteOff (Idx 4) 4 (Idx 1),
                  PokeByteOff
                    (Idx 4)
                    8
                    (Idx 2)])))}),
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
            (DeclPathStruct
              (DeclNameTag (CName "S2"))
              DeclPathTop),
          typedefSourceLoc =
          "examples/simple_structs.h:12:3"}},
  DeclNewtypeInstance
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
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/simple_structs.h:16:10"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTypedef (CName "S3_t"))
            DeclPathTop,
          structSizeof = 1,
          structAlignment = 1,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/simple_structs.h:16:10"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/simple_structs.h:15:9",
          structBitfields = []}},
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
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/simple_structs.h:16:10"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTypedef (CName "S3_t"))
              DeclPathTop,
            structSizeof = 1,
            structAlignment = 1,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/simple_structs.h:16:10"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/simple_structs.h:15:9",
            structBitfields = []}}
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:16:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTypedef (CName "S3_t"))
                      DeclPathTop,
                    structSizeof = 1,
                    structAlignment = 1,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:16:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/simple_structs.h:15:9",
                    structBitfields = []}})
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:16:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTypedef (CName "S3_t"))
                      DeclPathTop,
                    structSizeof = 1,
                    structAlignment = 1,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:16:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/simple_structs.h:15:9",
                    structBitfields = []}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
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
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/simple_structs.h:20:10"}},
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
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/simple_structs.h:21:9"}},
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
              fieldType = TypePointer
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              fieldSourceLoc =
              "examples/simple_structs.h:22:10"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "S4"))
            DeclPathTop,
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "b",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/simple_structs.h:20:10"},
            StructField {
              fieldName = CName "a",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/simple_structs.h:21:9"},
            StructField {
              fieldName = CName "c",
              fieldOffset = 64,
              fieldType = TypePointer
                (TypePrim
                  (PrimIntegral PrimInt Signed)),
              fieldSourceLoc =
              "examples/simple_structs.h:22:10"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/simple_structs.h:19:8",
          structBitfields = []}},
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
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/simple_structs.h:20:10"}},
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
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/simple_structs.h:21:9"}},
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
                fieldType = TypePointer
                  (TypePrim
                    (PrimIntegral PrimInt Signed)),
                fieldSourceLoc =
                "examples/simple_structs.h:22:10"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "S4"))
              DeclPathTop,
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "b",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/simple_structs.h:20:10"},
              StructField {
                fieldName = CName "a",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/simple_structs.h:21:9"},
              StructField {
                fieldName = CName "c",
                fieldOffset = 64,
                fieldType = TypePointer
                  (TypePrim
                    (PrimIntegral PrimInt Signed)),
                fieldSourceLoc =
                "examples/simple_structs.h:22:10"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/simple_structs.h:19:8",
            structBitfields = []}}
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:20:10"}},
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
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:21:9"}},
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
                        fieldType = TypePointer
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        fieldSourceLoc =
                        "examples/simple_structs.h:22:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "S4"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:20:10"},
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:21:9"},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        fieldSourceLoc =
                        "examples/simple_structs.h:22:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/simple_structs.h:19:8",
                    structBitfields = []}})
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:20:10"}},
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
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:21:9"}},
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
                        fieldType = TypePointer
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        fieldSourceLoc =
                        "examples/simple_structs.h:22:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "S4"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:20:10"},
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:21:9"},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypePrim
                            (PrimIntegral PrimInt Signed)),
                        fieldSourceLoc =
                        "examples/simple_structs.h:22:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/simple_structs.h:19:8",
                    structBitfields = []}}
              (Add 3)
              (Seq
                [
                  PokeByteOff (Idx 4) 0 (Idx 0),
                  PokeByteOff (Idx 4) 4 (Idx 1),
                  PokeByteOff
                    (Idx 4)
                    8
                    (Idx 2)])))}),
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
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/simple_structs.h:27:10"}},
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
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/simple_structs.h:28:9"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "S5"))
            DeclPathTop,
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/simple_structs.h:27:10"},
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/simple_structs.h:28:9"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/simple_structs.h:26:16",
          structBitfields = []}},
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
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/simple_structs.h:27:10"}},
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
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/simple_structs.h:28:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "S5"))
              DeclPathTop,
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/simple_structs.h:27:10"},
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/simple_structs.h:28:9"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/simple_structs.h:26:16",
            structBitfields = []}}
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:27:10"}},
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
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:28:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "S5"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:27:10"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:28:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/simple_structs.h:26:16",
                    structBitfields = []}})
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:27:10"}},
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
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:28:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "S5"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:27:10"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:28:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/simple_structs.h:26:16",
                    structBitfields = []}}
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
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/simple_structs.h:31:18"}},
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
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/simple_structs.h:31:25"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "S6"))
            DeclPathTop,
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/simple_structs.h:31:18"},
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/simple_structs.h:31:25"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/simple_structs.h:31:8",
          structBitfields = []}},
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
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/simple_structs.h:31:18"}},
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
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/simple_structs.h:31:25"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "S6"))
              DeclPathTop,
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/simple_structs.h:31:18"},
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/simple_structs.h:31:25"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/simple_structs.h:31:8",
            structBitfields = []}}
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:31:18"}},
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
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:31:25"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "S6"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:31:18"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:31:25"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/simple_structs.h:31:8",
                    structBitfields = []}})
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
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:31:18"}},
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
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:31:25"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "S6"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/simple_structs.h:31:18"},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/simple_structs.h:31:25"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/simple_structs.h:31:8",
                    structBitfields = []}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))})]
