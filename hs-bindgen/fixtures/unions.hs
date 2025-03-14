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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "unions.h:2:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "dim2_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "unions.h:3:9"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            (DeclNameTag (CName "Dim2"))
            DeclPathTop,
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "unions.h:2:9"},
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "unions.h:3:9"}],
          structFlam = Nothing,
          structSourceLoc =
          "unions.h:1:8"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "unions.h:2:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "dim2_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "unions.h:3:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "Dim2"))
              DeclPathTop,
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "unions.h:2:9"},
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "unions.h:3:9"}],
            structFlam = Nothing,
            structSourceLoc =
            "unions.h:1:8"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:2:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim2_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:3:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "Dim2"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:2:9"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:3:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "unions.h:1:8"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:2:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim2_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:3:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "Dim2"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:2:9"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:3:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "unions.h:1:8"}}
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "unions.h:7:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "dim3_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "unions.h:8:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "dim3_z",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "z",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "unions.h:9:9"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            (DeclNameTag (CName "Dim3"))
            DeclPathTop,
          structSizeof = 12,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "unions.h:7:9"},
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "unions.h:8:9"},
            StructField {
              fieldName = CName "z",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "unions.h:9:9"}],
          structFlam = Nothing,
          structSourceLoc =
          "unions.h:6:8"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "unions.h:7:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "dim3_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "unions.h:8:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "dim3_z",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "z",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "unions.h:9:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "Dim3"))
              DeclPathTop,
            structSizeof = 12,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "unions.h:7:9"},
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "unions.h:8:9"},
              StructField {
                fieldName = CName "z",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "unions.h:9:9"}],
            structFlam = Nothing,
            structSourceLoc =
            "unions.h:6:8"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:7:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim3_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:8:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim3_z",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:9:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "Dim3"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:7:9"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:8:9"},
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:9:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "unions.h:6:8"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:7:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim3_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:8:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim3_z",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:9:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "Dim3"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:7:9"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:8:9"},
                      StructField {
                        fieldName = CName "z",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:9:9"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "unions.h:6:8"}}
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
  DeclEmpty
    (HsName
      "@NsTypeConstr"
      "DimPayload"),
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "tag",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "unions.h:18:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "dim_payload",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "DimPayload"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "payload",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypeUnion
                (DeclPathConstr
                  DeclConstrUnion
                  (DeclNameTag
                    (CName "DimPayload"))
                  DeclPathTop),
              fieldSourceLoc =
              "unions.h:19:22"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            (DeclNameTag (CName "Dim"))
            DeclPathTop,
          structSizeof = 12,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "tag",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "unions.h:18:9"},
            StructField {
              fieldName = CName "payload",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypeUnion
                (DeclPathConstr
                  DeclConstrUnion
                  (DeclNameTag
                    (CName "DimPayload"))
                  DeclPathTop),
              fieldSourceLoc =
              "unions.h:19:22"}],
          structFlam = Nothing,
          structSourceLoc =
          "unions.h:17:8"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "tag",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "unions.h:18:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "dim_payload",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "DimPayload"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "payload",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypeUnion
                  (DeclPathConstr
                    DeclConstrUnion
                    (DeclNameTag
                      (CName "DimPayload"))
                    DeclPathTop),
                fieldSourceLoc =
                "unions.h:19:22"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "Dim"))
              DeclPathTop,
            structSizeof = 12,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "tag",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "unions.h:18:9"},
              StructField {
                fieldName = CName "payload",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypeUnion
                  (DeclPathConstr
                    DeclConstrUnion
                    (DeclNameTag
                      (CName "DimPayload"))
                    DeclPathTop),
                fieldSourceLoc =
                "unions.h:19:22"}],
            structFlam = Nothing,
            structSourceLoc =
            "unions.h:17:8"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "tag",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:18:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim_payload",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "DimPayload"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "payload",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathConstr
                            DeclConstrUnion
                            (DeclNameTag
                              (CName "DimPayload"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "unions.h:19:22"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "Dim"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "tag",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:18:9"},
                      StructField {
                        fieldName = CName "payload",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathConstr
                            DeclConstrUnion
                            (DeclNameTag
                              (CName "DimPayload"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "unions.h:19:22"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "unions.h:17:8"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "tag",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:18:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dim_payload",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "DimPayload"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "payload",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathConstr
                            DeclConstrUnion
                            (DeclNameTag
                              (CName "DimPayload"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "unions.h:19:22"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "Dim"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "tag",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:18:9"},
                      StructField {
                        fieldName = CName "payload",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathConstr
                            DeclConstrUnion
                            (DeclNameTag
                              (CName "DimPayload"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "unions.h:19:22"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "unions.h:17:8"}}
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
    (HsName "@NsTypeConstr" "Dim"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Dim")]
