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
          structDeclPath = DeclPathName
            (CName "Dim2")
            DeclPathCtxtTop,
          structAliases = [],
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
            structDeclPath = DeclPathName
              (CName "Dim2")
              DeclPathCtxtTop,
            structAliases = [],
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
                    structDeclPath = DeclPathName
                      (CName "Dim2")
                      DeclPathCtxtTop,
                    structAliases = [],
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
                    structDeclPath = DeclPathName
                      (CName "Dim2")
                      DeclPathCtxtTop,
                    structAliases = [],
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
          structDeclPath = DeclPathName
            (CName "Dim3")
            DeclPathCtxtTop,
          structAliases = [],
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
            structDeclPath = DeclPathName
              (CName "Dim3")
              DeclPathCtxtTop,
            structAliases = [],
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
                    structDeclPath = DeclPathName
                      (CName "Dim3")
                      DeclPathCtxtTop,
                    structAliases = [],
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
                    structDeclPath = DeclPathName
                      (CName "Dim3")
                      DeclPathCtxtTop,
                    structAliases = [],
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
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginUnion
        Union {
          unionDeclPath = DeclPathName
            (CName "DimPayload")
            DeclPathCtxtTop,
          unionAliases = [],
          unionSizeof = 8,
          unionAlignment = 4,
          unionFields = [
            UnionField {
              ufieldName = CName "dim2",
              ufieldType = TypeStruct
                (DeclPathName
                  (CName "Dim2")
                  DeclPathCtxtTop),
              ufieldSourceLoc =
              "unions.h:13:17"},
            UnionField {
              ufieldName = CName "dim3",
              ufieldType = TypeStruct
                (DeclPathName
                  (CName "Dim2")
                  DeclPathCtxtTop),
              ufieldSourceLoc =
              "unions.h:14:17"}],
          unionSourceLoc =
          "unions.h:12:7"}},
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
                (DeclPathName
                  (CName "DimPayload")
                  DeclPathCtxtTop),
              fieldSourceLoc =
              "unions.h:19:22"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "Dim")
            DeclPathCtxtTop,
          structAliases = [],
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
                (DeclPathName
                  (CName "DimPayload")
                  DeclPathCtxtTop),
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
                  (DeclPathName
                    (CName "DimPayload")
                    DeclPathCtxtTop),
                fieldSourceLoc =
                "unions.h:19:22"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "Dim")
              DeclPathCtxtTop,
            structAliases = [],
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
                  (DeclPathName
                    (CName "DimPayload")
                    DeclPathCtxtTop),
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
                          (DeclPathName
                            (CName "DimPayload")
                            DeclPathCtxtTop),
                        fieldSourceLoc =
                        "unions.h:19:22"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "Dim")
                      DeclPathCtxtTop,
                    structAliases = [],
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
                          (DeclPathName
                            (CName "DimPayload")
                            DeclPathCtxtTop),
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
                          (DeclPathName
                            (CName "DimPayload")
                            DeclPathCtxtTop),
                        fieldSourceLoc =
                        "unions.h:19:22"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "Dim")
                      DeclPathCtxtTop,
                    structAliases = [],
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
                          (DeclPathName
                            (CName "DimPayload")
                            DeclPathCtxtTop),
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
    (HsName "@NsTypeConstr" "Dim"),
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
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginUnion
        Union {
          unionDeclPath = DeclPathName
            (CName "DimPayloadB")
            DeclPathCtxtTop,
          unionAliases = [
            CName "DimPayloadB"],
          unionSizeof = 8,
          unionAlignment = 4,
          unionFields = [
            UnionField {
              ufieldName = CName "dim2",
              ufieldType = TypeStruct
                (DeclPathName
                  (CName "Dim2")
                  DeclPathCtxtTop),
              ufieldSourceLoc =
              "unions.h:24:17"},
            UnionField {
              ufieldName = CName "dim3",
              ufieldType = TypeStruct
                (DeclPathName
                  (CName "Dim2")
                  DeclPathCtxtTop),
              ufieldSourceLoc =
              "unions.h:25:17"}],
          unionSourceLoc =
          "unions.h:23:15"}},
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "tag",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "unions.h:29:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "dimB_payload",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "DimPayloadB"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "payload",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypeUnion
                (DeclPathName
                  (CName "DimPayloadB")
                  DeclPathCtxtTop),
              fieldSourceLoc =
              "unions.h:30:17"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "DimB")
            DeclPathCtxtTop,
          structAliases = [],
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
              "unions.h:29:9"},
            StructField {
              fieldName = CName "payload",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypeUnion
                (DeclPathName
                  (CName "DimPayloadB")
                  DeclPathCtxtTop),
              fieldSourceLoc =
              "unions.h:30:17"}],
          structFlam = Nothing,
          structSourceLoc =
          "unions.h:28:8"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "tag",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "unions.h:29:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "dimB_payload",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "DimPayloadB"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "payload",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypeUnion
                  (DeclPathName
                    (CName "DimPayloadB")
                    DeclPathCtxtTop),
                fieldSourceLoc =
                "unions.h:30:17"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "DimB")
              DeclPathCtxtTop,
            structAliases = [],
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
                "unions.h:29:9"},
              StructField {
                fieldName = CName "payload",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypeUnion
                  (DeclPathName
                    (CName "DimPayloadB")
                    DeclPathCtxtTop),
                fieldSourceLoc =
                "unions.h:30:17"}],
            structFlam = Nothing,
            structSourceLoc =
            "unions.h:28:8"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "tag",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:29:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dimB_payload",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "DimPayloadB"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "payload",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathName
                            (CName "DimPayloadB")
                            DeclPathCtxtTop),
                        fieldSourceLoc =
                        "unions.h:30:17"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "DimB")
                      DeclPathCtxtTop,
                    structAliases = [],
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
                        "unions.h:29:9"},
                      StructField {
                        fieldName = CName "payload",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathName
                            (CName "DimPayloadB")
                            DeclPathCtxtTop),
                        fieldSourceLoc =
                        "unions.h:30:17"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "unions.h:28:8"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "tag",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "unions.h:29:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "dimB_payload",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "DimPayloadB"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "payload",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathName
                            (CName "DimPayloadB")
                            DeclPathCtxtTop),
                        fieldSourceLoc =
                        "unions.h:30:17"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "DimB")
                      DeclPathCtxtTop,
                    structAliases = [],
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
                        "unions.h:29:9"},
                      StructField {
                        fieldName = CName "payload",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeUnion
                          (DeclPathName
                            (CName "DimPayloadB")
                            DeclPathCtxtTop),
                        fieldSourceLoc =
                        "unions.h:30:17"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "unions.h:28:8"}}
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
    (HsName "@NsTypeConstr" "DimB"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "DimB"),
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimFloating PrimDouble),
              fieldSourceLoc =
              "unions.h:35:21"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "anonA_xy_y",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "y",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimFloating PrimDouble),
              fieldSourceLoc =
              "unions.h:35:31"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathAnon
            (DeclPathCtxtField
              (Just (CName "AnonA"))
              (CName "xy")
              DeclPathCtxtTop),
          structAliases = [],
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimFloating PrimDouble),
              fieldSourceLoc =
              "unions.h:35:21"},
            StructField {
              fieldName = CName "y",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimFloating PrimDouble),
              fieldSourceLoc =
              "unions.h:35:31"}],
          structFlam = Nothing,
          structSourceLoc =
          "unions.h:35:5"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimFloating PrimDouble),
                fieldSourceLoc =
                "unions.h:35:21"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "anonA_xy_y",
            fieldType = HsPrimType
              HsPrimCDouble,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "y",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimFloating PrimDouble),
                fieldSourceLoc =
                "unions.h:35:31"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathAnon
              (DeclPathCtxtField
                (Just (CName "AnonA"))
                (CName "xy")
                DeclPathCtxtTop),
            structAliases = [],
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimFloating PrimDouble),
                fieldSourceLoc =
                "unions.h:35:21"},
              StructField {
                fieldName = CName "y",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimFloating PrimDouble),
                fieldSourceLoc =
                "unions.h:35:31"}],
            structFlam = Nothing,
            structSourceLoc =
            "unions.h:35:5"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:35:21"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "anonA_xy_y",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:35:31"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathAnon
                      (DeclPathCtxtField
                        (Just (CName "AnonA"))
                        (CName "xy")
                        DeclPathCtxtTop),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:35:21"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:35:31"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "unions.h:35:5"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:35:21"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "anonA_xy_y",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:35:31"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathAnon
                      (DeclPathCtxtField
                        (Just (CName "AnonA"))
                        (CName "xy")
                        DeclPathCtxtTop),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:35:21"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:35:31"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "unions.h:35:5"}}
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
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "r",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimFloating PrimDouble),
              fieldSourceLoc =
              "unions.h:36:21"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "anonA_polar_p",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "p",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimFloating PrimDouble),
              fieldSourceLoc =
              "unions.h:36:31"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathAnon
            (DeclPathCtxtField
              (Just (CName "AnonA"))
              (CName "polar")
              DeclPathCtxtTop),
          structAliases = [],
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "r",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimFloating PrimDouble),
              fieldSourceLoc =
              "unions.h:36:21"},
            StructField {
              fieldName = CName "p",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimFloating PrimDouble),
              fieldSourceLoc =
              "unions.h:36:31"}],
          structFlam = Nothing,
          structSourceLoc =
          "unions.h:36:5"}},
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
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "r",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimFloating PrimDouble),
                fieldSourceLoc =
                "unions.h:36:21"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "anonA_polar_p",
            fieldType = HsPrimType
              HsPrimCDouble,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "p",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimFloating PrimDouble),
                fieldSourceLoc =
                "unions.h:36:31"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathAnon
              (DeclPathCtxtField
                (Just (CName "AnonA"))
                (CName "polar")
                DeclPathCtxtTop),
            structAliases = [],
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "r",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimFloating PrimDouble),
                fieldSourceLoc =
                "unions.h:36:21"},
              StructField {
                fieldName = CName "p",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimFloating PrimDouble),
                fieldSourceLoc =
                "unions.h:36:31"}],
            structFlam = Nothing,
            structSourceLoc =
            "unions.h:36:5"}}
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "r",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:36:21"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "anonA_polar_p",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "p",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:36:31"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathAnon
                      (DeclPathCtxtField
                        (Just (CName "AnonA"))
                        (CName "polar")
                        DeclPathCtxtTop),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "r",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:36:21"},
                      StructField {
                        fieldName = CName "p",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:36:31"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "unions.h:36:5"}})
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
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "r",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:36:21"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "anonA_polar_p",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "p",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:36:31"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathAnon
                      (DeclPathCtxtField
                        (Just (CName "AnonA"))
                        (CName "polar")
                        DeclPathCtxtTop),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "r",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:36:21"},
                      StructField {
                        fieldName = CName "p",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "unions.h:36:31"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "unions.h:36:5"}}
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
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginUnion
        Union {
          unionDeclPath = DeclPathName
            (CName "AnonA")
            DeclPathCtxtTop,
          unionAliases = [],
          unionSizeof = 16,
          unionAlignment = 8,
          unionFields = [
            UnionField {
              ufieldName = CName "xy",
              ufieldType = TypeStruct
                (DeclPathAnon
                  (DeclPathCtxtField
                    (Just (CName "AnonA"))
                    (CName "xy")
                    DeclPathCtxtTop)),
              ufieldSourceLoc =
              "unions.h:35:36"},
            UnionField {
              ufieldName = CName "polar",
              ufieldType = TypeStruct
                (DeclPathAnon
                  (DeclPathCtxtField
                    (Just (CName "AnonA"))
                    (CName "polar")
                    DeclPathCtxtTop)),
              ufieldSourceLoc =
              "unions.h:36:36"}],
          unionSourceLoc =
          "unions.h:34:7"}},
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
